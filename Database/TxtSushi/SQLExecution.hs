-----------------------------------------------------------------------------
-- |
-- Module      :  Database.TxtSushi.SQLExecution
-- Copyright   :  (c) Keith Sheppard 2009
-- License     :  GPL3 or greater
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for executing a SQL statement
--
-----------------------------------------------------------------------------

module Database.TxtSushi.SQLExecution (
    select,
    databaseTableToTextTable,
    textTableToDatabaseTable) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Text.Regex.Posix

import Database.TxtSushi.SQLParser
import Database.TxtSushi.Transform
import Database.TxtSushi.Util.ListUtil

-- | an SQL table data structure
--   TODO: need allColumnsColumnIdentifiers and allColumnsTableRows so that
--         we can filter and order on columns that are selected out. we also
--         should track any column ordering that is in place
data DatabaseTable = DatabaseTable {
    -- | the columns in this table
    columnIdentifiers :: [ColumnIdentifier],
    
    -- | the actual table data
    tableRows :: [[EvaluatedExpression]]}

emptyTable = DatabaseTable [] []

stringExpression :: String -> EvaluatedExpression
stringExpression string = EvaluatedExpression {
    preferredType   = StringType,
    maybeIntValue   = maybeReadInt string,
    maybeRealValue  = maybeReadReal string,
    stringValue     = string,
    maybeBoolValue  = Just $
        (map toLower string /= "false") && (string /= "") && (string /= "0")}

intExpression int = EvaluatedExpression {
    preferredType   = IntType,
    maybeIntValue   = Just int,
    maybeRealValue  = Just $ fromIntegral int,
    stringValue     = show int,
    maybeBoolValue  = Just $ int /= 0}

realExpression real = EvaluatedExpression {
    preferredType   = RealType,
    maybeIntValue   = Just $ floor real,
    maybeRealValue  = Just real,
    stringValue     = show real,
    maybeBoolValue  = Just $ real /= 0.0}

boolExpression bool = EvaluatedExpression {
    preferredType   = BoolType,
    maybeIntValue   = Nothing,
    maybeRealValue  = Nothing,
    stringValue     = show bool,
    maybeBoolValue  = Just bool}

intValue :: EvaluatedExpression -> Int
intValue evalExpr = case maybeIntValue evalExpr of
    Just int -> int
    Nothing ->
        error $ "could not convert \"" ++ (stringValue evalExpr) ++
                "\" to an integer value"

realValue :: EvaluatedExpression -> Double
realValue evalExpr = case maybeRealValue evalExpr of
    Just real -> real
    Nothing ->
        error $ "could not convert \"" ++ (stringValue evalExpr) ++
                "\" to a numeric value"

boolValue :: EvaluatedExpression -> Bool
boolValue evalExpr = case maybeBoolValue evalExpr of
    Just bool -> bool
    Nothing ->
        error $ "could not convert \"" ++ (stringValue evalExpr) ++
                "\" to a boolean value"

data ExpressionType = StringType | RealType | IntType | BoolType deriving Eq

data EvaluatedExpression = EvaluatedExpression {
    preferredType   :: ExpressionType,
    stringValue     :: String,
    maybeRealValue  :: Maybe Double,
    maybeIntValue   :: Maybe Int,
    maybeBoolValue  :: Maybe Bool}

maybeReadBool :: String -> Maybe Bool
maybeReadBool boolStr = case map toLower boolStr of
    "true"      -> Just True
    "1"         -> Just True
    "1.0"       -> Just True
    "false"     -> Just False
    "0"         -> Just False
    "0.0"       -> Just False
    otherwise   -> Nothing

instance Eq EvaluatedExpression where
    -- base off of the Ord definition
    expr1 == expr2 = compare expr1 expr2 == EQ

instance Ord EvaluatedExpression where
    compare expr1 expr2
        | type1 == RealType || type2 == RealType    = realCompare expr1 expr2
        | type1 == IntType || type2 == IntType      = intCompare expr1 expr2
        | type1 == BoolType || type2 == BoolType    = boolCompare expr1 expr2
        | otherwise                                 = stringCompare expr1 expr2
        
        where
            type1 = preferredType expr1
            type2 = preferredType expr2

realCompare (EvaluatedExpression _ _ (Just r1) _ _) (EvaluatedExpression _ _ (Just r2) _ _) =
    compare r1 r2
realCompare expr1 expr2 = stringCompare expr1 expr2

intCompare (EvaluatedExpression _ _ _ (Just i1) _) (EvaluatedExpression _ _ _ (Just i2) _) =
    compare i1 i2
intCompare expr1 expr2 = realCompare expr1 expr2

boolCompare (EvaluatedExpression _ _ _ _ (Just b1)) (EvaluatedExpression _ _ _ _ (Just b2)) =
    compare b1 b2
boolCompare expr1 expr2 = stringCompare expr1 expr2

stringCompare expr1 expr2 = stringValue expr1 `compare` stringValue expr2

-- convert a text table to a database table by using the 1st row as column IDs
textTableToDatabaseTable :: String -> [[String]] -> DatabaseTable
textTableToDatabaseTable tableName (headerNames:tblRows) =
    DatabaseTable (map makeColId headerNames) (map (map stringExpression) tblRows)
    where
        makeColId colName = ColumnIdentifier (Just tableName) colName

databaseTableToTextTable :: DatabaseTable -> [[String]]
databaseTableToTextTable dbTable =
    let
        headerRow = (map columnId (columnIdentifiers dbTable))
        tailRows = map (map stringValue) (tableRows dbTable)
    in
        headerRow:tailRows

-- | perform a SQL select with the given select statement on the
--   given table map
select :: SelectStatement -> (Map.Map String DatabaseTable) -> DatabaseTable
select selectStatement tableMap =
    let
        -- TODO: do we need to care about the updated aliases for filtering
        --       in the "where" part??
        fromTbl = case maybeFromTable selectStatement of
            Nothing -> emptyTable
            Just fromTblExpr -> evalTableExpression fromTblExpr tableMap
        filteredTbl = case maybeWhereFilter selectStatement of
            Nothing -> fromTbl
            Just expr -> filterRowsBy expr fromTbl
        orderedTbl = orderRowsBy (orderByItems selectStatement) filteredTbl
        selectedTbl =
            evaluateColumnSelections (columnSelections selectStatement) orderedTbl
    in
        selectedTbl

-- | sorts table rows by the given order by items
orderRowsBy :: [OrderByItem] -> DatabaseTable -> DatabaseTable
orderRowsBy [] dbTable = dbTable
orderRowsBy orderBys dbTable =
    let
        -- curry in the order and col ID params to make a row comparison function
        compareRows = compareRowsOnOrderItems orderBys (columnIdentifiers dbTable)
        sortedRows = sortBy compareRows (tableRows dbTable)
    in
        dbTable {tableRows = sortedRows}

-- | Compares two rows using the given OrderByItems and column ID's
compareRowsOnOrderItems :: [OrderByItem] -> [ColumnIdentifier] -> [EvaluatedExpression] -> [EvaluatedExpression] -> Ordering
compareRowsOnOrderItems orderBys colIds row1 row2 =
    cascadingOrder $ toOrderList orderBys
    where
        toOrderList [] = []
        toOrderList (orderBy:orderByTail) =
            (compareRowsOnOrderItem orderBy colIds row1 row2):(toOrderList orderByTail)

-- | Compares two rows using the given OrderByItem and column ID's
compareRowsOnOrderItem :: OrderByItem -> [ColumnIdentifier] -> [EvaluatedExpression] -> [EvaluatedExpression] -> Ordering
compareRowsOnOrderItem orderBy colIds row1 row2 =
    let
        orderExpr = orderExpression orderBy
        rowComp = compareRowsOnExpression orderExpr colIds row1 row2
    in
        if orderAscending orderBy then
            rowComp
        else
            reverseOrdering rowComp

-- | reverses the given ordering. pretty CRAZY huh???
reverseOrdering :: Ordering -> Ordering
reverseOrdering EQ = EQ
reverseOrdering LT = GT
reverseOrdering GT = LT

-- | Compares two rows using the given expressions
compareRowsOnExpressions :: [Expression] -> [ColumnIdentifier] -> [EvaluatedExpression] -> [EvaluatedExpression] -> Ordering
compareRowsOnExpressions exprs colIds row1 row2 =
    cascadingOrder $ toOrderList exprs
    where
        toOrderList [] = []
        toOrderList (expr:exprTail) =
            (compareRowsOnExpression expr colIds row1 row2):(toOrderList exprTail)

-- | Compares two rows using the given expression
compareRowsOnExpression :: Expression -> [ColumnIdentifier] -> [EvaluatedExpression] -> [EvaluatedExpression] -> Ordering
compareRowsOnExpression expr colIds row1 row2 =
    let
        row1Eval = evalExpression expr colIds row1
        row2Eval = evalExpression expr colIds row2
    in
        row1Eval `compare` row2Eval

groupRowsBy :: [Expression] -> DatabaseTable -> [DatabaseTable]
groupRowsBy groupByExprs dbTable =
    -- create a new table for every row grouping
    map replaceTableRows rowGroups
    where
        tblRows = tableRows dbTable
        
        -- curry in the exprs and col ID params to make a row comparison function
        compareRows = compareRowsOnExpressions groupByExprs (columnIdentifiers dbTable)
        rowsEq row1 row2 = row1 `compareRows` row2 == EQ
        
        sortedRows = sortBy compareRows tblRows
        rowGroups = groupBy rowsEq sortedRows
        replaceTableRows newRows = dbTable {tableRows = newRows}

-- | Evaluate the FROM table part, and returns the FROM table. Also returns
--   a mapping of new table names from aliases etc.
evalTableExpression :: TableExpression -> (Map.Map String DatabaseTable) -> DatabaseTable
evalTableExpression tblExpr tableMap =
    case tblExpr of
        TableIdentifier tblName maybeTblAlias ->
            let
                -- find the from table map (error if missing)
                noTblError = error $ "failed to find table named " ++ tblName
                table = Map.findWithDefault noTblError tblName tableMap
            in
                maybeRename maybeTblAlias table
        
        -- TODO inner join should allow joining on expressions too!!
        InnerJoin leftJoinTblExpr rightJoinTblExpr onConditionExpr maybeTblAlias ->
            let
                leftJoinTbl = evalTableExpression leftJoinTblExpr tableMap
                rightJoinTbl = evalTableExpression rightJoinTblExpr tableMap
                joinCols = extractJoinCols onConditionExpr
                joinIndices = joinColumnIndices leftJoinTbl rightJoinTbl joinCols
                joinedTbl = innerJoin joinIndices leftJoinTbl rightJoinTbl
            in
                maybeRename maybeTblAlias joinedTbl
        
        -- TODO implement me
        CrossJoin leftJoinTbl maybeTblAlias rightJoinTbl ->
            error "Sorry! CROSS JOIN is not yet implemented"
    where
        maybeRename :: (Maybe String) -> DatabaseTable -> DatabaseTable
        maybeRename Nothing table = table
        maybeRename (Just newName) table = table {
            columnIdentifiers = map (\colId -> colId {maybeTableName = Just newName}) (columnIdentifiers table)}

extractJoinCols (FunctionExpression sqlFunc [arg1, arg2]) =
    case sqlFunc of
        SQLFunction "AND" _ _   -> extractJoinCols arg1 ++ extractJoinCols arg2
        SQLFunction "=" _ _     -> extractJoinColPair arg1 arg2
        
        -- Only expecting "AND" or "="
        otherwise -> onPartFormattingError
    where
        extractJoinColPair (ColumnExpression col1) (ColumnExpression col2) = [(col1, col2)]
        
        -- Only expecting "AND" or "="
        extractJoinColPair _ _ = onPartFormattingError

-- Only expecting "AND" or "="
extractJoinCols _ = onPartFormattingError

onPartFormattingError =
    error $ "The \"ON\" part of a join must only contain column equalities " ++
            "joined together by \"AND\" like: " ++
            "\"tbl1.id1 = table2.id1 AND tbl1.firstname = tbl2.name\""

-- | perform an inner join using the given join indices on the given
--   tables
innerJoin :: [(Int, Int)] -> DatabaseTable -> DatabaseTable -> DatabaseTable
innerJoin joinIndices leftJoinTbl rightJoinTbl = DatabaseTable {
    columnIdentifiers = (columnIdentifiers leftJoinTbl) ++ (columnIdentifiers rightJoinTbl),
    tableRows = joinTables joinIndices (tableRows leftJoinTbl) (tableRows rightJoinTbl)}

-- | convert the column ID pairs into index pairs
joinColumnIndices :: DatabaseTable -> DatabaseTable -> [(ColumnIdentifier, ColumnIdentifier)] -> [(Int, Int)]
joinColumnIndices leftJoinTbl rightJoinTbl joinCols =
    let
        leftHeader = columnIdentifiers leftJoinTbl
        rightHeader = columnIdentifiers rightJoinTbl
    in
        map (idPairToIndexPair leftHeader rightHeader) joinCols

-- | convert the column ID pair into an index pair
idPairToIndexPair :: [ColumnIdentifier] -> [ColumnIdentifier] -> (ColumnIdentifier, ColumnIdentifier) -> (Int, Int)
idPairToIndexPair leftColIds rightColIds joinColPair@(leftColId, rightColId) =
    let
        maybePairInOrder = maybeIdPairToIndexPair leftColIds rightColIds joinColPair
        maybePairSwapped = maybeIdPairToIndexPair leftColIds rightColIds (rightColId, leftColId)
    in
        case maybePairInOrder of
            Just thePairInOrder -> thePairInOrder
            Nothing ->
                case maybePairSwapped of
                    Just thePairSwapped -> thePairSwapped
                    Nothing -> error "failed to find given columns"

maybeIdPairToIndexPair :: [ColumnIdentifier] -> [ColumnIdentifier] -> (ColumnIdentifier, ColumnIdentifier) -> Maybe (Int, Int)
maybeIdPairToIndexPair leftColIds rightColIds (leftColId, rightColId) = do
    leftIndex <- findIndex (== leftColId) leftColIds
    rightIndex <- findIndex (== rightColId) rightColIds
    return (leftIndex, rightIndex)

evaluateColumnSelections :: [ColumnSelection] -> DatabaseTable -> DatabaseTable
evaluateColumnSelections colSelections dbTable =
    let
        selectionTbls = map ($ dbTable) (map evaluateColumnSelection colSelections)
    in
        foldl1' tableConcat selectionTbls

tableConcat :: DatabaseTable -> DatabaseTable -> DatabaseTable
tableConcat dbTable1 dbTable2 =
    let
        concatIds = (columnIdentifiers dbTable1) ++ (columnIdentifiers dbTable2)
        concatRows = zipWith (++) (tableRows dbTable1) (tableRows dbTable2)
    in
        DatabaseTable concatIds concatRows

evaluateColumnSelection :: ColumnSelection -> DatabaseTable -> DatabaseTable
evaluateColumnSelection AllColumns dbTable = dbTable
evaluateColumnSelection (AllColumnsFrom srcTblName) dbTable =
    let
        colIds = columnIdentifiers dbTable
        indices = findIndices matchesSrcTblName (map maybeTableName colIds)
        selectedColIds = selectIndices indices colIds
        selectedColRows = map (selectIndices indices) (tableRows dbTable)
    in
        DatabaseTable selectedColIds selectedColRows
    where
        matchesSrcTblName Nothing           = False
        matchesSrcTblName (Just tblName)    = tblName == srcTblName
        selectIndices indices xs = [xs !! i | i <- indices]
evaluateColumnSelection (ExpressionColumn expr) dbTable =
    let
        tblColIds = columnIdentifiers dbTable
        exprColId = expressionIdentifier expr
        evaluatedExprs = map (evalExpression expr tblColIds) (tableRows dbTable)
    in
        DatabaseTable [exprColId] (transpose [evaluatedExprs])

-- | This is a little different that a strict equals compare in that it returns
--   true if the query column has a Nothing table and the column name part
--   matches the reference column's name. Also not that this makes it
--   an asymetric comparison
columnMatches :: ColumnIdentifier -> ColumnIdentifier -> Bool
columnMatches (ColumnIdentifier Nothing queryColIdStr) referenceColumn =
    -- In this case we don't care about the table name so
    -- just check to make sure that the column names match up
    queryColIdStr == columnId referenceColumn

columnMatches queryColumn referenceColumn =
    -- table name is important here so match on the whole object
    queryColumn == referenceColumn

-- | filters the database's table rows on the given expression
filterRowsBy :: Expression -> DatabaseTable -> DatabaseTable
filterRowsBy filterExpr table =
    table {tableRows = filter myBoolEvalExpr (tableRows table)}
    where myBoolEvalExpr row =
            boolValue $ evalExpression filterExpr (columnIdentifiers table) row

evalExpression :: Expression -> [ColumnIdentifier] -> [EvaluatedExpression] -> EvaluatedExpression
-- Here's the easy stuff. evaluate constants
evalExpression (StringConstantExpression string) _ _ = stringExpression string
evalExpression (IntegerConstantExpression int) _ _ = intExpression int
evalExpression (RealConstantExpression real) _ _ = realExpression real

-- A little bit harder. evaluate a column expression
evalExpression (ColumnExpression col) columnIds tblRow =
    case findIndex (columnMatches col) columnIds of
        Just colIndex -> tblRow !! colIndex
        Nothing -> error $ "Failed to find column named: " ++ (prettyFormatColumn col)

-- this is where the action is. evaluate a function
evalExpression (FunctionExpression sqlFun funArgs) columnIds tblRow
    -- String functions
    | sqlFun == upperFunction = stringExpression $ map toUpper (stringValue arg1)
    | sqlFun == lowerFunction = stringExpression $ map toLower (stringValue arg1)
    | sqlFun == trimFunction = stringExpression $ trimSpace (stringValue arg1)
    | sqlFun == concatenateFunction = stringExpression $ concat (map stringValue evaluatedArgs)
    | sqlFun == substringFromToFunction =
        stringExpression $ take (intValue arg3) (drop (intValue arg2 - 1) (stringValue arg1))
    | sqlFun == substringFromFunction =
        stringExpression $ drop (intValue arg2 - 1) (stringValue arg1)
    | sqlFun == regexMatchFunction = boolExpression $ (stringValue arg1) =~ (stringValue arg2)
    
    -- negate
    | sqlFun == negateFunction =
        if length evaluatedArgs /= 1 then
            error "internal error: found a negate function with multiple args"
        else
            let evaldArg = head evaluatedArgs
            in
                if useRealAlgebra evaldArg then
                    realExpression $ negate (realValue evaldArg)
                else
                    intExpression $ negate (intValue evaldArg)
    
    -- algebraic
    | sqlFun == multiplyFunction = algebraWithCoercion (*) (*) evaluatedArgs
    | sqlFun == divideFunction = realExpression $ (realValue arg1) / (realValue arg2)
    | sqlFun == plusFunction = algebraWithCoercion (+) (+) evaluatedArgs
    | sqlFun == minusFunction = algebraWithCoercion (-) (-) evaluatedArgs
    
    -- boolean
    | sqlFun == isFunction = boolExpression (arg1 == arg2)
    | sqlFun == isNotFunction = boolExpression (arg1 /= arg2)
    | sqlFun == lessThanFunction = boolExpression (arg1 < arg2)
    | sqlFun == lessThanOrEqualToFunction = boolExpression (arg1 <= arg2)
    | sqlFun == greaterThanFunction = boolExpression (arg1 > arg2)
    | sqlFun == greaterThanOrEqualToFunction = boolExpression (arg1 >= arg2)
    | sqlFun == andFunction = boolExpression $ (boolValue arg1) && (boolValue arg2)
    | sqlFun == orFunction = boolExpression $ (boolValue arg1) || (boolValue arg2)
    | sqlFun == notFunction = boolExpression $ not (boolValue arg1)
    
    where
        arg1 = head evaluatedArgs
        arg2 = evaluatedArgs !! 1
        arg3 = evaluatedArgs !! 2
        subStringF start extent string = take extent (drop start string)
        evaluatedArgs = map evalArgExpr funArgs
        evalArgExpr expr = evalExpression expr columnIds tblRow
        algebraWithCoercion intFunc realFunc args =
            if any useRealAlgebra args then
                realExpression $ foldl1' realFunc (map realValue args)
            else
                intExpression $ foldl1' intFunc (map intValue args)
        
        useRealAlgebra expr =
            let
                prefType = preferredType expr
                maybeInt = maybeIntValue expr
            in
                prefType == RealType || maybeInt == Nothing

-- | trims leading and trailing spaces
trimSpace :: String -> String
trimSpace = f . f
   where f = reverse . dropWhile isSpace

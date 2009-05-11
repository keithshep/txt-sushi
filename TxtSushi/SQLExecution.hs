-----------------------------------------------------------------------------
-- |
-- Module      :  TxtSushi.SQLExecution
-- Copyright   :  (c) Keith Sheppard 2009
-- License     :  GPL3 or greater
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for executing a SQL statement
--
-----------------------------------------------------------------------------

module TxtSushi.SQLExecution (
    select,
    databaseTableToTextTable,
    textTableToDatabaseTable) where

import Data.Char
import Data.List
import qualified Data.Map as Map

import TxtSushi.SQLParser
import TxtSushi.Transform

-- | an SQL table data structure
--   TODO: need allColumnsColumnIdentifiers and allColumnsTableRows so that
--         we can filter and order on columns that are selected out. we also
--         should track any column ordering that is in place
data DatabaseTable = DatabaseTable {
    -- | the columns in this table
    columnIdentifiers :: [ColumnIdentifier],
    
    -- | the actual table data
    tableRows :: [[String]]}

emptyTable = DatabaseTable [] []

-- convert a text table to a database table by using the 1st row as column IDs
textTableToDatabaseTable :: String -> [[String]] -> DatabaseTable
textTableToDatabaseTable tableName (headerNames:tblRows) =
    DatabaseTable (map makeColId headerNames) tblRows
    where
        makeColId colName = ColumnIdentifier (Just tableName) colName

databaseTableToTextTable :: DatabaseTable -> [[String]]
databaseTableToTextTable dbTable =
    (map columnId (columnIdentifiers dbTable)) : tableRows dbTable

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
            selectTableColumns (columnSelections selectStatement) orderedTbl
    in
        selectedTbl

-- | sorts table rows by the given order by items
orderRowsBy :: [OrderByItem] -> DatabaseTable -> DatabaseTable
orderRowsBy [] dbTable = dbTable
orderRowsBy orderBys dbTable =
    let
        -- curry in the order and col ID params to make a row comparison function
        compareFunc = compareRowsOnOrderItems orderBys (columnIdentifiers dbTable)
    in
        dbTable {tableRows = sortBy compareFunc (tableRows dbTable)}

-- | Compares two rows using the given OrderByItem and column ID's
compareRowsOnOrderItems :: [OrderByItem] -> [ColumnIdentifier] -> [String] -> [String] -> Ordering
compareRowsOnOrderItems orderBys colIds row1 row2 =
    cascadingOrder $ toOrderList orderBys
    where
        toOrderList [] = []
        toOrderList (orderBy:orderByTail) =
            (compareRowsOnOrderItem orderBy colIds row1 row2):(toOrderList orderByTail)

-- | Compares two rows using the given OrderByItem and column ID's
compareRowsOnOrderItem :: OrderByItem -> [ColumnIdentifier] -> [String] -> [String] -> Ordering
compareRowsOnOrderItem orderBy colIds row1 row2 =
    let
        orderExpr = orderExpression orderBy
        row1Eval = evalExpression orderExpr colIds row1
        row2Eval = evalExpression orderExpr colIds row2
        rowComp = row1Eval `compare` row2Eval
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

-- | applies a cascading order logic where 1st non-equal ordering defines
--   the ordering for the list. If they're all equal (or the list is empty)
--   then return EQ
cascadingOrder :: [Ordering] -> Ordering
cascadingOrder [] = EQ
cascadingOrder (LT:_) = LT
cascadingOrder (GT:_) = GT
cascadingOrder (EQ:tailOrders) = cascadingOrder tailOrders

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
        SQLFunction "AND" _ _ -> extractJoinCols arg1 ++ extractJoinCols arg2
        SQLFunction "=" _ _ -> extractJoinColPair arg1 arg2
        
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

-- | select the given columns from the given table
selectTableColumns :: [ColumnSelection] -> DatabaseTable -> DatabaseTable
selectTableColumns columns table =
    let
        tableColIds = columnIdentifiers table
        indices = concat $ map (columnSelectionIndices tableColIds) columns
        currColIds = columnIdentifiers table
        currTblRows = tableRows table
    in
        table {
            columnIdentifiers = [currColIds !! i | i <- indices],
            tableRows = selectColumns indices currTblRows}

-- | get the indices of the given column selection
--   TODO we need to account for arbitrary expression cols too!!
columnSelectionIndices :: [ColumnIdentifier] -> ColumnSelection -> [Int]
columnSelectionIndices colIds AllColumns =
    [0 .. length colIds - 1]

columnSelectionIndices colIds (AllColumnsFrom srcTblName) =
    findIndices matchesSrcTblName (map maybeTableName colIds)
    where matchesSrcTblName Nothing         = False
          matchesSrcTblName (Just tblName)  = tblName == srcTblName

columnSelectionIndices colIds (QualifiedColumn colId) =
    findIndices (columnMatches colId) colIds

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

--------------------------------------------------------------------------------
-- Expression Evaluation
--------------------------------------------------------------------------------

-- | filters the database's table rows on the given expression
filterRowsBy :: Expression -> DatabaseTable -> DatabaseTable
filterRowsBy filterExpr table =
    table {tableRows = filter myBoolEvalExpr (tableRows table)}
    where myBoolEvalExpr row =
            boolValue $ evalExpression filterExpr (columnIdentifiers table) row

evalExpression :: Expression -> [ColumnIdentifier] -> [String] -> EvaluatedExpression
-- Here's the easy stuff. evaluate constants
evalExpression (StringConstantExpression string) _ _ = stringExpression string
evalExpression (IntegerConstantExpression int) _ _ = intExpression int
evalExpression (RealConstantExpression real) _ _ = realExpression real

-- A little bit harder. evaluate a column expression
evalExpression (ColumnExpression col) columnIds tblRow =
    case findIndex (columnMatches col) columnIds of
        Just colIndex -> stringExpression $ tblRow !! colIndex
        Nothing -> error $ "Failed to find column named: " ++ (prettyFormatColumn col)

-- this is where the action is. evaluate a function
evalExpression (FunctionExpression sqlFun funArgs) columnIds tblRow
    -- String functions
    | sqlFun == upperFunction = stringExpression $ map toUpper (stringValue (head evaluatedArgs))
    | sqlFun == lowerFunction = stringExpression $ map toLower (stringValue (head evaluatedArgs))
    | sqlFun == trimFunction = stringExpression $ trimSpace (stringValue (head evaluatedArgs)) -- TODO SQL trim takes params
    
    -- algebraic infix functions
    | sqlFun == multiplyFunction = algebraWithCoercion (*) (*) evaluatedArgs
    | sqlFun == divideFunction = realExpression $ foldl1' (/) (map realValue evaluatedArgs)
    | sqlFun == plusFunction = algebraWithCoercion (+) (+) evaluatedArgs
    | sqlFun == minusFunction = algebraWithCoercion (-) (-) evaluatedArgs
    
    -- boolean infix functions
    | sqlFun == isFunction = boolExpression (arg1 == arg2)
    | sqlFun == isNotFunction = boolExpression (arg1 /= arg2)
    | sqlFun == lessThanFunction = boolExpression (arg1 < arg2)
    | sqlFun == lessThanOrEqualToFunction = boolExpression (arg1 <= arg2)
    | sqlFun == greaterThanFunction = boolExpression (arg1 > arg2)
    | sqlFun == greaterThanOrEqualToFunction = boolExpression (arg1 >= arg2)
    | sqlFun == andFunction = boolExpression $ (boolValue arg1) && (boolValue arg2)
    | sqlFun == orFunction = boolExpression $ (boolValue arg1) || (boolValue arg2)
    
    where
        arg1 = head evaluatedArgs
        arg2 = evaluatedArgs !! 1
        evaluatedArgs = map evalArgExpr funArgs
        evalArgExpr expr = evalExpression expr columnIds tblRow
        algebraWithCoercion intFunc realFunc args =
            if any useRealAlgebra args then
                realExpression $ foldl1' realFunc (map realValue args)
            else
                intExpression $ foldl1' intFunc (map intValue args)
        
        useRealAlgebra expr =
            let prefType = preferredType expr
            in prefType == StringType || prefType == RealType

stringExpression :: String -> EvaluatedExpression
stringExpression string = EvaluatedExpression {
    preferredType   = StringType,
    intValue        = read string,
    realValue       = read string,
    stringValue     = string,
    boolValue       =
        (map toLower string /= "false") && (string /= "") && (string /= "0")}

intExpression int = EvaluatedExpression {
    preferredType   = IntType,
    intValue        = int,
    realValue       = fromIntegral int,
    stringValue     = show int,
    boolValue       = int /= 0}

realExpression real = EvaluatedExpression {
    preferredType   = RealType,
    intValue        = floor real,
    realValue       = real,
    stringValue     = show real,
    boolValue       = real /= 0.0}

boolExpression bool = EvaluatedExpression {
    preferredType   = BoolType,
    intValue        = if bool then 1 else 0,
    realValue       = if bool then 1.0 else 0.0,
    stringValue     = show bool,
    boolValue       = bool}

data ExpressionType = StringType | RealType | IntType | BoolType deriving Eq

data EvaluatedExpression = EvaluatedExpression {
    preferredType   :: ExpressionType,
    stringValue     :: String,
    realValue       :: Double,
    intValue        :: Int,
    boolValue       :: Bool}

instance Eq EvaluatedExpression where
    -- base off of the Ord definition
    expr1 == expr2 = compare expr1 expr2 == EQ

instance Ord EvaluatedExpression where
    -- compare on string based on preferred type
    compare (EvaluatedExpression StringType string1 _ _ _) expr2 = compare string1 (stringValue expr2)
    compare expr1 (EvaluatedExpression StringType string2 _ _ _) = compare (stringValue expr1) string2
    
    -- compare on real based on preferred type
    compare (EvaluatedExpression RealType _ real1 _ _) expr2 = compare real1 (realValue expr2)
    compare expr1 (EvaluatedExpression RealType _ real2 _ _) = compare (realValue expr1) real2
    
    -- compare on int based on preferred type
    compare (EvaluatedExpression IntType _ _ int1 _) expr2 = compare int1 (intValue expr2)
    compare expr1 (EvaluatedExpression IntType _ _ int2 _) = compare (intValue expr1) int2
    
    -- compare on bool based on preferred type (we know it's a bool here)
    compare expr1 expr2 = compare (boolValue expr1) (boolValue expr2)

-- | trims leading and trailing spaces
trimSpace :: String -> String
trimSpace = f . f
   where f = reverse . dropWhile isSpace

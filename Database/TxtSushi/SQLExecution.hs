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
    textTableToDatabaseTable,
    SortConfiguration(..)) where

import Data.Binary
import Data.Char
import Data.List
import qualified Data.Map as Map
import Text.Regex.Posix

import Database.TxtSushi.ExternalSort
import Database.TxtSushi.SQLParser
import Database.TxtSushi.Transform
import Database.TxtSushi.Util.ListUtil

-- | We will use the sort configuration to determine whether tables should
--   be sorted external or in memory
data SortConfiguration =
    UseInMemorySort |
    UseExternalSort deriving Show

sortByCfg :: (Binary b) => SortConfiguration -> (b -> b -> Ordering) -> [b] -> [b]
sortByCfg UseInMemorySort = sortBy
sortByCfg UseExternalSort = externalSortBy

-- | an SQL table data structure
--   TODO: need allColumnsColumnIdentifiers and allColumnsTableRows so that
--         we can filter and order on columns that are selected out. we also
--         should track any column ordering that is in place
data DatabaseTable d = DatabaseTable {
    -- | the columns in this table
    columnIdentifiers :: [ColumnIdentifier],
    
    -- | the actual table data
    tableRows :: [[d]]}

type SimpleTable = DatabaseTable EvaluatedExpression

type GroupedTable = DatabaseTable [EvaluatedExpression]

data EvaluatedExpression =
    StringExpression    String |
    RealExpression      Double |
    IntExpression       Int |
    BoolExpression      Bool deriving Show

-- order evaluated expressions using our type coercion rules where possible
instance Ord EvaluatedExpression where
    compare expr1@(RealExpression _) expr2 = expr1 `realCompare` expr2
    compare expr1 expr2@(RealExpression _) = expr1 `realCompare` expr2
    
    compare expr1@(IntExpression _) expr2 = expr1 `intCompare` expr2
    compare expr1 expr2@(IntExpression _) = expr1 `intCompare` expr2
    
    compare expr1@(BoolExpression _) expr2 = expr1 `boolCompare` expr2
    compare expr1 expr2@(BoolExpression _) = expr1 `boolCompare` expr2
    
    compare expr1 expr2 = expr1 `stringCompare` expr2

realCompare :: EvaluatedExpression -> EvaluatedExpression -> Ordering
realCompare expr1 expr2 =
    maybeCoerceReal expr1 `myCompare` maybeCoerceReal expr2
    where
        myCompare (Just r1) (Just r2) = r1 `compare` r2
        myCompare _ _ = expr1 `stringCompare` expr2

intCompare :: EvaluatedExpression -> EvaluatedExpression -> Ordering
intCompare expr1 expr2 =
    maybeCoerceInt expr1 `myCompare` maybeCoerceInt expr2
    where
        myCompare (Just i1) (Just i2) = i1 `compare` i2
        myCompare _ _ = expr1 `realCompare` expr2

boolCompare :: EvaluatedExpression -> EvaluatedExpression -> Ordering
boolCompare expr1 expr2 =
    maybeCoerceBool expr1 `myCompare` maybeCoerceBool expr2
    where
        myCompare (Just b1) (Just b2) = b1 `compare` b2
        myCompare _ _ = expr1 `stringCompare` expr2

stringCompare :: EvaluatedExpression -> EvaluatedExpression -> Ordering
stringCompare expr1 expr2 = coerceString expr1 `compare` coerceString expr2

-- base equality off of the Ord definition. pretty simple huh?
instance Eq EvaluatedExpression where
    expr1 == expr2 = expr1 `compare` expr2 == EQ

instance Binary EvaluatedExpression where
    put (StringExpression  s)   = put (0 :: Word8) >> put s
    put (RealExpression r)      = put (1 :: Word8) >> put r
    put (IntExpression i)       = put (2 :: Word8) >> put i
    put (BoolExpression b)      = put (3 :: Word8) >> put b
    
    get = do
        typeWord <- get :: Get Word8
        case typeWord of
            0 -> get >>= return . StringExpression
            1 -> get >>= return . RealExpression
            2 -> get >>= return . IntExpression
            3 -> get >>= return . BoolExpression
            _ -> error $ "unexpected type word value: " ++ show typeWord

coerceString :: EvaluatedExpression -> String
coerceString (StringExpression string)  = string
coerceString (RealExpression real)      = show real
coerceString (IntExpression int)        = show int
coerceString (BoolExpression bool)      = if bool then "true" else "false"

maybeCoerceInt :: EvaluatedExpression -> Maybe Int
maybeCoerceInt (StringExpression string) = maybeReadInt string
maybeCoerceInt (RealExpression real)     = Just $ floor real -- TOOD: floor OK for negatives too?
maybeCoerceInt (IntExpression int)       = Just int
maybeCoerceInt (BoolExpression _)        = Nothing

coerceInt :: EvaluatedExpression -> Int
coerceInt evalExpr = case maybeCoerceInt evalExpr of
    Just int -> int
    Nothing ->
        error $ "could not convert \"" ++ (coerceString evalExpr) ++
                "\" to an integer value"

maybeCoerceReal :: EvaluatedExpression -> Maybe Double
maybeCoerceReal (StringExpression string) = maybeReadReal string
maybeCoerceReal (RealExpression real)     = Just real
maybeCoerceReal (IntExpression int)       = Just $ fromIntegral int
maybeCoerceReal (BoolExpression _)        = Nothing

coerceReal :: EvaluatedExpression -> Double
coerceReal evalExpr = case maybeCoerceReal evalExpr of
    Just real -> real
    Nothing ->
        error $ "could not convert \"" ++ (coerceString evalExpr) ++
                "\" to a numeric value"

maybeReadBool :: String -> Maybe Bool
maybeReadBool boolStr = case map toLower $ trimSpace boolStr of
    "true"      -> Just True
    "false"     -> Just False
    _           -> Nothing

maybeCoerceBool :: EvaluatedExpression -> Maybe Bool
maybeCoerceBool (StringExpression string) = maybeReadBool string
maybeCoerceBool (RealExpression _)        = Nothing
maybeCoerceBool (IntExpression _)         = Nothing
maybeCoerceBool (BoolExpression bool)     = Just bool

coerceBool :: EvaluatedExpression -> Bool
coerceBool evalExpr = case maybeCoerceBool evalExpr of
    Just bool -> bool
    Nothing ->
        error $ "could not convert \"" ++ (coerceString evalExpr) ++
                "\" to a boolean value"

-- convert a text table to a database table by using the 1st row as column IDs
textTableToDatabaseTable :: String -> [[String]] -> SimpleTable
textTableToDatabaseTable tblName (headerNames:tblRows) =
    DatabaseTable (map makeColId headerNames) (map (map StringExpression) tblRows)
    where
        makeColId colName = ColumnIdentifier (Just tblName) colName
textTableToDatabaseTable tblName [] =
    error $ "invalid table \"" ++ tblName ++ "\". There is no header row"

databaseTableToTextTable :: SimpleTable -> [[String]]
databaseTableToTextTable dbTable =
    let
        headerRow = map columnId (columnIdentifiers dbTable)
        tailRows = map (map coerceString) (tableRows dbTable)
    in
        headerRow:tailRows

{-
optimizeClassicJoins :: SelectStatement -> SelectStatement
optimizeClassicJoins selectStmt@(SelectStatement _ (Just fromTbl) (Just whereFilter) _ _) =
    let (optFromTbl, optMaybeWhereFilter) = optimizeFromWhere Nothing fromTbl whereFilter
    in  selectStmt {
            maybeFromTable = Just optFromTbl,
            maybeWhereFilter = optMaybeWhereFilter}
optimizeClassicJoins selectStmt = selectStmt

optimizeFromWhere :: Maybe String -> TableExpression -> Maybe Expression -> (TableExpression, Maybe Expression)
optimizeFromWhere maybeParentAlias _ Nothing =
    (fromTbl, Just expr)
optimizeFromWhere maybeParentAlias fromTbl@(InnerJoin leftJoinTbl rightJoinTbl _ maybeChildAlias) (Just expr) =
    let
        maybeAlias = case maybeParentAlias of
            Nothing -> maybeChildAlias
            Just -  -> maybeParentAlias
        (optLeftTbl, expr2) = optimizeFromWhere maybeAlias leftJoinTbl expr
        (optRightTbl, expr3) = optimizeFromWhere maybeAlias rightJoinTbl expr2
        optFromTbl = fromTbl {
            leftJoinTable,
            rightJoinTable}
    in
        (optFromTbl, Just expr3)
-}

-- | perform a SQL select with the given select statement on the
--   given table map
select :: SortConfiguration -> SelectStatement -> (Map.Map String SimpleTable) -> SimpleTable
select sortCfg selectStmt tableMap =
    let
        fromTbl = case maybeFromTable selectStmt of
            Nothing -> DatabaseTable [] []
            Just fromTblExpr -> evalTableExpression sortCfg fromTblExpr tableMap
        fromTblWithAliases =
            appendAliasColumns (columnSelections selectStmt) fromTbl
        filteredTbl = case maybeWhereFilter selectStmt of
            Nothing -> fromTblWithAliases
            Just expr -> filterRowsBy expr fromTblWithAliases
    in
        case maybeGroupByHaving selectStmt of
            Nothing ->
                if selectStatementContainsAggregates selectStmt then
                    -- for the case where we find aggregate functions but
                    -- no "GROUP BY" part, that means we should apply the
                    -- aggregate to the table as a single group
                    finishWithAggregateSelect
                        sortCfg
                        selectStmt
                        (DatabaseTable (columnIdentifiers filteredTbl) [tableRows filteredTbl])
                else
                    finishWithNormalSelect sortCfg selectStmt filteredTbl
            Just groupByPart ->
                let
                    tblGroups = performGroupBy sortCfg groupByPart filteredTbl
                in
                    finishWithAggregateSelect sortCfg selectStmt tblGroups

-- TODO this approach wont let you refer to an alias in the column selection
appendAliasColumns :: [ColumnSelection] -> SimpleTable -> SimpleTable
appendAliasColumns [] dbTable = dbTable
appendAliasColumns cols dbTable@(DatabaseTable colIds tblRows) =
    let colAliasExprs = extractColumnAliases cols
        -- TODO which is the right fold here?
        evaluatedColExprsTbl = foldl1' tableConcat (evalAliasCols colAliasExprs)
    in
        if null colAliasExprs
        then dbTable
        else dbTable `tableConcat` evaluatedColExprsTbl
    where
        evalAliasCols :: [(ColumnIdentifier, Expression)] -> [SimpleTable]
        evalAliasCols [] = []
        evalAliasCols ((aliasColId, aliasExpr) : tailAliasExprs) =
            DatabaseTable [aliasColId] [[evalExpression aliasExpr colIds row] | row <- tblRows] :
            evalAliasCols tailAliasExprs

extractColumnAliases :: [ColumnSelection] -> [(ColumnIdentifier, Expression)]
extractColumnAliases [] = []
extractColumnAliases ((ExpressionColumn expr (Just alias)) : colsTail) =
    (ColumnIdentifier Nothing alias, expr) : extractColumnAliases colsTail
extractColumnAliases xs = extractColumnAliases $ tail xs

finishWithNormalSelect :: SortConfiguration -> SelectStatement -> SimpleTable -> SimpleTable
finishWithNormalSelect sortCfg selectStmt filteredDbTable =
    let
        orderedTbl =
            orderRowsBy sortCfg (orderByItems selectStmt) filteredDbTable
        selectedTbl =
            evaluateColumnSelections (columnSelections selectStmt) orderedTbl
    in
        selectedTbl

finishWithAggregateSelect :: SortConfiguration -> SelectStatement -> GroupedTable -> SimpleTable
finishWithAggregateSelect sortCfg selectStmt aggregateTbls =
    let
        orderedTbls =
            orderGroupsBy sortCfg (orderByItems selectStmt) aggregateTbls
        selectedTbl =
            evaluateAggregateColumnSelections (columnSelections selectStmt) orderedTbls
    in
        selectedTbl

performGroupBy :: SortConfiguration -> ([Expression], Maybe Expression) -> SimpleTable -> GroupedTable
performGroupBy sortCfg (groupByExprs, maybeExpr) dbTable =
    let
        tblGroups = groupRowsBy sortCfg groupByExprs dbTable
    in
        case maybeExpr of
            Nothing -> tblGroups
            Just expr -> filterGroupsBy expr tblGroups

-- | sorts table rows by the given order by items
orderRowsBy :: SortConfiguration -> [OrderByItem] -> SimpleTable -> SimpleTable
orderRowsBy _ [] dbTable = dbTable
orderRowsBy sortCfg orderBys dbTable =
    let
        -- curry in the order and col ID params to make a row comparison function
        compareRows = compareRowsOnOrderItems orderBys (columnIdentifiers dbTable)
        sortedRows = sortByCfg sortCfg compareRows (tableRows dbTable)
    in
        dbTable {tableRows = sortedRows}

orderGroupsBy :: SortConfiguration -> [OrderByItem] -> GroupedTable -> GroupedTable
orderGroupsBy _ [] groupedTable = groupedTable
orderGroupsBy sortCfg orderBys groupedTable =
    let
        -- curry in the order and col ID params to make a group comparison function
        compareGroups = compareGroupsOnOrderItems orderBys (columnIdentifiers groupedTable)
        sortedGroups = sortByCfg sortCfg compareGroups (tableRows groupedTable)
    in
        groupedTable {tableRows = sortedGroups}

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

compareGroupsOnOrderItems :: [OrderByItem] -> [ColumnIdentifier] -> [[EvaluatedExpression]] -> [[EvaluatedExpression]] -> Ordering
compareGroupsOnOrderItems orderBys colIds group1 group2 =
    cascadingOrder $ toOrderList orderBys
    where
        toOrderList [] = []
        toOrderList (orderBy:orderByTail) =
            (compareGroupsOnOrderItem orderBy colIds group1 group2):(toOrderList orderByTail)

compareGroupsOnOrderItem :: OrderByItem -> [ColumnIdentifier] -> [[EvaluatedExpression]] -> [[EvaluatedExpression]] -> Ordering
compareGroupsOnOrderItem orderBy colIds group1 group2 =
    let
        orderExpr = orderExpression orderBy
        grpComp = compareGroupsOnExpression orderExpr colIds group1 group2
    in
        if orderAscending orderBy then
            grpComp
        else
            reverseOrdering grpComp

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

compareGroupsOnExpression :: Expression -> [ColumnIdentifier] -> [[EvaluatedExpression]] -> [[EvaluatedExpression]] -> Ordering
compareGroupsOnExpression expr colIds grp1 grp2 =
    evalExprOn grp1 `compare` evalExprOn grp2
    where
        evalExprOn grp = evalAggregateExpression expr (DatabaseTable colIds grp)

groupRowsBy :: SortConfiguration -> [Expression] -> SimpleTable -> GroupedTable
groupRowsBy sortCfg groupByExprs dbTable =
    DatabaseTable (columnIdentifiers dbTable) rowGroups
    where
        tblRows = tableRows dbTable
        
        -- curry in the exprs and col ID params to make a row comparison function
        compareRows = compareRowsOnExpressions groupByExprs (columnIdentifiers dbTable)
        row1 `rowsEq` row2 = (row1 `compareRows` row2) == EQ
        
        sortedRows = sortByCfg sortCfg compareRows tblRows
        rowGroups = groupBy rowsEq sortedRows

-- | Evaluate the FROM table part, and returns the FROM table. Also returns
--   a mapping of new table names from aliases etc.
evalTableExpression :: SortConfiguration -> TableExpression -> (Map.Map String SimpleTable) -> SimpleTable
evalTableExpression sortCfg tblExpr tableMap =
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
                leftJoinTbl = evalTableExpression sortCfg leftJoinTblExpr tableMap
                rightJoinTbl = evalTableExpression sortCfg rightJoinTblExpr tableMap
                joinCols = extractJoinCols onConditionExpr
                joinIndices = joinColumnIndices leftJoinTbl rightJoinTbl joinCols
                joinedTbl = innerJoin joinIndices leftJoinTbl rightJoinTbl
            in
                maybeRename maybeTblAlias joinedTbl
        
        SelectExpression selectStmt maybeTblAlias ->
            maybeRename maybeTblAlias (select sortCfg selectStmt tableMap)
        
        -- TODO implement me
        CrossJoin leftJoinTblExpr rightJoinTblExpr maybeTblAlias ->
            let
                leftJoinTbl = evalTableExpression sortCfg leftJoinTblExpr tableMap
                rightJoinTbl = evalTableExpression sortCfg rightJoinTblExpr tableMap
                joinedTbl = crossJoin leftJoinTbl rightJoinTbl
            in
                maybeRename maybeTblAlias joinedTbl
    
    where
        maybeRename :: (Maybe String) -> SimpleTable -> SimpleTable
        maybeRename Nothing table = table
        maybeRename (Just newName) table = table {
            columnIdentifiers = map (\colId -> colId {maybeTableName = Just newName}) (columnIdentifiers table)}

extractJoinCols :: Expression -> [(ColumnIdentifier, ColumnIdentifier)]
extractJoinCols (FunctionExpression sqlFunc [arg1, arg2]) =
    case sqlFunc of
        SQLFunction "AND" _ _   -> extractJoinCols arg1 ++ extractJoinCols arg2
        SQLFunction "=" _ _     -> extractJoinColPair arg1 arg2
        
        -- Only expecting "AND" or "="
        _ -> onPartFormattingError
    where
        extractJoinColPair (ColumnExpression col1) (ColumnExpression col2) = [(col1, col2)]
        
        -- Only expecting "AND" or "="
        extractJoinColPair _ _ = onPartFormattingError

-- Only expecting "AND" or "="
extractJoinCols _ = onPartFormattingError

onPartFormattingError :: a
onPartFormattingError =
    error $ "The \"ON\" part of a join must only contain column equalities " ++
            "joined together by \"AND\" like: " ++
            "\"tbl1.id1 = table2.id1 AND tbl1.firstname = tbl2.name\""

-- | perform an inner join using the given join indices on the given
--   tables
innerJoin :: [(Int, Int)] -> SimpleTable -> SimpleTable -> SimpleTable
innerJoin joinIndices leftJoinTbl rightJoinTbl = DatabaseTable {
    columnIdentifiers = (columnIdentifiers leftJoinTbl) ++ (columnIdentifiers rightJoinTbl),
    tableRows = joinTables joinIndices (tableRows leftJoinTbl) (tableRows rightJoinTbl)}

-- | perform a cross join using the given join indices on the given
--   tables
crossJoin :: SimpleTable -> SimpleTable -> SimpleTable
crossJoin leftJoinTbl rightJoinTbl = DatabaseTable {
    columnIdentifiers = (columnIdentifiers leftJoinTbl) ++ (columnIdentifiers rightJoinTbl),
    tableRows = crossJoinTables (tableRows leftJoinTbl) (tableRows rightJoinTbl)}

-- | convert the column ID pairs into index pairs
joinColumnIndices :: SimpleTable -> SimpleTable -> [(ColumnIdentifier, ColumnIdentifier)] -> [(Int, Int)]
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

evaluateColumnSelections :: [ColumnSelection] -> SimpleTable -> SimpleTable
evaluateColumnSelections colSelections dbTable =
    let
        selectionTbls = map ($ dbTable) (map evaluateColumnSelection colSelections)
    in
        foldl1' tableConcat selectionTbls

tableConcat :: SimpleTable -> SimpleTable -> SimpleTable
tableConcat dbTable1 dbTable2 =
    let
        concatIds = (columnIdentifiers dbTable1) ++ (columnIdentifiers dbTable2)
        concatRows = zipWith (++) (tableRows dbTable1) (tableRows dbTable2)
    in
        DatabaseTable concatIds concatRows

evaluateAggregateColumnSelections :: [ColumnSelection] -> GroupedTable -> SimpleTable
evaluateAggregateColumnSelections colSelections tblGroups =
    let
        selectionTbls = map ($ tblGroups) (map evaluateAggregateColumnSelection colSelections)
    in
        foldl1' tableConcat selectionTbls

evaluateAggregateColumnSelection :: ColumnSelection -> GroupedTable -> SimpleTable
evaluateAggregateColumnSelection AllColumns _ =
    error "* is not allowed for aggregate column selections"
evaluateAggregateColumnSelection (AllColumnsFrom srcTblName) _ =
    error $ srcTblName ++ ".* is not allowed for aggregate column selections"
evaluateAggregateColumnSelection (ExpressionColumn expr maybeAlias) groupedTbl =
    let
        tbls = map makeTbl (tableRows groupedTbl)
        evaluatedExprs = map (evalAggregateExpression expr) tbls
        exprColId = case maybeAlias of
            Nothing     -> expressionIdentifier expr
            Just alias  -> (expressionIdentifier expr) {columnId = alias}
    in
        DatabaseTable [exprColId] (transpose [evaluatedExprs])
    where
        makeTbl grp = DatabaseTable (columnIdentifiers groupedTbl) grp

evaluateColumnSelection :: ColumnSelection -> SimpleTable -> SimpleTable
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
evaluateColumnSelection (ExpressionColumn expr maybeAlias) dbTable =
    let
        tblColIds = columnIdentifiers dbTable
        exprColId = case maybeAlias of
            Nothing     -> expressionIdentifier expr
            Just alias  -> (expressionIdentifier expr) {columnId = alias}
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
filterRowsBy :: Expression -> SimpleTable -> SimpleTable
filterRowsBy filterExpr table =
    table {tableRows = filter myBoolEvalExpr (tableRows table)}
    where myBoolEvalExpr row =
            coerceBool $ evalExpression filterExpr (columnIdentifiers table) row

filterGroupsBy :: Expression -> GroupedTable -> GroupedTable
filterGroupsBy expr groupedTbl =
    groupedTbl {tableRows = map tableRows filteredTbls}
    where
        makeTbl grp = DatabaseTable (columnIdentifiers groupedTbl) grp
        filterFunc = coerceBool . evalAggregateExpression expr
        filteredTbls = filter filterFunc (map makeTbl (tableRows groupedTbl))

-- | evaluate the given expression against a table
--   TODO need better error detection and reporting for non-aggregate
--   expressions
evalAggregateExpression :: Expression -> SimpleTable -> EvaluatedExpression
evalAggregateExpression (StringConstantExpression string) _ = StringExpression string
evalAggregateExpression (IntegerConstantExpression int) _   = IntExpression int
evalAggregateExpression (RealConstantExpression real) _     = RealExpression real
evalAggregateExpression (ColumnExpression col) dbTable =
    case findIndex (columnMatches col) (columnIdentifiers dbTable) of
        Just colIndex -> (head $ tableRows dbTable) !! colIndex
        Nothing -> error $ "Failed to find column named: " ++ (prettyFormatColumn col)

evalAggregateExpression (FunctionExpression sqlFun funArgs) dbTable =
    evalSQLFunction sqlFun $ if isAggregate sqlFun then manyArgs else aggregatedArgs
    where
        aggregatedArgs = map (\e -> evalAggregateExpression e dbTable) funArgs
        manyArgs =
            let
                tblColIds = columnIdentifiers dbTable
                tblRows = tableRows dbTable
                evaluateExprs expr = map (evalExpression expr tblColIds) tblRows
                allArgs = concatMap evaluateExprs funArgs
            in
                allArgs

-- | evaluate the given expression against a table row
evalExpression :: Expression -> [ColumnIdentifier] -> [EvaluatedExpression] -> EvaluatedExpression
evalExpression (StringConstantExpression string) _ _    = StringExpression string
evalExpression (IntegerConstantExpression int) _ _      = IntExpression int
evalExpression (RealConstantExpression real) _ _        = RealExpression real
evalExpression (ColumnExpression col) columnIds tblRow =
    case findIndex (columnMatches col) columnIds of
        Just colIndex -> tblRow !! colIndex
        Nothing -> error $ "Failed to find column named: " ++ (prettyFormatColumn col)
evalExpression (FunctionExpression sqlFun funArgs) columnIds tblRow =
    evalSQLFunction sqlFun (map evalArgExpr funArgs)
    where
        evalArgExpr expr = evalExpression expr columnIds tblRow

-- TODO this ugly function needs to be modularized
evalSQLFunction :: SQLFunction -> [EvaluatedExpression] -> EvaluatedExpression
evalSQLFunction sqlFun evaluatedArgs
    -- Global validation
    -- TODO this error should be more helpful than it is
    | argCountIsInvalid =
        error $ "cannot apply " ++ show (length evaluatedArgs) ++
                " arguments to " ++ functionName sqlFun
    
    -- String functions
    | sqlFun == upperFunction = StringExpression $ map toUpper (coerceString arg1)
    | sqlFun == lowerFunction = StringExpression $ map toLower (coerceString arg1)
    | sqlFun == trimFunction = StringExpression $ trimSpace (coerceString arg1)
    | sqlFun == concatenateFunction = StringExpression $ concat (map coerceString evaluatedArgs)
    | sqlFun == substringFromToFunction =
        StringExpression $ take (coerceInt arg3) (drop (coerceInt arg2 - 1) (coerceString arg1))
    | sqlFun == substringFromFunction =
        StringExpression $ drop (coerceInt arg2 - 1) (coerceString arg1)
    | sqlFun == regexMatchFunction = BoolExpression $ (coerceString arg1) =~ (coerceString arg2)
    
    -- unary functions
    | sqlFun == absFunction = evalUnaryAlgebra abs abs
    | sqlFun == negateFunction = evalUnaryAlgebra negate negate
    
    -- algebraic
    | sqlFun == multiplyFunction = algebraWithCoercion (*) (*) evaluatedArgs
    | sqlFun == divideFunction = RealExpression $ (coerceReal arg1) / (coerceReal arg2)
    | sqlFun == plusFunction = algebraWithCoercion (+) (+) evaluatedArgs
    | sqlFun == minusFunction = algebraWithCoercion (-) (-) evaluatedArgs
    
    -- boolean
    | sqlFun == isFunction = BoolExpression (arg1 == arg2)
    | sqlFun == isNotFunction = BoolExpression (arg1 /= arg2)
    | sqlFun == lessThanFunction = BoolExpression (arg1 < arg2)
    | sqlFun == lessThanOrEqualToFunction = BoolExpression (arg1 <= arg2)
    | sqlFun == greaterThanFunction = BoolExpression (arg1 > arg2)
    | sqlFun == greaterThanOrEqualToFunction = BoolExpression (arg1 >= arg2)
    | sqlFun == andFunction = BoolExpression $ (coerceBool arg1) && (coerceBool arg2)
    | sqlFun == orFunction = BoolExpression $ (coerceBool arg1) || (coerceBool arg2)
    | sqlFun == notFunction = BoolExpression $ not (coerceBool arg1)
    
    -- aggregate
    -- TODO AVG(...) holds the whole arg list in memory. reimplement!
    | sqlFun == avgFunction =
        RealExpression $
            foldl1' (+) (map coerceReal evaluatedArgs) /
            (fromIntegral $ length evaluatedArgs)
    | sqlFun == countFunction = IntExpression $ length evaluatedArgs
    | sqlFun == firstFunction = head evaluatedArgs
    | sqlFun == lastFunction = last evaluatedArgs
    | sqlFun == maxFunction = maximum evaluatedArgs
    | sqlFun == minFunction = minimum evaluatedArgs
    | sqlFun == sumFunction = algebraWithCoercion (+) (+) evaluatedArgs
    
    -- error!!
    | otherwise = error $
        "internal error: missing evaluation code for function: " ++
        functionName sqlFun ++ ". please report this error"
    
    where
        arg1 = head evaluatedArgs
        arg2 = evaluatedArgs !! 1
        arg3 = evaluatedArgs !! 2
        algebraWithCoercion intFunc realFunc args =
            if any useRealAlgebra args then
                RealExpression $ foldl1' realFunc (map coerceReal args)
            else
                IntExpression $ foldl1' intFunc (map coerceInt args)
        
        useRealAlgebra (RealExpression _) = True
        useRealAlgebra expr = case maybeCoerceInt expr of
            Nothing -> True
            Just _  -> False
        
        argCountIsInvalid =
            let
                -- TODO the use of length is bad (unnecessarily traversing
                -- the entire arg list and keeping it in memory). Redo this
                -- so that we only check length w.r.t. minArgs
                argCount = length evaluatedArgs
                minArgs = minArgCount sqlFun
                argsFixed = argCountIsFixed sqlFun
            in
                argCount < minArgs || (argCount > minArgs && argsFixed)
        
        evalUnaryAlgebra intFunc realFunc =
            if length evaluatedArgs /= 1 then
                error $
                    "internal error: found a " ++ show sqlFun ++
                    " function with multiple args. please report this error"
            else
                if useRealAlgebra arg1 then
                    RealExpression $ realFunc (coerceReal arg1)
                else
                    IntExpression $ intFunc (coerceInt arg1)

-- | trims leading and trailing spaces
trimSpace :: String -> String
trimSpace = f . f
    where f = reverse . dropWhile isSpace

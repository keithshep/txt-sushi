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

{-# LANGUAGE ExistentialQuantification #-}
module Database.TxtSushi.SQLExecution (
    select,
    databaseTableToTextTable,
    textTableToDatabaseTable,
    SortConfiguration(..)) where

import Control.Applicative
import Data.Binary
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as M
import Text.Regex.Posix

import Database.TxtSushi.SQLParser
import Database.TxtSushi.EvaluatedExpression
import Database.TxtSushi.ExternalSort
import Database.TxtSushi.SQLParser
import Database.TxtSushi.Transform

-- | We will use the sort configuration to determine whether tables should
--   be sorted external or in memory
data SortConfiguration =
    UseInMemorySort |
    UseExternalSort deriving Show

sortByCfg :: (Binary b) => SortConfiguration -> (b -> b -> Ordering) -> [b] -> [b]
sortByCfg UseInMemorySort = sortBy
sortByCfg UseExternalSort = externalSortBy

-- convert a text table to a database table by using the 1st row as column IDs
textTableToDatabaseTable :: String -> [[String]] -> BoxedTable
textTableToDatabaseTable tblName [] = noTableHeaderError tblName
textTableToDatabaseTable tblName (headerNames:tblRows) =
    renameDbTable tblName $ BoxedTable DatabaseTable {
        columnsWithContext = zip (map makeColExpr headerNames) (repeat evalCtxt),
        qualifiedColumnsWithContext = M.empty,
        evaluationContext = evalCtxt,
        tableData = tblRows,
        isInScope = idInHeader}
    where
        makeColExpr colName = ColumnExpression $ ColumnIdentifier Nothing colName
        
        idInHeader (ColumnIdentifier (Just _) _)        = False
        idInHeader (ColumnIdentifier Nothing colName)   = colName `elem` headerNames
        
        evalCtxt colId@(ColumnIdentifier (Just _) _) _ =
            columnNotInScopeError $ columnToString colId
        evalCtxt (ColumnIdentifier Nothing colName) row =
            case elemIndices colName headerNames of
                [colIndex]  -> SingleElement $ StringExpression (row !! colIndex)
                []          -> columnNotInScopeError colName
                _           -> ambiguousColumnError colName

databaseTableToTextTable :: BoxedTable -> [[String]]
databaseTableToTextTable (BoxedTable dbTable) = headerRow : tailRows
    where
        headerRow = map (expressionToString . fst) colsWCtxt
        tailRows = map evalRow (tableData dbTable)
        
        colsWCtxt = columnsWithContext dbTable
        
        evalRowExpr ctxt colExpr row =
            coerceString . collapseGroups colExpr $ evaluateWithContext ctxt colExpr row
        evalRow row =
            [evalRowExpr ctxt colExpr row | (colExpr, ctxt) <- colsWCtxt]

emptyTable :: BoxedTable
emptyTable = BoxedTable $ DatabaseTable {
    columnsWithContext = [],
    qualifiedColumnsWithContext = M.empty,
    evaluationContext = const . (columnNotInScopeError . columnToString),
    tableData = [error "Internal Error: this error should never occur!"] :: [String],
    isInScope = const False}

-- | perform a SQL select with the given select statement on the
--   given table map
select :: SortConfiguration -> SelectStatement -> (M.Map String BoxedTable) -> BoxedTable
select sortCfg selectStmt tableMap =
    let
        fromTbl = case maybeFromTable selectStmt of
            Nothing -> emptyTable
            Just fromTblExpr -> evalTableExpression sortCfg fromTblExpr tableMap
        fromTblWithAliases =
            addAliases fromTbl (extractColumnAliases $ columnSelections selectStmt)
        filteredTbl = maybeFilterTable (maybeWhereFilter selectStmt) fromTblWithAliases
        groupedTbl = maybeGroupTable sortCfg selectStmt filteredTbl
    in
        finishWithNormalSelect sortCfg selectStmt groupedTbl

maybeGroupTable :: SortConfiguration -> SelectStatement -> BoxedTable -> BoxedTable
maybeGroupTable sortCfg selectStmt table =
    case maybeGroupByHaving selectStmt of
        Nothing ->
            if selectStatementContainsAggregates selectStmt
                -- for the case where we find aggregate functions but
                -- no "GROUP BY" part, that means we should apply the
                -- aggregate to the table as a single group
                then singleGroupDbTable table
                else table
        Just (groupByPart, maybeHaving) ->
            let groupedTable = groupDbTable sortCfg groupByPart table
                groupedTableWithAliases =
                    addAliases groupedTable (extractColumnAliases $ columnSelections selectStmt)
            in  maybeFilterTable maybeHaving groupedTableWithAliases

maybeFilterTable :: Maybe Expression -> BoxedTable -> BoxedTable
maybeFilterTable Nothing table = table
maybeFilterTable (Just expr) table = filterRowsBy expr table

extractColumnAliases :: [ColumnSelection] -> [(String, Expression)]
extractColumnAliases [] = []
extractColumnAliases ((ExpressionColumn expr (Just alias)) : colsTail) =
    (alias, expr) : extractColumnAliases colsTail
extractColumnAliases (_:xt) = extractColumnAliases xt

-- | Evaluate the FROM table part, and returns the FROM table. Also returns
--   a mapping of new table names from aliases etc.
evalTableExpression :: SortConfiguration -> TableExpression -> (M.Map String BoxedTable) -> BoxedTable
evalTableExpression sortCfg tblExpr tableMap =
    case tblExpr of
        TableIdentifier tblName maybeTblAlias ->
            let table = M.findWithDefault (tableNotInScopeError tblName) tblName tableMap
            in  maybeRename maybeTblAlias table
        
        -- TODO inner join should allow joining on expressions too!!
        InnerJoin leftJoinTblExpr rightJoinTblExpr onConditionExpr maybeTblAlias ->
            let
                leftJoinTbl = evalTableExpression sortCfg leftJoinTblExpr tableMap
                rightJoinTbl = evalTableExpression sortCfg rightJoinTblExpr tableMap
                joinExprs = extractJoinExprs leftJoinTbl rightJoinTbl onConditionExpr
                joinedTbl = innerJoinDbTables sortCfg joinExprs leftJoinTbl rightJoinTbl
            in
                maybeRename maybeTblAlias joinedTbl
        
        SelectExpression selectStmt maybeTblAlias ->
            maybeRename maybeTblAlias (select sortCfg selectStmt tableMap)
        
        -- TODO implement me
        CrossJoin leftJoinTblExpr rightJoinTblExpr maybeTblAlias ->
            let
                leftJoinTbl = evalTableExpression sortCfg leftJoinTblExpr tableMap
                rightJoinTbl = evalTableExpression sortCfg rightJoinTblExpr tableMap
                joinedTbl = crossJoinDbTables leftJoinTbl rightJoinTbl
            in
                maybeRename maybeTblAlias joinedTbl

finishWithNormalSelect :: SortConfiguration -> SelectStatement -> BoxedTable -> BoxedTable
finishWithNormalSelect sortCfg selectStmt filteredDbTable =
    selectOrderedResults $ sortDbTable sortCfg (orderByItems selectStmt) filteredDbTable
    where
        selectOrderedResults (BoxedTable unboxedOrderedTbl) =
            BoxedTable unboxedOrderedTbl {columnsWithContext =
                concatMap (selectionToExpressions unboxedOrderedTbl) (columnSelections selectStmt)}

selectionToExpressions :: DatabaseTable a -> ColumnSelection -> [(Expression, EvaluationContext a)]
selectionToExpressions dbTable AllColumns = columnsWithContext dbTable
selectionToExpressions dbTable (AllColumnsFrom srcTblName) =
    M.findWithDefault errMsg srcTblName (qualifiedColumnsWithContext dbTable)
    where errMsg = tableNotInScopeError srcTblName

selectionToExpressions dbTable (ExpressionColumn expr Nothing) =
    [(expr, evaluationContext dbTable)]
selectionToExpressions dbTable (ExpressionColumn _ (Just exprAlias)) =
    [(ColumnExpression $ ColumnIdentifier Nothing exprAlias, evaluationContext dbTable)]

extractJoinExprs :: BoxedTable -> BoxedTable -> Expression -> [(Expression, Expression)]
extractJoinExprs bTbl1@(BoxedTable tbl1) bTbl2@(BoxedTable tbl2) (FunctionExpression sqlFunc [arg1, arg2]) =
    case sqlFunc of
        SQLFunction "=" _ _     -> extractJoinExprPair
        SQLFunction "AND" _ _   ->
            extractJoinExprs bTbl1 bTbl2 arg1 ++ extractJoinExprs bTbl1 bTbl2 arg2
        
        -- Only expecting "AND" or "="
        _ -> onPartFormattingError
    where
        fromScope tbl expr = anyInScope tbl expr && allInScope tbl expr
        
        extractJoinExprPair =
            if fromScope tbl1 arg1 && fromScope tbl2 arg2
                then [(arg1, arg2)]
                else
                    if fromScope tbl2 arg1 && fromScope tbl1 arg2
                        then [(arg2, arg1)]
                        else joinOnRequiresBothTablesError

-- Only expecting "AND" or "="
extractJoinExprs _ _ _ = onPartFormattingError

data NestedDataGroups e =
    SingleElement e |
    GroupedData [NestedDataGroups e] deriving (Ord, Eq, Show)

instance Functor NestedDataGroups where
    fmap f (SingleElement e) = SingleElement (f e)
    fmap f (GroupedData grps) = GroupedData $ map (fmap f) grps

instance Applicative NestedDataGroups where
    pure = SingleElement
    
    (SingleElement f)  <*> (SingleElement x)  = SingleElement (f x)
    (SingleElement f)  <*> gd@(GroupedData _) = fmap f gd
    gd@(GroupedData _) <*> (SingleElement x)  = fmap ($ x) gd
    (GroupedData fs)   <*> (GroupedData xs)   = GroupedData $ zipWith (<*>) fs xs

flattenGroups :: NestedDataGroups e -> [e]
flattenGroups (SingleElement myElem) = [myElem]
flattenGroups (GroupedData grps) = concatMap flattenGroups grps

collapseGroups ::
    Expression
    -> NestedDataGroups EvaluatedExpression
    -> EvaluatedExpression
collapseGroups expr grps = case group (flattenGroups grps) of
    [singleGroup]   -> head singleGroup
    
    -- it's an error if there is more than one grouping
    manyGroups      ->
        let
            (elemsToShow, remaining) = splitAt 5 (map head manyGroups)
            commaSepElems = intercalate ", " (map coerceString elemsToShow)
            exprStr = expressionToString expr
            errorMsg =
                "Error: error evaluating \"" ++ exprStr ++
                "\". Cannot evaluate a grouped expression unless all " ++
                "of the grouped values match. Found multiple different " ++
                "values including: " ++ commaSepElems
        in case remaining of
            []  -> error errorMsg
            _   -> error $ errorMsg ++ " etc..."

-- | takes a list of data groups which can have different shapes and returns
--   a single group of lists which obviously must have the same shape
normalizeGroups :: [NestedDataGroups a] -> NestedDataGroups [a]
normalizeGroups grps =
    foldl
        -- this function will reshape and concatinate as it's folded
        (liftA2 (++))
        
        -- an empty single element is the starting point for the fold
        (SingleElement [])
        
        -- fmap is from NestedDataGroups and return from the list monad
        (map (fmap return) grps)

type EvaluationContext a = ColumnIdentifier -> a -> NestedDataGroups EvaluatedExpression

-- | a data type for representing a database table
data DatabaseTable a = DatabaseTable {
    
    -- | column expressions with their evaluation contexts
    columnsWithContext :: [(Expression, EvaluationContext a)],
    
    -- | columns with context qualified by table name (the map key)
    qualifiedColumnsWithContext :: M.Map String [(Expression, EvaluationContext a)],
    
    -- | the evaluation context for this table
    evaluationContext :: EvaluationContext a,
    
    -- | the data in this table
    tableData :: [a],
    
    -- | is the given identifier in scope for this table
    isInScope :: ColumnIdentifier -> Bool}

allIdentifiers :: Expression -> [ColumnIdentifier]
allIdentifiers (FunctionExpression _ args) = concatMap allIdentifiers args
allIdentifiers (ColumnExpression col) = [col]
allIdentifiers _ = []

allInScope :: DatabaseTable a -> Expression -> Bool
allInScope tbl expr = all (isInScope tbl) (allIdentifiers expr)

anyInScope :: DatabaseTable a -> Expression -> Bool
anyInScope tbl expr = any (isInScope tbl) (allIdentifiers expr)

data BoxedTable = forall a. (Binary a) =>
    BoxedTable (DatabaseTable a)

-- | filters the database's table rows on the given expression
filterRowsBy :: Expression -> BoxedTable -> BoxedTable
filterRowsBy filterExpr (BoxedTable table) =
    BoxedTable table {tableData = filter myBoolEvalExpr (tableData table)}
    where
        evalFilterExpr = evaluateWithContext (evaluationContext table) filterExpr
        myBoolEvalExpr = coerceBool . collapseGroups filterExpr . evalFilterExpr

addAliases :: BoxedTable -> [(String, Expression)] -> BoxedTable
addAliases boxedTbl [] = boxedTbl
addAliases (BoxedTable tbl) aliases =
    BoxedTable tbl {
        evaluationContext = aliasedContext,
        isInScope = aliasedScope}
    where
        aliasMap = M.fromList aliases
        
        aliasedScope colId@(ColumnIdentifier (Just _) _) = isInScope tbl colId
        aliasedScope colId@(ColumnIdentifier Nothing colName) =
            M.member colName aliasMap || isInScope tbl colId
        
        aliasedContext colId@(ColumnIdentifier (Just _) _) = evaluationContext tbl colId
        aliasedContext colId@(ColumnIdentifier Nothing colName) =
            case M.lookup colName aliasMap of
                Nothing     -> evaluationContext tbl colId
                Just expr   -> evaluateWithContext aliasedContext expr

maybeRename :: (Maybe String) -> BoxedTable -> BoxedTable
maybeRename Nothing boxedTable = boxedTable
maybeRename (Just newName) boxedTable = renameDbTable newName boxedTable

renameDbTable :: String -> BoxedTable -> BoxedTable
renameDbTable name (BoxedTable tbl) =
    BoxedTable tbl {
        qualifiedColumnsWithContext = M.insert name (columnsWithContext tbl) (qualifiedColumnsWithContext tbl),
        evaluationContext = renameContext (evaluationContext tbl),
        isInScope = isInRenamedScope}
    where
        isInRenamedScope colId@(ColumnIdentifier Nothing _) = isInScope tbl colId
        isInRenamedScope (ColumnIdentifier (Just tblName) colName)
            | tblName == name   = isInScope tbl (ColumnIdentifier Nothing colName)
            | otherwise         = False
        
        renameContext ctxt colId@(ColumnIdentifier Nothing _) = ctxt colId
        renameContext ctxt colId@(ColumnIdentifier (Just tblName) colName)
            | tblName == name   = ctxt (ColumnIdentifier Nothing colName)
            | otherwise         = columnNotInScopeError $ columnToString colId

evaluateWithContext :: EvaluationContext a -> Expression -> a -> NestedDataGroups EvaluatedExpression
evaluateWithContext _ (StringConstantExpression s) _ = SingleElement (StringExpression s)
evaluateWithContext _ (RealConstantExpression r) _   = SingleElement (RealExpression r)
evaluateWithContext _ (IntConstantExpression i) _    = SingleElement (IntExpression i)
evaluateWithContext _ (BoolConstantExpression b) _   = SingleElement (BoolExpression b)
evaluateWithContext ctxt (ColumnExpression colId) row = ctxt colId row
evaluateWithContext ctxt (FunctionExpression sqlFun args) row =
    case (isAggregate sqlFun, args) of
        -- if its an aggregate function with a single arg use aggregate evaluation
        (True, [_]) -> aggregateEval
        
        -- otherwise just evaluate it as a single function
        _           -> standardEval
    where
        normEvaldArgs = normalizeGroups [(evaluateWithContext ctxt) arg row | arg <- args]
        evalGivenFun = evalSQLFunction sqlFun
        aggregateEval =
            SingleElement $ evalSQLFunction sqlFun (concat (flattenGroups normEvaldArgs))
        standardEval = fmap evalGivenFun normEvaldArgs

toGroupContext :: EvaluationContext a -> EvaluationContext [a]
toGroupContext ctxt = grpCtxt
    where grpCtxt colId rowGrp = GroupedData $ map (ctxt colId) rowGrp

groupDbTable ::
    SortConfiguration
    -> [Expression]
    -> BoxedTable
    -> BoxedTable
groupDbTable sortCfg grpExprs (BoxedTable tbl) =
    BoxedTable tbl {
        columnsWithContext = mapSnd toGroupContext (columnsWithContext tbl),
        qualifiedColumnsWithContext = M.map (mapSnd toGroupContext) (qualifiedColumnsWithContext tbl),
        evaluationContext = toGroupContext $ evaluationContext tbl,
        tableData = groupedData}
    where
        eval = evaluateWithContext (evaluationContext tbl)
        rowOrd row = [eval expr row | expr <- grpExprs]
        sortedData = sortByCfg sortCfg (compare `on` rowOrd) (tableData tbl)
        groupedData = groupBy ((==) `on` rowOrd) sortedData

singleGroupDbTable ::
    BoxedTable
    -> BoxedTable
singleGroupDbTable (BoxedTable tbl) =
    BoxedTable tbl {
        columnsWithContext = mapSnd toGroupContext (columnsWithContext tbl),
        qualifiedColumnsWithContext = M.map (mapSnd toGroupContext) (qualifiedColumnsWithContext tbl),
        evaluationContext = toGroupContext $ evaluationContext tbl,
        tableData = [tableData tbl]}

sortDbTable ::
    SortConfiguration
    -> [OrderByItem]
    -> BoxedTable
    -> BoxedTable
sortDbTable sortCfg orderBys (BoxedTable table) =
    BoxedTable table {tableData = sortOnOrderBys (tableData table)}
    where
        ordAscs = map orderAscending orderBys
        ordExprs = map orderExpression orderBys
        
        evalCtxt = evaluateWithContext (evaluationContext table)
        rowOrd row = [evalCtxt expr row | expr <- ordExprs]
        sortOnOrderBys = sortByCfg sortCfg (compareWithDirection ordAscs `on` rowOrd)

compareWithDirection :: (Ord a) => [Bool] -> [a] -> [a] -> Ordering
compareWithDirection (asc:ascTail) (x:xt) (y:yt) = case x `compare` y of
    LT -> if asc then LT else GT
    GT -> if asc then GT else LT
    EQ -> compareWithDirection ascTail xt yt
compareWithDirection [] [] [] = EQ
compareWithDirection _ _ _ = error "Internal Error: List sizes should match"

innerJoinDbTables ::
    SortConfiguration
    -> [(Expression, Expression)]
    -> BoxedTable
    -> BoxedTable
    -> BoxedTable
innerJoinDbTables sortCfg joinExprs (BoxedTable fstTable) (BoxedTable sndTable) =
    BoxedTable $ zipDbTables joinedData fstTable sndTable
    where
        fstEval = evaluateWithContext (evaluationContext fstTable)
        fstRowOrd row = [fstEval expr row | expr <- map fst joinExprs]
        
        sndEval = evaluateWithContext (evaluationContext sndTable)
        sndRowOrd row = [sndEval expr row | expr <- map snd joinExprs]
        
        sortedFstData = sortByCfg sortCfg (compare `on` fstRowOrd) (tableData fstTable)
        sortedSndData = sortByCfg sortCfg (compare `on` sndRowOrd) (tableData sndTable)
        
        joinedData = joinPresortedTables fstRowOrd sortedFstData sndRowOrd sortedSndData

crossJoinDbTables ::
    BoxedTable
    -> BoxedTable
    -> BoxedTable
crossJoinDbTables (BoxedTable fstTable) (BoxedTable sndTable) =
    BoxedTable $ zipDbTables joinedData fstTable sndTable
    where
        joinedData = [(x, y) | x <- tableData fstTable, y <- tableData sndTable]

zipDbTables :: [(a, b)] -> DatabaseTable a -> DatabaseTable b -> DatabaseTable (a, b)
zipDbTables zippedData fstTable sndTable = DatabaseTable {
    columnsWithContext = fstCols ++ sndCols,
    qualifiedColumnsWithContext = M.unionWithKey ambiguousTableError fstQualCols sndQualCols,
    evaluationContext = evalCtxt,
    tableData = zippedData,
    isInScope = isInFstOrSndScope}
    
    where
        isInFstScope = isInScope fstTable
        isInSndScope = isInScope sndTable
        isInFstOrSndScope iden = isInFstScope iden || isInSndScope iden
        
        toFstCtxt ctxt colId row = ctxt colId (fst row)
        toSndCtxt ctxt colId row = ctxt colId (snd row)
        
        fstCols = mapSnd toFstCtxt (columnsWithContext fstTable)
        sndCols = mapSnd toSndCtxt (columnsWithContext sndTable)
        
        fstQualCols = M.map (mapSnd toFstCtxt) (qualifiedColumnsWithContext fstTable)
        sndQualCols = M.map (mapSnd toSndCtxt) (qualifiedColumnsWithContext sndTable)
        
        evalCtxt colId row =
            case (isInFstScope colId, isInSndScope colId) of
                (True, False)   -> evaluationContext fstTable colId (fst row)
                (False, True)   -> evaluationContext sndTable colId (snd row)
                (True, True)    -> ambiguousColumnError $ columnToString colId
                (False, False)  -> columnNotInScopeError $ columnToString colId

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f xs = [(x, f y) | (x, y) <- xs]

-- TODO this ugly function needs to be modularized
evalSQLFunction :: SQLFunction -> [EvaluatedExpression] -> EvaluatedExpression
evalSQLFunction sqlFun evaluatedArgs
    -- Global validation
    -- TODO this error should be more helpful than it is
    | argCountIsInvalid =
        error $ "Error: cannot apply " ++ show (length evaluatedArgs) ++
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
                    "Internal Error: found a " ++ show sqlFun ++
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

ambiguousTableError, noTableHeaderError, tableNotInScopeError, columnNotInScopeError, ambiguousColumnError :: String -> a
ambiguousTableError tblName = error $ "Error: The table name \"" ++ tblName ++ "\" is ambiguous"
noTableHeaderError tblName = error $ "Error: invalid table \"" ++ tblName ++ "\". There is no header row"
tableNotInScopeError tblName = error $ "Error: failed to find a table named \"" ++ tblName ++ "\" in the current scope"
columnNotInScopeError colName = error $ "Error: failed to find a column named \"" ++ colName ++ "\" in the current scope"
ambiguousColumnError colName = error $ "Error: ambiguous column name (found multiple matches in the current scope): " ++ colName

onPartFormattingError, joinOnRequiresBothTablesError :: a
onPartFormattingError = error $
    "Error: The \"ON\" part of a join must only contain " ++
    "expression equalities joined together by \"AND\" like: " ++
    "\"tbl1.id1 = table2.id1 AND tbl1.firstname = tbl2.name\""

joinOnRequiresBothTablesError = error $
    "Error: the expressions used in the \"ON\" part of a table join must use " ++
    "identifiers from each of the two join tables like: " ++
    "\"tbl1.id1 = table2.id1 AND tbl1.firstname = tbl2.name\""

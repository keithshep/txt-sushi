module TxtSushi.SQLExecution (
    select,
    databaseTableToTextTable,
    textTableToDatabaseTable) where

import Data.List
import qualified Data.Map as Map

import Table.Transform
import TxtSushi.SQLParser

-- | an SQL table data structure
data DatabaseTable = DatabaseTable {
    -- | the columns in this table
    columnIdentifiers :: [ColumnIdentifier],
    
    -- | the actual table data
    tableRows :: [[String]]}

emptyTable = DatabaseTable [] []

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
        selectedTbl =
            selectTableColumns (columnSelections selectStatement) filteredTbl
    in
        selectedTbl

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
        InnerJoin leftJoinTblExpr rightJoinTblExpr joinCols maybeTblAlias ->
            let
                leftJoinTbl = evalTableExpression leftJoinTblExpr tableMap
                rightJoinTbl = evalTableExpression rightJoinTblExpr tableMap
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

-- | perform an inner join using the given join indices on the given
--   tables
innerJoin :: [(Int, Int)] -> DatabaseTable -> DatabaseTable -> DatabaseTable
innerJoin joinIndices leftJoinTbl rightJoinTbl = DatabaseTable {
    columnIdentifiers = (columnIdentifiers leftJoinTbl) ++ (columnIdentifiers rightJoinTbl),
    tableRows = joinTables joinIndices (tableRows leftJoinTbl) (tableRows rightJoinTbl)}

joinColumnIndices :: DatabaseTable -> DatabaseTable -> [(ColumnIdentifier, ColumnIdentifier)] -> [(Int, Int)]
joinColumnIndices leftJoinTbl rightJoinTbl joinCols =
    let
        leftHeader = columnIdentifiers leftJoinTbl
        rightHeader = columnIdentifiers rightJoinTbl
    in
        map (idPairToIndexPair leftHeader rightHeader) joinCols

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
    findIndices matchesCol colIds
    where matchesCol colId2 =
            case colId of
                ColumnIdentifier Nothing colIdStr ->
                    -- In this case we don't care about the table name so
                    -- just check to make sure that the column names match up
                    colIdStr == columnId colId2
                otherwise ->
                    -- table name is important here so match on the whole
                    -- object
                    colId == colId2

filterRowsBy filterExpr table = table -- TODO implement me.. maybe aliases

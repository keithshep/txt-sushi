{- |
Simple table transformations
-}
module Database.TxtSushi.Transform (
    sortColumns,
    joinTables,
    crossJoinTables,
    joinPresortedTables,
    rowComparison) where

import Data.List

-- | sort the given 'table' on the given columns
sortColumns :: (Ord a) => [Int] -> [[a]] -> [[a]]
sortColumns columns table =
    sortBy (rowComparison columns) table

-- | compare two rows based on given column balues
rowComparison :: (Ord a) => [Int] -> [a] -> [a] -> Ordering
rowComparison [] _ _ = EQ
rowComparison (columnHead:columnsTail) row1 row2 =
    let colComparison = (row1 !! columnHead) `compare` (row2 !! columnHead)
    in
        case colComparison of
            EQ  -> rowComparison columnsTail row1 row2
            _   -> colComparison

-- | join together two tables on the given column index pairs
joinTables :: (Ord o) => [(Int, Int)] -> [[o]] -> [[o]] -> [[o]]
joinTables joinColumnZipList table1 table2 =
    let
        (joinColumns1, joinColumns2) = unzip joinColumnZipList
        sortedTable1 = sortColumns joinColumns1 table1
        sortedTable2 = sortColumns joinColumns2 table2
    in
        joinPresortedTables joinColumnZipList sortedTable1 sortedTable2

-- | join together two tables that are presorted on the given column index pairs
joinPresortedTables :: (Ord o) => [(Int, Int)] -> [[o]] -> [[o]] -> [[o]]
joinPresortedTables joinColumnZipList sortedTable1 sortedTable2 =
    let
        (joinColumns1, joinColumns2) = unzip joinColumnZipList
        rowEq1 = (\a b -> (rowComparison joinColumns1 a b) == EQ)
        rowEq2 = (\a b -> (rowComparison joinColumns2 a b) == EQ)
        tableGroups1 = groupBy rowEq1 sortedTable1
        tableGroups2 = groupBy rowEq2 sortedTable2
    in
        joinGroupedTables joinColumnZipList tableGroups1 tableGroups2

crossJoinTables :: [[a]] -> [[a]] -> [[a]]
crossJoinTables [] _ = []
crossJoinTables _ [] = []
crossJoinTables (table1HeadRow:table1Tail) table2 =
    let
        prependHead = (table1HeadRow ++)
        newTable2 = map prependHead table2
    in
        newTable2 ++ (crossJoinTables table1Tail table2)

joinGroupedTables :: (Ord a) => [(Int, Int)] -> [[[a]]] -> [[[a]]] -> [[a]]
joinGroupedTables _ [] _  = []
joinGroupedTables _ _  [] = []
joinGroupedTables joinColumnZipList tableGroups1@(headTableGroup1:tableGroupsTail1) tableGroups2@(headTableGroup2:tableGroupsTail2) =
    let
        headRow1 = head headTableGroup1
        headRow2 = head headTableGroup2
    in
        case asymmetricRowComparison joinColumnZipList headRow1 headRow2 of
            -- drop the 1st group if its smaller
            LT -> joinGroupedTables joinColumnZipList tableGroupsTail1 tableGroups2
            
            -- drop the 2nd group if its smaller
            GT -> joinGroupedTables joinColumnZipList tableGroups1 tableGroupsTail2
            
            -- the two groups are equal so permute
            _  ->
                (crossJoinTables headTableGroup1 headTableGroup2) ++
                (joinGroupedTables joinColumnZipList tableGroupsTail1 tableGroupsTail2)

asymmetricRowComparison :: (Ord a) => [(Int, Int)] -> [a] -> [a] -> Ordering
asymmetricRowComparison [] _ _ = EQ
asymmetricRowComparison (columnsZipHead:columnsZipTail) row1 row2 =
    let
        (columnHead1, columnHead2) = columnsZipHead
        colComparison = (row1 !! columnHead1) `compare` (row2 !! columnHead2)
    in
        case colComparison of
            EQ  -> asymmetricRowComparison columnsZipTail row1 row2
            _   -> colComparison

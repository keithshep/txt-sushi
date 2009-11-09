{- |
Simple table transformations
-}
module Database.TxtSushi.Transform (
    sortRows,
    joinTables,
    crossJoinTables,
    joinPresortedTables,
    rowComparison) where

import Data.List

-- | sort the given 'table' on the given columns
sortRows :: (Ord o) => [([a] -> o)] -> [[a]] -> [[a]]
sortRows rowOrdFuncs table =
    sortBy (rowComparison rowOrdFuncs) table

rowComparison :: (Ord o) =>
    [([a] -> o)]
    -> [a]
    -> [a]
    -> Ordering
rowComparison rowOrdFuncs row1 row2 =
    map ($ row1) rowOrdFuncs `compare` map ($ row2) rowOrdFuncs

-- | join together two tables on the given column index pairs
joinTables :: (Ord o) =>
    [([a] -> o, [a] -> o)]
    -> [[a]]
    -> [[a]]
    -> [[a]]
joinTables zippedJoinOrdFuncs table1 table2 =
    let
        (joinOrdFuncs1, joinOrdFuncs2) = unzip zippedJoinOrdFuncs
        sortedTable1 = sortRows joinOrdFuncs1 table1
        sortedTable2 = sortRows joinOrdFuncs2 table2
    in
        joinPresortedTables zippedJoinOrdFuncs sortedTable1 sortedTable2

-- | join together two tables that are presorted on the given column index pairs
joinPresortedTables :: (Ord o) =>
    [([a] -> o, [a] -> o)]
    -> [[a]]
    -> [[a]]
    -> [[a]]
joinPresortedTables zippedJoinOrdFuncs sortedTable1 sortedTable2 =
    let
        (joinOrdFuncs1, joinOrdFuncs2) = unzip zippedJoinOrdFuncs
        rowEq1 = (\a b -> (rowComparison joinOrdFuncs1 a b) == EQ)
        rowEq2 = (\a b -> (rowComparison joinOrdFuncs2 a b) == EQ)
        tableGroups1 = groupBy rowEq1 sortedTable1
        tableGroups2 = groupBy rowEq2 sortedTable2
    in
        joinGroupedTables zippedJoinOrdFuncs tableGroups1 tableGroups2

crossJoinTables :: [[a]] -> [[a]] -> [[a]]
crossJoinTables [] _ = []
crossJoinTables _ [] = []
crossJoinTables (table1HeadRow:table1Tail) table2 =
    let
        prependHead = (table1HeadRow ++)
        newTable2 = map prependHead table2
    in
        newTable2 ++ (crossJoinTables table1Tail table2)

joinGroupedTables :: (Ord o) =>
    [([a] -> o, [a] -> o)]
    -> [[[a]]]
    -> [[[a]]]
    -> [[a]]
joinGroupedTables _ [] _  = []
joinGroupedTables _ _  [] = []
joinGroupedTables
    zippedJoinOrdFuncs
    tableGroups1@(headTableGroup1:tableGroupsTail1)
    tableGroups2@(headTableGroup2:tableGroupsTail2) =
    let
        (joinFuncs1, joinFuncs2) = unzip zippedJoinOrdFuncs
        headRow1 = head headTableGroup1
        headRow2 = head headTableGroup2
    in
        case map ($ headRow1) joinFuncs1 `compare` map ($ headRow2) joinFuncs2 of
            -- drop the 1st group if its smaller
            LT -> joinGroupedTables zippedJoinOrdFuncs tableGroupsTail1 tableGroups2
            
            -- drop the 2nd group if its smaller
            GT -> joinGroupedTables zippedJoinOrdFuncs tableGroups1 tableGroupsTail2
            
            -- the two groups are equal so permute
            _  ->
                (crossJoinTables headTableGroup1 headTableGroup2) ++
                (joinGroupedTables zippedJoinOrdFuncs tableGroupsTail1 tableGroupsTail2)

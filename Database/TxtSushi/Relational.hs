{- |
Simple table transformations
-}
module Database.TxtSushi.Relational (
    joinTables,
    crossJoinTables,
    joinPresortedTables) where

import Data.List
import Data.Function

-- | join together two tables on the given column index pairs
joinTables :: (Ord o) =>
    (a -> o)
    -> [a]
    -> (b -> o)
    -> [b]
    -> [(a, b)]
joinTables joinOrdFunc1 table1 joinOrdFunc2 table2 =
    let
        sortedTable1 = sortBy (compare `on` joinOrdFunc1) table1
        sortedTable2 = sortBy (compare `on` joinOrdFunc2) table2
    in
        joinPresortedTables joinOrdFunc1 sortedTable1 joinOrdFunc2 sortedTable2

-- | join together two tables that are presorted on the given column index pairs
joinPresortedTables :: (Ord o) =>
    (a -> o)
    -> [a]
    -> (b -> o)
    -> [b]
    -> [(a, b)]
joinPresortedTables joinOrdFunc1 sortedTable1 joinOrdFunc2 sortedTable2 =
    let
        tableGroups1 = groupBy rowEq1 sortedTable1
        tableGroups2 = groupBy rowEq2 sortedTable2
    in
        joinGroupedTables joinOrdFunc1 tableGroups1 joinOrdFunc2 tableGroups2
    where
        rowEq1 x y = (compare `on` joinOrdFunc1) x y == EQ
        rowEq2 x y = (compare `on` joinOrdFunc2) x y == EQ

crossJoinTables :: [a] -> [b] -> [(a, b)]
crossJoinTables [] _ = []
crossJoinTables _ [] = []
crossJoinTables (table1Head:table1Tail) table2 =
    map (\x -> (table1Head, x)) table2 ++ (crossJoinTables table1Tail table2)

joinGroupedTables :: (Ord o) =>
    (a -> o)
    -> [[a]]
    -> (b -> o)
    -> [[b]]
    -> [(a, b)]
joinGroupedTables _ [] _ _  = []
joinGroupedTables _ _  _ [] = []
joinGroupedTables
    joinOrdFunc1
    tableGroups1@(headTableGroup1:tableGroupsTail1)
    joinOrdFunc2
    tableGroups2@(headTableGroup2:tableGroupsTail2) =
    let
        headRow1 = head headTableGroup1
        headRow2 = head headTableGroup2
    in
        case joinOrdFunc1 headRow1 `compare` joinOrdFunc2 headRow2 of
            -- drop the 1st group if its smaller
            LT -> joinGroupedTables joinOrdFunc1 tableGroupsTail1 joinOrdFunc2 tableGroups2
            
            -- drop the 2nd group if its smaller
            GT -> joinGroupedTables joinOrdFunc1 tableGroups1 joinOrdFunc2 tableGroupsTail2
            
            -- the two groups are equal so permute
            _  ->
                (crossJoinTables headTableGroup1 headTableGroup2) ++
                (joinGroupedTables joinOrdFunc1 tableGroupsTail1 joinOrdFunc2 tableGroupsTail2)

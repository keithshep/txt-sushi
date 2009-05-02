{- |
Simple table transformations
-}
module Table.Transform (
    selectColumns,
    sortColumns,
    fileBasedSortTable,
    mergeAllBy,
    joinTables,
    joinPresortedTables,
    rowComparison) where

import Data.List
import System.Directory
import System.IO

import Table.IO

{- |
Create a new table by selecting the given 'columnIndices'
-}
selectColumns _ [] = []
selectColumns columnIndices (headRow:tableTail) =
    ([headRow !! i | i <- columnIndices]):(selectColumns columnIndices tableTail)

removeColumns _ [] = []
removeColumns columnIndices (headRow:tableTail) =
    let indexesToSelect = [0 .. ((length headRow) - 1)] \\ columnIndices
    in
        ([headRow !! i | i <- indexesToSelect]):(removeColumns columnIndices tableTail)

-- | sort the given 'table' on the given columns
sortColumns columns table =
    sortBy (rowComparison columns) table

-- | compare two rows based on given column balues
rowComparison [] _ _ = EQ
rowComparison (columnHead:columnsTail) row1 row2 =
    let colComparison = (row1 !! columnHead) `compare` (row2 !! columnHead)
    in
        case colComparison of
            EQ          -> rowComparison columnsTail row1 row2
            otherwise   -> colComparison

-- | merge two sorted lists into a single sorted list
mergeBy comparisonFunction []    list2   = list2
mergeBy comparisonFunction list1 []      = list1
mergeBy comparisonFunction list1@(head1:tail1) list2@(head2:tail2) =
    case head1 `comparisonFunction` head2 of
        GT          -> (head2:(mergeBy comparisonFunction list1 tail2))
        otherwise   -> (head1:(mergeBy comparisonFunction tail1 list2))

-- | merge the sorted lists in the list to a list about 1/2 the size
mergePairsBy _ [] = []
mergePairsBy comparisonFunction singletonListList@(headList:[]) = singletonListList
mergePairsBy comparisonFunction (list1:list2:listListTail) =
    let mergedPair = mergeBy comparisonFunction list1 list2
    in
        mergedPair:(mergePairsBy comparisonFunction listListTail)

-- | merge a list of sorted lists into a single sorted list
mergeAllBy _ [] = []
mergeAllBy comparisonFunction listList =
    let mergedPairs = mergePairsBy comparisonFunction listList
    in
        case mergedPairs of
            singletonListHead:[]    -> singletonListHead
            otherwise               -> mergeAllBy comparisonFunction mergedPairs

{- |
perform a table sort using files to keep from holding the whole list in memory
-}
fileBasedSortTable columns table = do
    partialSortFiles <- bufferPartialSorts columns table
    partialSortFileHandles <- (unwrapIOList [openFile file ReadMode | file <- partialSortFiles])
    partialSortContents <- (unwrapIOList [hGetContents handle | handle <- partialSortFileHandles])
    let partialSortTables = map (parseTable csvFormat) partialSortContents
    return (partialSortTables, partialSortFileHandles, partialSortFiles)

-- | unwrap a list of 'IO' boxed items
unwrapIOList [] = do return []
unwrapIOList (ioHead:ioTail) = do
    unwrappedHead <- ioHead
    unwrappedTail <- unwrapIOList ioTail
    return (unwrappedHead:unwrappedTail)

-- | create a list of parial sorts
bufferPartialSorts columns [] = return []
bufferPartialSorts columns table = do
    let rowLimit = 100000
        (rowsToSortNow, rowsToSortLater) = splitAt rowLimit table
        sortedRows = sortColumns columns rowsToSortNow
    sortBuffer <- bufferToTempFile sortedRows
    otherSortBuffers <- bufferPartialSorts columns rowsToSortLater
    return (sortBuffer:otherSortBuffers)

-- | buffer the table to a temporary file and return a handle to that file
bufferToTempFile table = do
    tempDir <- getTemporaryDirectory
    (tempFilePath, tempFileHandle) <- openTempFile tempDir "buffer.txt"
    hPutStr tempFileHandle (formatTable csvFormat table)
    hClose tempFileHandle
    return tempFilePath

-- | join together two tables on the given column index pairs
joinTables :: [(Int, Int)] -> [[String]] -> [[String]] -> [[String]]
joinTables joinColumnZipList table1 table2 =
    let
        (joinColumns1, joinColumns2) = unzip joinColumnZipList
        sortedTable1 = sortColumns joinColumns1 table1
        sortedTable2 = sortColumns joinColumns2 table2
    in
        joinPresortedTables joinColumnZipList sortedTable1 sortedTable2

-- | join together two tables that are presorted on the given column index pairs
joinPresortedTables :: [(Int, Int)] -> [[String]] -> [[String]] -> [[String]]
joinPresortedTables joinColumnZipList sortedTable1 sortedTable2 =
    let (joinColumns1, joinColumns2) = unzip joinColumnZipList
        rowEq1 = (\a b -> (rowComparison joinColumns1 a b) == EQ)
        rowEq2 = (\a b -> (rowComparison joinColumns2 a b) == EQ)
        tableGroups1 = groupBy rowEq1 sortedTable1
        tableGroups2 = groupBy rowEq2 sortedTable2
    in
        joinGroupedTables joinColumnZipList tableGroups1 tableGroups2

permutePrependRows [] _ = []
permutePrependRows _ [] = []
permutePrependRows (table1HeadRow:table1Tail) table2 =
    let prependHead = (table1HeadRow ++)
        newTable2 = map prependHead table2
    in
        newTable2 ++ (permutePrependRows table1Tail table2)

joinGroupedTables _ [] _  = []
joinGroupedTables _ _  [] = []
joinGroupedTables joinColumnZipList tableGroups1@(headTableGroup1:tableGroupsTail1) tableGroups2@(headTableGroup2:tableGroupsTail2) =
    let headRow1 = head headTableGroup1
        headRow2 = head headTableGroup2
    in
        case asymmetricRowComparison joinColumnZipList headRow1 headRow2 of
            -- drop the 1st group if its smaller
            LT -> joinGroupedTables joinColumnZipList tableGroupsTail1 tableGroups2
            
            -- drop the 2nd group if its smaller
            GT -> joinGroupedTables joinColumnZipList tableGroups1 tableGroupsTail2
            
            -- the two groups are equal so permute
            otherwise ->
                (permutePrependRows headTableGroup1 headTableGroup2) ++
                (joinGroupedTables joinColumnZipList tableGroupsTail1 tableGroupsTail2)

asymmetricRowComparison [] _ _ = EQ
asymmetricRowComparison (columnsZipHead:columnsZipTail) row1 row2 =
    let (columnHead1, columnHead2) = columnsZipHead
        colComparison = (row1 !! columnHead1) `compare` (row2 !! columnHead2)
    in
        case colComparison of
            EQ          -> asymmetricRowComparison columnsZipTail row1 row2
            otherwise   -> colComparison


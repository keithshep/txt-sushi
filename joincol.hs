import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Database.TxtSushi.IO
import Database.TxtSushi.Transform
import Database.TxtSushi.Util.IOUtil

closeAll [] = do return ()
closeAll (handle:tail) = do
    hClose handle
    closeAll tail

removeAll [] = do return ()
removeAll (file:tail) = do
    removeFile file
    removeAll tail

main = do
    args <- getArgs
    
    let lenArgs = length args
    
    if lenArgs < 4 || odd lenArgs
        then do
            hPutStrLn stderr $
                    "ERROR: program requires at least four arguments " ++
                    "(two or more join column arguments and two filenames), " ++
                    "also the argument count should be even"
            exitFailure
        else do
            let lenNonFileArgs = (lenArgs - 2)
                (nonFileArgs, fileName1:fileName2:[]) = splitAt lenNonFileArgs args
                allJoinIndices = map read nonFileArgs :: [Int]
                table1ColIndices = [allJoinIndices !! i | i <- [0, 2 .. (lenNonFileArgs - 1)]]
                table2ColIndices = [allJoinIndices !! i | i <- [1, 3 .. (lenNonFileArgs - 1)]]
                zippedColumnIndices = zip table1ColIndices table2ColIndices
            
            contents1 <- getContentsFromFileOrStdin fileName1
            contents2 <- getContentsFromFileOrStdin fileName2
            
            let table1 = parseTable csvFormat contents1
                table2 = parseTable csvFormat contents2
            
            (partialSortTables1, partialSortFileHandles1, partialSortFiles1) <- fileBasedSortTable table1ColIndices table1
            (partialSortTables2, partialSortFileHandles2, partialSortFiles2) <- fileBasedSortTable table2ColIndices table2
            
            let sortedTable1 = mergeAllBy (rowComparison table1ColIndices) partialSortTables1
                sortedTable2 = mergeAllBy (rowComparison table2ColIndices) partialSortTables2
                joinedTable = joinPresortedTables zippedColumnIndices sortedTable1 sortedTable2
                joinedTableText = formatTable csvFormat joinedTable
            
            putStr joinedTableText
            
            closeAll $ partialSortFileHandles1 ++ partialSortFileHandles2
            removeAll $ partialSortFiles1 ++ partialSortFiles2
            
            return ()

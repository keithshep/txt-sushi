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
    
    if (length args) <= 1
        then do
            hPutStrLn stderr $
                    "ERROR: program requires at least two arguments " ++
                    "(one column argument and a filename or '-')"
            exitFailure
        else do
            contents <- getContentsFromFileOrStdin (last args)
            
            let argsWithoutFile = init args
                colIndices = map read (init args) :: [Int]
                table = parseTable csvFormat contents
            
            (partialSortTables, partialSortFileHandles, partialSortFiles) <- fileBasedSortTable colIndices table
            
            let sortedTable     = mergeAllBy (rowComparison colIndices) partialSortTables
                sortedTableText = formatTable csvFormat sortedTable
            
            putStr sortedTableText
            
            closeAll partialSortFileHandles
            removeAll partialSortFiles
            
            return ()

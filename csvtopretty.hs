import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Table.IO
import Util.IOUtil

main = do
    args <- getArgs
    
    if (length args) /= 1
        then do
            hPutStrLn stderr $
                    "ERROR: program requires a single command line argument " ++
                    "(filename or '-')"
            exitFailure
        else do
            let fileArg = head args
                useStdio = (fileArg == "-")
            
            file <- if useStdio then bufferStdioToTempFile else return fileArg
            
            handle1 <- openFile file ReadMode
            contents1 <- hGetContents handle1
            handle2 <- openFile file ReadMode
            contents2 <- hGetContents handle2
            
            let table1 = parseTable csvFormat contents1
                columnWidths = map succ (maxTableColumnWidths table1)
                table2 = parseTable csvFormat contents2
                prettyTableText = formatTableWithWidths columnWidths table2
            
            putStr prettyTableText
            hClose handle1
            hClose handle2
            if useStdio then removeFile file else return ()

import System.Directory
import System.Environment
import System.IO

import Database.TxtSushi.FlatFile
import Database.TxtSushi.Util.IOUtil

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        [fileArg] -> do
            let useStdio = (fileArg == "-")
            
            file <- if useStdio then bufferStdioToTempFile else return fileArg
            
            handle1 <- openFile file ReadMode
            contents1 <- hGetContents handle1
            handle2 <- openFile file ReadMode
            contents2 <- hGetContents handle2
            
            let table1 = parseTable csvFormat contents1
                columnWidths = maxTableColumnWidths table1
                table2 = parseTable csvFormat contents2
                prettyTableText = formatTableWithWidths "|" columnWidths table2
            
            putStr prettyTableText
            hClose handle1
            hClose handle2
            if useStdio then removeFile file else return ()
            
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

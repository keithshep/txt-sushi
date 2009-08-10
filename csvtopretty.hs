import Data.List
import Data.Version (Version(..))
import System.Directory
import System.Environment
import System.IO

import Database.TxtSushi.IO
import Database.TxtSushi.Util.IOUtil

import Paths_txt_sushi

printUsage :: String -> IO ()
printUsage progName = do
    putStrLn $ progName ++ " (" ++ versionStr ++ ")"
    putStrLn $ "Usage: " ++ progName ++ " file_name_or_dash"
    where
        versionStr = intercalate "." (map show $ versionBranch version)

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    
    if (length args) /= 1
        then do
            printUsage progName
        else do
            let fileArg = head args
                useStdio = (fileArg == "-")
            
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

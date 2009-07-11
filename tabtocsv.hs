import Data.List
import Data.Version (Version(..))
import System.Environment
import System.Exit
import System.IO

import Database.TxtSushi.IO
import Database.TxtSushi.Util.IOUtil

import Paths_txt_sushi

printUsage progName = do
    putStrLn $ progName ++ " (" ++ versionStr ++ ")"
    putStrLn $ "Usage: " ++ progName ++ " file_name_or_dash"
    where
        versionStr = intercalate "." (map show $ versionBranch version)

main = do
    args <- getArgs
    progName <- getProgName
    
    if (length args) /= 1
        then do
            printUsage progName
        else do
            contents <- getContentsFromFileOrStdin (last args)
            
            let table = parseTable tabDelimitedFormat contents
                csvText = formatTable csvFormat table
            putStr csvText

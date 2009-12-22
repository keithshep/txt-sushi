import Data.List
import System.Environment
import System.IO

import Database.TxtSushi.FlatFile
import Database.TxtSushi.IOUtil

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        [fileArg] -> do
            contents <- getContentsFromFileOrStdin fileArg
            
            let table = transpose . parseTable tabDelimitedFormat $ contents
                tabText = formatTable tabDelimitedFormat table
            putStr tabText
        
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

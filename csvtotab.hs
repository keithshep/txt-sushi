import System.Environment
import System.IO

import Database.TxtSushi.IO
import Database.TxtSushi.Util.IOUtil

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        [fileArg] -> do
            contents <- getContentsFromFileOrStdin fileArg
            
            let table = parseTable csvFormat contents
                tabDelimitedText = formatTable tabDelimitedFormat table
            putStr tabDelimitedText
        
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

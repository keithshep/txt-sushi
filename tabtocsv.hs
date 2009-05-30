import Data.List
import System.Environment
import System.Exit
import System.IO

import TxtSushi.IO
import TxtSushi.Util.IOUtil

main = do
    args <- getArgs
    
    if (length args) /= 1
        then do
            hPutStrLn stderr $
                    "ERROR: program requires a single command line argument " ++
                    "(filename or '-')"
            exitFailure
        else do
            contents <- getContentsFromFileOrStdin (last args)
            
            let table = parseTable tabDelimitedFormat contents
                csvText = formatTable csvFormat table
            putStr csvText

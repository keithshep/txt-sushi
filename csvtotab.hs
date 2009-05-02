import Data.List
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
            contents <- getContentsFromFileOrStdin (last args)
            
            let table = parseTable csvFormat contents
                tabDelimitedText = formatTable tabDelimitedFormat table
            putStr tabDelimitedText

import Data.List
import System.Environment
import System.Exit
import System.IO

import Database.TxtSushi.IO
import Database.TxtSushi.Util.IOUtil

main = do
    args <- getArgs
    progName <- getProgName
    
    if (length args) /= 1
        then do
            hPutStrLn stderr $
                    "ERROR: " ++ progName ++
                    " requires a single command line argument " ++
                    "(filename or '-')"
            exitFailure
        else do
            contents <- getContentsFromFileOrStdin (last args)
            
            let table@(headRow:tableTail) = parseTable csvFormat contents
                colNames = map ("col" ++) (map show [1 .. length headRow])
                namedTable = colNames:table
            
            putStr $ if null table then "" else formatTable csvFormat namedTable

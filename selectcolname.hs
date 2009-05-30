import Data.List
import System.Environment
import System.Exit
import System.IO

import TxtSushi.IO
import TxtSushi.Transform
import TxtSushi.Util.IOUtil

maybeIndexToIndexOrError :: [String] -> [Maybe Int] -> [Int]
maybeIndexToIndexOrError [] [] = []
maybeIndexToIndexOrError (name:namesTail) (Nothing:indexTail) =
    error $ "Could not find column header named \"" ++ name ++ "\""
maybeIndexToIndexOrError (name:namesTail) ((Just index):indexTail) =
    index:(maybeIndexToIndexOrError namesTail indexTail)

main = do
    args <- getArgs
    
    if (length args) <= 1
        then do
            hPutStrLn stderr $
                    "ERROR: program requires at least two arguments " ++
                    "(one column name argument and a filename or '-')"
            exitFailure
        else do
            contents <- getContentsFromFileOrStdin (last args)
            
            let argsWithoutFile = init args
                colNames = init args
                table = parseTable csvFormat contents
                header = head table
                maybeColIndices = [elemIndex name header | name <- colNames]
                colIndices = maybeIndexToIndexOrError colNames maybeColIndices
                subsetTable = selectColumns colIndices table
                subsetTableText = formatTable csvFormat subsetTable
            putStr subsetTableText
            return ()

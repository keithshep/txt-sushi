import Data.List
import System.Environment
import System.Exit
import System.IO

import TxtSushi.IO
import TxtSushi.Transform
import TxtSushi.Util.CommandLineArgument
import TxtSushi.Util.IOUtil

{-
byNameOption = OptionDescription
    False       -- isRequired
    "-by-name"  -- optionFlag
    []          -- argumentNames
    0           -- minArgumentCount
    True        -- argumentCountIsFixed

helpOption = OptionDescription
    False       -- isRequired
    "-help"     -- optionFlag
    []          -- argumentNames
    0           -- minArgumentCount
    True        -- argumentCountIsFixed

options = [helpOption, byNameOption]
-}

--toColumnIndices optionMap [] = 

main = do
    args <- getArgs
    progName <- getProgName
    
    if (length args) <= 1
        then do
            error $ progName ++ " requires at least two arguments " ++
                    "(one column argument and a filename or '-')"
        else do
            let argCount = length args
                (argsWithoutFile, [fileName]) = splitAt (argCount - 1) args
                --(optionsMap, tailArgs) = extractOptions options argsWithoutFile
                colIndices = map read (init args) :: [Int]
            
            contents <- getContentsFromFileOrStdin fileName
            
            let table = parseTable csvFormat contents
                subsetTable = selectColumns colIndices table
                subsetTableText = formatTable csvFormat subsetTable
            putStr subsetTableText
            return ()

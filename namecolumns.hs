-----------------------------------------------------------------------------
-- |
-- Module      :  namecolumns
-- Copyright   :  (c) Keith Sheppard 2009-2010
-- License     :  BSD3
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Adds header names like: col1, col2, col3 ... to a CSV file
--
-----------------------------------------------------------------------------
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
            
            let table = parseTable csvFormat contents
            case table of
                
                -- we only need the 1st row's column count to name the columns
                (headRow : _) ->
                    let colNames = map (("col" ++) . show) [1 .. length headRow]
                        namedTable = formatTable csvFormat [colNames] ++ contents
                    in
                        putStr namedTable
                
                -- the given table was empty (is it better for this to be an
                -- error ?)
                _ -> return ()
        
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

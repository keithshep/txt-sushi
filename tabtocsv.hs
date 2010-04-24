-----------------------------------------------------------------------------
-- |
-- Module      :  tabtocsv
-- Copyright   :  (c) Keith Sheppard 2009-2010
-- License     :  BSD3
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts a tab-delimited file to a CSV file
--
-----------------------------------------------------------------------------
import System.Environment

import Database.TxtSushi.FlatFile
import Database.TxtSushi.IOUtil

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        [fileArg] -> do
            contents <- getContentsFromFileOrStdin fileArg
            
            let table = parseTable tabDelimitedFormat contents
                csvText = formatTable csvFormat table
            putStr csvText
        
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

-----------------------------------------------------------------------------
-- |
-- Module      :  csvtotab
-- Copyright   :  (c) Keith Sheppard 2009-2010
-- License     :  BSD3
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts CSV into tab-delimited
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
                tabDelimitedText = formatTable tabDelimitedFormat table
            putStr tabDelimitedText
        
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

-----------------------------------------------------------------------------
-- |
-- Module      :  csvtopretty
-- Copyright   :  (c) Keith Sheppard 2009-2010
-- License     :  BSD3
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts CSV input into pretty-printed output
--
-----------------------------------------------------------------------------
import Control.Monad
import System.Directory
import System.Environment
import System.IO

import Database.TxtSushi.FlatFile
import Database.TxtSushi.IOUtil

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        [fileArg] -> do
            let useStdio = (fileArg == "-")
            
            file <- if useStdio then bufferStdioToTempFile else return fileArg
            
            handle1 <- openFile file ReadMode
            contents1 <- hGetContents handle1
            handle2 <- openFile file ReadMode
            contents2 <- hGetContents handle2
            
            let table1 = parseTable csvFormat contents1
                columnWidths = maxTableColumnWidths table1
                table2 = parseTable csvFormat contents2
                prettyTableText = formatTableWithWidths "|" columnWidths table2
            
            putStr prettyTableText
            hClose handle1
            hClose handle2
            when useStdio (removeFile file)
        
        -- we were expecting a single file name arg
        _ -> printSingleFileUsage

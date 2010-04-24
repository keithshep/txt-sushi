-----------------------------------------------------------------------------
-- |
-- Module      :  csvzip
-- Copyright   :  (c) Keith Sheppard 2009-2010
-- License     :  BSD3
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Joins CSV files by pasting the columns together. Analogous to cbind for
-- those familliar with the R programming language. This utility streams
-- data so it can work on very large files. If the table row lengths don't
-- match then the shorter tables will be padded with empty cells.
--
-----------------------------------------------------------------------------
import Data.List(intercalate)
import Data.Version (Version(versionBranch))
import System.Environment (getArgs, getProgName)

import Database.TxtSushi.FlatFile (csvFormat, formatTable, parseTable)
import Database.TxtSushi.IOUtil (getContentsFromFileOrStdin)

import Paths_txt_sushi(version)

main :: IO ()
main = do
    fileNames <- getArgs
    
    case fileNames of
        -- parse all CSV files giving us a list of tables, then zip and print them
        (_ : _ : _) -> do
            tables <- sequence $ map getAndParseTable fileNames
            putStr $ formatTable csvFormat (zipAllColumns tables)
        
        _ -> printUsage

-- | read the contents of the given files name and parse it as a CSV file
getAndParseTable :: String -> IO [[String]]
getAndParseTable = fmap (parseTable csvFormat) . getContentsFromFileOrStdin

-- | zips together the columns of a non-empty list of tables
zipAllColumns :: [[[String]]] -> [[String]]
zipAllColumns = foldl1 (zipCols [] [])
    where
        -- if row counts don't match we pad the table that fell short with empty cells
        zipCols _     _     (x:xt) (y:yt) = (x ++ y) : zipCols x y xt yt
        zipCols _     _     []     []     = []
        zipCols _     prevY xs     []     = zipWith (++) xs (padCols prevY)
        zipCols prevX _     []     ys     = zipWith (++) (padCols prevX) ys
        
        padCols lastRow = repeat (replicate (length lastRow) "")

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn $ progName ++ " (" ++ versionStr ++ ")"
    putStrLn $ "Usage: " ++ progName ++ " csvfile_or_dash csvfile_or_dash ..."
    
    where
        versionStr = intercalate "." (map show . versionBranch $ version)

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
-- those familliar with the R programming language.
--
-----------------------------------------------------------------------------
import Data.List(intercalate)
import Data.Version (Version(..))
import System.Environment (getArgs, getProgName)
import System.IO (putStr)

import Database.TxtSushi.FlatFile (csvFormat, formatTable, parseTable)
import Database.TxtSushi.IOUtil (getContentsFromFileOrStdin)

import Paths_txt_sushi(version)

parseCsv :: String -> [[String]]
parseCsv = parseTable csvFormat

formatCsv :: [[String]] -> String
formatCsv = formatTable csvFormat

-- | zips together the columns of a non-empty list of tables
zipAllColumns :: [[[String]]] -> [[String]]
zipAllColumns = foldl1 (keepZipWith (++))
    where
        -- like zipWith except that we keep the tail if list lengths don't match
        keepZipWith f (x:xt) (y:yt) = f x y : keepZipWith f xt yt
        keepZipWith _ xs [] = xs
        keepZipWith _ [] ys = ys

main :: IO()
main = do
    fileNames <- getArgs
    
    case fileNames of
        -- parse all CSV files giving us a list of tables, then zip and print them
        (_ : _ : _) -> do
            tables <- sequence $ map (fmap parseCsv . getContentsFromFileOrStdin) fileNames
            putStr $ formatCsv (zipAllColumns tables)
        
        _ -> printUsage

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn $ progName ++ " (" ++ versionStr ++ ")"
    putStrLn $ "Usage: " ++ progName ++ " csvfile_or_dash csvfile_or_dash ..."
    
    where
        versionStr = intercalate "." (map show . versionBranch $ version)

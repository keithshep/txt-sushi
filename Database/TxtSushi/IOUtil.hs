-----------------------------------------------------------------------------
-- |
-- Module      :  Database.TxtSushi.IOUtil
-- Copyright   :  (c) Keith Sheppard 2009-2010
-- License     :  BSD3
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A couple of util functions for txt-sushi
--
-----------------------------------------------------------------------------
module Database.TxtSushi.IOUtil (
    bufferStdioToTempFile,
    getContentsFromFileOrStdin,
    printSingleFileUsage) where

import Data.List
import Data.Version (Version(..))
import System.Directory
import System.Environment
import System.IO

import Paths_txt_sushi

-- | buffers standard input to a temp file and returns a path to that file
bufferStdioToTempFile :: IO FilePath
bufferStdioToTempFile = do
    stdioText <- getContents
    tempDir <- getTemporaryDirectory
    (tempFilePath, tempFileHandle) <- openTempFile tempDir "stdiobuffer.txt"
    hPutStr tempFileHandle stdioText
    hClose tempFileHandle
    return tempFilePath

-- | if given "-" this file reads from stdin otherwise it reads from the named
--   file
getContentsFromFileOrStdin :: String -> IO String
getContentsFromFileOrStdin filePath =
    if filePath == "-"
        then getContents
        else readFile filePath

-- | print a cookie-cutter usage message for the command line utilities
--   that take a single file name or "-" as input
printSingleFileUsage :: IO ()
printSingleFileUsage = do
    progName <- getProgName
    putStrLn $ progName ++ " (" ++ versionStr ++ ")"
    putStrLn $ "Usage: " ++ progName ++ " file_name_or_dash"
    
    where
        versionStr = intercalate "." (map show . versionBranch $ version)

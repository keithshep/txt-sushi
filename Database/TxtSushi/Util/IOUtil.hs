module Database.TxtSushi.Util.IOUtil (
    bufferStdioToTempFile,
    getContentsFromFileOrStdin) where

import System.Directory
import System.IO

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
getContentsFromFileOrStdin filePath = do
    if filePath == "-"
        then do
            getContents
        else do
            handle <- openFile filePath ReadMode
            hGetContents handle

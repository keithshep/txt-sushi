module Database.TxtSushi.Util.IOUtil (
    bufferStdioToTempFile,
    getContentsFromFileOrStdin) where

import System.Directory
import System.IO

bufferStdioToTempFile = do
    stdioText <- getContents
    tempDir <- getTemporaryDirectory
    (tempFilePath, tempFileHandle) <- openTempFile tempDir "stdiobuffer.txt"
    hPutStr tempFileHandle stdioText
    hClose tempFileHandle
    return tempFilePath

getContentsFromFileOrStdin filePath = do
    if filePath == "-"
        then do
            getContents
        else do
            handle <- openFile filePath ReadMode
            hGetContents handle

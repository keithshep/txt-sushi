module Util.IOUtil (
    bufferStdioToTempFile,
    getContentsFromFileOrStdin,
    getAll) where

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

getAll [] = []
getAll (x:xt) = do
    y <- x
    yt <- getAll xt
    return (y:yt)

-- | returns an infinite list of gets from a monad
keepGetting getAction = do
    headVal <- getAction
    tailVal <- keepGetting getAction
    return (headVal:tailVal)

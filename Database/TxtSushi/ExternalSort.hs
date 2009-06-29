module Database.TxtSushi.ExternalSort (
    externalSortBy) where

import Database.TxtSushi.IO
import Database.TxtSushi.Transform

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.List
import System.IO
import System.Directory

-- | merge two sorted lists into a single sorted list
mergeBy comparisonFunction []    list2   = list2
mergeBy comparisonFunction list1 []      = list1
mergeBy comparisonFunction list1@(head1:tail1) list2@(head2:tail2) =
    case head1 `comparisonFunction` head2 of
        GT  -> (head2:(mergeBy comparisonFunction list1 tail2))
        _   -> (head1:(mergeBy comparisonFunction tail1 list2))

-- | merge the sorted lists in the list to a list about 1/2 the size
mergePairsBy _ [] = []
mergePairsBy cmp singletonListList@(headList:[]) = singletonListList
mergePairsBy cmp (list1:list2:listListTail) =
    let mergedPair = mergeBy cmp list1 list2
    in  mergedPair:(mergePairsBy cmp listListTail)

-- | merge a list of sorted lists into a single sorted list
mergeAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeAllBy _ [] = []
mergeAllBy comparisonFunction listList =
    let mergedPairs = mergePairsBy comparisonFunction listList
    in
        case mergedPairs of
            singletonListHead:[]    -> singletonListHead
            _                       -> mergeAllBy comparisonFunction mergedPairs

-- | perform a table sort using files to keep from holding the whole list
--   in memory
externalSortBy :: (Binary b) => (b -> b -> Ordering) -> [b] -> IO [b]
externalSortBy cmp xs = do
    partialSortFiles <- bufferPartialSortsBy cmp xs
    partialSortFileHandles <-
        unwrapIOList [openBinaryFile file ReadMode | file <- partialSortFiles]
    partialSortBinStrs <-
        unwrapIOList $ map hGetAllBinStr partialSortFileHandles
    
    return $ mergeAllBy cmp (map (map decode) partialSortBinStrs)

-- | unwrap a list of 'IO' boxed items
unwrapIOList [] = do return []
unwrapIOList (ioHead:ioTail) = do
    unwrappedHead <- ioHead
    unwrappedTail <- unwrapIOList ioTail
    return (unwrappedHead:unwrappedTail)

-- | create a list of parial sorts
bufferPartialSortsBy cmp [] = return []
bufferPartialSortsBy cmp xs = do
    let inMemLimit = 100000
        (sortNowList, sortLaterList) = splitAt inMemLimit xs
        sortedRows = sortBy cmp sortNowList
    sortBuffer <- bufferToTempFile sortedRows
    otherSortBuffers <- bufferPartialSortsBy cmp sortLaterList
    return (sortBuffer:otherSortBuffers)

-- | buffer the table to a temporary file and return a handle to that file
bufferToTempFile :: (Binary b) => [b] -> IO String
bufferToTempFile [] = return []
bufferToTempFile xs = do
    tempDir <- getTemporaryDirectory
    (tempFilePath, tempFileHandle) <- openBinaryTempFile tempDir "buffer.txt"
    hPutAllBinStr tempFileHandle (map encode xs)
    hClose tempFileHandle
    return tempFilePath

hPutAllBinStr :: Handle -> [BS.ByteString] -> IO ()
hPutAllBinStr handle [] = return ()
hPutAllBinStr handle (binStrHead:binStrTail) =
    hPutBinStr handle binStrHead >> hPutAllBinStr handle binStrTail

hPutBinStr :: Handle -> BS.ByteString -> IO ()
hPutBinStr handle binStr = do
    let lenPut = putWord32host $ fromIntegral (BS.length binStr)
    BS.hPut handle (runPut lenPut)
    BS.hPut handle binStr

hGetAllBinStr :: Handle -> IO [BS.ByteString]
hGetAllBinStr handle = do
    eof <- hIsEOF handle
    if eof
        then return []
        else do
            headBin <- hGetBinStr handle
            tailBin <- hGetAllBinStr handle
            return $ headBin:tailBin

hGetBinStr :: Handle -> IO BS.ByteString
hGetBinStr handle = do
    lenStr <- BS.hGet handle 4
    let len = fromIntegral $ runGet getWord32host lenStr :: Int
    BS.hGet handle len

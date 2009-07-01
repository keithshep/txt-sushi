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
import System.IO.Unsafe
import System.Directory

-- TODO configuration based on max bytes and max file merge

-- | performs an external sort on the given list
externalSort :: (Binary b, Ord b) => [b] -> [b]
externalSort = externalSortBy compare

-- | performs an external sort on the given list using the given comparison
--   function
externalSortBy :: (Binary b) => (b -> b -> Ordering) -> [b] -> [b]
externalSortBy cmp xs = unsafePerformIO $ do
    partialSortFiles <- bufferPartialSortsBy cmp xs
    partialSortFileHandles <-
        unwrapMonadList [openBinaryFile file ReadMode | file <- partialSortFiles]
    partialSortByteStrs <-
        unwrapMonadList $ map BS.hGetContents partialSortFileHandles
    
    let partialSortBinaries = map decodeAll partialSortByteStrs
    return $ mergeAllBy cmp partialSortBinaries

-- | unwrap a list of Monad 'boxed' items
unwrapMonadList [] = return []
unwrapMonadList (ioHead:ioTail) = do
    unwrappedHead <- ioHead
    unwrappedTail <- unwrapMonadList ioTail
    return (unwrappedHead:unwrappedTail)

-- | merge a list of sorted lists into a single sorted list
mergeAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeAllBy _ [] = []
mergeAllBy comparisonFunction listList =
    let mergedPairs = mergePairsBy comparisonFunction listList
    in
        case mergedPairs of
            singletonListHead:[] -> singletonListHead
            _                    -> mergeAllBy comparisonFunction mergedPairs

-- | merge the sorted lists in the list to a list about 1/2 the size
mergePairsBy _ [] = []
mergePairsBy cmp singletonListList@(headList:[]) = singletonListList
mergePairsBy cmp (list1:list2:listListTail) =
    let mergedPair = mergeBy cmp list1 list2
    in  mergedPair:(mergePairsBy cmp listListTail)

-- | merge two sorted lists into a single sorted list
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy comparisonFunction []    list2   = list2
mergeBy comparisonFunction list1 []      = list1
mergeBy comparisonFunction list1@(head1:tail1) list2@(head2:tail2) =
    case head1 `comparisonFunction` head2 of
        GT  -> (head2:(mergeBy comparisonFunction list1 tail2))
        _   -> (head1:(mergeBy comparisonFunction tail1 list2))

-- | create a list of parial sorts
bufferPartialSortsBy cmp [] = return []
bufferPartialSortsBy cmp xs = do
    let inMemLimit = 100000
        (sortNowList, sortLaterList) = splitAt inMemLimit xs
        sortedRows = sortBy cmp sortNowList
    sortBuffer <- bufferToTempFile sortedRows
    otherSortBuffers <- bufferPartialSortsBy cmp sortLaterList
    return (sortBuffer:otherSortBuffers)

-- | buffer the binaries to a temporary file and return a handle to that file
bufferToTempFile :: (Binary b) => [b] -> IO String
bufferToTempFile [] = return []
bufferToTempFile xs = do
    tempDir <- getTemporaryDirectory
    (tempFilePath, tempFileHandle) <- openBinaryTempFile tempDir "buffer.txt"
    
    BS.hPut tempFileHandle (encodeAll xs)
    hClose tempFileHandle
    
    return tempFilePath

encodeAll :: (Binary b) => [b] -> BS.ByteString
encodeAll = BS.concat . map encode

decodeAll :: (Binary b) => BS.ByteString -> [b]
decodeAll bs
    | BS.null bs = []
    | otherwise =
        let (decodedBin, remainingBs, _) = runGetState get bs 0
        in  decodedBin : decodeAll remainingBs

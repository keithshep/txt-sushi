module Database.TxtSushi.ExternalSort (
    externalSortBy,
    externalSortByQuota) where

import Database.TxtSushi.IO
import Database.TxtSushi.Transform

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import qualified Data.ByteString.Lazy as BS
import Data.List
import System.IO
import System.IO.Unsafe
import System.Directory

-- | performs an external sort on the given list
externalSort :: (Binary b, Ord b) => [b] -> [b]
externalSort = externalSortBy compare

-- | performs an external sort on the given list using the given comparison
--   function
externalSortBy :: (Binary b) => (b -> b -> Ordering) -> [b] -> [b]
externalSortBy = externalSortByQuota defaultByteQuota
    -- 128 MB is the default quota
    where defaultByteQuota = 64 * 1024 * 1024

-- | performs an external sort on the given list using the given comparison
--   function
externalSortByQuota :: (Binary b, Integral i) => i -> (b -> b -> Ordering) -> [b] -> [b]
externalSortByQuota byteQuota cmp xs = unsafePerformIO $ do
    partialSortFiles <- bufferPartialSortsBy (fromIntegral byteQuota) cmp xs
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
bufferPartialSortsBy :: (Binary b) => Int64 -> (b -> b -> Ordering) -> [b] -> IO [String]
bufferPartialSortsBy _ _ [] = return []
bufferPartialSortsBy byteQuota cmp xs = do
    let (sortNowList, sortLaterList) = splitAfterQuota byteQuota xs
        sortedRows = sortBy cmp sortNowList
    sortBuffer <- bufferToTempFile sortedRows
    otherSortBuffers <- bufferPartialSortsBy byteQuota cmp sortLaterList
    return (sortBuffer:otherSortBuffers)

splitAfterQuota :: (Binary b) => Int64 -> [b] -> ([b], [b])
splitAfterQuota _ [] = ([], [])
splitAfterQuota quotaInBytes (binaryHead:binaryTail) =
    let
        quotaRemaining = quotaInBytes - BS.length (encode binaryHead)
        (fstBinsTail, sndBins) = splitAfterQuota quotaRemaining binaryTail
    in
        if quotaRemaining <= 0
        then ([binaryHead], binaryTail)
        else (binaryHead:fstBinsTail, sndBins)

{-
-- | Encode the given binaries respecting the given byte quota (Well, kind
--   of respecting. Really we'll probably go over by one element)
encodeByQuota :: (Binary b, Integral i) => i -> [b] -> (BS.ByteString, [b])
encodeByQuota byteQuota binaries =
    let (byteStrings, binsLeft) = go (fromIntegral byteQuota) binaries
    in  (BS.concat byteStrings, binsLeft)
    where
        -- the go function does all of the real work
        go _ [] = ([], [])
        go quota (binHead:binTail) =
            let
                headByteStr = encode binHead
                quotaRemaining = quota - BS.length headByteStr
                (tailByteStrs, binsLeft) = go quotaRemaining binTail
            in
                if quotaRemaining >= 0
                then (headByteStr:tailByteStrs, binsLeft)
                else ([headByteStr], binTail)
-}

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

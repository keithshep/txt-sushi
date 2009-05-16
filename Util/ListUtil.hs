module Util.ListUtil (
    cascadingOrder,
    replaceAll) where

import Data.List

{-
replace all instances of 'targetSublist' found in 'list' with
'replacementList'
-}
replaceAll :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceAll [] _ _ = []
replaceAll list@(listHead:listTail) targetSublist replacementList =
    if targetSublist `isPrefixOf` list then
        let remainingList = drop (length targetSublist) list
        in  replacementList ++ (replaceAll remainingList targetSublist replacementList)
    else
        listHead:(replaceAll listTail targetSublist replacementList)

-- | applies a cascading order logic where 1st non-equal ordering defines
--   the ordering for the list. If they're all equal (or the list is empty)
--   then return EQ
cascadingOrder :: [Ordering] -> Ordering
cascadingOrder [] = EQ
cascadingOrder (LT:_) = LT
cascadingOrder (GT:_) = GT
cascadingOrder (EQ:tailOrders) = cascadingOrder tailOrders

module Util.ListUtil (
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

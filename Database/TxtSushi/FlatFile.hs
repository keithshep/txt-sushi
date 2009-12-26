{- |
The 'FlatFile' module is for reading misc. 'FlatFile' formats like CSV or
tab delimited
-}
module Database.TxtSushi.FlatFile (
    formatTableWithWidths,
    maxTableColumnWidths,
    formatTable,
    parseTable,
    Format(Format),
    csvFormat,
    tabDelimitedFormat,
    doubleQuote) where

import Data.List
import Database.TxtSushi.ParseUtil
import Text.ParserCombinators.Parsec

{- |
'Format' allows you to specify different flat-file formats so that you
can use 'parseTable' for CSV, tab-delimited etc.
-}
data Format = Format {
    quote :: Char,
    fieldDelimiter :: Char} deriving (Show)

csvFormat :: Format
csvFormat = Format '"' ','

tabDelimitedFormat :: Format
tabDelimitedFormat = Format '"' '\t'

{- |
get a quote escape sequence for the given 'Format'
-}
doubleQuote :: Format -> String
doubleQuote format = replicate 2 (quote format)

formatTableWithWidths :: String -> [Int] -> [[String]] -> String
formatTableWithWidths _ _ [] = []
formatTableWithWidths boundaryString widths (row:tableTail) =
    let
        (initCells, [lastCell]) = splitAt (length row - 1) row
    in
        (concat $ zipWith ensureWidth widths initCells) ++ lastCell ++
        "\n" ++ (formatTableWithWidths boundaryString widths tableTail)
    where
        ensureWidth width field =
            let lengthField = length field
            in
                if width > lengthField then
                    field ++ (replicate (width - lengthField) ' ') ++ boundaryString
                else
                    field ++ boundaryString

{- |
for a table, calculate the max width in characters for each column
-}
maxTableColumnWidths :: [[String]] -> [Int]
maxTableColumnWidths [] = []
maxTableColumnWidths table =
    maxTableColumnWidthsInternal table []

maxTableColumnWidthsInternal :: [[String]] -> [Int] -> [Int]
maxTableColumnWidthsInternal [] prevMaxValues = prevMaxValues
maxTableColumnWidthsInternal (row:tableTail) prevMaxValues
    | seqList prevMaxValues = undefined
    | otherwise = maxTableColumnWidthsInternal tableTail (maxRowFieldWidths row prevMaxValues)

-- this filthy little function is for making the list strict... otherwise
-- we run out of memory
seqList :: [a] -> Bool
seqList [] = False
seqList (x:xt)
    | x `seq` False = undefined
    | otherwise = seqList xt

maxRowFieldWidths :: [String] -> [Int] -> [Int]
maxRowFieldWidths row prevMaxValues =
    zipWithD max (map length row) prevMaxValues

zipWithD :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithD f (x:xt) (y:yt) = (f x y):(zipWithD f xt yt)
zipWithD _ [] ys = ys
zipWithD _ xs [] = xs

{- |
Format the given table (the 2D String array) into a flat-file string using
the given 'Format'
-}
formatTable :: Format -> [[String]] -> String
formatTable _ [] = ""
formatTable format (headRow:tableTail) =
    (formatRow format headRow) ++ "\n" ++ (formatTable format tableTail)

{- |
Format the row into a flat file sub-string using the given 'Format'
-}
formatRow :: Format -> [String] -> String
formatRow _ [] = []
formatRow format (headField:rowTail) =
    -- we need to escape any quotes
    let escapedField = encodeField format headField
    in
        -- use a field delimiter on all but the last field
        if null rowTail then
            escapedField
        else
            escapedField ++ [fieldDelimiter format] ++ (formatRow format rowTail)

{- |
encode the given text field if it contains any special formatting characters
-}
encodeField :: Format -> String -> String
encodeField format field =
    if quote format `elem` field then
        let escapedField = replaceAll field [quote format] (doubleQuote format)
        in  (quote format) : escapedField ++ [quote format]
    else if '\n' `elem` field ||
            [fieldDelimiter format] `isInfixOf` field then
        (quote format) : field ++ [quote format]
    else
        field

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

-- flat file parsing (based on Real World Haskell example)

tableParser :: Format -> GenParser Char st [[String]]
--tableParser format = endBy (line format) eol
tableParser format = fmap trimLastEmpty (sepBy (line format) eol)
    where
        trimLastEmpty [] = []
        trimLastEmpty [[""]] = []
        trimLastEmpty (x:xt) = x : trimLastEmpty xt

line :: Format -> GenParser Char st [String]
line format = sepBy (cell format) (char $ fieldDelimiter format)

cell :: Format -> GenParser Char st String
cell format = quotedText True (quote format) <|> many (noneOf ",\n\r")

eol :: GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseTableOrError :: Format -> String -> Either ParseError [[String]]
parseTableOrError format input = parse (tableParser format) "(unknown)" input

{- |
Parse the given text using the given flat file 'Format'. The result
is a list of list of strings. The strings are fields and the string
lists are rows
-}
parseTable :: Format -> String -> [[String]]
parseTable format text = case parseTableOrError format text of
    Left err -> error $ show err
    Right table -> table

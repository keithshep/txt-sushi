{- |
The 'FlatFile' module is for reading misc. 'FlatFile' formats like CSV or
tab delimited
-}
module TxtSushi.IO (
    formatTableWithWidths,
    maxTableColumnWidths,
    formatTable,
    parseTable,
    Format(Format),
    csvFormat,
    tabDelimitedFormat,
    doubleQuote) where

import Data.List
import Util.ListUtil

{- |
'Format' allows you to specify different flat-file formats so that you
can use 'parseTable' for CSV, tab-delimited etc.
-}
data Format = Format {
    quote :: String,
    fieldDelimiter :: String,
    rowDelimiter :: String} deriving (Show)

csvFormat = Format "\"" "," "\n"
tabDelimitedFormat = Format "\"" "\t" "\n"

{- |
get a quote escape sequence for the given 'Format'
-}
doubleQuote :: Format -> String
doubleQuote format = (quote format) ++ (quote format)

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
seqList [] = False
seqList (head:tail)
    | head `seq` False = undefined
    | otherwise = seqList tail

maxRowFieldWidths :: [String] -> [Int] -> [Int]
maxRowFieldWidths row prevMaxValues =
    zipWithD max (map length row) prevMaxValues
{-
    let colLengths = map length row
        lengthOfRow = length row
        lengthOfPrevMax = length prevMaxValues
        maxPrefixList = zipWith max colLengths prevMaxValues
    in
        if lengthOfRow == lengthOfPrevMax then
            maxPrefixList
        else if lengthOfRow > lengthOfPrevMax then
            maxPrefixList ++ (drop lengthOfPrevMax colLengths)
        else
            maxPrefixList ++ (drop lengthOfRow prevMaxValues)
-}

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
    (formatRow format headRow) ++ (rowDelimiter format) ++ (formatTable format tableTail)

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
            escapedField ++ (fieldDelimiter format) ++ (formatRow format rowTail)

{- |
encode the given text field if it contains any special formatting characters
-}
encodeField format field =
    if (quote format) `isInfixOf` field then
        let escapedField = replaceAll field (quote format) (doubleQuote format)
        in  (quote format) ++ escapedField ++ (quote format)
    else if (rowDelimiter format) `isInfixOf` field ||
            (fieldDelimiter format) `isInfixOf` field then
        (quote format) ++ field ++ (quote format)
    else
        field

{- |
Parse the given text using the given flat file 'Format'. The result
is a list of list of strings. The strings are fields and the string
lists are rows
-}
parseTable :: Format -> String -> [[String]]
parseTable _ [] = []
parseTable format text =
    let (nextLine, remainingText) = parseLine format text
    in  nextLine:(parseTable  format remainingText)

-- parse a row giving (rowFields, remainingText)
parseLine :: Format -> String -> ([String], String)
parseLine _ [] = ([], "")
parseLine format text =
    let (nextField, moreFieldsInRow, textRemainingAfterField) = parseField format text
    in
        -- if there are more fields, recursively add them to the row
        if moreFieldsInRow then
            let (rowTail, remainingText) = parseLine format textRemainingAfterField
            in  (nextField:rowTail, remainingText)
        
        -- if there are no more fields return the current fields as a singleton
        -- list
        else
            ([nextField], textRemainingAfterField)

-- parse a field giving (field, moreFieldsInRow, remainingText)
parseField :: Format -> String -> (String, Bool, String)
parseField _ [] = ("", False, "")
parseField format text =
    -- check if this field is quoted or not
    if (quote format) `isPrefixOf` text then
        let tailOfQuote = drop (length (quote format)) text
        in  parseQuotedField format tailOfQuote
    else
        parseUnquotedField format text

-- parse a quoted field giving (field, moreFieldsInRow, remainingText)
parseQuotedField :: Format -> String -> (String, Bool, String)
parseQuotedField _ [] = ("", False, "")
parseQuotedField format text@(textHead:textTail) =
    -- a double quote is an escaped quote, so add a quote to the field
    if (doubleQuote format) `isPrefixOf` text then
        let tailOfDoubleQuote = drop (length (doubleQuote format)) text
            (fieldTail, moreFieldsInRow, remainingText) = parseQuotedField format tailOfDoubleQuote
        in  ((quote format) ++ fieldTail, moreFieldsInRow, remainingText)
    
    -- a single quote is the end of the field, we can use parseUnquotedField to
    -- chew up any chars between the ending quote and the next delimiter (there
    -- really shouldn't be any if the text is formatted well, but you never
    -- know)
    else if (quote format) `isPrefixOf` text then
        let tailOfQuote = drop (length (quote format)) text
            (_, moreFieldsInRow, remainingText) = parseUnquotedField format tailOfQuote
        in  ("", moreFieldsInRow, remainingText)
    
    -- just another character... toss it in the field and keep going
    else
        let (fieldTail, moreFieldsInRow, remainingText) = parseQuotedField format textTail
        in  (textHead:fieldTail, moreFieldsInRow, remainingText)

-- parse an unquoted field giving (field, moreFieldsInRow, remainingText)
parseUnquotedField :: Format -> String -> (String, Bool, String)
parseUnquotedField _ [] = ("", False, "")
parseUnquotedField format text@(textHead:textTail) =
    -- if we hit a field delimiter: return an empty string and let caller know
    -- there are more fields in this row
    if (fieldDelimiter format) `isPrefixOf` text then
        let tailOfDelimiter = drop (length (fieldDelimiter format)) text
        in  ([], True, tailOfDelimiter)
    
    -- if we hit a row delimiter: return an empty string and let caller know there
    -- are no more fields in this row
    else if (rowDelimiter format) `isPrefixOf` text then
        let tailOfDelimiter = drop (length (rowDelimiter format)) text
        in  ([], False, tailOfDelimiter)
    
    -- just another character... toss it in the field and keep going
    else
        let (fieldTail, moreFieldsInRow, remainingText) = parseUnquotedField format textTail
        in  (textHead:fieldTail, moreFieldsInRow, remainingText)

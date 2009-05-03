-----------------------------------------------------------------------------
-- |
-- Module      :  TxtSushi.SQLParser
-- Copyright   :  (c) Keith Sheppard 2009
-- License     :  GPL3 or greater
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for parsing SQL
--
-----------------------------------------------------------------------------

module TxtSushi.SQLParser (
    allMaybeTableNames,
    parseSelectStatement,
    SelectStatement(..),
    TableExpression(..),
    ColumnIdentifier(..),
    ColumnSelection(..),
    Expression,
    SQLFunction) where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Util.ListUtil

--------------------------------------------------------------------------------
-- The data definition for select statements
--------------------------------------------------------------------------------

-- | represents a select statement
data SelectStatement = SelectStatement {
    columnSelections :: [ColumnSelection],
    maybeFromTable :: Maybe TableExpression,
    maybeWhereFilter :: Maybe Expression}
    deriving (Show, Ord, Eq)

data TableExpression =
    TableIdentifier {
        tableName :: String,
        maybeTableAlias :: Maybe String} |
    InnerJoin {
        leftJoinTable :: TableExpression,
        rightJoinTable :: TableExpression,
        -- TODO this should eventually be more general like
        -- joinCondition :: ConditionalExpression}
        joinColumns :: [(ColumnIdentifier, ColumnIdentifier)],
        maybeTableAlias :: Maybe String} |
    CrossJoin {
        leftJoinTable :: TableExpression,
        rightJoinTable :: TableExpression,
        maybeTableAlias :: Maybe String}
    deriving (Show, Ord, Eq)

allMaybeTableNames :: (Maybe TableExpression) -> [String]
allMaybeTableNames Nothing = []
allMaybeTableNames (Just tblExp) = allTableNames tblExp

allTableNames (TableIdentifier tblName _) = [tblName]
allTableNames (InnerJoin lftTbl rtTbl _ _) =
    (allTableNames lftTbl) ++ (allTableNames rtTbl)
allTableNames (CrossJoin lftTbl rtTbl _) =
    (allTableNames lftTbl) ++ (allTableNames rtTbl)

data ColumnSelection =
    AllColumns |
    AllColumnsFrom {sourceTableName :: String} |
    QualifiedColumn {
        qualifiedColumnId :: ColumnIdentifier}
    deriving (Show, Ord, Eq)

data ColumnIdentifier =
    ColumnIdentifier {
        maybeTableName :: Maybe String,
        columnId :: String}
    deriving (Show, Ord, Eq)

data Expression =
    FunctionExpression {
        sqlFunction :: SQLFunction,
        functionArguments :: [Expression]} |
    ColumnExpression {
        column :: ColumnIdentifier} |
    StringConstantExpression {
        stringConstant :: String} |
    IntegerConstantExpression {
        intConstant :: Int}
    deriving (Show, Ord, Eq)

data SQLFunction = SQLFunction {
    functionName :: String,
    minArgCount :: Int,
    argCountIsFixed :: Bool}
    deriving (Show, Ord, Eq)

parseSelectStatement = do
    try $ upperOrLower "SELECT" >> spaces1
    parseSelectBody

parseSelectBody = do
    columnVals <- parseColumnSelections
    -- TODO need a better error message for missing "ON" etc. in
    -- the from part, can do this by grabing "FROM" first
    maybeFrom <- maybeParseFromPart
    maybeWhere <- maybeParseWherePart
    spaces
    eof
    
    return SelectStatement {
        columnSelections    = columnVals,
        maybeFromTable      = maybeFrom,
        maybeWhereFilter    = maybeWhere}
    
    where maybeParseFromPart =
            ifParseThen (spaces1 >> upperOrLower "FROM" >> spaces1) parseTableExpression
          maybeParseWherePart =
            ifParseThen (spaces1 >> upperOrLower "WHERE" >> spaces1) parseExpression

--------------------------------------------------------------------------------
-- Functions for parsing the column names specified after "SELECT"
--------------------------------------------------------------------------------

parseColumnSelections =
    sepBy1 parseAnyColType (try parseCommaSeparator)
    where parseAnyColType = parseAllCols <|>
                            (try parseAllColsFromTbl) <|>
                            (try parseSpecificCol)

parseAllCols = string "*" >> return AllColumns

parseAllColsFromTbl = do
    tableVal <- parseIdentifier
    string ".*"
    
    return $ AllColumnsFrom tableVal

parseSpecificCol =
    parseColumnId >>= (\colId -> return $ QualifiedColumn colId)

parseColumnId = do
    firstId <- parseIdentifier
    
    maybeFullyQual <- maybeParse $ char '.'
    case maybeFullyQual of
        -- No '.' means it's a partially qualified column
        Nothing -> return $ ColumnIdentifier Nothing firstId
        Just _ -> do
            secondId <- parseIdentifier
            return $ ColumnIdentifier (Just firstId) secondId

--------------------------------------------------------------------------------
-- Functions for parsing the table part (after "FROM")
--------------------------------------------------------------------------------

parseTableExpression = do
    nextTblChunk <- parseNextTblExpChunk
    
    let ifInnerJoinParse = ifParseThenElse
            -- if
            parseInnerJoinKeywords
            -- then
            (parseInnerJoinRemainder nextTblChunk)
            -- else
            (return nextTblChunk)
        
        ifCrossOrInnerJoinParse = ifParseThenElse
            -- if
            parseCrossJoinKeywords
            -- then
            (parseCrossJoinRemainder nextTblChunk)
            -- else
            ifInnerJoinParse
    
    ifCrossOrInnerJoinParse

parseNextTblExpChunk =
    parenthesize parseTableExpression <|>  parseTableIdentifier

parseJoinColumns = sepBy1 parseColEqCol (try parseAndSeparator)

parseAndSeparator = spaces1 >> upperOrLower "AND" >> spaces1

parseColEqCol = do
    leftCol <- parseColumnId
    spaces
    string "="
    spaces
    rightCol <- parseColumnId
    
    return (leftCol, rightCol)

parseCrossJoinKeywords = do
    spaces1
    upperOrLower "CROSS"
    spaces1
    upperOrLower "JOIN"
    spaces1

parseInnerJoinKeywords = do
    spaces1
    maybeParse $ upperOrLower "INNER" >> spaces1
    upperOrLower "JOIN"
    spaces1

parseInnerJoinRemainder leftTblExpr = do
    rightTblExpr <- parseTableExpression
    
    spaces1
    upperOrLower "ON"
    spaces1
    colIds <- parseJoinColumns
    
    maybeAlias <- maybeParse $ spaces1 >> parseTableAlias
    
    return InnerJoin {
            leftJoinTable=leftTblExpr,
            rightJoinTable=rightTblExpr,
            joinColumns=colIds,
            maybeTableAlias=maybeAlias}

parseCrossJoinRemainder leftTblExpr = do
    rightTblExpr <- parseTableExpression
    maybeAlias <- maybeParse $ spaces1 >> parseTableAlias
    
    return CrossJoin {
            leftJoinTable=leftTblExpr,
            rightJoinTable=rightTblExpr,
            maybeTableAlias=maybeAlias}

parseTableIdentifier = do
    theId <- parseIdentifier
    maybeAlias <- maybeParse $ spaces1 >> parseTableAlias
    return $ TableIdentifier theId maybeAlias

parseTableAlias = upperOrLower "AS" >> spaces1 >> parseIdentifier

--------------------------------------------------------------------------------
-- Expression parsing: These can be after "SELECT", "WHERE" or "HAVING"
--------------------------------------------------------------------------------

parseExpression = parseAnyNormalFunction {-<|> parseAnyInfixFunction-}

parseAnyNormalFunction =
    let allParsers = map (try . parseNormalFunction) normalSyntaxFunctions
    in choice allParsers

parseNormalFunction sqlFunc = do
    upperOrLower $ functionName sqlFunc
    args <- argSepBy (minArgCount sqlFunc) parseExpression parseCommaSeparator
    return $ FunctionExpression sqlFunc args
    where argSepBy = if argCountIsFixed sqlFunc then sepByExactly else sepByAtLeast

-- Functions with "normal" syntax --
normalSyntaxFunctions = [upperFunction, lowerFunction, trimFunction]

upperFunction = SQLFunction {
    functionName    = "UPPER",
    minArgCount     = 1,
    argCountIsFixed = True}

lowerFunction = SQLFunction {
    functionName    = "LOWER",
    minArgCount     = 1,
    argCountIsFixed = True}

trimFunction = SQLFunction {
    functionName    = "TRIM",
    minArgCount     = 1,
    argCountIsFixed = True}

-- TODO we need to care about operator precidence
--parseInfixFunction =
--    let allInfixOps = map (try . string) (map functionName (concat infixFunctions))

-- Infix functions --
infixFunctions =
    [[multiplyFunction, divideFunction],
     [plusFunction, minusFunction],
     [isFunction, isNotFunction, lessThanFunction, lessThanOrEqualToFunction,
      greaterThanFunction, greaterThanOrEqualToFunction],
     [andFunction, orFunction]]

-- Algebraic
multiplyFunction = SQLFunction {
    functionName    = "*",
    minArgCount     = 2,
    argCountIsFixed = True}

divideFunction = SQLFunction {
    functionName    = "/",
    minArgCount     = 2,
    argCountIsFixed = True}

plusFunction = SQLFunction {
    functionName    = "+",
    minArgCount     = 2,
    argCountIsFixed = True}

minusFunction = SQLFunction {
    functionName    = "-",
    minArgCount     = 2,
    argCountIsFixed = True}

-- Boolean
isFunction = SQLFunction {
    functionName    = "=",
    minArgCount     = 2,
    argCountIsFixed = True}

isNotFunction = SQLFunction {
    functionName    = "<>",
    minArgCount     = 2,
    argCountIsFixed = True}

lessThanFunction = SQLFunction {
    functionName    = "<",
    minArgCount     = 2,
    argCountIsFixed = True}

lessThanOrEqualToFunction = SQLFunction {
    functionName    = "<=",
    minArgCount     = 2,
    argCountIsFixed = True}

greaterThanFunction = SQLFunction {
    functionName    = ">",
    minArgCount     = 2,
    argCountIsFixed = True}

greaterThanOrEqualToFunction = SQLFunction {
    functionName    = ">=",
    minArgCount     = 2,
    argCountIsFixed = True}

andFunction = SQLFunction {
    functionName    = "AND",
    minArgCount     = 2,
    argCountIsFixed = True}

orFunction = SQLFunction {
    functionName    = ">",
    minArgCount     = 2,
    argCountIsFixed = True}



-- Functions with special syntax --
specialFunctions = [substringFromFunction,
                    substringFromToFunction]

-- | SUBSTRING(extraction_string FROM starting_position [FOR length]
--             [COLLATE collation_name])
--   We don't yet support the COLLATE part
substringFromFunction = SQLFunction {
    functionName    = "SUBSTRING",
    minArgCount     = 2,
    argCountIsFixed = True}
substringFromToFunction = SQLFunction {
    functionName    = "SUBSTRING",
    minArgCount     = 3,
    argCountIsFixed = True}

--------------------------------------------------------------------------------
-- Parse utility functions
--------------------------------------------------------------------------------

-- | parses an identifier. you can use a tick '`' as a quote for
--   an identifier with white-space
parseIdentifier = do
    let parseId = do
            let idChar = alphaNum <|> char '_'
            quotedText False '`' <|> many1 idChar
    (parseId `genExcept` parseReservedWord) <?> "identifier"

quotedText allowEmpty quoteChar = do
    let quote = char quoteChar
        manyFunc = if allowEmpty then many else many1
    
    quote
    textValue <- manyFunc $ (anyChar `genExcept` quote) <|>
                            try (escapedQuote quoteChar)
    quote
    
    return textValue

exceptChar parser theException = notFollowedBy theException >> parser

escapedQuote quoteChar = string [quoteChar, quoteChar] >> return quoteChar

parseCommaSeparator = spaces >> char ',' >> spaces

parenthesize innerParser = do
    char '('
    spaces
    innerParseResults <- innerParser
    spaces
    char ')'
    return innerParseResults

eitherParse leftParser rightParser =
    do {parseResult <- try leftParser; return $ Left parseResult} <|>
    do {parseResult <- rightParser; return $ Right parseResult}

spaces1 = skipMany1 space <?> "whitespace"

ifParseThen ifParse thenPart = do
    ifResult <- maybeParse ifParse
    case ifResult of
        Just _ ->   thenPart >>= (\x -> return $ Just x)
        Nothing ->  return Nothing

ifParseThenElse ifParse thenPart elsePart = do
    ifResult <- maybeParse ifParse
    case ifResult of
        Just _ -> thenPart
        Nothing -> elsePart

parseReservedWord = do
    let reservedWordParsers = map reservedWordParser reservedWords
    choice reservedWordParsers
    where reservedWordParser word = do
            parseVal <- upperOrLower word
            notFollowedBy alphaNum
            return parseVal

reservedWords = ["BY",
                 "CROSS",
                 "FROM",
                 "GROUP",
                 "HAVING",
                 "INNER",
                 "JOIN",
                 "ON",
                 "SELECT",
                 "WHERE"]

upperOrLower stringToParse =
    string (map toUpper stringToParse) <|>
    string (map toLower stringToParse) <?> stringToParse

genExcept parser theException = do
    genNotFollowedBy theException
    parser

genNotFollowedBy theParser = try $ do
    mayParseResult <- maybeParse theParser
    case mayParseResult of
        Nothing -> return ()
        Just x -> unexpected $ show x

-- | returns Just parseResult if the parse succeeds and Nothing if it fails
maybeParse parser =
    (try parser >>= (\x -> return $ Just x)) <|>
    return Nothing

-- | parse `itemParser`s seperated by exactly `minCount` `sepParser`s
sepByExactly  :: Int -> GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepByExactly count itemParser sepParser =
    let itemParsers = replicate count itemParser
    in parseEach itemParsers
    where parseEach [] = return []
          parseEach (headParser:parserTail) = do
            resultHead <- headParser
            sepParser
            resultTail <- parseEach parserTail
            
            return $ resultHead:resultTail

-- | parse `itemParser`s seperated by at least `minCount` `sepParser`s
sepByAtLeast  :: Int -> GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepByAtLeast minCount itemParser sepParser = do
    minResults <- sepByExactly minCount itemParser sepParser
    ifParseThenElse
        sepParser
        (sepBy1 itemParser sepParser >>= (\tailResults -> return $ minResults ++ tailResults))
        (return minResults)

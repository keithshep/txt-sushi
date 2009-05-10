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
    prettyFormatColumn,
    ColumnSelection(..),
    Expression(..),
    OrderByItem(..),
    SQLFunction(..),
    
    -- SQL functions with "normal" syntax
    upperFunction,
    lowerFunction,
    trimFunction,
    
    -- Algebraic infix SQL functions
    multiplyFunction,
    divideFunction,
    plusFunction,
    minusFunction,
    
    -- Boolean infix SQL functions
    isFunction,
    isNotFunction,
    lessThanFunction,
    lessThanOrEqualToFunction,
    greaterThanFunction,
    greaterThanOrEqualToFunction,
    andFunction,
    orFunction) where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Util.ListUtil

--------------------------------------------------------------------------------
-- The data definition for select statements
--------------------------------------------------------------------------------

-- | represents a select statement
--   TODO this should be moved inside the TableExpression type
data SelectStatement = SelectStatement {
    columnSelections :: [ColumnSelection],
    maybeFromTable :: Maybe TableExpression,
    maybeWhereFilter :: Maybe Expression,
    orderByItems :: [OrderByItem]}
    deriving (Show, Ord, Eq)

data TableExpression =
    TableIdentifier {
        tableName :: String,
        maybeTableAlias :: Maybe String} |
    InnerJoin {
        leftJoinTable :: TableExpression,
        rightJoinTable :: TableExpression,
        onCondition :: Expression,
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

-- | I wanted to leave the default Show, but I also wanted a pretty print, so
--   here it is!
prettyFormatColumn :: ColumnIdentifier -> String
prettyFormatColumn (ColumnIdentifier (Just tblName) colId) = tblName ++ "." ++ colId
prettyFormatColumn (ColumnIdentifier (Nothing) colId) = colId

data Expression =
    FunctionExpression {
        sqlFunction :: SQLFunction,
        functionArguments :: [Expression]} |
    ColumnExpression {
        column :: ColumnIdentifier} |
    StringConstantExpression {
        stringConstant :: String} |
    IntegerConstantExpression {
        intConstant :: Int} |
    RealConstantExpression {
        realConstant :: Double}
    deriving (Show, Ord, Eq)

data SQLFunction = SQLFunction {
    functionName :: String,
    minArgCount :: Int,
    argCountIsFixed :: Bool}
    deriving (Show, Ord, Eq)

data OrderByItem = OrderByItem {
    orderExpression :: Expression,
    orderAscending :: Bool}
    deriving (Show, Ord, Eq)

-- | Parses a SQL select statement
parseSelectStatement :: GenParser Char st SelectStatement
parseSelectStatement = do
    try $ upperOrLower "SELECT" >> spaces1
    parseSelectBody

-- | Parses all of the stuff that comes after "SELECT "
parseSelectBody :: GenParser Char st SelectStatement
parseSelectBody = do
    columnVals <- parseColumnSelections
    -- TODO need a better error message for missing "ON" etc. in
    -- the from part, can do this by grabing "FROM" first
    maybeFrom <- maybeParseFromPart
    maybeWhere <- maybeParseWherePart
    orderBy <- parseOrderByPart
    
    spaces
    eof
    
    return SelectStatement {
        columnSelections    = columnVals,
        maybeFromTable      = maybeFrom,
        maybeWhereFilter    = maybeWhere,
        orderByItems        = orderBy}
    
    where
        maybeParseFromPart =
            ifParseThen (spaces1 >> upperOrLower "FROM" >> spaces1) parseTableExpression
        
        maybeParseWherePart =
            ifParseThen (spaces1 >> upperOrLower "WHERE" >> spaces1) parseExpression

-- | Parses the "ORDER BY ..." part of a select statement returning the list
--   of OrderByItem's that were parsed (this list will be empty if there is no
--   "ORDER BY" parsed
parseOrderByPart :: GenParser Char st [OrderByItem]
parseOrderByPart =
    ifParseThenElse
        -- if we see an "ORDER BY"
        (spaces1 >> upperOrLower "ORDER" >> spaces1 >> upperOrLower "BY" >> spaces1)
        
        -- then parse the order expression
        (sepByAtLeast 1 parseOrderByItem parseCommaSeparator)
        
        -- else there is nothing to sort by
        (return [])
    
    where
        parseOrderByItem :: GenParser Char st OrderByItem
        parseOrderByItem = do
            orderExpr <- parseExpression
            isAscending <- ifParseThenElse
                -- if we parse "DESC"
                (try parseDescending)
                
                -- then return false, it isn't ascending
                (return False)
                
                -- else try to consume "ASC" but even if we don't it's still
                -- ascending so return true
                ((try parseAscending <|> return []) >> return True)
            
            return $ OrderByItem orderExpr isAscending
        
        parseAscending  = spaces1 >> ((try $ upperOrLower "ASCENDING") <|> upperOrLower "ASC")
        parseDescending = spaces1 >> ((try $ upperOrLower "DESCENDING") <|> upperOrLower "DESC")

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
    onPart <- parseExpression
    
    maybeAlias <- maybeParse $ spaces1 >> parseTableAlias
    
    return InnerJoin {
            leftJoinTable=leftTblExpr,
            rightJoinTable=rightTblExpr,
            onCondition=onPart,
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

parseExpression :: GenParser Char st Expression
parseExpression =
    let opTable = map (map parseInfixOp) infixFunctions
    in buildExpressionParser opTable parseAnyNonInfixExpression

parseAnyNonInfixExpression :: GenParser Char st Expression
parseAnyNonInfixExpression =
    parenthesize parseExpression <|>
    parseStringConstant <|>
    try parseRealConstant <|>
    parseIntConstant <|>
    parseAnyNormalFunction <|>
    (parseColumnId >>= (\colId -> return $ ColumnExpression colId))

parseStringConstant :: GenParser Char st Expression
parseStringConstant =
    quotedText True '"' >>= (\txt -> return $ StringConstantExpression txt)

parseIntConstant :: GenParser Char st Expression
parseIntConstant =
    parseInt >>= (\int -> return $ IntegerConstantExpression int)

parseInt :: GenParser Char st Int
parseInt = do
    digitTxt <- anyParseTxt
    return $ read digitTxt
    where
        anyParseTxt = signedParseTxt <|> unsignedParseTxt <?> "integer"
        unsignedParseTxt = many1 digit
        signedParseTxt = do
            char '-'
            unsignedDigitTxt <- unsignedParseTxt
            return ('-':unsignedDigitTxt)

parseRealConstant :: GenParser Char st Expression
parseRealConstant =
    parseReal >>= (\real -> return $ RealConstantExpression real)

parseReal :: GenParser Char st Double
parseReal = do
    realTxt <- anyParseTxt
    return $ read realTxt
    where
        anyParseTxt = signedParseTxt <|> unsignedParseTxt <?> "real"
        unsignedParseTxt = do
            intTxt <- many1 digit
            char '.'
            fracTxt <- many1 digit
            return $ intTxt ++ "." ++ fracTxt
        signedParseTxt = do
            char '-'
            unsignedDigitTxt <- unsignedParseTxt
            return ('-':unsignedDigitTxt)

parseAnyNormalFunction :: GenParser Char st Expression
parseAnyNormalFunction =
    let allParsers = map parseNormalFunction normalSyntaxFunctions
    in choice allParsers

parseNormalFunction sqlFunc = do
    try $ (upperOrLower $ functionName sqlFunc)
    spaces -- TODO careful here: what happens with upperVar? it breaks because of "upper" (maybe a blog entry!!)
    args <- parenthesize $ argSepBy (minArgCount sqlFunc) parseExpression parseCommaSeparator
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

-- Infix functions --
infixFunctions =
    [[multiplyFunction, divideFunction],
     [plusFunction, minusFunction],
     [isFunction, isNotFunction, lessThanFunction, lessThanOrEqualToFunction,
      greaterThanFunction, greaterThanOrEqualToFunction],
     [andFunction, orFunction]]

-- | This function parses the operator part of the infix function and returns
--   a function that excepts a left expression and right expression to form
--   an Expression from the FunctionExpression constructor
parseInfixOp infixFunc =
    -- use the magic infix type, always assuming left associativity
    Infix opParser AssocLeft
    where
        opParser =
            if funcIsAlphaNum
            then do
                try (spaces1 >> (upperOrLower $ functionName infixFunc) >> spaces1)
                return $ buildExpr
            else do
                try (spaces >> (upperOrLower $ functionName infixFunc) >> notOpChar) >> spaces
                return $ buildExpr
        buildExpr leftSubExpr rightSubExpr =
            FunctionExpression {
                sqlFunction = infixFunc,
                functionArguments = [leftSubExpr, rightSubExpr]}
        funcIsAlphaNum = any isAlphaNum (functionName infixFunc)
        notOpChar =
            notFollowedBy $ oneOf "*/+-=<>^"

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
    functionName    = "OR",
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

-- | Wraps parentheses parsers around the given inner parser
parenthesize :: GenParser Char st a -> GenParser Char st a
parenthesize innerParser = do
    char '('
    spaces
    innerParseResults <- innerParser
    spaces
    char ')'
    return innerParseResults

-- | Either parses the left or right parser returning the result of the
--   successful parser
eitherParse :: GenParser tok st a -> GenParser tok st b -> GenParser tok st (Either a b)
eitherParse leftParser rightParser =
    do {parseResult <- try leftParser; return $ Left parseResult} <|>
    do {parseResult <- rightParser; return $ Right parseResult}

-- parses 1 or more spaces
spaces1 = skipMany1 space <?> "whitespace"

-- | if the ifParse parser succeeds return the result of thenParse, else
--   return Nothing without parsing any input
ifParseThen :: GenParser tok st a -> GenParser tok st b -> GenParser tok st (Maybe b)
ifParseThen ifParse thenPart = do
    ifResult <- maybeParse ifParse
    case ifResult of
        Just _ ->   thenPart >>= (\x -> return $ Just x)
        Nothing ->  return Nothing

-- | if ifParse succeeds then parse thenPart otherwise parse elsePart
ifParseThenElse :: GenParser tok st a -> GenParser tok st b -> GenParser tok st b -> GenParser tok st b
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

-- TODO are function names reserved... i don't think so
reservedWords =
    map functionName normalSyntaxFunctions ++
    map functionName (concat infixFunctions) ++
    map functionName specialFunctions ++
    ["BY","CROSS", "FROM", "GROUP", "HAVING", "INNER", "JOIN", "ON", "ORDER", "SELECT", "WHERE"]

-- | tries parsing both the upper and lower case versions of the given string
upperOrLower :: String -> GenParser Char st String
upperOrLower stringToParse =
    string (map toUpper stringToParse) <|>
    string (map toLower stringToParse) <?> stringToParse

-- | accepst the same input as the given parser except and input that matches
--   theException parser
genExcept :: (Show b) => GenParser tok st a -> GenParser tok st b -> GenParser tok st a
genExcept parser theException = do
    genNotFollowedBy theException
    parser

-- | a generic version of the notFollowedBy library function. We require
--   Show types so that we can better report failures
genNotFollowedBy :: (Show a) => GenParser tok st a -> GenParser tok st ()
genNotFollowedBy theParser = try $ do
    mayParseResult <- maybeParse theParser
    case mayParseResult of
        Nothing -> return ()
        Just x -> unexpected $ show x

-- | returns Just parseResult if the parse succeeds and Nothing if it fails
maybeParse :: GenParser tok st a -> GenParser tok st (Maybe a)
maybeParse parser =
    (try parser >>= (\x -> return $ Just x)) <|>
    return Nothing

-- | parse `itemParser`s seperated by exactly `minCount` `sepParser`s
sepByExactly :: Int -> GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepByExactly count itemParser sepParser =
    let itemParsers = replicate count itemParser
    in parseEach itemParsers
    where
        -- for an empty parser list return an empty result
        parseEach [] = return []
        
        -- for a parser list of 1 we don't want to use a separator
        parseEach [lastParser] = lastParser >>= (\x -> return [x])
        
        -- for lists greater than 1 we do need to care about the separator
        parseEach (headParser:parserTail) = do
            resultHead <- headParser
            sepParser
            resultTail <- parseEach parserTail
            
            return $ resultHead:resultTail

-- | parse `itemParser`s seperated by at least `minCount` `sepParser`s
sepByAtLeast :: Int -> GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepByAtLeast minCount itemParser sepParser = do
    minResults <- sepByExactly minCount itemParser sepParser
    ifParseThenElse
        sepParser
        (sepBy1 itemParser sepParser >>= (\tailResults -> return $ minResults ++ tailResults))
        (return minResults)

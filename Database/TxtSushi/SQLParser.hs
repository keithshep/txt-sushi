-----------------------------------------------------------------------------
-- |
-- Module      :  Database.TxtSushi.SQLParser
-- Copyright   :  (c) Keith Sheppard 2009
-- License     :  GPL3 or greater
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for parsing SQL
--
-----------------------------------------------------------------------------

module Database.TxtSushi.SQLParser (
    allMaybeTableNames,
    parseSelectStatement,
    SelectStatement(..),
    TableExpression(..),
    ColumnIdentifier(..),
    prettyFormatColumn,
    ColumnSelection(..),
    expressionIdentifier,
    Expression(..),
    OrderByItem(..),
    prettyFormatWithArgs,
    SQLFunction(..),
    withTrailing,
    withoutTrailing,
    isAggregate,
    selectStatementContainsAggregates,
    
    -- aggregates
    avgFunction,
    countFunction,
    firstFunction,
    lastFunction,
    maxFunction,
    minFunction,
    sumFunction,
    
    -- String SQL function
    concatenateFunction,
    absFunction,
    upperFunction,
    lowerFunction,
    trimFunction,
    substringFromFunction,
    substringFromToFunction,
    
    -- Algebraic SQL functions
    multiplyFunction,
    divideFunction,
    plusFunction,
    minusFunction,
    negateFunction,
    
    -- Boolean SQL functions
    isFunction,
    isNotFunction,
    lessThanFunction,
    lessThanOrEqualToFunction,
    greaterThanFunction,
    greaterThanOrEqualToFunction,
    andFunction,
    orFunction,
    notFunction,
    regexMatchFunction,
    
    -- Etc...
    maybeReadInt,
    maybeReadReal) where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

--------------------------------------------------------------------------------
-- The data definition for select statements
--------------------------------------------------------------------------------

-- | represents a select statement
--   TODO this should be moved inside the TableExpression type
data SelectStatement = SelectStatement {
    columnSelections :: [ColumnSelection],
    maybeFromTable :: Maybe TableExpression,
    maybeWhereFilter :: Maybe Expression,
    maybeGroupByHaving :: Maybe ([Expression], Maybe Expression),
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
        maybeTableAlias :: Maybe String} |
    SelectExpression {
        selectStatement :: SelectStatement,
        maybeTableAlias :: Maybe String}
    deriving (Show, Ord, Eq)

-- | convenience function for extracting all of the table names used by the
--   given table expression
allMaybeTableNames :: (Maybe TableExpression) -> [String]
allMaybeTableNames Nothing = []
allMaybeTableNames (Just tblExp) = allTableNames tblExp

allTableNames :: TableExpression -> [String]
allTableNames (TableIdentifier tblName _) = [tblName]
allTableNames (InnerJoin lftTbl rtTbl _ _) =
    (allTableNames lftTbl) ++ (allTableNames rtTbl)
allTableNames (CrossJoin lftTbl rtTbl _) =
    (allTableNames lftTbl) ++ (allTableNames rtTbl)
allTableNames (SelectExpression selectStmt _) =
    allMaybeTableNames $ maybeFromTable selectStmt

data ColumnSelection =
    AllColumns |
    AllColumnsFrom {sourceTableName :: String} |
    ExpressionColumn {
        expression :: Expression,
        maybeColumnAlias :: Maybe String}
    --QualifiedColumn {
    --    qualifiedColumnId :: ColumnIdentifier}
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
    IntConstantExpression {
        intConstant :: Int} |
    RealConstantExpression {
        realConstant :: Double}
    deriving (Show, Ord, Eq)

-- | an aggregate function is one whose min function count is 1 and whose
--   arg count is not fixed
isAggregate :: SQLFunction -> Bool
isAggregate sqlFun = minArgCount sqlFun == 1 && not (argCountIsFixed sqlFun)

containsAggregates :: Expression -> Bool
containsAggregates (FunctionExpression sqlFun args) =
    isAggregate sqlFun || any containsAggregates args
containsAggregates _ = False

selectionContainsAggregates :: ColumnSelection -> Bool
selectionContainsAggregates (ExpressionColumn expr _) =
    containsAggregates expr
selectionContainsAggregates _ = False

orderByItemContainsAggregates :: OrderByItem -> Bool
orderByItemContainsAggregates (OrderByItem expr _) =
    containsAggregates expr

selectStatementContainsAggregates :: SelectStatement -> Bool
selectStatementContainsAggregates select =
    any selectionContainsAggregates (columnSelections select) ||
    any orderByItemContainsAggregates (orderByItems select)

expressionIdentifier :: Expression -> ColumnIdentifier
expressionIdentifier (FunctionExpression func args) =
    ColumnIdentifier Nothing ((prettyFormatWithArgs func) args)
expressionIdentifier (ColumnExpression col) = col
expressionIdentifier (StringConstantExpression str) =
    ColumnIdentifier Nothing ("\"" ++ str ++ "\"")
expressionIdentifier (IntConstantExpression int) =
    ColumnIdentifier Nothing (show int)
expressionIdentifier (RealConstantExpression real) =
    ColumnIdentifier Nothing (show real)

needsParens :: Expression -> Bool
needsParens (FunctionExpression _ _) = True
needsParens _ = False

toArgString :: Expression -> String
toArgString expr =
    let exprFmt = prettyFormatColumn $ expressionIdentifier expr
    in if needsParens expr then "(" ++ exprFmt ++ ")" else exprFmt

prettyFormatWithArgs :: SQLFunction -> [Expression] -> String
prettyFormatWithArgs sqlFunc funcArgs
    | sqlFunc `elem` normalSyntaxFunctions = prettyFormatNormalFunctionExpression sqlFunc funcArgs
    | or (map (sqlFunc `elem`) infixFunctions) = prettyFormatInfixFunctionExpression sqlFunc funcArgs
    | sqlFunc == negateFunction = "-" ++ toArgString (head funcArgs)
    | sqlFunc == countFunction = functionName countFunction ++ "(*)"
    | sqlFunc == substringFromToFunction ||
      sqlFunc == substringFromFunction ||
      sqlFunc == notFunction =
        prettyFormatNormalFunctionExpression sqlFunc funcArgs
    | otherwise =
        error $ "don't know how to format the given SQL function : " ++
                show sqlFunc

prettyFormatInfixFunctionExpression :: SQLFunction -> [Expression] -> String
prettyFormatInfixFunctionExpression sqlFunc funcArgs =
    let
        arg1 = head funcArgs
        arg2 = funcArgs !! 1
    in
        toArgString arg1 ++ functionName sqlFunc ++ toArgString arg2

prettyFormatNormalFunctionExpression :: SQLFunction -> [Expression] -> String
prettyFormatNormalFunctionExpression sqlFunc funcArgs =
    let argString = intercalate ", " (map toArgString funcArgs)
    in functionName sqlFunc ++ "(" ++ argString ++ ")"

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
parseSelectStatement = (try $ spaces >> parseToken "SELECT") >> parseSelectBody

-- | Parses all of the stuff that comes after "SELECT "
parseSelectBody :: GenParser Char st SelectStatement
parseSelectBody = do
    columnVals <- parseColumnSelections
    -- TODO need a better error message for missing "ON" etc. in
    -- the from part, can do this by grabing "FROM" first
    maybeFrom <- maybeParseFromPart
    maybeWhere <- maybeParseWherePart
    groupByExprs <- maybeParseGroupByPart
    orderBy <- parseOrderByPart
    
    return SelectStatement {
        columnSelections    = columnVals,
        maybeFromTable      = maybeFrom,
        maybeWhereFilter    = maybeWhere,
        orderByItems        = orderBy,
        maybeGroupByHaving  = groupByExprs}
    
    where
        maybeParseFromPart =
            ifParseThen (parseToken "FROM") parseTableExpression
        
        maybeParseWherePart =
            ifParseThen (parseToken "WHERE") parseExpression

-- | Parses the "ORDER BY ..." part of a select statement returning the list
--   of OrderByItem's that were parsed (this list will be empty if there is no
--   "ORDER BY" parsed
parseOrderByPart :: GenParser Char st [OrderByItem]
parseOrderByPart =
    ifParseThenElse
        -- if we see an "ORDER BY"
        (parseToken "ORDER")
        
        -- then parse the order expressions
        (parseToken "BY" >> sepByAtLeast 1 parseOrderByItem commaSeparator)
        
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
                -- ascending so return true unconditionally
                ((parseAscending <|> return []) >> return True)
            
            return $ OrderByItem orderExpr isAscending
        
        parseAscending  = parseToken "ASCENDING" <|> parseToken "ASC"
        parseDescending = parseToken "DESCENDING" <|> parseToken "DESC"

maybeParseGroupByPart :: GenParser Char st (Maybe ([Expression], Maybe Expression))
maybeParseGroupByPart =
    ifParseThen
        -- if we see a "GROUP BY"
        (parseToken "GROUP")
        
        -- then parse the expressions
        (parseToken "BY" >> parseGroupBy)
    
    where
        parseGroupBy = do
            groupExprs <- atLeastOneExpr
            maybeHavingExpr <- ifParseThen (parseToken "HAVING") parseExpression
            return (groupExprs, maybeHavingExpr)

atLeastOneExpr :: GenParser Char st [Expression]
atLeastOneExpr = sepByAtLeast 1 parseExpression commaSeparator

--------------------------------------------------------------------------------
-- Functions for parsing the column names specified after "SELECT"
--------------------------------------------------------------------------------

parseColumnSelections :: GenParser Char st [ColumnSelection]
parseColumnSelections =
    sepBy1 parseAnyColType (try commaSeparator)
    where parseAnyColType = parseAllCols <|>
                            (try parseAllColsFromTbl) <|>
                            (try parseColExpression)

parseAllCols :: GenParser Char st ColumnSelection
parseAllCols = parseToken "*" >> return AllColumns

parseAllColsFromTbl :: GenParser Char st ColumnSelection
parseAllColsFromTbl = do
    tableVal <- parseIdentifier
    string "." >> spaces >> parseToken "*"
    
    return $ AllColumnsFrom tableVal

parseColExpression :: GenParser Char st ColumnSelection
parseColExpression = do
    expr <- parseExpression
    maybeAlias <- maybeParseAlias
    
    return $ ExpressionColumn expr maybeAlias

parseColumnId :: GenParser Char st ColumnIdentifier
parseColumnId = do
    firstId <- parseIdentifier
    
    maybeFullyQual <- maybeParse $ (char '.' >> spaces)
    case maybeFullyQual of
        -- No '.' means it's a partially qualified column
        Nothing -> return $ ColumnIdentifier Nothing firstId
        Just _ -> do
            secondId <- parseIdentifier
            return $ ColumnIdentifier (Just firstId) secondId

--------------------------------------------------------------------------------
-- Functions for parsing the table part (after "FROM")
--------------------------------------------------------------------------------

parseTableExpression :: GenParser Char a TableExpression
parseTableExpression =
    parenthesize parseTableExpression <|>
    parseSelectExpression <|>
    parseTableIdentifierOrJoin <?> "Table Expression"

parseSelectExpression :: GenParser Char a TableExpression
parseSelectExpression = do
    selectStmt <- parseSelectStatement
    maybeAlias <- maybeParseAlias
    
    return $ SelectExpression selectStmt maybeAlias

parseTableIdentifierOrJoin :: GenParser Char a TableExpression
parseTableIdentifierOrJoin = do
    nextTblId <- parseTableIdentifier
    
    let
        ifCrossOrInnerJoinParse = ifParseThenElse
            -- if
            crossJoinSep -- TODO commit to join
            -- then
            (parseCrossJoinRemainder nextTblId)
            -- else
            ifInnerJoinParse
    
        ifInnerJoinParse = ifParseThenElse
            -- if
            innerJoinSep -- TODO commit to join
            -- then
            (parseInnerJoinRemainder nextTblId)
            -- else
            (return nextTblId)
        
    ifCrossOrInnerJoinParse
    
    where
        crossJoinSep = (commaSeparator >> return "") <|> (parseToken "CROSS" >> parseToken "JOIN")
        innerJoinSep = ((maybeParse $ parseToken "INNER") >> parseToken "JOIN")

parseInnerJoinRemainder :: TableExpression -> GenParser Char a TableExpression
parseInnerJoinRemainder leftTblExpr = do
    rightTblExpr <- parseTableExpression
    
    parseToken "ON"
    onPart <- parseExpression
    
    maybeAlias <- maybeParseAlias
    
    return InnerJoin {
            leftJoinTable=leftTblExpr,
            rightJoinTable=rightTblExpr,
            onCondition=onPart,
            maybeTableAlias=maybeAlias}

parseCrossJoinRemainder :: TableExpression -> GenParser Char a TableExpression
parseCrossJoinRemainder leftTblExpr = do
    rightTblExpr <- parseTableExpression
    maybeAlias <- maybeParseAlias
    
    return CrossJoin {
            leftJoinTable=leftTblExpr,
            rightJoinTable=rightTblExpr,
            maybeTableAlias=maybeAlias}

parseTableIdentifier :: GenParser Char st TableExpression
parseTableIdentifier = do
    theId <- parseIdentifier
    maybeAlias <- maybeParseAlias
    return $ TableIdentifier theId maybeAlias

maybeParseAlias :: GenParser Char st (Maybe [Char])
maybeParseAlias = ifParseThen (parseToken "AS") parseIdentifier

--------------------------------------------------------------------------------
-- Expression parsing: These can be after "SELECT", "WHERE" or "HAVING"
--------------------------------------------------------------------------------

parseExpression :: GenParser Char st Expression
parseExpression =
    let opTable = map (map parseInfixOp) infixFunctions
    in buildExpressionParser opTable parseAnyNonInfixExpression <?> "expression"

parseAnyNonInfixExpression :: GenParser Char st Expression
parseAnyNonInfixExpression =
    parenthesize parseExpression <|>
    parseStringConstant <|>
    try parseRealConstant <|>
    try parseIntConstant <|>
    parseAnyNormalFunction <|>
    parseNegateFunction <|>
    parseSubstringFunction <|>
    parseNotFunction <|>
    parseCountStar <|>
    (parseColumnId >>= return . ColumnExpression)

parseStringConstant :: GenParser Char st Expression
parseStringConstant =
    (quotedText True '"' <|> quotedText True '\'') >>=
    (return . StringConstantExpression)

parseIntConstant :: GenParser Char st Expression
parseIntConstant = parseInt >>= return . IntConstantExpression

parseInt :: GenParser Char st Int
parseInt = eatSpacesAfter . try . (withoutTrailing alphaNum) $ do
    digitTxt <- anyParseTxt
    return $ read digitTxt
    where
        anyParseTxt = signedParseTxt <|> unsignedParseTxt <?> "integer"
        unsignedParseTxt = many1 digit
        signedParseTxt = do
            char '-'
            unsignedDigitTxt <- unsignedParseTxt
            return $ '-' : unsignedDigitTxt

-- | returns an int if it can be read from the string
maybeReadInt :: String -> Maybe Int
maybeReadInt intStr =
    case parse (withTrailing (spaces >> eof) (spaces >> parseInt)) "" intStr of
        Left _      -> Nothing
        Right int   -> Just int

-- | returns a real if it can be read from the string
maybeReadReal :: String -> Maybe Double
maybeReadReal realStr =
    case parse (withTrailing (spaces >> eof) (spaces >> parseReal)) "" realStr of
        Left _      -> maybeReadInt realStr >>= (\int -> Just $ fromIntegral int)
        Right real  -> Just real

parseRealConstant :: GenParser Char st Expression
parseRealConstant =
    parseReal >>= (\real -> return $ RealConstantExpression real)

parseReal :: GenParser Char st Double
parseReal = eatSpacesAfter . try . (withoutTrailing alphaNum) $ do
    realTxt <- anyParseTxt <?> "real"
    return $ read realTxt
    where
        anyParseTxt = do
            txtWithoutExp <- txtWithoutExponent
            expPart <- try exponentPart <|> return ""
            return $ txtWithoutExp ++ expPart
        exponentPart = do
            e <- (char 'e' <|> char 'E')
            negPart <- (char '-' >> return "-") <|> return ""
            numPart <- many1 digit
            return $ (e:negPart) ++ numPart
        txtWithoutExponent = signedTxt <|> unsignedTxt <?> "real"
        unsignedTxt = do
            intTxt <- many1 digit
            char '.'
            fracTxt <- many1 digit
            return $ intTxt ++ "." ++ fracTxt
        signedTxt = do
            char '-'
            unsignedDigitTxt <- unsignedTxt
            return ('-':unsignedDigitTxt)

parseAnyNormalFunction :: GenParser Char st Expression
parseAnyNormalFunction =
    let allParsers = map parseNormalFunction normalSyntaxFunctions
    in choice allParsers

parseNormalFunction :: SQLFunction -> GenParser Char st Expression
parseNormalFunction sqlFunc =
    try (parseToken $ functionName sqlFunc) >> parseNormalFunctionArgs sqlFunc

parseNormalFunctionArgs :: SQLFunction -> GenParser Char st Expression
parseNormalFunctionArgs sqlFunc = do
    args <- parenthesize $ argSepBy (minArgCount sqlFunc) parseExpression commaSeparator
    return $ FunctionExpression sqlFunc args
    where argSepBy = if argCountIsFixed sqlFunc then sepByExactly else sepByAtLeast

-- Functions with "normal" syntax --
normalSyntaxFunctions :: [SQLFunction]
normalSyntaxFunctions =
    [absFunction, upperFunction, lowerFunction, trimFunction,
     -- all aggregates except count which accepts a (*)
     avgFunction, firstFunction, lastFunction, maxFunction,
     minFunction, sumFunction]

-- non aggregates
absFunction :: SQLFunction
absFunction = SQLFunction {
    functionName    = "ABS",
    minArgCount     = 1,
    argCountIsFixed = True}

upperFunction :: SQLFunction
upperFunction = SQLFunction {
    functionName    = "UPPER",
    minArgCount     = 1,
    argCountIsFixed = True}

lowerFunction :: SQLFunction
lowerFunction = SQLFunction {
    functionName    = "LOWER",
    minArgCount     = 1,
    argCountIsFixed = True}

trimFunction :: SQLFunction
trimFunction = SQLFunction {
    functionName    = "TRIM",
    minArgCount     = 1,
    argCountIsFixed = True}

-- aggregates
avgFunction :: SQLFunction
avgFunction = SQLFunction {
    functionName    = "AVG",
    minArgCount     = 1,
    argCountIsFixed = False}

countFunction :: SQLFunction
countFunction = SQLFunction {
    functionName    = "COUNT",
    minArgCount     = 1,
    argCountIsFixed = False}

firstFunction :: SQLFunction
firstFunction = SQLFunction {
    functionName    = "FIRST",
    minArgCount     = 1,
    argCountIsFixed = False}

lastFunction :: SQLFunction
lastFunction = SQLFunction {
    functionName    = "LAST",
    minArgCount     = 1,
    argCountIsFixed = False}

maxFunction :: SQLFunction
maxFunction = SQLFunction {
    functionName    = "MAX",
    minArgCount     = 1,
    argCountIsFixed = False}

minFunction :: SQLFunction
minFunction = SQLFunction {
    functionName    = "MIN",
    minArgCount     = 1,
    argCountIsFixed = False}

sumFunction :: SQLFunction
sumFunction = SQLFunction {
    functionName    = "SUM",
    minArgCount     = 1,
    argCountIsFixed = False}

-- Infix functions --
infixFunctions :: [[SQLFunction]]
infixFunctions =
    [[multiplyFunction, divideFunction],
     [plusFunction, minusFunction],
     [concatenateFunction],
     [isFunction, isNotFunction, lessThanFunction, lessThanOrEqualToFunction,
      greaterThanFunction, greaterThanOrEqualToFunction, regexMatchFunction],
     [andFunction],
     [orFunction]]

-- | This function parses the operator part of the infix function and returns
--   a function that excepts a left expression and right expression to form
--   an Expression from the FunctionExpression constructor
parseInfixOp :: SQLFunction -> Operator Char st Expression
parseInfixOp infixFunc =
    -- use the magic infix type, always assuming left associativity
    Infix opParser AssocLeft
    where
        opParser = parseToken (functionName infixFunc) >> return buildExpr
        buildExpr leftSubExpr rightSubExpr = FunctionExpression {
            sqlFunction = infixFunc,
            functionArguments = [leftSubExpr, rightSubExpr]}

-- Algebraic
multiplyFunction :: SQLFunction
multiplyFunction = SQLFunction {
    functionName    = "*",
    minArgCount     = 2,
    argCountIsFixed = True}

divideFunction :: SQLFunction
divideFunction = SQLFunction {
    functionName    = "/",
    minArgCount     = 2,
    argCountIsFixed = True}

plusFunction :: SQLFunction
plusFunction = SQLFunction {
    functionName    = "+",
    minArgCount     = 2,
    argCountIsFixed = True}

minusFunction :: SQLFunction
minusFunction = SQLFunction {
    functionName    = "-",
    minArgCount     = 2,
    argCountIsFixed = True}

-- Boolean
isFunction :: SQLFunction
isFunction = SQLFunction {
    functionName    = "=",
    minArgCount     = 2,
    argCountIsFixed = True}

isNotFunction :: SQLFunction
isNotFunction = SQLFunction {
    functionName    = "<>",
    minArgCount     = 2,
    argCountIsFixed = True}

lessThanFunction :: SQLFunction
lessThanFunction = SQLFunction {
    functionName    = "<",
    minArgCount     = 2,
    argCountIsFixed = True}

lessThanOrEqualToFunction :: SQLFunction
lessThanOrEqualToFunction = SQLFunction {
    functionName    = "<=",
    minArgCount     = 2,
    argCountIsFixed = True}

greaterThanFunction :: SQLFunction
greaterThanFunction = SQLFunction {
    functionName    = ">",
    minArgCount     = 2,
    argCountIsFixed = True}

greaterThanOrEqualToFunction :: SQLFunction
greaterThanOrEqualToFunction = SQLFunction {
    functionName    = ">=",
    minArgCount     = 2,
    argCountIsFixed = True}

andFunction :: SQLFunction
andFunction = SQLFunction {
    functionName    = "AND",
    minArgCount     = 2,
    argCountIsFixed = True}

orFunction :: SQLFunction
orFunction = SQLFunction {
    functionName    = "OR",
    minArgCount     = 2,
    argCountIsFixed = True}

concatenateFunction :: SQLFunction
concatenateFunction = SQLFunction {
    functionName    = "||",
    minArgCount     = 2,
    argCountIsFixed = True}

regexMatchFunction :: SQLFunction
regexMatchFunction = SQLFunction {
    functionName    = "=~",
    minArgCount     = 2,
    argCountIsFixed = True}

-- Functions with special syntax --
specialFunctions :: [SQLFunction]
specialFunctions = [substringFromFunction,
                    substringFromToFunction,
                    negateFunction,
                    notFunction]

-- | SUBSTRING(extraction_string FROM starting_position [FOR length]
--             [COLLATE collation_name])
--   TODO implement COLLATE part
substringFromFunction :: SQLFunction
substringFromFunction = SQLFunction {
    functionName    = "SUBSTRING",
    minArgCount     = 2,
    argCountIsFixed = True}

substringFromToFunction :: SQLFunction
substringFromToFunction = SQLFunction {
    functionName    = "SUBSTRING",
    minArgCount     = 3,
    argCountIsFixed = True}

parseSubstringFunction :: GenParser Char st Expression
parseSubstringFunction = do
    parseToken $ functionName substringFromFunction
    eatSpacesAfter $ char '('
    strExpr <- parseExpression
    parseToken "FROM"
    startExpr <- parseExpression
    maybeLength <- ifParseThen (parseToken "FOR") parseExpression
    eatSpacesAfter $ char ')'
    
    return $ case maybeLength of
        Nothing     -> FunctionExpression substringFromFunction [strExpr, startExpr]
        Just len    -> FunctionExpression substringFromToFunction [strExpr, startExpr, len]

negateFunction :: SQLFunction
negateFunction = SQLFunction {
    functionName    = "-",
    minArgCount     = 1,
    argCountIsFixed = True}

parseNegateFunction :: GenParser Char st Expression
parseNegateFunction = do
    parseToken "-"
    expr <- parseAnyNonInfixExpression
    return $ FunctionExpression negateFunction [expr]

notFunction :: SQLFunction
notFunction = SQLFunction {
    functionName    = "NOT",
    minArgCount     = 1,
    argCountIsFixed = True}

parseNotFunction :: GenParser Char st Expression
parseNotFunction = do
    parseToken $ functionName notFunction
    expr <- parseAnyNonInfixExpression
    return $ FunctionExpression notFunction [expr]

parseCountStar :: GenParser Char st Expression
parseCountStar = do
    try (parseToken $ functionName countFunction)
    try parseStar <|> parseNormalFunctionArgs countFunction
    
    where
        parseStar = do
            parenthesize $ parseToken "*"
            return $ FunctionExpression countFunction [IntConstantExpression 0]

--------------------------------------------------------------------------------
-- Parse utility functions
--------------------------------------------------------------------------------

parseOpChar :: CharParser st Char
parseOpChar = oneOf opChars

opChars :: [Char]
opChars = "~!@#$%^&*-+=|\\<>/?"

withoutTrailing :: (Show s) => GenParser tok st s -> GenParser tok st a -> GenParser tok st a
withoutTrailing end p = p >>= (\x -> genNotFollowedBy end >> return x)

withTrailing :: (Monad m) => m a -> m b -> m b
withTrailing end p = p >>= (\x -> end >> return x)

-- | like the lexeme function, this function eats all spaces after the given
--   parser, but this one works for me and lexeme doesn't
eatSpacesAfter :: GenParser Char st a -> GenParser Char st a
eatSpacesAfter p = p >>= (\x -> spaces >> return x)

-- | find out if the given string ends with an op char
endsWithOp :: String -> Bool
endsWithOp strToTest = last strToTest `elem` opChars

-- | A token parser that allows either upper or lower case. all trailing
--   whitespace is consumed
parseToken :: String -> GenParser Char st String
parseToken tokStr =
    eatSpacesAfter (try $ if endsWithOp tokStr then parseOpTok else parseAlphaNumTok)
    where
        parseOpTok = withoutTrailing parseOpChar (string tokStr)
        parseAlphaNumTok =
            withoutTrailing (alphaNum <|> char '_') (upperOrLower tokStr)

-- | parses an identifier. you can use a tick '`' as a quote for
--   an identifier with white-space
parseIdentifier :: GenParser Char st String
parseIdentifier = do
    let parseId = do
            let idChar = alphaNum <|> char '_'
            notFollowedBy digit
            quotedText False '`' <|> many1 idChar
    ((eatSpacesAfter parseId) `genExcept` parseReservedWord) <?> "identifier"

-- | quoted text which allows escaping by doubling the quote char
--   like \"escaped quote char here:\"\"\"
quotedText :: Bool -> Char -> GenParser Char st String
quotedText allowEmpty quoteChar = do
    let quote = char quoteChar
        manyFunc = if allowEmpty then many else many1
    
    quote
    textValue <- manyFunc $ (anyChar `genExcept` quote) <|>
                            try (escapedQuote quoteChar)
    quote
    spaces
    
    return textValue

escapedQuote :: Char -> GenParser Char st Char
escapedQuote quoteChar = string [quoteChar, quoteChar] >> return quoteChar

commaSeparator :: GenParser Char st Char
commaSeparator = eatSpacesAfter $ char ','

-- | Wraps parentheses parsers around the given inner parser
parenthesize :: GenParser Char st a -> GenParser Char st a
parenthesize innerParser = do
    eatSpacesAfter $ char '('
    innerParseResults <- innerParser
    eatSpacesAfter $ char ')'
    return innerParseResults

{-
-- | Either parses the left or right parser returning the result of the
--   successful parser
eitherParse :: GenParser tok st a -> GenParser tok st b -> GenParser tok st (Either a b)
eitherParse leftParser rightParser =
    (try leftParser >>= return . Left) <|> (rightParser >>= return . Right)
-}

-- | if the ifParse parser succeeds return the result of thenParse, else
--   return Nothing without parsing any input
ifParseThen :: GenParser tok st a -> GenParser tok st b -> GenParser tok st (Maybe b)
ifParseThen ifParse thenPart = do
    ifResult <- maybeParse ifParse
    case ifResult of
        Just _ ->   thenPart >>= return . Just
        Nothing ->  return Nothing

-- | if ifParse succeeds then parse thenPart otherwise parse elsePart
ifParseThenElse :: GenParser tok st a -> GenParser tok st b -> GenParser tok st b -> GenParser tok st b
ifParseThenElse ifParse thenPart elsePart = do
    ifResult <- maybeParse ifParse
    case ifResult of
        Just _ -> thenPart
        Nothing -> elsePart

parseReservedWord :: GenParser Char st String
parseReservedWord =
    let reservedWordParsers = map parseToken reservedWords
    in  choice reservedWordParsers

-- TODO are function names reserved... i don't think so
reservedWords :: [String]
reservedWords =
    map functionName normalSyntaxFunctions ++
    map functionName (concat infixFunctions) ++
    map functionName specialFunctions ++
    ["BY","CROSS", "FROM", "FOR", "GROUP", "HAVING", "INNER", "JOIN", "ON", "ORDER", "SELECT", "WHERE"]

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
    (try parser >>= return . Just) <|> return Nothing

-- | parse `itemParser`s seperated by exactly `minCount` `sepParser`s
sepByExactly :: Int -> GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepByExactly itemCount itemParser sepParser =
    let itemParsers = replicate itemCount itemParser
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
    tailResults <-
        ifParseThenElse sepParser (sepBy itemParser sepParser) (return [])
    
    return $ minResults ++ tailResults

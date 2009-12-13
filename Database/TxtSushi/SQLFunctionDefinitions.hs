-----------------------------------------------------------------------------
-- |
-- Module      :  Database.TxtSushi.SQLFunctionDefinitions
-- Copyright   :  (c) Keith Sheppard 2009
-- License     :  GPL3 or greater
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- SQL Functions
--
-----------------------------------------------------------------------------

module Database.TxtSushi.SQLFunctionDefinitions (
    SQLFunction(..),
    evalSQLFunction,
    normalSyntaxFunctions,
    infixFunctions,
    specialFunctions,
    
    -- TODO remove these
    negateFunction,
    countFunction,
    substringFromToFunction,
    substringFromFunction,
    notFunction) where

import Data.Char
import Data.List
import Text.Regex.Posix

import Database.TxtSushi.EvaluatedExpression
import Database.TxtSushi.SQLExpression

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

negateFunction :: SQLFunction
negateFunction = SQLFunction {
    functionName    = "-",
    minArgCount     = 1,
    argCountIsFixed = True}

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

notFunction :: SQLFunction
notFunction = SQLFunction {
    functionName    = "NOT",
    minArgCount     = 1,
    argCountIsFixed = True}

-- TODO this ugly function needs to be modularized
evalSQLFunction :: SQLFunction -> [EvaluatedExpression] -> EvaluatedExpression
evalSQLFunction sqlFun evaluatedArgs
    -- Global validation
    -- TODO this error should be more helpful than it is
    | argCountIsInvalid =
        error $ "Error: cannot apply " ++ show (length evaluatedArgs) ++
                " arguments to " ++ functionName sqlFun
    
    -- String functions
    | sqlFun == upperFunction = StringExpression $ map toUpper (coerceString arg1)
    | sqlFun == lowerFunction = StringExpression $ map toLower (coerceString arg1)
    | sqlFun == trimFunction = StringExpression $ trimSpace (coerceString arg1)
    | sqlFun == concatenateFunction = StringExpression $ concat (map coerceString evaluatedArgs)
    | sqlFun == substringFromToFunction =
        StringExpression $ take (coerceInt arg3) (drop (coerceInt arg2 - 1) (coerceString arg1))
    | sqlFun == substringFromFunction =
        StringExpression $ drop (coerceInt arg2 - 1) (coerceString arg1)
    | sqlFun == regexMatchFunction = BoolExpression $ (coerceString arg1) =~ (coerceString arg2)
    
    -- unary functions
    | sqlFun == absFunction = evalUnaryAlgebra abs abs
    | sqlFun == negateFunction = evalUnaryAlgebra negate negate
    
    -- algebraic
    | sqlFun == multiplyFunction = algebraWithCoercion (*) (*) evaluatedArgs
    | sqlFun == divideFunction = RealExpression $ (coerceReal arg1) / (coerceReal arg2)
    | sqlFun == plusFunction = algebraWithCoercion (+) (+) evaluatedArgs
    | sqlFun == minusFunction = algebraWithCoercion (-) (-) evaluatedArgs
    
    -- boolean
    | sqlFun == isFunction = BoolExpression (arg1 == arg2)
    | sqlFun == isNotFunction = BoolExpression (arg1 /= arg2)
    | sqlFun == lessThanFunction = BoolExpression (arg1 < arg2)
    | sqlFun == lessThanOrEqualToFunction = BoolExpression (arg1 <= arg2)
    | sqlFun == greaterThanFunction = BoolExpression (arg1 > arg2)
    | sqlFun == greaterThanOrEqualToFunction = BoolExpression (arg1 >= arg2)
    | sqlFun == andFunction = BoolExpression $ (coerceBool arg1) && (coerceBool arg2)
    | sqlFun == orFunction = BoolExpression $ (coerceBool arg1) || (coerceBool arg2)
    | sqlFun == notFunction = BoolExpression $ not (coerceBool arg1)
    
    -- aggregate
    -- TODO AVG(...) holds the whole arg list in memory. reimplement!
    | sqlFun == avgFunction =
        RealExpression $
            foldl1' (+) (map coerceReal evaluatedArgs) /
            (fromIntegral $ length evaluatedArgs)
    | sqlFun == countFunction = IntExpression $ length evaluatedArgs
    | sqlFun == firstFunction = head evaluatedArgs
    | sqlFun == lastFunction = last evaluatedArgs
    | sqlFun == maxFunction = maximum evaluatedArgs
    | sqlFun == minFunction = minimum evaluatedArgs
    | sqlFun == sumFunction = algebraWithCoercion (+) (+) evaluatedArgs
    
    -- error!!
    | otherwise = error $
        "internal error: missing evaluation code for function: " ++
        functionName sqlFun ++ ". please report this error"
    
    where
        arg1 = head evaluatedArgs
        arg2 = evaluatedArgs !! 1
        arg3 = evaluatedArgs !! 2
        algebraWithCoercion intFunc realFunc args =
            if any useRealAlgebra args then
                RealExpression $ foldl1' realFunc (map coerceReal args)
            else
                IntExpression $ foldl1' intFunc (map coerceInt args)
        
        useRealAlgebra (RealExpression _) = True
        useRealAlgebra expr = case maybeCoerceInt expr of
            Nothing -> True
            Just _  -> False
        
        argCountIsInvalid =
            let
                -- TODO the use of length is bad (unnecessarily traversing
                -- the entire arg list and keeping it in memory). Redo this
                -- so that we only check length w.r.t. minArgs
                argCount = length evaluatedArgs
                minArgs = minArgCount sqlFun
                argsFixed = argCountIsFixed sqlFun
            in
                argCount < minArgs || (argCount > minArgs && argsFixed)
        
        evalUnaryAlgebra intFunc realFunc =
            if length evaluatedArgs /= 1 then
                error $
                    "Internal Error: found a " ++ show sqlFun ++
                    " function with multiple args. please report this error"
            else
                if useRealAlgebra arg1 then
                    RealExpression $ realFunc (coerceReal arg1)
                else
                    IntExpression $ intFunc (coerceInt arg1)

-- | trims leading and trailing spaces
trimSpace :: String -> String
trimSpace = f . f
    where f = reverse . dropWhile isSpace

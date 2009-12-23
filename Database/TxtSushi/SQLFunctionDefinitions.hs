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
    [absFunction, upperFunction, lowerFunction, trimFunction, ifThenElseFunction,
     -- all aggregates except count which accepts a (*)
     avgFunction, firstFunction, lastFunction, maxFunction,
     minFunction, sumFunction]

-- non aggregates
absFunction :: SQLFunction
absFunction = SQLFunction {
    functionName    = "ABS",
    minArgCount     = 1,
    argCountIsFixed = True,
    applyFunction   = applyUnaryNumeric absFunction abs abs}

upperFunction :: SQLFunction
upperFunction = SQLFunction {
    functionName    = "UPPER",
    minArgCount     = 1,
    argCountIsFixed = True,
    applyFunction   = applyUnaryString upperFunction (map toUpper)}

lowerFunction :: SQLFunction
lowerFunction = SQLFunction {
    functionName    = "LOWER",
    minArgCount     = 1,
    argCountIsFixed = True,
    applyFunction   = applyUnaryString lowerFunction (map toLower)}

trimFunction :: SQLFunction
trimFunction = SQLFunction {
    functionName    = "TRIM",
    minArgCount     = 1,
    argCountIsFixed = True,
    applyFunction   = applyUnaryString trimFunction trimSpace}

ifThenElseFunction :: SQLFunction
ifThenElseFunction = SQLFunction {
    functionName    = "IF_THEN_ELSE",
    minArgCount     = 3,
    argCountIsFixed = True,
    applyFunction   = ifThenElse . checkArgCount ifThenElseFunction}
    where
        ifThenElse [ifExpr, thenExpr, elseExpr] =
            if coerceBool ifExpr
            then thenExpr
            else elseExpr
        ifThenElse _ = internalError

-- aggregates
avgFunction :: SQLFunction
avgFunction = SQLFunction {
    functionName    = "AVG",
    minArgCount     = 1,
    argCountIsFixed = False,
    applyFunction   = avgFun . checkArgCount avgFunction}
    -- TODO this AVG(...) holds the whole arg list in memory. reimplement!
    where
        avgFun args = RealExpression $
            foldl1' (+) (map coerceReal args) /
            (fromIntegral $ length args)

countFunction :: SQLFunction
countFunction = SQLFunction {
    functionName    = "COUNT",
    minArgCount     = 0,
    argCountIsFixed = False,
    applyFunction   = IntExpression . length}

firstFunction :: SQLFunction
firstFunction = SQLFunction {
    functionName    = "FIRST",
    minArgCount     = 1,
    argCountIsFixed = False,
    applyFunction   = head . checkArgCount firstFunction}

lastFunction :: SQLFunction
lastFunction = SQLFunction {
    functionName    = "LAST",
    minArgCount     = 1,
    argCountIsFixed = False,
    applyFunction   = last . checkArgCount lastFunction}

maxFunction :: SQLFunction
maxFunction = SQLFunction {
    functionName    = "MAX",
    minArgCount     = 1,
    argCountIsFixed = False,
    applyFunction   = maximum . checkArgCount maxFunction}

minFunction :: SQLFunction
minFunction = SQLFunction {
    functionName    = "MIN",
    minArgCount     = 1,
    argCountIsFixed = False,
    applyFunction   = minimum . checkArgCount minFunction}

sumFunction :: SQLFunction
sumFunction = SQLFunction {
    functionName    = "SUM",
    minArgCount     = 0,
    argCountIsFixed = False,
    applyFunction   = foldl stepSum (IntExpression 0)}
    where
        stepSum prevSum currArg =
            if useRealAlgebra prevSum || useRealAlgebra currArg
                then RealExpression $ coerceReal prevSum + coerceReal currArg
                else IntExpression $ coerceInt prevSum + coerceInt currArg

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
    argCountIsFixed = True,
    applyFunction   = applyBinaryNumeric multiplyFunction (*) (*)}

divideFunction :: SQLFunction
divideFunction = SQLFunction {
    functionName    = "/",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = divFun . checkArgCount divideFunction}
    where
        divFun [numExpr, denomExpr] =
            RealExpression $ (coerceReal numExpr) / (coerceReal denomExpr)
        divFun _ = internalError

plusFunction :: SQLFunction
plusFunction = SQLFunction {
    functionName    = "+",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryNumeric plusFunction (+) (+)}

minusFunction :: SQLFunction
minusFunction = SQLFunction {
    functionName    = "-",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryNumeric minusFunction (-) (-)}

-- Boolean
isFunction :: SQLFunction
isFunction = SQLFunction {
    functionName    = "=",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryComparison isFunction (==)}

isNotFunction :: SQLFunction
isNotFunction = SQLFunction {
    functionName    = "<>",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryComparison isNotFunction (/=)}

lessThanFunction :: SQLFunction
lessThanFunction = SQLFunction {
    functionName    = "<",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryComparison lessThanFunction (<)}

lessThanOrEqualToFunction :: SQLFunction
lessThanOrEqualToFunction = SQLFunction {
    functionName    = "<=",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryComparison lessThanOrEqualToFunction (<=)}

greaterThanFunction :: SQLFunction
greaterThanFunction = SQLFunction {
    functionName    = ">",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryComparison greaterThanFunction (>)}

greaterThanOrEqualToFunction :: SQLFunction
greaterThanOrEqualToFunction = SQLFunction {
    functionName    = ">=",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryComparison greaterThanOrEqualToFunction (>=)}

andFunction :: SQLFunction
andFunction = SQLFunction {
    functionName    = "AND",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryBooleanTest andFunction (&&)}

orFunction :: SQLFunction
orFunction = SQLFunction {
    functionName    = "OR",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = applyBinaryBooleanTest orFunction (||)}

concatenateFunction :: SQLFunction
concatenateFunction = SQLFunction {
    functionName    = "||",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = catExprs . checkArgCount concatenateFunction}
    where
        catExprs [arg1, arg2] = StringExpression $ (coerceString arg1) ++ (coerceString arg2)
        catExprs _ = internalError

regexMatchFunction :: SQLFunction
regexMatchFunction = SQLFunction {
    functionName    = "=~",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = regexMatch . checkArgCount regexMatchFunction}
    where
        regexMatch [arg1, arg2] = BoolExpression $ (coerceString arg1) =~ (coerceString arg2)
        regexMatch _ = internalError

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
    argCountIsFixed = True,
    applyFunction   = applyUnaryNumeric negateFunction negate negate}

-- | SUBSTRING(extraction_string FROM starting_position [FOR length]
--             [COLLATE collation_name])
--   TODO implement COLLATE part
substringFromFunction :: SQLFunction
substringFromFunction = SQLFunction {
    functionName    = "SUBSTRING",
    minArgCount     = 2,
    argCountIsFixed = True,
    applyFunction   = substringFrom . checkArgCount substringFromFunction}
    where
        substringFrom [strExpr, fromExpr] = StringExpression $
            drop (coerceInt fromExpr - 1) (coerceString strExpr)
        substringFrom _ = internalError

substringFromToFunction :: SQLFunction
substringFromToFunction = SQLFunction {
    functionName    = "SUBSTRING",
    minArgCount     = 3,
    argCountIsFixed = True,
    applyFunction   = substringFromTo . checkArgCount substringFromToFunction}
    where
        substringFromTo [strExpr, fromExpr, toExpr] = StringExpression $
            take (coerceInt toExpr) (drop (coerceInt fromExpr - 1) (coerceString strExpr))
        substringFromTo _ = internalError

notFunction :: SQLFunction
notFunction = SQLFunction {
    functionName    = "NOT",
    minArgCount     = 1,
    argCountIsFixed = True,
    applyFunction   = applyUnaryBool notFunction not}

-- some evaluation helper functions

applyUnaryString ::
    SQLFunction
    -> (String -> String)
    -> [EvaluatedExpression]
    -> EvaluatedExpression
applyUnaryString sqlFun f =
    StringExpression . f . coerceString . head . checkArgCount sqlFun

applyBinaryBooleanTest ::
    SQLFunction
    -> (Bool -> Bool -> Bool)
    -> [EvaluatedExpression]
    -> EvaluatedExpression
applyBinaryBooleanTest _ f [arg1, arg2] =
        BoolExpression $ f (coerceBool arg1) (coerceBool arg2)
applyBinaryBooleanTest sqlFun _ args  = badArgCountError sqlFun args

applyBinaryComparison ::
    SQLFunction
    -> (t -> t -> Bool)
    -> [t]
    -> EvaluatedExpression
applyBinaryComparison _      cmp [arg1, arg2] = BoolExpression $ cmp arg1 arg2
applyBinaryComparison sqlFun _   args         = badArgCountError sqlFun args

applyUnaryNumeric ::
    SQLFunction
    -> (Int -> Int)
    -> (Double -> Double)
    -> [EvaluatedExpression]
    -> EvaluatedExpression
applyUnaryNumeric _ intFunc realFunc [arg] =
    if useRealAlgebra arg then
        RealExpression $ realFunc (coerceReal arg)
    else
        IntExpression $ intFunc (coerceInt arg)
applyUnaryNumeric sqlFun _ _ args  = badArgCountError sqlFun args

applyBinaryNumeric ::
    SQLFunction
    -> (Int -> Int -> Int)
    -> (Double -> Double -> Double)
    -> [EvaluatedExpression]
    -> EvaluatedExpression
applyBinaryNumeric _ intFunc realFunc [arg1, arg2] =
    if useRealAlgebra arg1 || useRealAlgebra arg2 then
        RealExpression $ realFunc (coerceReal arg1) (coerceReal arg2)
    else
        IntExpression $ intFunc (coerceInt arg1) (coerceInt arg2)
applyBinaryNumeric sqlFun _ _ args  = badArgCountError sqlFun args

applyUnaryBool ::
    SQLFunction
    -> (Bool -> Bool)
    -> [EvaluatedExpression]
    -> EvaluatedExpression
applyUnaryBool _      f [arg] = BoolExpression $ f (coerceBool arg)
applyUnaryBool sqlFun _ args  = badArgCountError sqlFun args

checkArgCount :: SQLFunction -> [a] -> [a]
checkArgCount sqlFun args =
    if argCountOK then args else badArgCountError sqlFun args
    where
        minArgs = minArgCount sqlFun
        
        argCountOK =
            if argCountIsFixed sqlFun
                then lengthEquals args minArgs
                else lengthAtLeast args minArgs
            where
                lengthEquals xs len = go xs 0
                    where
                        go [] cumLen = cumLen == len
                        go (_:yt) cumLen = if cumLen >= len then False else go yt (cumLen + 1)
                
                lengthAtLeast xs len = go xs 0
                    where
                        go [] cumLen = cumLen >= len
                        go (_:yt) cumLen = if cumLen >= len then True else go yt (cumLen + 1)

badArgCountError :: SQLFunction -> [a] -> b
badArgCountError sqlFun args =
    if argCountIsFixed sqlFun then error $
        "Error: bad argument count in " ++ functionName sqlFun ++
        " expected " ++ show (minArgCount sqlFun) ++
        " argument(s) but was given " ++ show received
    else error $
        "Error: bad argument count in " ++ functionName sqlFun ++
        " expected at least " ++ show (minArgCount sqlFun) ++
        " argument(s) but was given " ++ show received
    where received = length args

internalError :: a
internalError = error "Internal Error: this should never occur"

useRealAlgebra :: EvaluatedExpression -> Bool
useRealAlgebra (RealExpression _) = True
useRealAlgebra expr = case maybeCoerceInt expr of
    Nothing -> True
    Just _  -> False

-- | trims leading and trailing spaces
trimSpace :: String -> String
trimSpace = f . f
    where f = reverse . dropWhile isSpace

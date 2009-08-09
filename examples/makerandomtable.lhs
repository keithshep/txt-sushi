#!/usr/bin/runhaskell

This is a little script that generates a CSV table containing all random
numbers. It takes two arguments: row count and column count

\begin{code}

import Data.List
import System.Environment
import System.Exit
import System.IO
import System.Random

main = do
    args <- getArgs
    progName <- getProgName
    
    if (length args) /= 2
        then do
            hPutStrLn stderr $
                    "ERROR: " ++ progName ++
                    " requires two command line arguments " ++
                    "(row count and column count)"
            exitFailure
        else do
            stdGen <- getStdGen
            
            let infStrs = randNumStrings stdGen
                rowCountStr:colCountStr:[] = args
                row = read rowCountStr :: Int
                col = read colCountStr :: Int
                tbl = toTable row col infStrs
            
            putStr . unlines . map (intercalate ",") $ tbl

randNumStrings randGen =
    let (nextInt, nextGen) = next randGen
    in  show nextInt : randNumStrings nextGen

toTable 0 _ _ = []
toTable rows cols list
    | rows <= 0 = []
    | otherwise =
        let (currRow, listRemaining) = splitAt cols list
        in  currRow : toTable (rows - 1) cols listRemaining

\end{code}

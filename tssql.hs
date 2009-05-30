-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Keith Sheppard 2009
-- License     :  GPL3 or greater
-- Maintainer  :  keithshep@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Main entry point for the TxtSushi SQL command line
--
-----------------------------------------------------------------------------
import Data.List
import qualified Data.Map as Map
import System.Environment
import System.Exit
import System.IO

import Text.ParserCombinators.Parsec

import Database.TxtSushi.IO
import Database.TxtSushi.SQLExecution
import Database.TxtSushi.SQLParser
import Database.TxtSushi.Transform
import Database.TxtSushi.Util.CommandLineArgument
import Database.TxtSushi.Util.IOUtil

helpOption = OptionDescription {
    isRequired              = False,
    optionFlag              = "-help",
    argumentNames           = [],
    minArgumentCount        = 0,
    argumentCountIsFixed    = True}

tableDefOption = OptionDescription {
    isRequired              = False,
    optionFlag              = "-table",
    argumentNames           = ["CSV-file-name", "table-name"],
    minArgumentCount        = 2,
    argumentCountIsFixed    = True}

turtleSqlOpts = [helpOption, tableDefOption]

sqlCmdLine = CommandLineDescription {
    options                     = turtleSqlOpts,
    minTailArgumentCount        = 1,
    tailArgumentNames           = ["SQL-select-statement"],
    tailArgumentCountIsFixed    = True}

validateTableNames :: [String] -> [String] -> Bool
validateTableNames [] _ = True
validateTableNames (argTblHead:argTblTail) selectTbls =
    if argTblHead `elem` selectTbls then
        validateTableNames argTblTail selectTbls
    else
        error $ "The given table name \"" ++ argTblHead ++
                "\" does not appear in the SELECT statement"

tableArgsToMap [] = Map.empty
tableArgsToMap (currTableArgs:tailTableArgs) =
    case currTableArgs of
        [fileName, tableName] ->
            Map.insert fileName tableName (tableArgsToMap tailTableArgs)
        otherwise ->
            error $ "the \"" ++ (optionFlag tableDefOption) ++
                    "\" option should have exactly two arguments"

unwrapMapList [] = return []
unwrapMapList ((key, value):mapTail) = do
    unwrappedValue <- value
    unwrappedTail <- unwrapMapList mapTail
    return $ (key, unwrappedValue):unwrappedTail

printUsage progName =
    putStrLn $ "Usage: " ++ progName ++ " " ++ formatCommandLine sqlCmdLine

main = do
    args <- getArgs
    progName <- getProgName
    
    let (argMap, argTail) = extractCommandLineArguments sqlCmdLine args
        showHelp = Map.member helpOption argMap || length argTail /= 1
        parseOutcome = parse (withTrailing eof parseSelectStatement) "" (head argTail)
    
    if showHelp then printUsage progName else
        case parseOutcome of
            Left  err        -> print err
            Right selectStmt ->
                let
                    -- create a table file map from the user args
                    tableArgs = Map.findWithDefault [] tableDefOption argMap
                    tableArgMap = tableArgsToMap tableArgs
                    
                    -- get a default table to file map from the select statement
                    selectTblNames = allMaybeTableNames (maybeFromTable selectStmt)
                    defaultTblMap = Map.fromList (zip selectTblNames selectTblNames)
                    
                    -- join the two with arg values taking precidence over
                    -- the default values
                    finalTblFileMap = tableArgMap `Map.union` defaultTblMap
                in
                    -- turn the files into strings
                    if validateTableNames (Map.keys tableArgMap) selectTblNames
                        then do
                            let contentsMap = Map.map getContentsFromFileOrStdin finalTblFileMap
                            
                            unwrappedContents <- unwrapMapList $ Map.toList contentsMap
                            
                            let unwrappedContentsMap = Map.fromList unwrappedContents
                                textTableMap = Map.map (parseTable csvFormat) unwrappedContentsMap
                                dbTableMap = Map.mapWithKey textTableToDatabaseTable textTableMap
                                selectedDbTable = select selectStmt dbTableMap
                                selectedTxtTable = databaseTableToTextTable selectedDbTable
                            
                            putStr $ formatTable csvFormat selectedTxtTable
                        else
                            exitFailure

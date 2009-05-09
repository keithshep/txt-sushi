import Distribution.PackageDescription(PackageDescription)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)

import Text.ParserCombinators.Parsec

import TxtSushi.SQLParser

main = defaultMainWithHooks $ simpleUserHooks {runTests = runTxtSushiTests}

--------------------------------------------------------------------------------
-- Test code
--------------------------------------------------------------------------------

runTxtSushiTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTxtSushiTests _ _ _ _ = do
    let
        -- test statement 1
        stmt1 = SelectStatement {
                    columnSelections = [
                        QualifiedColumn {qualifiedColumnId = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}},
                        AllColumnsFrom {sourceTableName = "table2"}],
                    maybeFromTable = Just (
                        InnerJoin {
                            leftJoinTable = TableIdentifier {tableName = "table1", maybeTableAlias = Nothing},
                            rightJoinTable = TableIdentifier {tableName = "table2", maybeTableAlias = Nothing},
                            onCondition = FunctionExpression {
                                sqlFunction = SQLFunction {functionName = "=", minArgCount = 2, argCountIsFixed = True},
                                functionArguments = [
                                    ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}},
                                    ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table2", columnId = "col1"}}]},
                            maybeTableAlias = Nothing}),
                    maybeWhereFilter = Nothing}
        stmt1_1Txt =
            "select table1.col1, table2.* " ++
            "from table1 inner join table2 on table1.col1 = table2.col1"
        stmt1_2Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1"
        
        -- test statement 2
        stmt2 = SelectStatement {
                    columnSelections = [
                        QualifiedColumn {qualifiedColumnId = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}},
                        AllColumnsFrom {sourceTableName = "table2"}],
                    maybeFromTable = Just (
                        InnerJoin {
                            leftJoinTable = TableIdentifier {tableName = "table1", maybeTableAlias = Nothing},
                            rightJoinTable = TableIdentifier {tableName = "table2", maybeTableAlias = Nothing},
                            onCondition = FunctionExpression {
                                sqlFunction = SQLFunction {functionName = "=", minArgCount = 2, argCountIsFixed = True},
                                functionArguments = [
                                    ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}},
                                    ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table2", columnId = "col1"}}]},
                            maybeTableAlias = Nothing}),
                    maybeWhereFilter = Just (
                        FunctionExpression {
                            sqlFunction = SQLFunction {functionName = "<>", minArgCount = 2, argCountIsFixed = True},
                            functionArguments = [
                                FunctionExpression {
                                    sqlFunction = SQLFunction {functionName = "UPPER", minArgCount = 1, argCountIsFixed = True},
                                    functionArguments = [ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}]},
                                FunctionExpression {
                                    sqlFunction = SQLFunction {functionName = "LOWER", minArgCount = 1, argCountIsFixed = True},
                                    functionArguments = [ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}]}]})}
        stmt2_1Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper(table1.col1)<>lower(table1.col1)"
        stmt2_2Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper(table1.col1) <> lower(table1.col1)"
    
    testSqlSelect stmt1 stmt1_1Txt
    testSqlSelect stmt1 stmt1_2Txt
    testSqlSelect stmt2 stmt2_1Txt
    testSqlSelect stmt2 stmt2_2Txt

testSqlSelect :: SelectStatement -> String -> IO ()
testSqlSelect expectedResult selectStatementText = do
    let testingPrefix = "Testing: "
        stmtParseResult = parse parseSelectStatement "" selectStatementText
    putStrLn $ testingPrefix ++ selectStatementText
    case stmtParseResult of
        Left errMsg -> error $ show errMsg
        Right selectStatement ->
            if selectStatement == expectedResult
                then
                    putStrLn "Success"
                else
                    error $ "\n" ++ (show selectStatement) ++ "\nNOT EQUAL TO\n" ++ (show expectedResult)

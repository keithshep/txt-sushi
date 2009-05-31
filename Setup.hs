import Distribution.PackageDescription(PackageDescription)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)

import Text.ParserCombinators.Parsec

import Database.TxtSushi.SQLParser

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
                        ExpressionColumn {expression = ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}},
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
                    maybeWhereFilter = Nothing,
                    orderByItems = [],
                    maybeGroupByHaving = Nothing}
        stmt1_1Txt =
            "select table1.col1, table2.* " ++
            "from table1 inner join table2 on table1.col1 = table2.col1"
        stmt1_2Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1"
        
        -- test statement 2
        stmt2 = SelectStatement {
                    columnSelections = [
                        ExpressionColumn {expression = ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}},
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
                                    functionArguments = [ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}]}]}),
                    orderByItems = [],
                    maybeGroupByHaving = Nothing}
        stmt2_1Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper(table1.col1)<>lower(table1.col1)"
        stmt2_2Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper(table1.col1) <> lower(table1.col1)"
        
        -- test statement 3
        stmt3 = SelectStatement {
                    columnSelections = [
                        ExpressionColumn {expression = ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}},
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
                                    functionArguments = [ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}]}]}),
                    orderByItems = [OrderByItem {
                        orderExpression = ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "firstName"}},
                        orderAscending = True}],
                    maybeGroupByHaving = Nothing}
        stmt3_1Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper(table1.col1)<>lower(table1.col1) order by table1.firstName asc"
        stmt3_2Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper(table1.col1)<>lower(table1.col1) order by table1.firstName"
        stmt3_3Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper (table1.col1) <> lower ( table1.col1 ) order by  table1.firstName ascending"
        
        -- test statement 4
        stmt4 = SelectStatement {
                    columnSelections = [
                        ExpressionColumn {expression = ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}},
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
                                    functionArguments = [ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "col1"}}]}]}),
                    orderByItems = [OrderByItem {
                        orderExpression = ColumnExpression {column = ColumnIdentifier {maybeTableName = Just "table1", columnId = "firstName"}},
                        orderAscending = False}],
                    maybeGroupByHaving = Nothing}
        stmt4_1Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper (table1.col1) <> lower ( table1.col1 ) order by  table1.firstName descending"
        stmt4_2Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper (table1.col1) <> lower ( table1.col1 ) order by  table1.firstName DESCENDING"
        stmt4_3Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper (table1.col1) <> lower ( table1.col1 ) order by  table1.firstName desc"
        stmt4_4Txt =
            "select table1.col1, table2.* " ++
            "from table1 join table2 on table1.col1 = table2.col1 " ++
            "where upper (table1.col1) <> lower ( table1.col1 ) order by  table1.firstName DESC"
    
    testSqlSelect stmt1 stmt1_1Txt
    testSqlSelect stmt1 stmt1_2Txt
    
    testSqlSelect stmt2 stmt2_1Txt
    testSqlSelect stmt2 stmt2_2Txt
    
    testSqlSelect stmt3 stmt3_1Txt
    testSqlSelect stmt3 stmt3_2Txt
    testSqlSelect stmt3 stmt3_3Txt

    testSqlSelect stmt4 stmt4_1Txt
    testSqlSelect stmt4 stmt4_2Txt
    testSqlSelect stmt4 stmt4_3Txt
    testSqlSelect stmt4 stmt4_4Txt

testSqlSelect :: SelectStatement -> String -> IO ()
testSqlSelect expectedResult selectStatementText = do
    let stmtParseResult = parse (withTrailing eof parseSelectStatement) "" selectStatementText
        colNums = take (length selectStatementText) ([1 .. 9] ++ cycle [0 .. 9])
    putStrLn ""
    putStrLn "Testing:"
    putStrLn $ concat (map show colNums)
    putStrLn selectStatementText
    case stmtParseResult of
        Left errMsg -> error $ show errMsg
        Right selectStatement ->
            if selectStatement == expectedResult
                then
                    putStrLn "Success"
                else
                    error $ "\n" ++ (show selectStatement) ++ "\nNOT EQUAL TO\n" ++ (show expectedResult)

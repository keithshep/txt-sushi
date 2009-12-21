#!/bin/bash

# Example adapted from http://en.wikipedia.org/wiki/Join_(SQL)#Self-join
#
# results in:
# EmployeeID|LastName|EmployeeID|LastName |Country
# 123       |Rafferty|124       |Jones    |Australia
# 123       |Rafferty|145       |Steinberg|Australia
# 124       |Jones   |145       |Steinberg|Australia
# 305       |Smith   |306       |Jasper   |United Kingdom

../dist/build/tssql/tssql -table Employee employees.csv \
'SELECT F.EmployeeID, F.LastName, S.EmployeeID, S.LastName, F.Country
FROM Employee AS F JOIN Employee AS S
ON F.Country = S.Country
WHERE F.EmployeeID < S.EmployeeID
ORDER BY F.EmployeeID, S.EmployeeID'


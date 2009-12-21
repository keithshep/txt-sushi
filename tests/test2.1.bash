#!/bin/bash

# Example adapted from http://www.w3schools.com/sql/sql_groupby.asp

../dist/build/tssql/tssql -table Orders orders.csv \
'SELECT Customer, SUM(OrderPrice) FROM Orders GROUP BY Customer'

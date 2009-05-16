#!/bin/bash

# replicating SELECT from
# http://www.itl.nist.gov/div897/ctg/dm/sql_examples.htm
#
# SELECT * FROM STATION
# WHERE LAT_N > 39.7

../dist/build/tssql/tssql -table STATION station.csv \
'SELECT * FROM STATION WHERE LAT_N > 39.7' \
| ../dist/build/csvtopretty/csvtopretty -

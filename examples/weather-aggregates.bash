#!/bin/bash

# replicating aggregate SELECT from
# http://www.itl.nist.gov/div897/ctg/dm/sql_examples.htm
#
# SELECT MAX(TEMP_F), MIN(TEMP_F), AVG(RAIN_I), ID
# FROM STATS
# GROUP BY ID;
#
# MAX(TEMP_F)   MIN(TEMP_F) AVG(RAIN_I) ID
# 91.7          57.4        2.73        13
# 74.8          27.3        1.145       44
# 65.8          6.7         3.31        66
../dist/build/tssql/tssql -table STATS stats.csv \
'SELECT MAX(TEMP_F+0) AS MAX_TEMP, MIN(TEMP_F+0) AS MIN_TEMP, AVG(RAIN_I) AS AVG_RAIN_I, ID
FROM STATS
GROUP BY ID' \
| ../dist/build/csvtopretty/csvtopretty -

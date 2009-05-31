#!/bin/bash

# replicating JOIN from
# http://www.itl.nist.gov/div897/ctg/dm/sql_examples.htm
#
# SELECT LAT_N, CITY, TEMP_F
# FROM STATS, STATION
# WHERE MONTH = 7
# AND STATS.ID = STATION.ID
# ORDER BY TEMP_F;

../dist/build/tssql/tssql -table STATION station.csv -table STATS stats.csv \
'SELECT LAT_N, CITY, TEMP_F FROM STATS JOIN STATION ON STATS.ID = STATION.ID
WHERE MONTH = 7 ORDER BY TEMP_F + 0' \
| ../dist/build/csvtopretty/csvtopretty -

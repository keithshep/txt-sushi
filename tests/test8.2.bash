#!/bin/bash

# replicating JOIN from
# http://www.itl.nist.gov/div897/ctg/dm/sql_examples.htm
#
# SELECT LAT_N, CITY, TEMP_F
# FROM STATS, STATION
# WHERE MONTH = 7
# AND STATS.ID = STATION.ID
# ORDER BY TEMP_F;
# LAT_N CITY    TEMP_F
# 47    Caribou 65.8
# 40    Denver  74.8
# 33    Phoenix 91.7

../dist/build/tssql/tssql -table STATION station.csv -table STATS stats.csv \
'SELECT LAT_N, CITY, TEMP_F FROM STATS JOIN STATION ON STATS.ID = STATION.ID
WHERE MONTH = 7 ORDER BY as_real(TEMP_F)'

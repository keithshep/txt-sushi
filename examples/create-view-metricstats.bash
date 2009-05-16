#!/bin/bash

# replicating 1st CREATE VIEW from
# http://www.itl.nist.gov/div897/ctg/dm/sql_examples.htm
#
# CREATE VIEW METRIC_STATS (ID, MONTH, TEMP_C, RAIN_C) AS
# SELECT ID,
# MONTH,
# (TEMP_F - 32) * 5 /9,
# RAIN_I * 0.3937
# FROM STATS;

# After redirecting to metric_stats.csv we can simply use that as a table.
# So, that's as close as TxtSushi gets to creating a view :-)
../dist/build/tssql/tssql -table STATS stats.csv \
'SELECT ID, MONTH, (TEMP_F - 32) * 5 /9, RAIN_I * 0.3937 FROM STATS' \
> metric_stats.csv

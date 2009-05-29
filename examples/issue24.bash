#!/bin/bash

# see: http://code.google.com/p/txt-sushi/issues/detail?id=24

../dist/build/tssql/tssql -table tbl issue24.csv \
'SELECT `UPPERVAR` FROM tbl' \
| ../dist/build/csvtopretty/csvtopretty -

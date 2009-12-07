#!/bin/bash

# Our kitchen sink example to exercise all of the TxtSushi functionality as
# we can in one SELECT.
# NOTE: the "+ 0" is needed to coerce id to an integer. Otherwise it is
# sorted as text which is not what we want
../dist/build/tssql/tssql \
'select `example1.csv`.*, `example2.csv`.`other val`, `example2.csv`.id2
from `example1.csv` join `example2.csv` on `example1.csv`.id = `example2.csv`.id2
where `example1.csv`.`other val` <> "hello 269" and id2 > 246
order by id + 0 asc' \
| ../dist/build/csvtopretty/csvtopretty -

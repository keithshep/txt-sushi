#!/bin/bash

# An example of using TxtSushi to query one of the MGI (mouse genome informatics)
# database reports
#
# Selects accession ID, Symbol, chromosome, ID and centimorgan position (without
# the "trim" we end up with extra spaces).
# Only select chromosomes 1, 8 and 19 and look for "N/A" positions
# Sort the rows by chromosome (the "+0" coerces the chromosome from text to an
# integer) then symbol

wget -q -O - ftp://ftp.informatics.jax.org/pub/reports/MRK_List2.rpt \
| ../dist/build/tabtocsv/tabtocsv - \
| ../dist/build/tssql/tssql -table mgi - \
'select Chr, min(`cM Position` + 0), max(`cM Position` + 0), avg(`cM Position`), count(*)
from mgi where (Chr = 1) and trim(`cM Position`) <> "N/A" and trim(`cM Position`) <> "syntenic"
order by Chr+0 descending' \
| ../dist/build/csvtopretty/csvtopretty -

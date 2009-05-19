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
'select `MGI Accession ID`, Symbol, Chr, trim(`cM Position`)
from mgi where (Chr = 1 or Chr = 8 or Chr = 19) and trim(`cM Position`) = "N/A"
order by Chr+0, Symbol' \
| ../dist/build/csvtopretty/csvtopretty -

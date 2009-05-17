#!/bin/bash

# An example of using TxtSushi to query one of the MGI (mouse genome informatics)
# database reports

wget -q -O - ftp://ftp.informatics.jax.org/pub/reports/MRK_List2.rpt | tabtocsv - \
| tssql -table mgi - \
'select `MGI Accession ID`, Symbol, Chr, trim(`cM Position`)
from mgi where (Chr = 1 or Chr = 8 or Chr = 19) and trim(`cM Position`) = "N/A"
order by Chr+0, Symbol' \
| csvtopretty -


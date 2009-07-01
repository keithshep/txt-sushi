#!/bin/bash

# An example of using TxtSushi to query one of the CGD (center for genome dynamics)
# datasets
# 
# Selects chromosome, base pair position, data source and a couple of strains.
# Filters out any rows from perlegen and any rows where A/J and BPH/2J differ

curl -s http://cgd.jax.org/ImputedSNPData/v1.1/PBupdatedv1.1_HMM_filleddata/PBupdatedv1.1_SNP_genoData_chr4.csv.gz \
| zcat \
| ../dist/build/tssql/tssql -table cgd - \
'select * from (select ChrID, `build 36 bp Position`, Source, `BPH/2J`, `A/J` from cgd)
where Source <> "Perlegen36_b03" and `A/J` = `BPH/2J`
order by `build 36 bp Position` + 0 desc' \
| ../dist/build/csvtopretty/csvtopretty -

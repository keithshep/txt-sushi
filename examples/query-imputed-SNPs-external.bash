#!/bin/bash

# An example of using TxtSushi to query one of the CGD (center for genome dynamics)
# datasets
# 
# Selects chromosome, base pair position, data source and a couple of strains.
# Filters out any rows from perlegen and any rows where A/J and BPH/2J differ

echo "running external sort"
curl -s http://cgd.jax.org/ImputedSNPData/v1.1/PBupdatedv1.1_HMM_filleddata/PBupdatedv1.1_SNP_genoData_chr4.csv.gz \
| zcat \
| time ../dist/build/tssql/tssql -external-sort -table cgd - \
'select ChrID, `build 36 bp Position`, Source, `BPH/2J`, `A/J` from cgd
order by `build 36 bp Position` + 0 desc' > external-sort.tmp

echo "running in memory sort"
curl -s http://cgd.jax.org/ImputedSNPData/v1.1/PBupdatedv1.1_HMM_filleddata/PBupdatedv1.1_SNP_genoData_chr4.csv.gz \
| zcat \
| time ../dist/build/tssql/tssql -table cgd - \
'select ChrID, `build 36 bp Position`, Source, `BPH/2J`, `A/J` from cgd
order by `build 36 bp Position` + 0 desc' > sort-in-memory.tmp

echo "now you can diff the two files to make sure that they're the same"

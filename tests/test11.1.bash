#!/bin/bash

../dist/build/tssql/tssql -table snps snp-sample.csv \
'select `build 36 bp Position`, for strain in [`CAST/EiJ` .. `DBA/1J`] yield strain from snps'

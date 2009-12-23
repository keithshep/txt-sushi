#!/bin/bash

../dist/build/tssql/tssql -table snps snp-sample.csv \
'select `build 36 bp Position`,
    for strain in [`CAST/EiJ` .. `DBA/1J`]
    yield if_then_else(upper(strain) = `C57BL/6J`, "B6", "NON-B6") from snps'

#!/bin/bash

../dist/build/tssql/tssql -table snps snp-sample.csv 'select * from snps'

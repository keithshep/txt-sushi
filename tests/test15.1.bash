#!/bin/bash

../dist/build/tssql/tssql 'select is_numeric(maybe_num) from `maybe-nums.csv`'

#!/bin/bash

./tssql 'select `example1.csv`.*, `example2.csv`.`other val`, `example2.csv`.id2 from `example1.csv` join `example2.csv` on `example1.csv`.id = `example2.csv`.id2'


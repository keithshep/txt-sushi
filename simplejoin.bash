#!/bin/bash

./tssql 'select `example.csv`.*, `example2.csv`.`other val`, `example2.csv`.id2 from `example.csv` join `example2.csv` on `example.csv`.id = `example2.csv`.id2'

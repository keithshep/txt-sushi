#!/bin/bash

for i in ./test*.*.bash
do
  expected_file="$(expr ${i} : '\(\./test.*\)\..*\.bash').expected"
  echo "diffing ${i} output vs ${expected_file}"
  $i | diff - ${expected_file}
done

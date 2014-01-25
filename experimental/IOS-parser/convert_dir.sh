#!/bin/bash

DIR="$1"
FILES=""

for file in `ls -1 $DIR/*_rtr_config.txt`; do
  config=`basename $file`
  FILES="$FILES $config"
done

echo "./ios2flowlog --path $DIR $FILES"

./ios2flowlog --path $DIR $FILES

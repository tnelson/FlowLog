#!/bin/bash
echo '-----------------------------------------------------'
for rtrid in `ls $1 | grep "config" | cut -d_ -f1`; do  ./measure.sh $rtrid; done
echo '-----------------------------------------------------'g

# ./prepare.sh $1 $rtrid; sleep 15;
# sudo killall mn; sudo mn -c; sudo killall flowlog.native
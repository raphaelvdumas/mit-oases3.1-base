#!/bin/sh
BASE=$1
oasp ${BASE}

i=0
n=16

while [ $i -lt $n ]
do
 i=`expr $i + 1`
 sed -e "s/N s g/N s g r/g" ${BASE}_scat.dat > ${BASE}_${i}.dat
 oassp ${BASE}_${i} ${BASE}
done

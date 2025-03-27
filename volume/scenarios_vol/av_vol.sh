#!/bin/sh
for VAR in vol_c_iso6
do  
echo $VAR aviso.sh
aviso.sh $VAR >> l
rm fort.*
rm $VAR.trf
rm $VAR.vol
rm $VAR.rhs
rm $VAR.ifl
done

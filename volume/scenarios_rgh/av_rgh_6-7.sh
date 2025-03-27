#!/bin/sh
for VAR in rgh_c_iso6 rgh_c_iso7 
do  
echo $VAR aviso.sh
aviso.sh $VAR >> l
rm fort.*
rm $VAR.trf
rm $VAR.vol
rm $VAR.rhs
rm $VAR.ifl
done

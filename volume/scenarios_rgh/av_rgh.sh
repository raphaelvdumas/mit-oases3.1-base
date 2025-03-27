#!/bin/sh
for VAR in rgh_c_iso1 rgh_c_iso2 rgh_c_iso3
do  
echo $VAR aviso.sh
aviso.sh $VAR >> l
rm fort.*
rm $VAR.trf
rm $VAR.vol
rm $VAR.rhs
rm $VAR.ifl
done

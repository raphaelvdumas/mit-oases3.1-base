#!/bin/sh
#
echo "Attempting graphical demo of Oases"

# check existence of plotmtv
command -v plotmtv >/dev/null 2>&1 || { echo >&2 "plotmtv required but it's not installed.  Aborting."; exit 1; }

echo "Generating data from saffip3 sample"
cplot tloss/saffip3 

echo "Plotting data from saffip3 sample in plotMTV by setting CON_PACKGE=MTV and PLP_PACKGE=MTV"
CON_PACKGE=MTV,PLP_PACKGE=MTV xterm -e mplot tloss/saffip3 &

sleep 5

echo "Plotting data from saffip3 sample in mindis by setting PLP_PACKGE=MIN"
CON_PACKGE=,PLP_PACKGE=MIN xterm -e mplot tloss/saffip3 &


echo "YOU SHOULD NOW SEE TWO WINDOWS CONTAINING GRAPHS"


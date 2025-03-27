#!/bin/sh

# build.sh
# by Ian Katz, 2013
# ifreecarve@gmail.com


# usage function to display help for the hapless user

usage ()
{
     mycmd=`basename $0`
     echo "$mycmd"
     echo "usage: $mycmd <installation root path>"
     echo 
     echo "Builds and installs Oases"
     echo " - Binaries  in <installation root path>/bin"
     echo " - Libraries in <installation root path>/lib"
}


# test if we have an arguments on the command line
if [ $# -lt 1 ]
then
    usage
    exit
fi

# prepare directory
mkdir build
cd build

# actual work done here
cmake ../ -DCMAKE_INSTALL_PREFIX:PATH=$1
make -j`nproc`
make install

# messaging
echo 
echo 
echo "----------------------------------------------------------"
echo 
echo "            build.sh INSTALLATION COMPLETE"
echo 
echo "----------------------------------------------------------"
echo
echo "You must now add environment variables to your shell."
echo " - OASES_SH = $1/bin"
echo " - OASES_BIN = $1/bin"
echo " - OASES_LIB = $1/lib"
echo " - PATH = <current PATH>:$1/bin"
echo " - USRTERMTYPE = x"
echo " - CON_PACKGE = MTV"
echo " - PLP_PACKGE = MTV"
echo

# OASES 2.1 Base Package - Installation Guide

[OASES](https://tlo.mit.edu/technologies/oases-software-modeling-seismo-acoustic-propagation-horizontally-stratified-waveguides) is a general purpose computer code made by the [Massachusetts Institute of Technology](http://www.mit.edu/) for modeling seismo-acoustic propagation in horizontally stratified waveguides using wavenumber integration in combination with the Direct Global Matrix solution technique.

As I found myself in many difficulties during the installation of this software, this repository contains a full friendly installation guide to install the [OASES 2.1 Base Package](http://lamss.mit.edu/lamss/tars/oases-public.tgz) on Windows 10.

## Installation of OASES 2.1

Simply download this repository and follow the instructions of the ```OASES 2.1 - Installation Guide.pdf```.

### Sofwares installed
1. [Ubuntu application](https://www.microsoft.com/en-us/p/ubuntu/9nblggh4msv6?activetab=pivot:overviewtab) from the Microsoft Store made by [Canonical](https://canonical.com/) Group Limited.

2. [VcXsrv](https://sourceforge.net/projects/vcxsrv/) to handle the graphical interface between the Ubuntu application and Windows 10.

### Dependencies
```
cmake, gcc, g++, gfortran, csh, libx11-dev, plotmtv
```

## Running OASES 2.1

After completing the installation, run an example from the ```tloss``` folder with the ```oast``` module, for example
```
  oast ~/oases-public/tloss/pekeris 
```
and then plot the transmission loss with the ```cplot``` module
```
  cplot ~/oases-public/tloss/pekeris 
```

Further information on how this software works with its numerous modules can be found in the OASES 3.1 User Guide given in this repository. Even though it is intended for the version 3.1 of OASES, it can still be used here.

## Notes
Even though the installation works, it is possible to install it in a simpler manner using MinGW directly on Windows 10. But this doesn't work for me yet !

The following modifications of ```oases-public``` have been made: 
* removed the ```paroases-src``` folder and changed ```CMakeLists.txt``` accordingly because I couldn't get it to work yet and it avoids installing more dependencies,
* changed ```CMakeLists.txt``` of ```bin``` folder as range dependent modules ```rdoasp``` and ```rdoast``` aren't in this free version of OASES.

_Last successful installation: december 14, 2019_

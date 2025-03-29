# OASES

[OASES](https://tlo.mit.edu/technologies/oases-software-modeling-seismo-acoustic-propagation-horizontally-stratified-waveguides) is a general-purpose computer code developed by the [Massachusetts Institute of Technology](http://www.mit.edu/) for modeling seismo-acoustic propagation in horizontally stratified waveguides. It uses wavenumber integration combined with the Direct Global Matrix solution technique.

**I had a lot of trouble installing this base package on Windows, so I decided to create a friendly installation guide to help others.**

## Recommended: Using OASES 3.1 on Windows (Portable Edition)

For ease of use, you can run OASES 3.1 directly on Windows by using the portable edition. Follow these steps:

1. **Download** the portable edition from the release section. Choose the version corresponding to your operating system (32-bit or 64-bit).
   
2. **Extract** the archive to a folder of your choice.

3. **Update your PATH environment variable** to include the `bin` folder from the extracted folder. This allows you to run any OASES module directly from `cmd` or `PowerShell` (e.g., `oast`, `oasr`, etc.) using the syntax provided in the official documentation.

**Note**: The portable edition is built using Windows 11 with MSYS2 (mingw32 and mingw64) and GCC. The source code is located in the `core-oases` branch of this repository.

**Warning**: This portable edition is compute-only. You won’t be able to plot results directly. To plot your results, use the functions provided in the `third_party` folder with MATLAB or Python.

_Last successful run: March 19, 2025_

## Alternative: Building OASES 3.1 on WSL (Not Recommended)

While installing OASES 3.1 on Windows using WSL is possible, it is more complex and may encounter various issues. If you choose this method, follow these instructions:

1. **Download** this repository.

2. **Refer to the installation guide** in the `OASES 3.1 - Installation Guide.pdf` file for detailed steps.

### Required Software
1. [Ubuntu](https://www.microsoft.com/en-us/p/ubuntu/9nblggh4msv6?activetab=pivot:overviewtab) from the Microsoft Store (developed by [Canonical](https://canonical.com/)).
   
2. [VcXsrv](https://vcxsrv.com/) to enable the graphical interface between Ubuntu and Windows.

### Dependencies
You’ll need the following dependencies installed:
```
    cmake, gcc, g++, gfortran, csh, libx11-dev, plotmtv
```
### Running OASES 3.1

After installation, you can run an example from the `tloss` folder using the `oast` module. For example:
```
  oast ~/oases-public/tloss/pekeris 
```
and then plot the transmission loss with the ```cplot``` module
```
  cplot ~/oases-public/tloss/pekeris 
```

Additional information about using OASES and its modules can be found in the **OASES 3.1 User Guide** in this repository.

_Last successful installation: december 14, 2019_

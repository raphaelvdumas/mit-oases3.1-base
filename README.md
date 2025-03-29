# Core OASES
MSYS 2 with Ninja on Windows 11 x86_x64.

Windows64 :
cmake -G "Ninja" -B build  -DCMAKE_C_COMPILER="gcc.exe" -DCMAKE_Fortran_COMPILER="gfortran.exe"

Windows32 :
cmake -G "Ninja" -B build -DCMAKE_C_COMPILER="i686-w64-mingw32-gcc.exe" -DCMAKE_Fortran_COMPILER="i686-w64-mingw32-gfortran.exe"

cmake --build build
cmake --install build

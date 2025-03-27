cmake_minimum_required (VERSION 2.6)

project(Oases)
enable_language(Fortran)

option (ENABLE_PAROASES   "Enable PAROASES installation (default = enabled)" ON)

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -static -fstrength-reduce -fexpensive-optimizations -fall-intrinsics")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "fort")
  # fort (untested, copied from Oases makefile)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "f77")
  # f77 (jntested, copied from Oases makefile)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O -arch host -fast -tune host")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O -g -m32")
elseif (Fortran_COMPILER_NAME MATCHES "cf77")
  # cf77 (jntested, copied from Oases makefile)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O -g -m32")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME MATCHES "gfortran")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran")

add_subdirectory(bin)
add_subdirectory(src)

#if (EXISTS "${ROOT}/src-3d" AND IS_DIRECTORY "${ROOT}/src-3d")
  add_subdirectory(src-3d)
#endif (EXISTS "${ROOT}/src-3d" AND IS_DIRECTORY "${ROOT}/src-3d")

if (EXISTS "${ROOT}/src-rd" AND IS_DIRECTORY "${ROOT}/src-rd")
  add_subdirectory(src-rd)
endif (EXISTS "${ROOT}/src-rd" AND IS_DIRECTORY "${ROOT}/src-rd")

if (EXISTS "src-mfp" AND IS_DIRECTORY "src-mfp")
  add_subdirectory(src-mfp)
endif (EXISTS "src-mfp" AND IS_DIRECTORY "src-mfp")



add_subdirectory(mindis)
add_subdirectory(plot)
add_subdirectory(contour)
add_subdirectory(pulsplot)
#add_subdirectory(misc) # broken, commented out in original Makefile
add_subdirectory(Benchmarks)

if (ENABLE_PAROASES)
  add_subdirectory(paroases-src)
endif()


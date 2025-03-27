cmake_minimum_required (VERSION 3.15)

project(Oases LANGUAGES C Fortran)

# CMAKE_Fortran_FLAGS for release and debug
set(CMAKE_Fortran_FLAGS "-std=legacy")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -O3 -static -fstrength-reduce -fexpensive-optimizations -fall-intrinsics")
# set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS} -O0 -fno-f2c -g")

# CMAKE_Fortran_FLAGS for release and debug
set(CMAKE_C_FLAGS "-std=c89")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -O3")
# set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -O0")

add_subdirectory(bin)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/core)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/3d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/mfp)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/pulsplot)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_subdirectory(${CMAKE_SOURCE_DIR}/third_party/plot_X11/contour)
  add_subdirectory(${CMAKE_SOURCE_DIR}/third_party/plot_X11/plot)
  add_subdirectory(${CMAKE_SOURCE_DIR}/third_party/plot_X11/mindis)
endif()



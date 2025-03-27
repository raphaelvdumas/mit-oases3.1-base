# Set the minimum required CMake version
cmake_minimum_required(VERSION 3.10)

# Set the project name
project(Oases)



execute_process(
  COMMAND where gcc
  OUTPUT_VARIABLE GCC_PATH
  RESULT_VARIABLE GCC_RESULT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND where g++
  OUTPUT_VARIABLE GXX_PATH
  RESULT_VARIABLE GXX_RESULT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND where gfortran
  OUTPUT_VARIABLE GFORTRAN_PATH
  RESULT_VARIABLE GFORTRAN_RESULT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# If the paths are found, set them as the compilers
if(GCC_RESULT EQUAL 0)
  string(REPLACE "\\" "/" GCC_PATH ${GCC_PATH})  # Convert backslashes to forward slashes
  set(CMAKE_C_COMPILER ${GCC_PATH})
else()
  message(FATAL_ERROR "gcc not found.")
endif()

if(GXX_RESULT EQUAL 0)
  string(REPLACE "\\" "/" GXX_PATH ${GXX_PATH})  # Convert backslashes to forward slashes
  set(CMAKE_CXX_COMPILER ${GXX_PATH})
else()
  message(FATAL_ERROR "g++ not found")
endif()

if(GFORTRAN_RESULT EQUAL 0)
  string(REPLACE "\\" "/" GFORTRAN_PATH ${GFORTRAN_PATH})  # Convert backslashes to forward slashes
  set(CMAKE_Fortran_COMPILER ${GFORTRAN_PATH})
else()
  message(FATAL_ERROR "gfortran not found")
endif()

# Enable Fortran and C languages
enable_language(C)
enable_language(Fortran)


# Options to build
option (ENABLE_PLOT   "Enable plot installation (default = disabled)" OFF)

# CMAKE_Fortran_FLAGS for release and debug
set(CMAKE_Fortran_FLAGS "-std=legacy")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} -O3 -static -fstrength-reduce -fexpensive-optimizations -fall-intrinsics")
set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS} -O0 -fno-f2c -g")

# CMAKE_Fortran_FLAGS for release and debug
set(CMAKE_C_FLAGS "-std=c89")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS} -O3")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -O0")

message(STATUS "Paroases integration is disabled.")

# # Add subdirectories for the project
# add_subdirectory(bin)
# add_subdirectory(src)

# # PAROASES option
# option (ENABLE_PAROASES   "Enable PAROASES installation (default = enabled)" ON)
# if (ENABLE_PAROASES)
#   if (EXISTS "${CMAKE_SOURCE_DIR}/paroases-src")
#     add_subdirectory(paroases-src)
#   else()
#     message(WARNING "The 'paroases-src' directory does not exist. Skipping add_subdirectory.")
#   endif()
# else()
#   message(STATUS "Paroases integration is disabled.")
# endif()

# # Check if 3d source exists
# if (EXISTS "${CMAKE_SOURCE_DIR}/src-3d" AND IS_DIRECTORY "${CMAKE_SOURCE_DIR}/src-3d")
#   add_subdirectory(src-3d)
# endif()

# # Check if rd source exists
# if (EXISTS "${CMAKE_SOURCE_DIR}/src-rd" AND IS_DIRECTORY "${CMAKE_SOURCE_DIR}/src-rd")
#   add_subdirectory(src-rd)
# endif()

# # Check if mfp source exists
# if (EXISTS "${CMAKE_SOURCE_DIR}/src-mfp" AND IS_DIRECTORY "${CMAKE_SOURCE_DIR}/src-mfp")
#   add_subdirectory(src-mfp)
# endif()

# add_subdirectory(Benchmarks)


# # Add other subdirectories
# add_subdirectory(mindis)
# add_subdirectory(plot)
# add_subdirectory(contour)
# add_subdirectory(pulsplot)
# #add_subdirectory(misc) # broken, commented out in original Makefile




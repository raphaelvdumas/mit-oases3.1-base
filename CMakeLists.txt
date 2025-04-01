cmake_minimum_required (VERSION 3.21)

project(Oases LANGUAGES C Fortran)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
else()
message(FATAL_ERROR "Unsupported system name '${CMAKE_SYSTEM_NAME}'. CMake for GCC with 'Windows' or 'Linux' only.")
endif()


# Check if the string contains "86". Supposing we're in a 64 bit system to build
string(FIND "${CMAKE_C_COMPILER}" "86" RESULT)

if(RESULT GREATER -1)
    set(NBITS "32")  # Set NBITS to "32" if "86" is found
    set(REQUIRED_DLLS "libgfortran-5.dll" "libgcc_s_dw2-1.dll")
else()    
    set(NBITS "64")  # Set NBITS to "64" if "86" is not found
    set(REQUIRED_DLLS "libgfortran-5.dll" "libgcc_s_seh-1.dll")
endif()
# set(NBITS "32")  # Set NBITS to "32" ito force Linux for now ...
set(CMAKE_INSTALL_PREFIX "release/Oases-3.1-${CMAKE_SYSTEM_NAME}${NBITS}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -O3 -std=legacy -fstrength-reduce -fexpensive-optimizations -fall-intrinsics -m${NBITS}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -O3 -std=c11 -m${NBITS}")

#
# if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
#     # find_library(GCC_S_LIB gcc_s PATHS /lib /usr/lib /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu)
#     # find_library(GFORTRAN_LIB gfortran PATHS /lib /usr/lib /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu)
#     find_library(GCC_S_LIB gcc_s HINTS /lib /usr/lib /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu /custom/path/to/libs)
#     find_library(GFORTRAN_LIB gfortran HINTS /lib /usr/lib /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu /custom/path/to/libs)
    
#     if(NOT GCC_S_LIB)
#         message(FATAL_ERROR 
#             "ERROR: libgcc_s.so.1 not found.\n"
#             "Please install it using the following command:\n"
#             "    sudo apt update && sudo apt install libgcc-s1"
#         )
#     else()
#         message(STATUS "Found libgcc_s: ${GCC_S_LIB}")
#     endif()

#     if(NOT GFORTRAN_LIB)
#         message(FATAL_ERROR 
#             "ERROR: libgfortran.so.5 not found.\n"
#             "Please install it using the following command:\n"
#             "    sudo apt update && sudo apt install gfortran-12"
#             "\n(Replace 'gfortran-12' with the correct version if needed.)"
#         )
#     else()
#         message(STATUS "Found libgfortran: ${GFORTRAN_LIB}")
#     endif()
# endif()

#---------------------------------------------------
# Utility static libraries
#---------------------------------------------------
add_subdirectory(${CMAKE_SOURCE_DIR}/src/2d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/3d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/pulsplot)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_subdirectory(${CMAKE_SOURCE_DIR}/third_party/plot_X11/mindis)
  add_subdirectory(${CMAKE_SOURCE_DIR}/third_party/plot_X11/contour)
  add_subdirectory(${CMAKE_SOURCE_DIR}/third_party/plot_X11/plot)
  message(WARNING "'Plotmtv won't be installed. Ensure that you installed it.")
else()
  message(WARNING "'contour', 'plot' and 'mindis' depends on X11 ('Linux' only). They are not available on 'Windows'.")
endif()


# Don't know yet how to avoid msys64 path with Ninja... Anyways, copy the required DLL on install automatically.
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    foreach(EACH_DLL ${REQUIRED_DLLS})
        message(STATUS "Finding ${EACH_DLL} in C:/msys64/mingw${NBITS}/bin/ for copy...")    
        install(CODE "
            execute_process(
                COMMAND ${CMAKE_COMMAND} -E copy
                \"C:/msys64/mingw${NBITS}/bin/${EACH_DLL}\" 
                \"${CMAKE_INSTALL_PREFIX}/bin/${EACH_DLL}\"
            )
        ")
    endforeach()
endif()

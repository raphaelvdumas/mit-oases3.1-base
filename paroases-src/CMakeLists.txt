cmake_minimum_required (VERSION 3.0)

project(paroases LANGUAGES NONE)

if(${CMAKE_VERSION} VERSION_LESS "3.12.0")
  find_package(PythonInterp 3 REQUIRED)
  set (PYTHON_SITE "lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages")
  # sets PYTHON_EXECUTABLE
else()
  find_package(Python3 COMPONENTS Interpreter REQUIRED)
  set (PYTHON_SITE "lib/python${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}/site-packages")
  set (PYTHON_EXECUTABLE "${Python3_EXECUTABLE}")
endif()

## Check for Python modules
execute_process (COMMAND ${PYTHON_EXECUTABLE} "-c" "import numpy"
  RESULT_VARIABLE NUMPY_RES)

if (NOT ${NUMPY_RES} STREQUAL "0")
  MESSAGE (FATAL_ERROR "Python module 'numpy' is required")
endif()

execute_process (COMMAND ${PYTHON_EXECUTABLE} "-c" "import scipy"
  RESULT_VARIABLE SCIPY_RES)

if (NOT ${SCIPY_RES} STREQUAL "0")
  MESSAGE (FATAL_ERROR "Python module 'scipy' is required")
endif()

execute_process (COMMAND ${PYTHON_EXECUTABLE} "-c" "import matplotlib"
  RESULT_VARIABLE MATP_RES)

if (NOT ${MATP_RES} STREQUAL "0")
  MESSAGE (WARNING "Python module 'matplotlib' is required for some of the modules")
endif()

execute_process (COMMAND ${PYTHON_EXECUTABLE} "-c" "import tqdm"
  RESULT_VARIABLE TQDM_RES)

if (NOT ${TQDM_RES} STREQUAL "0")
  MESSAGE (WARNING "Python module 'tqdm' is recommended for progress information")
endif()


## Install the entire package into python site, the executable scripts can be
## run directly. But to be sure that the Python module is found the scripts are
## wrapped in a script that sets the PYTHONPATH before launching them.

install (DIRECTORY
  paroases
  DESTINATION ${PYTHON_SITE}
  PATTERN .gitignore EXCLUDE
  )

install (PROGRAMS
  paroasp
  paroast
  plot_oast
  DESTINATION ${PYTHON_SITE}/paroases)

# Custom source destination
install (PROGRAMS
  paroases/utils/tflipmem.py
  DESTINATION ${PYTHON_SITE}/paroases
  RENAME tflipmem
  )

# MPI duplicates
install (PROGRAMS
  paroasp
  DESTINATION ${PYTHON_SITE}/paroases
  RENAME mpioasp
  )

install (PROGRAMS
  paroast
  DESTINATION ${PYTHON_SITE}/paroases
  RENAME mpioast
  )

find_program (BASH bash)

set (wrapper_target "paroasp")
configure_file (
  wrapper.in
  paroasp
  @ONLY
  )

foreach (wrapper_target IN ITEMS
    "mpioasp" "paroasp"
    "mpioast" "paroast" "plot_oast"
    "tflipmem"
    )
  configure_file (
    wrapper.in
    ${wrapper_target}
    @ONLY
    )

  install (PROGRAMS
    ${CMAKE_CURRENT_BINARY_DIR}/${wrapper_target}
    DESTINATION bin)
endforeach(wrapper_target)

MESSAGE (STATUS "If you are unable to import the paroases python modules make sure '${CMAKE_INSTALL_PREFIX}/${PYTHON_SITE}' is in your PYTHONPATH.")


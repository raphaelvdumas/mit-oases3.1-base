#-- Create oas2d library (static)
file(GLOB LIBOAS_FILES "${CMAKE_SOURCE_DIR}/lib/oas2d/*")
add_library(oas2d STATIC ${LIBOAS_FILES})
target_include_directories(oas2d PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

#-- Create apmath2d library (static)
file(GLOB LIBAPMATH_FILES "${CMAKE_SOURCE_DIR}/lib/apmath/*")
add_library(apmath2d STATIC ${LIBAPMATH_FILES})
target_include_directories(apmath2d PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

#-- Combine oas2d apmath2d and gfortran as LIBF
set(LIBF oas2d apmath2d gfortran)

#---------------------------------------------------
# Utility executables
#---------------------------------------------------
# Define a list of source files for each executable
set(EXECUTABLES addcov nrmcov coher munkgen)

# Create all executables and link with LIBF
foreach(EXE ${EXECUTABLES})
  add_executable(${EXE} ${EXE}.f)
  target_link_libraries(${EXE} PRIVATE ${LIBF})
  target_include_directories(${EXE} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

#---------------------------------------------------
# Static libraries to executables
#---------------------------------------------------
set(CORE_LIBRARIES oast oasp oasr oasn oass oassp oasi mfp)

#-- Create 'oast_f' static library from unoast31.f (OASTL entry point)
add_library(oast_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoast31.f)

#-- Create 'oasp_f' static library from unoasp22.f (OASP entry point)
add_library(oasp_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoasp22.f)

#-- Create 'oasr_f' static library from unoasr21.f (OASRC entry point)
add_library(oasr_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoasr21.f)

#-- Create 'oasn_f' static library from unoasn22.f (OASNR entry point)
add_library(oasn_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoasn22.f)

#-- Create 'oassf_f' static library from unoass21.f (SCATFLD entry point)
add_library(oass_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoass21.f ${CMAKE_CURRENT_SOURCE_DIR}/oassun26.f) 

#-- Create 'oassp_f' static library from unoassp31.f (OASPS30 entry point)
add_library(oassp_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoassp31.f ${CMAKE_CURRENT_SOURCE_DIR}/oasvun31.f ${CMAKE_CURRENT_SOURCE_DIR}/oassun26.f)

#-- Create 'oasi_f' static library from unoasi23.f
add_library(oasi_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoasi23.f ${CMAKE_CURRENT_SOURCE_DIR}/oasoun21.f)

#-- Create 'mfp_f' static library from unoasm21.f
add_library(mfp_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoasm21.f ${CMAKE_CURRENT_SOURCE_DIR}/oasoun21.f)

#-- Create all executables from .c files and link with Fortran and LIBF
foreach(CORE_LIB ${CORE_LIBRARIES})
  add_executable(${CORE_LIB} ${CORE_LIB}.c)
  target_link_libraries(${CORE_LIB} PRIVATE ${CORE_LIB}_f ${LIBF})
  target_include_directories(${CORE_LIB} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} PRIVATE ${CMAKE_SOURCE_DIR}/include)
endforeach()

install(TARGETS ${EXECUTABLES} DESTINATION)
install(TARGETS ${CORE_LIBRARIES} DESTINATION)

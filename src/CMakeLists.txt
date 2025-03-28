include_directories(./)

file(GLOB LIBOAS_FILES "./liboas/*")
add_library(liboas STATIC ${LIBOAS_FILES})

#-- Create libapmath library (static)
file(GLOB LIBAPMATH_FILES "./libapmath/*")
add_library(libapmath STATIC ${LIBAPMATH_FILES})

#-- Combine liboas libapmath and gfortran as LIBF
set(LIBF liboas libapmath gfortran)

#---------------------------------------------------
# Utility executables
#---------------------------------------------------
# Define a list of source files for each executable
set(EXECUTABLES addcov nrmcov coher munkgen)

# Create all executables and link with LIBF
foreach(EXE ${EXECUTABLES})
  add_executable(${EXE} ${EXE}.f)
  target_link_libraries(${EXE} PRIVATE ${LIBF})
endforeach()

#---------------------------------------------------
# Static libraries to executables
#---------------------------------------------------
set(CORE_LIBRARIES oast oasp oasr oasn oass oassp oasi mfp)

#-- Create 'oast_f' static library from unoast31.f (OASTL entry point)
add_library(oast_f STATIC unoast31.f)

#-- Create 'oasp_f' static library from unoasp22.f (OASP entry point)
add_library(oasp_f STATIC unoasp22.f)

#-- Create 'oasr_f' static library from unoasr21.f (OASRC entry point)
add_library(oasr_f STATIC unoasr21.f)

#-- Create 'oasn_f' static library from unoasn22.f (OASNR entry point)
add_library(oasn_f STATIC unoasn22.f)

#-- Create 'oassf_f' static library from unoass21.f (SCATFLD entry point)
add_library(oass_f STATIC unoass21.f oassun26.f) 

#-- Create 'oassp_f' static library from unoassp31.f (OASPS30 entry point)
add_library(oassp_f STATIC unoassp31.f oasvun31.f oassun26.f)

#-- Create 'oasi_f' static library from unoasi23.f
add_library(oasi_f STATIC unoasi23.f oasoun21.f)

#-- Create 'mfp_f' static library from unoasm21.f
add_library(mfp_f STATIC unoasm21.f oasoun21.f)

#-- Create all executables from .c files and link with Fortran and LIBF
foreach(CORE_LIB ${CORE_LIBRARIES})
  add_executable(${CORE_LIB} ${CORE_LIB}.c)
  target_link_libraries(${CORE_LIB} PRIVATE ${CORE_LIB}_f ${LIBF})
endforeach()

install(TARGETS ${EXECUTABLES} DESTINATION)
install(TARGETS ${CORE_LIBRARIES} DESTINATION)

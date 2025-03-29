#-- Create oas3d library (static)
file(GLOB LIBOAS3D_FILES "${CMAKE_SOURCE_DIR}/lib/oas3d/*")
add_library(oas3d STATIC ${LIBOAS3D_FILES})
target_include_directories(oas3d PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

#-- Create apmath3d library (static)
file(GLOB LIBAPMATH_FILES "${CMAKE_SOURCE_DIR}/lib/apmath/*")
add_library(apmath3d STATIC ${LIBAPMATH_FILES})
target_include_directories(apmath3d PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

#-- Combine oas3d apmath3d and gfortran as LIB3DF
set(LIB3DF oas3d apmath3d gfortran)

#---------------------------------------------------
# Static libraries to executables
#---------------------------------------------------
set(CORE_LIBRARIES oasp3 ) # oast3 : ERROR

#-- Create 'oasp3_f' static library from unoasp39.f (OASP35 entry point)
add_library(oasp3_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoasp39.f)

# #-- Create 'oast_f' static library from unoasp35.f (OAST35 entry point)
# add_library(oast3_f STATIC ${CMAKE_CURRENT_SOURCE_DIR}/unoast35.f)

#-- Create all executables from .c files and link with Fortran and LIBF
foreach(CORE_LIB ${CORE_LIBRARIES})
  add_executable(${CORE_LIB} ${CORE_LIB}.c)
  target_link_libraries(${CORE_LIB} PRIVATE ${CORE_LIB}_f ${LIB3DF})
  target_include_directories(${CORE_LIB} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} PRIVATE ${CMAKE_SOURCE_DIR}/include)
endforeach()

#-- Install 
install(TARGETS ${CORE_LIBRARIES} DESTINATION)




include_directories(./)

#file(GLOB LIBOAS_SOURCES oas*.f) # can't use this because there are misc oas* files!
set(LIBOAS_SOURCES oasaun32.f oasbun21.f oascun31.f oasdun21.f oaseun32.f oasfun22.f oasgun21.f oashun21.f oasiun23.f oasjun21.f oaskun21.f oasmun21_bin.f oasnun22.f)


# oas becomes liboas.a -- previously called oaslib.a
add_library(oas STATIC ${LIBOAS_SOURCES} )

# oas becomes libapmath.a -- previously called apmath.a
add_library(apmath STATIC apmath21.f zmsolv.f matsubs_.c)


add_executable(oast2 unoast31.f)
add_executable(oasp2 unoasp22.f)
add_executable(oasr2 unoasr21.f)
add_executable(oasn2_bin unoasn22.f)
add_executable(oass2 unoass21.f oassun26.f)
add_executable(oassp2 unoassp31.f oasvun31.f oassun26.f)

add_executable(addcov addcov.f ${LIBOAS_SOURCES})
add_executable(nrmcov nrmcov.f ${LIBOAS_SOURCES})
add_executable(coher coher.f ${LIBOAS_SOURCES})
add_executable(munkgen munkgen.f)


target_link_libraries(oast2 oas apmath)
target_link_libraries(oasp2 oas apmath)
target_link_libraries(oasr2 oas apmath)
target_link_libraries(oasn2_bin oas apmath)
target_link_libraries(oass2 oas apmath)
target_link_libraries(oassp2 oas apmath)

target_link_libraries(addcov oas apmath)
target_link_libraries(nrmcov oas apmath)
target_link_libraries(coher oas apmath)


install(TARGETS oast2 oasp2 oasr2 oasn2_bin oass2 oassp2 addcov nrmcov coher munkgen
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


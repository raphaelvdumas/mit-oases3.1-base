include_directories(./)

add_executable(oasp3d unoasp39.f)

add_library(oas3d STATIC oasaun37.f oasbun21.f oascun31.f oasdun13.f oaseun37.f oasgun11.f oashun11.f oasiun39.f)

target_link_libraries(oasp3d oas3d apmath)

install(TARGETS oasp3d oas3d
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


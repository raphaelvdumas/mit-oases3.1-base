include_directories(./ ../core)


add_executable(oasi2_bin unoasi23.f oasoun21.f)
add_executable(mfp2_bin unoasm21.f oasoun21.f)

target_link_libraries(oasi2_bin oas apmath)
target_link_libraries(mfp2_bin oas apmath)

install(TARGETS oasi2_bin mfp2_bin
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


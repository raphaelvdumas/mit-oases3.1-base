add_executable(pps pltsplit.f atofi.c)

install(TARGETS pps
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


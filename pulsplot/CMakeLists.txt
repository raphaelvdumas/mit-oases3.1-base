
include_directories(./)

add_library(puls STATIC pulssubs.f pulsuars.f shmenu.f read3d.f gldmake.f matsubs_.c)

add_executable(pp pp.f)
add_executable(trftoascii trftoascii.f)
add_executable(trfsplit trfsplit.f)

target_link_libraries(pp puls apmath oas)

install(TARGETS puls pp trftoascii trfsplit
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


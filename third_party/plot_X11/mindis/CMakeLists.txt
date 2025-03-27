include_directories(./)

add_library(mindis STATIC mindis.f mindis1.f mindis2.f mindis3.f mindis4.f mindis6.f hershey.f outidn.f gistart.c)

add_executable(mintopost mintopost.f)
add_executable(mintops mintops.f)

# mintotek executable didn't build by default on prev system.  
# if you uncomment this, you'll have to figure out the undefined references
#add_executable(mintotek mintotek.f )
#target_link_libraries(mintotek X11)

install(TARGETS mindis mintopost mintops
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


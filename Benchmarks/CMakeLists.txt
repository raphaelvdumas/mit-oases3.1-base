
add_custom_target(all
    COMMENT "Copying plpsub.pl to bin directory"
    COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/bin/plpsub.pl ${CMAKE_BINARY_DIR}/bin/
    DEPENDS yesperl cleanall)

add_custom_target(yesperl
    COMMENT "Using perl: YES"
    COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/tloss/*.plp out.dat
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/bin/
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/bin/YES_perl/* ${CMAKE_CURRENT_BINARY_DIR}/bin/)

add_custom_target(noperl
    COMMENT "Using perl: NO"
    COMMAND rm -f ./tloss/*.plp out.dat
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/bin/
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/tloss/
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/bin/NO_perl/* ${CMAKE_CURRENT_BINARY_DIR}/bin/
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/tloss/default_plps/* ${CMAKE_CURRENT_SOURCE_DIR}/tloss)

add_custom_target(cleanall
    echo "Custom target 'cleanall' executing"
    COMMAND rm -f ./run/* ./tloss/*.pl? ./tloss/*.pl?.old ./logs/*
    COMMAND rm -f out.dat)

add_subdirectory(src)

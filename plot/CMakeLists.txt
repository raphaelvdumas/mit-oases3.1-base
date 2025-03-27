include_directories(./)

add_library(plot STATIC axdec.f rfform.f angplt.f modplt.f xaxlog.f numdec.f yaxlin.f simula.f blanks.f smootl.f vclip.f input.f extleg.f redata.f out_tl.f restore.f decode.f plotfip.f initplt.f multplt.f legend.f extn.f scissor.f proflr.f seisl.f)

add_executable(fipplot fipplot.f)

target_link_libraries(fipplot plot mindis X11)

install(TARGETS plot fipplot
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib)


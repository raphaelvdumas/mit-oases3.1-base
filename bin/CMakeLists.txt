set(OASES_SCRIPTS
    avbeam
    bemp bempo bempog bemt bemto bemtog bistat
    cplot
    mfp mplot mtvplot multmtv
    oasi oasn oasp oasp3 oasr oass oassp oassp_g oassp_old oast
    plp2mtv
    rdoasp rdoast
    snap)

install(PROGRAMS ${OASES_SCRIPTS}
        DESTINATION bin)


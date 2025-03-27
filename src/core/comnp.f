c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CC
C     ARRAYS FOR DEPTH-DEPENDENT GREEN'S FUNCTIONS
C     AND WORKING ARRAYS
C
      COMPLEX CFF(NP,NPAR)
      COMPLEX CFFS(NP)
      COMMON /STRDIS/ CFF,CFFS
      COMPLEX CBUF(NP),CFILE(ISIZE),CFILEK(isize)
      REAL ARG(NP),FAC(NP)
      COMMON /BUFF1/ ARG,FAC
      COMMON /BUFF2/ CBUF
      COMMON /BUFF3/ CFILE,CFILEK

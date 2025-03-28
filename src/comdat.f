c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C**** DEFINITION OF MAX REAL ARGUMENT TO THE EXPONENTIAL FUNCTION
      COMMON /ARGMAX/ AM
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE FPS164
CFPS  DATA AM /300./
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE VAX
      DATA AM /65./     
      DATA PROGNM /'OASTL '/
      DATA OMEGIM /0.0/
      DATA LUGRN,LUTRF,LUTGRN,LUTTRF /30,35,30,35/
      DATA SHEAR,DECOMP,SCTOUT,NFLAG,PADE,rftvty 
     &     /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA MSUFT,MBMAX,MBMAXI,SRCTYP,ISROW,ISINC /1,1,2,1,1,0/

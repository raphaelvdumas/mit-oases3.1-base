      PARAMETER (    NLTYP = 4    )
      PARAMETER (    NLEQ  = 6    )
      PARAMETER (    MOMAX = 512, MMAX  = 2*(MOMAX-1)+1 )
c      PARAMETER (    MMAX  = 5    )
      PARAMETER (    NPAR  = 4    )
      PARAMETER (    ISROWMAX = 1 )
      PARAMETER (    NLA   = 200,
     &               NLA1  = ((NLA-1)*2-1)*NLEQ,
     &               NLA2  = NLEQ*NLA+1,
     &               NLA3  = NLEQ*NLA,
     &               NLA4  = 9*NLA+3*NLA3+NLA2+NLA1+2*NLA+(MMAX-1)*NLA,
     &               NLA6  = NLA3*MMAX,
     &               NLA5  = 2*NLA1+2*NLA6,
     &               NLA3MMAX=NLA3*MMAX,
     &               NLA36 = NLA*NLEQ*NLEQ          )
      PARAMETER (    IBSI  = 4*NLEQ*NLEQ*NLA)
      PARAMETER (    NRSMAX= NLA3*MMAX*ISROWMAX  )
      PARAMETER (NDV=10,NLA10=(NDV+1)*NLA)
      PARAMETER (    NRD   = 1001   )
      PARAMETER (    NMOD  = 10   )
      PARAMETER (    NPEXP = 16,
     &               NP    = 2**NPEXP,
     &               NP2   = 2*NP,
     &               ITWONP= 2*NP,
     &               NP3   = NPAR*NP,
     &               NP4   = 4*NP,
     &               NP6   = 2*NPAR*NP,
     &               NPHP1 = NP/2+1,
     &               NPHALF= NP/2 )
      PARAMETER (    MODULO=  128 )
      PARAMETER (    ISIZE =  100000 )
      PARAMETER (    NSIP  = 101,
     &               IMXMIN= 20   )
      PARAMETER (    NSMAX = 101   ,
     &               NRMAX = 51   ,
     &               NPMAX = 5    ,
     &               NRNP  = NRMAX*NPMAX,
     &               NRNR  = NRMAX*NRMAX,
     &               NPNP  = NPMAX*NPMAX) 
      PARAMETER (    NCONM = 150  )
C
C     COMMON CONSTANTS
C
      COMPLEX AI,CNUL
      REAL PI
      COMMON /CONST/ PI,CNUL,AI
      COMMON /FREQS/ NFREQ,FREQ1,FREQ2,DLFREQ
      COMPLEX DSQ,CSQ
      COMMON /VARS1/ FREQ,DSQ,CSQ,NUMI,NUML,LS,IR,ZU,ZL,PCORR
      COMMON /VARS2/ NWVNO,ICUT1,ICUT2
      COMMON /VARS3/ DLWVNO,DLRAN,R1,FNI5,FNIFAC,LMAX,ISPACE,
     &               NPLOTS,NUMFR,IFN
      COMMON /VARS4/ WK0
      COMPLEX WVNO,S2
      COMMON /WAVENO/ WVNO,S2
      COMMON /ERRCOM/ IERR,IOERR
      COMMON /CONTUR/ OFFDB,OFFIMA,OFFDBS
      COMMON /POPT/   KPLOT,NOUT,IOUT(NPAR),IREF,ISTYP,LINA,ICDR,
     &                NDEC,NCON
C *** SOURCE DATA
      INTEGER SRCTYP
      REAL HANGLE,VANGLE,F1,F2,F3
      REAL RFOR,RMOM,ADELTA,RDELTA
      REAL TENMOM(3),TENDEL(3),ATENDEL(3)
      COMMON /BLOCKV/ SRCTYP,HANGLE,VANGLE,F1,F2,F3,ISROW,ISINC
      COMMON /BLOCV/ RFOR,RMOM,ADELTA,RDELTA,TENMOM,TENDEL,ATENDEL
C *** FOURIER ORDERS
      COMMON /DETMS/ MSUFT,MBMAX,MBMAXI
C ***
      COMMON /INTG/   INTTYP
      COMMON /DFLAG/ IDERIV
      LOGICAL SHEAR,DECOMP,SCTOUT
      COMMON /LOGFLG/ SHEAR,DECOMP,SCTOUT
      COMMON /LOGUNI/ LUGRN,LUTRF,LUTGRN,LUTTRF
      CHARACTER*6 PROGNM
      COMMON /SAFANM/ PROGNM
      COMMON /DISNRM/ DISNRM
      logical DEBUG
      COMMON /DBG/ DEBUG,IPRINT,TIMF
      COMMON /OMEGIM/ OMEGIM
      real frqdec,troff
      logical deconv,log_ts
      character decstr,logstr
      common /decnvl/ frqdec,troff,deconv,log_ts,decstr,logstr

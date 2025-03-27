c
c Parameters and common blocks
c 3-dimensional version of OASES
c
      PARAMETER (    NLTYP = 4    )
      PARAMETER (    NLEQ  = 6 ,
     &               NLEQH = NLEQ/2   )
      PARAMETER (    MMAX  = 5    )
      PARAMETER (    NBFMAX= MMAX/2+2 )
      PARAMETER (    NPAR  = 8    )
      PARAMETER (    ISROWMAX = 1 )
      PARAMETER (    NLA   = 200,
     &               NLA1  = ((NLA-1)*2-1)*NLEQ*NLEQ,
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
      PARAMETER (    NRD   = 101   )
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
      PARAMETER (    ISIZE =  10000 )
      PARAMETER (    NSIP  = 101,
     &               IMXMIN= 20   )
      PARAMETER (    NSMAX = 101   ,
     &               NRMAX = 51   ,
     &               NPMAX = 5    ,
     &               NRNP  = NRMAX*NPMAX,
     &               NRNR  = NRMAX*NRMAX,
     &               NPNP  = NPMAX*NPMAX) 
      PARAMETER (    NCONM = 150  )
c >>> Number of stresses and displacements computed in ti media
      PARAMETER (    NTIPAR = 40  )
C
C     COMMON CONSTANTS
C
      COMPLEX AI,CNUL
      REAL PI
      COMMON /CONST/ PI,CNUL,AI
      COMMON /FREQS/ NFREQ,FREQ1,FREQ2,DLFREQ
      COMPLEX DSQ,CSQ,CWUFAC,CPFAC
      COMMON /VARS1/ FREQ,DSQ,CSQ,NUMI,NUML,LS,IR,ZU,ZL,PCORR,
     &               CWUFAC,CPFAC
      COMMON /VARS2/ NWVNO,ICUT1,ICUT2,ICW1,ICW2
      COMMON /VARS3/ DLWVNO,DLRAN,R1,FNI5,FNIFAC,LMAX,ISPACE,
     &               NPLOTS,NUMFR,IFN
      COMMON /VARS4/ WK0
      COMPLEX WVNO,S2
      COMMON /WAVENO/ WVNO,S2,DETMNT
      COMMON /ERRCOM/ IERR,IOERR
      COMMON /CONTUR/ OFFDB,OFFIMA,OFFDBS
      COMMON /POPT/   KPLOT,NOUT,IOUT(NPAR),IREF,ISTYP,LINA,ICDR,
     &                NDEC,NCON
C *** SOURCE DATA
      INTEGER SRCTYP
      REAL HANGLE,VANGLE,F1,F2,F3
      REAL RFOR,RMOM,ADELTA,RDELTA
      REAL TENMOM(3),TENDEL(3),ATENDEL(3),m11,m12,m13,m22,m23,m33
      COMMON /BLOCKV/ SRCTYP,HANGLE,VANGLE,F1,F2,F3,ISROW,ISINC
      COMMON /BLOCV/ RFOR,RMOM,ADELTA,RDELTA,TENMOM,TENDEL,ATENDEL,
     &               m11,m12,m13,m22,m23,m33,
     &               DELTA,THETA,LTYP,FOCDEP
C *** FOURIER ORDERS
      COMMON /DETMS/ MSUFT,MBMAX,MBMAXI,brk_0,mbf_0
C ***
      COMMON /INTG/   INTTYP
      COMMON /DFLAG/ IDERIV

      LOGICAL         SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,bintrf,double_trf
      COMMON /LOGFLG/ SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,bintrf,double_trf
      COMMON /LOGUNI/ LUGRN,LUTRF,LUTGRN,LUTTRF
      CHARACTER*6 PROGNM
      COMMON /SAFANM/ PROGNM
      COMMON /DISNRM/ DISNRM,strnrm,akmax
      logical DEBUG
      COMMON /DBG/ DEBUG,IPRINT,TIMF
      COMMON /OMEGIM/ OMEGIM
      COMMON /PADENM/ NWSTEP,NKFIL,NPADE
c>>> declaration of truncated exponential external
      complex cexpt
c      external cexpt
c >>> parameters for roughness sources
      logical rghsou
      integer intrgh,nrgh,mrgh
      real dlkrgh
      common /sourgh/ rghsou,intrgh, nrgh, mrgh, dlkrgh


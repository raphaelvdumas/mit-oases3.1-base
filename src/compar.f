c
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c Parameters and common blocks
c 2-dimensional version of OASES
c
c >>> Porous media added 960426
c
      PARAMETER (    NLTYP = 5    )
      PARAMETER (    NLEQ  = 6,
     &               NLEQH = NLEQ/2   )
      PARAMETER (    MMAX  = 1    )
      PARAMETER (    NBFMAX= MMAX/2+2 )
      PARAMETER (    NPAR  = 7    )
      PARAMETER (    ISROWMAX = 1001 )
      PARAMETER (    NLA   = 1001,
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
      PARAMETER (    NRD   = 2001   )
      PARAMETER (    NMOD  = 10   )
      PARAMETER (    NPEXP = 20,
     &               NP    = 2**NPEXP,
     &               NP2   = 2*NP,
     &               ITWONP= 2*NP,
     &               NP3   = NPAR*NP,
     &               NP4   = 4*NP,
     &               NP6   = 2*NPAR*NP,
     &               NPHP1 = NP/2+1,
     &               NPHALF= NP/2 )
      PARAMETER (    MODULO=  128 )
      PARAMETER (    ISISQR=  1000,
     &               ISIZE =  ISISQR**2 )
      PARAMETER (    NSIP  = 101,
     &               IMXMIN= 20   )
      PARAMETER (    NSMAX = 301   ,
     &               NRMAX = 501   ,
     &               NPMAX = 5    ,
     &               NRNP  = NRMAX*NPMAX,
     &               NRNR  = NRMAX*NRMAX,
     &               NPNP  = NPMAX*NPMAX) 
      PARAMETER (    NCONM = 150  )
c >>> Number of stresses and displacements computed in ti media
      PARAMETER (    NTIPAR = 40  )
c Rougness and volume scattering parameters moved from comvol.f 051806
      integer nnkr,nnkz
c      Parameter (nnkz=512,nnkr=8192)
c      Parameter (nnkz=256,nnkr=8192)
      Parameter (nkrexp=14,nkzexp=7,nnkr=2**nkrexp,nnkz=2**nkzexp,
     &           nkrz=2*nnkr*nnkz)
C
C     COMMON CONSTANTS
C
      parameter (RNUL = 0, RONE = 1)
      COMPLEX AI,CNUL
      REAL PI
      COMMON /CONST/ PI,CNUL,AI
      COMMON /FREQS/ FREQ1,FREQ2,DLFREQ,nfreq
      COMPLEX DSQ,CSQ,CWUFAC,CPFAC
      COMMON /VARS1/ DSQ,CSQ,CWUFAC,CPFAC,PCORR,FREQ,ZU,ZL,
     &               sectl,NUMI,NUML,LS,IR
      COMMON /VARS2/ NWVNO,ICUT1,ICUT2,ICW1,ICW2
      COMMON /VARS3/ WK0,DLWVNO,DLRAN,R1,FNI5,FNIFAC,rpat,LMAX,ISPACE,
     &               NPLOTS,NUMFR,IFN
      COMPLEX WVNO,S2,onok
      COMMON /WAVENO/ WVNO,S2,onok,DETMNT
      COMMON /ERRCOM/ IERR,IOERR
      COMMON /CONTUR/ OFFDB,OFFIMA,OFFDBS
      COMMON /POPT/   KPLOT,NOUT,IOUT(NPAR),IREF,ISTYP,LINA,ICDR,
     &                NDEC,NCON
C *** SOURCE DATA
      INTEGER SRCTYP
      REAL HANGLE,VANGLE,F1,F2,F3
      REAL RFOR,RMOM,ADELTA,RDELTA
      REAL TENMOM(3),TENDEL(3),ATENDEL(3)
      COMMON /BLOCKV/ HANGLE,VANGLE,F1,F2,F3,SRCTYP,ISROW,nsrow,ISINC
      COMMON /BLOCV/ RFOR,RMOM,ADELTA,RDELTA,TENMOM,TENDEL,ATENDEL,
     &               DELTA,THETA,FOCDEP,ztilt,atilt,vsou,vrec,LTYP
C *** FOURIER ORDERS
      COMMON /DETMS/ MSUFT,MBMAX,MBMAXI,brk_0,mbf_0
C ***
      COMMON /INTG/   INTTYP
      COMMON /DFLAG/ IDERIV
      LOGICAL         SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,icerc,freerc,goff,bintrf,tablerc,
     &                doppler,calnse,trfout,rescat,rctable,bottomrc,
     &                slowrc,rdoas,outpot,db,extrap,ver_for,hor_for,
     &                dip_sou,mom_sou,double_trf,transmit,cont_45,
     &                pierson,bistat,rftvty
      COMMON /LOGFLG/ SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,icerc,freerc,goff,bintrf,tablerc,
     &                doppler,calnse,trfout,rescat,rctable,bottomrc,
     &                slowrc,rdoas,outpot,db,extrap,ver_for,hor_for,
     &                dip_sou,mom_sou,double_trf,transmit,cont_45,
     &                pierson,bistat,rftvty

      COMMON /LOGUNI/ LUGRN,LUTRF,LUTGRN,LUTTRF,ldaun
      CHARACTER*8 PROGNM
      COMMON /SAFANM/ PROGNM
      COMMON /DISNRM/ DISNRM,strnrm,akmax
      logical DEBUG,rdtest
      COMMON /DBG/ TIMF,IPRINT,DEBUG,rdtest
      COMMON /OMEGIM/ OMEGIM
      COMMON /PADENM/ NWSTEP,NKFIL,NPADE,npadat,npait
c>>> declaration of truncated exponential external
      complex cexpt,sqrtt
c >>> Version number
      parameter (version=2.1)
c
c     common variables for OASK 
c     Copied from Jaiyong's comsk.f
c
      real pcenter,splen_x,splen_y
      integer nfft_x,nfft_y,npatch,inpatch,inpatch1
      common /rpatch1/ pcenter,splen_x,splen_y,patchx1,patchx2,
     &                 patchy1,patchy2
      common /rpatch2/ nfft_x,nfft_y,npatch,inpatch,inpatch1

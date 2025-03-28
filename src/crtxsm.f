c     Program to create covariance matrix in OASES format from
c   an ensemple average covariance matrix created by the matlab script
c   est_cov.m
      program crtxsm
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
      PARAMETER (    ISROWMAX = 2 )
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
      PARAMETER (    ISISQR=  400,
     &               ISIZE =  ISISQR**2 )
      PARAMETER (    NSIP  = 101,
     &               IMXMIN= 20   )
      PARAMETER (    NSMAX = 201   ,
     &               NRMAX = 501   ,
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
     &                pierson,bistat
      COMMON /LOGFLG/ SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,icerc,freerc,goff,bintrf,tablerc,
     &                doppler,calnse,trfout,rescat,rctable,bottomrc,
     &                slowrc,rdoas,outpot,db,extrap,ver_for,hor_for,
     &                dip_sou,mom_sou,double_trf,transmit,cont_45,
     &                pierson,bistat

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
      



      complex corrfd(isize)
      character*300 est,xsmin,xsmout
      write(6,*) 'Template xsm file'
      read(5,'(a)') xsmin
      write(6,*) 'Output xsm file'
      read(5,'(a)') xsmout
      write(6,*) 'Estimated xsm file'
      read(5,'(a)') est


c
      call dasrec(ldaun)
         OPEN  ( UNIT       = 15
     -,          FILE       = xsmin
     -,          STATUS     = 'OLD'
     -,          FORM       = 'UNFORMATTED'
     -,          ACCESS     = 'DIRECT'
     -,          RECL       = 2 * ldaun         )

      call GETXSM (CORRFD,NRCV,-1,LERR)

         OPEN  ( UNIT       = 15
     -,          FILE       = xsmin
     -,          STATUS     = 'OLD'
     -,          FORM       = 'UNFORMATTED'
     -,          ACCESS     = 'DIRECT'
     -,          RECL       = 2 * ldaun         )

      ifr=1
      call GETXSM (CORRFD,NRCV, ifr,LERR)

      open(17,file=est,form='formatted',status='old')

      do i=1,nrcv
       do j=1,nrcv
        read(17,*) aa,bb
        corrfd(i+(j-1)*nrcv)=cmplx(aa,bb)
c        corrfd(i+(j-1)*nrcv)=cmplx(aa,-bb)
        write(6,*) i,j
       end do
      end do
 
         OPEN  ( UNIT       = 16
     -,          FILE       = xsmout
     -,          STATUS     = 'UNKNOWN'
     -,          FORM       = 'UNFORMATTED'
     -,          ACCESS     = 'DIRECT'
     -,          RECL       = 2 * ldaun         )


      call putxsm(corrfd,nrcv,ifr,lerr)
      end

      SUBROUTINE GETXSM (CORRFD,NRCV,IFR,LERR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c

C*******************************************************************
C*       READ CROSS SPECTRAL MATRIX FROM AN EXTERNAL FILE          *
C*   intended for use by subroutine PAREST, main routine SAFARI    *
C*                   *
C*   c.f. PUTXSM     *
C*                   *
C*  EXTERNAL FILE FORMAT :                  *
C*    This routine assumes a formatted, direct access file with    *
C*    a record length of 40 bytes. The first 2 records are ASCII   *
C*    comments, the next 8 records are various numerical header    *
C*    information, and the rest of the records contain the XSM     *
C*    in column sequential form.            *
C*                   *
C*  written by :   Bruce H Pasewark   September 30, 1986           *
C*                   *
C*******************************************************************

      PARAMETER ( LUN    = 15    )
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
      PARAMETER (    ISROWMAX = 2 )
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
      PARAMETER (    ISISQR=  400,
     &               ISIZE =  ISISQR**2 )
      PARAMETER (    NSIP  = 101,
     &               IMXMIN= 20   )
      PARAMETER (    NSMAX = 201   ,
     &               NRMAX = 501   ,
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
     &                pierson,bistat
      COMMON /LOGFLG/ SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,icerc,freerc,goff,bintrf,tablerc,
     &                doppler,calnse,trfout,rescat,rctable,bottomrc,
     &                slowrc,rdoas,outpot,db,extrap,ver_for,hor_for,
     &                dip_sou,mom_sou,double_trf,transmit,cont_45,
     &                pierson,bistat

      COMMON /LOGUNI/ LUGRN,LUTRF,LUTGRN,LUTTRF,ldaun
      CHARACTER*6 PROGNM
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
      
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SLS(4),NWS(3),SLD(4),NWD(3)
      DIMENSION CONMAX(3)
      DIMENSION SLEVF(NRD),SLEVDB(NRD)
      DIMENSION ZDN(NSMAX),XDN(NSMAX),YDN(NSMAX),DNLEVDB(NSMAX),
     -          DNLEV(NSMAX)
	logical dnmask(nsmax),dndone(nsmax),ldnlin(1000),
     &          dntlev(1000)
      COMMON /NOIPRM/ SNLEVDB,WNLEVDB,DPLEVDB,
     &                SLEVEL ,WNLEV  ,DPLEV  ,
     &                CMINN  ,CMAXN  , SLS ,
     &                CMINP  ,CMAXP  ,SLD , DPSD, CMIND  ,CMAXD  ,
     &                SLOW1D ,SLOW2D ,CONMAX , SLEVF  ,SLEVDB  ,
     &                ZDN    ,XDN    ,YDN    ,DNLEVDB ,DNLEV , 
     &                NDNS,NWVNON , NWS , NWVNOP , NWD , NWVNOD , 
     &                ICUT1D , ICUT2D ,
     &		      dntlev,dnmask,dndone,ldnlin
	




      CHARACTER*36 NOTE1, NOTE2
      character*40 title
      CHARACTER*80 XSMFILE
      INTEGER SRTREC, ENDREC
      INTEGER ENSEM
      INTEGER LERR
      INTEGER SRTPUT, SRTGET

      REAL SRTFRQ, ENDFRQ, DELFRQ

      COMPLEX CORRFD (1)

      COMMON /GETPUT/ SRTPUT,SRTGET
c      COMMON /FREQS/  NFREQ, SRTFRQ, ENDFRQ, DELFRQ
       EQUIVALENCE (SRTFRQ,FREQ1),(ENDFRQ,FREQ2),(DELFRQ,DLFREQ)
C**********************************************************************
C*****  DO OPERATIONS ONLY REQUIRED FIRST TIME SUBROUTINE CALLED

      IF (SRTGET.EQ.0)  THEN

C        *****  OPEN XSM FILE...Note that the logical unit number must 
C        be assigned a filename external to the program
C        *****  READ HEADER

         read (LUN,REC=1) TITLE(1:8)
         read (LUN,REC=2) TITLE(9:16)
         read (LUN,REC=3) TITLE(17:24)
         read (LUN,REC=4) TITLE(25:32)
         read (LUN,REC=5) NDIM, NFRQ
         write(6,*) ndim,nfrq
         read (LUN,REC=6) ENSEM, izero
         read (LUN,REC=7) SRTFRQ, ENDFRQ
         write(6,*) srtfrq,endfrq
         read (LUN,REC=8) DELFRQ, ZERO
         read (LUN,REC=9) SNLEVDB,WNLEVDB

         if (ifr.lt.0) then
          nrcv=ndim
          nfreq=nfrq
          freq1=srtfrq
          freq2=endfrq
          close(15)
          return
         end if

         IF (NDIM.NE.NRCV) THEN
           STOP '>>>>>GETXSM: Number of receivers does not match'
         END IF
         IF (NFRQ.NE.NFREQ) THEN
           STOP '>>>>>GETXSM: Number of frequencies does not match'
         END IF
         IF ((ABS(FREQ1-SRTFRQ).GT.1E-4*SRTFRQ).OR.
     &       (ABS(FREQ2-ENDFRQ).GT.1E-4*ENDFRQ)    ) THEN
           STOP '>>>>>GETREP: Frequency interval discrepancy'
         END IF

C        *****  WRITE HEADER

         WRITE (6,200) title
         WRITE (6,210) NDIM, NFREQ
         WRITE (6,215) FREQ1, FREQ2, DLFREQ
         WRITE (6,220) ENSEM
         write (6,225) snlevdb,wnlevdb
C        *****  RESET FLAG
         SRTGET = 99


C        *****  IF IFR=0, ONLY THE HEADER IS REQUESTED...RETURN
         IF (IFR.EQ.0) RETURN

      END IF

      WRITE (6,205)

C     *****  TEST THAT NUMBER OF FREQUENCIES HAS NOT BEEN EXCEEDED
      IF (IFR.GT.NFREQ) THEN
         WRITE (6,*) '>>>>> XSM: Too Many Frequencies Requested'
         WRITE (6,*) '>>>>>      NO READ OPERATION PERFORMED'
         RETURN
      END IF

C*****  INITIALIZE RECORD INFORMATION

      SRTREC = 11 + (IFR-1)*NRCV*NRCV
      ENDREC = SRTREC + NRCV*NRCV - 1
      write(6,*) 'Rec.',srtrec,' - ',endrec
C*****  READ XSM

      DO 20 IREC=SRTREC,ENDREC
         READ (LUN,REC=IREC) CORRFD(IREC-SRTREC+1)
c          write(6,*) 'rec.',irec
20    CONTINUE

      RETURN

C*****  FORMATS

100   FORMAT (A)
110   FORMAT (5(1X,I6))
115   FORMAT (4(1X,F8.3))
130   FORMAT (2(2X,E16.9))
131   FORMAT (A)

200   FORMAT (//1X,'>>>>> READING XSM FROM EXTERNAL FILE'
     -        //1X,A /1X,A )

205   FORMAT ( /1X,'>>>>> READING XSM FROM EXTERNAL FILE'/)

210   FORMAT (//1X,'Cross Spectral Matrix Dimension = ',I5
     -         /1X,'Number of Frequency Lines       = ',I5)

215   FORMAT ( /1X,'       Starting Frequency       = ',F8.3,' Hz'
     -         /1X,'       Ending Frequency         = ',F8.3,' Hz'
     -         /1X,'       Delta Frequency          = ',F8.3,' Hz')

220   FORMAT ( /1X,'XSM Ensemble Size               = ',I5/)
 225  FORMAT (//1X,'Surface source level            = ',F5.1
     -         /1X,'White noise level               = ',F5.1)


      END

      SUBROUTINE PUTXSM (CORRFD,NRCV,IFR,LERR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c

C*******************************************************************
C*        WRITE CROSS SPECTRAL MATRIX TO AN EXTERNAL FILE          *
C*   intended for use by subroutine PAREST, main routine SAFARI    *
C*                   *
C*  c.f. GETXSM      *
C*                   *
C*  EXTERNAL FILE FORMAT :                  *
C*                   *
C*   This routine produces a formatted, direct access file with a  *
C*   fixed record length of 40 bytes. The first 2 records are      *
C*   produced from the TITLE passed through COMMON, the next 8     *
C*   records are various numerical header information, and the     *
C*   rest of the records contain the complex XSM in column         *
C*   sequential form. If multiple source level option is in        *
C*   effect, the XSM for each source level is written to the same  *
C*   records, therefore only data for the last source level will   *
C*   exist on this file.                    *
C*                   *
C*   Note: Each record in the direct access file produced by this  *
C*   subroutine is terminated by a "|". This is necessary for      *
C*   proper file transfer between the VAX and FPS 164 Array        *
C*   Processor (apparently the FPS strips away trailing blanks     *
C*   in transfer, destroying the fixed record format).             *
C*                   *
C*  written by :   Bruce H Pasewark   September 30, 1986           *
C*                   *
C*******************************************************************

      PARAMETER ( LUN    = 16    )

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
      PARAMETER (    ISROWMAX = 2 )
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
      PARAMETER (    ISISQR=  400,
     &               ISIZE =  ISISQR**2 )
      PARAMETER (    NSIP  = 101,
     &               IMXMIN= 20   )
      PARAMETER (    NSMAX = 201   ,
     &               NRMAX = 501   ,
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
     &                pierson,bistat
      COMMON /LOGFLG/ SHEAR,DECOMP,SCTOUT,DETERM,NFLAG,PADE,extlar,
     &                trfsou,tilt,icerc,freerc,goff,bintrf,tablerc,
     &                doppler,calnse,trfout,rescat,rctable,bottomrc,
     &                slowrc,rdoas,outpot,db,extrap,ver_for,hor_for,
     &                dip_sou,mom_sou,double_trf,transmit,cont_45,
     &                pierson,bistat

      COMMON /LOGUNI/ LUGRN,LUTRF,LUTGRN,LUTTRF,ldaun
      CHARACTER*6 PROGNM
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
      
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SLS(4),NWS(3),SLD(4),NWD(3)
      DIMENSION CONMAX(3)
      DIMENSION SLEVF(NRD),SLEVDB(NRD)
      DIMENSION ZDN(NSMAX),XDN(NSMAX),YDN(NSMAX),DNLEVDB(NSMAX),
     -          DNLEV(NSMAX)
	logical dnmask(nsmax),dndone(nsmax),ldnlin(1000),
     &          dntlev(1000)
      COMMON /NOIPRM/ SNLEVDB,WNLEVDB,DPLEVDB,
     &                SLEVEL ,WNLEV  ,DPLEV  ,
     &                CMINN  ,CMAXN  , SLS ,
     &                CMINP  ,CMAXP  ,SLD , DPSD, CMIND  ,CMAXD  ,
     &                SLOW1D ,SLOW2D ,CONMAX , SLEVF  ,SLEVDB  ,
     &                ZDN    ,XDN    ,YDN    ,DNLEVDB ,DNLEV , 
     &                NDNS,NWVNON , NWS , NWVNOP , NWD , NWVNOD , 
     &                ICUT1D , ICUT2D ,
     &		      dntlev,dnmask,dndone,ldnlin
	




      CHARACTER*36 NOTE1, NOTE2
      CHARACTER*80  TITLE
      CHARACTER*80 XSMFILE
      INTEGER SRTREC, ENDREC
      INTEGER ENSEM
      INTEGER SRTPUT, SRTGET
      INTEGER IZERO

      COMPLEX CORRFD (1)


      COMMON /RTITLE/ TITLE
      COMMON /GETPUT/ SRTPUT, SRTGET
c      COMMON /FREQS/  NFREQ, SRTFRQ, ENDFRQ, DELFRQ
      EQUIVALENCE (SRTFRQ,FREQ1),(ENDFRQ,FREQ2),(DELFRQ,DLFREQ)
C**********************************************************************
C*****  DO OPERATIONS ONLY REQUIRED FIRST TIME SUBROUTINE CALLED

      IF (SRTPUT.EQ.0)  THEN

C        *****  OPEN XSM FILE...Note that the logical unit number must 
C        be assigned a filename external to the program
c
c Direct access file record length
c
C        *****  INITIALIZE HEADER INFORMATION

C         NOTE1( 1:16) = TITLE( 1)//TITLE( 2)//TITLE( 3)//TITLE( 4)
C         NOTE1(17:32) = TITLE( 5)//TITLE( 6)//TITLE( 7)//TITLE( 8)
C         NOTE1(33:36) = TITLE( 9)
C         NOTE2( 1:16) = TITLE(10)//TITLE(11)//TITLE(12)//TITLE(13)
C         NOTE2(17:32) = TITLE(14)//TITLE(15)//TITLE(16)//TITLE(17)
C         NOTE2(33:36) = TITLE(18) 

         ENSEM = 1
         IZERO = 0

         ZERO =0.0

C        *****  WRITE HEADER

         WRITE (LUN,REC=1) TITLE(1:8)
         WRITE (LUN,REC=2) TITLE(9:16)
         WRITE (LUN,REC=3) TITLE(17:24)
         WRITE (LUN,REC=4) TITLE(25:32)
         WRITE (LUN,REC=5) NRCV, NFREQ
         WRITE (LUN,REC=6) ENSEM, IZERO
         WRITE (LUN,REC=7) FREQ1, FREQ2
         WRITE (LUN,REC=8) DLFREQ, ZERO
         WRITE (LUN,REC=9) SNLEVDB, WNLEVDB

C        *****  BLANK FILL NEXT RECORD FOR FUTURE USE
         WRITE (LUN,REC=10) ZERO,ZERO

C        *****  ECHO HEADER TO TERMINAL

         WRITE (6,'(A)') TITLE
         WRITE (6,210) NRCV, NFREQ
         WRITE (6,215) SRTFRQ, ENDFRQ, DELFRQ
         WRITE (6,220) ENSEM

C        *****  RESET FLAG
         SRTPUT = 99
      END IF

C*****  INITIALIZE RECORD INFORMATION

      SRTREC = 11 + (IFR-1)*NRCV*NRCV
      ENDREC = SRTREC + NRCV*NRCV - 1
      write(6,*) 'Rec.',srtrec,' - ',endrec

C*****  WRITE XSM

      WRITE (6,205)

      DO 20 IREC=SRTREC,ENDREC
         WRITE (LUN,REC=IREC) CORRFD(IREC-SRTREC+1)
C          write(6,*) 'rec.',irec
20    CONTINUE

      RETURN

C*****  FORMATS

100   FORMAT (A           ,'   |')
110   FORMAT (5(1X,I6)   ,'    |')
115   FORMAT (4(1X,F8.3)  ,'   |')
120   FORMAT (36(' ')     ,'   |')
130   FORMAT (2(2X,E16.9) ,'   |')

200   FORMAT (//1X,'>>>>> WRITING XSM TO EXTERNAL FILE'
     -        //1X,A /1X,A )

205   FORMAT ( /1X,'>>>>> WRITING XSM TO EXTERNAL FILE'/)

210   FORMAT (//1X,'Cross Spectral Matrix Dimension = ',I5
     -         /1X,'Number of Frequency Lines       = ',I5)

215   FORMAT ( /1X,'       Starting Frequency       = ',F8.3,' Hz'
     -         /1X,'       Ending Frequency         = ',F8.3,' Hz'
     -         /1X,'       Delta Frequency          = ',F8.3,' Hz')

220   FORMAT ( /1X,'XSM Ensemble Size               = ',I5/)

      END 

c
c The following routines are used for determining record length allocation 
c for direct access files.
C
C     NAME     : DURLM
C     PURPOSE  : DYNAMIC UNIT RECORD LENGTH MEASUREMENT SUBROUTINES.
C     AUTHOR   : Austin J Lee
C     SYNTAX   : STRICT ANSI FORTRAN 77 
C     USAGE    :
C            CALL SREC(L) ! single precision real variable
C            "L" is retrun integer variable which has 
C            unit record length for corresponding variable.
C
      SUBROUTINE DASREC(L)
      INTEGER L,LU
      REAL V(2)
      LOGICAL Q
      CALL SOPEND(LU,2)
      WRITE(UNIT=LU,REC=1,ERR=100) V(1),V(2)
      CLOSE(LU)
      L=1
      RETURN
 100  INQUIRE(UNIT=LU,OPENED=Q)
      IF (Q) CLOSE(LU)
      CALL SOPEND(LU,4)
      WRITE(UNIT=LU,REC=1,ERR=200) V(1),V(2)
      CLOSE(LU)
      L=2
      RETURN
 200  INQUIRE(UNIT=LU,OPENED=Q)
      IF (Q) CLOSE(LU)
      CALL SOPEND(LU,8)
      WRITE(UNIT=LU,REC=1,ERR=300) V(1),V(2)
      CLOSE(LU)
      L=4
      RETURN
 300  INQUIRE(UNIT=LU,OPENED=Q)
      IF (Q) CLOSE(LU)
      L=8
      RETURN
      END
C
      SUBROUTINE SOPEND(LU,LVEC)
      INTEGER LU,LVEC,LUFREE
      LU = LUFREE()
      OPEN(UNIT=LU,FORM='UNFORMATTED',STATUS='SCRATCH',ACCESS='DIRECT',
     &     RECL=LVEC,ERR=100)
      RETURN
 100  CALL DA_ERRMSG('SOPEND: FILE OPEN ERROR',6)
      END
C
      FUNCTION LUFREE()
      INTEGER LUFREE,I
      LOGICAL QOPEN
      DO 10 I=99,10,-1
         INQUIRE(UNIT=I,OPENED=QOPEN)
         IF (.NOT.QOPEN) THEN
            LUFREE = I
            RETURN
         END IF
 10   CONTINUE
      LUFREE = -1
      CALL DA_ERRMSG('lufree : no availible logical unit number',6)
      RETURN
      END
C
      SUBROUTINE DA_ERRMSG(A,LU)
      CHARACTER*(*) A
      INTEGER LU
      WRITE(LU,*) A
      STOP
      END


      


      

      SUBROUTINE RFLD(NXSRC,NYSRC,ROTFLG,INTERP)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C
C     HS 27-AUG-87   EXTRACTED FROM ORIGINAL SUBROUTINE RFIELD
C                    AND RANGE LOOP MOVED INSIDE. 
C                    CRARAO FIELD MOVED TO SUBROUTINE CRFLD
C     HS  3-DEC-88   INTERPOLATION ANF FFT INTEGRATION ADDED
C
      PARAMETER (ITUN=39)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      INCLUDE 'srccoo.f'
      LOGICAL ROTFLG,INTERP
      COMPLEX CC
      COMPLEX CARG,FILON
      COMPLEX CPXU
      COMPLEX CINTPL

      REAL FFS(2,NP)

      EQUIVALENCE (CFFS(1),FFS(1,1))
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)

      CHARACTER*6 CRTYP(4)
      CHARACTER*10 TYP(3)

      DATA TYP /'PRESSURE  ','VERT.PART.','HOR.PART. '/
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/

C
C     DETERMINE HOW MANY LOOPS ARE NEEDED
C
      NXPRLP=ISIZE/(NRCV*NYSRC)
      IF (NRCV.GT.ISIZE.OR.NXPRLP.LT.1) THEN
        STOP '*** BUFFER CFILE TOO SMALL IN RFLD***'
      END IF
      
C
      TERM=PI*0.25E0
C *** SAVE NUMBER OF WAVE NUMBERS
      NWVNSV=NWVNO
      R1SV=R1
      DLRSV=DLRAN

C *** GET GREEN'S FUNCTIONS FOR FIRST RECEIVER DEPTH
C *** IF INTERPOLATION WAS CHOSEN, DO FFT INTEGRATION

      IF (INTERP) THEN
c ***  DETERMINE FFT SIZE
       WKMAX=WK0+(NWVNSV-1)*DLWVNO
       NN=8*WKMAX/DLWVNO
       NWVNO=1
 10    NWVNO=NWVNO*2
       IF (NWVNO.LT.NN) GO TO 10
       write(6,*) 'Interpolation FFT size NWVNO:',NWVNO
       IF (NWVNO.GT.NP*4) THEN
        WRITE(6,*) '>>> Warning: NWVNO too LARGE for INTERPOLATION'
        WRITE(6,*) '>>>          NP required:',NWVNO/4
        WRITE(6,*) '>>>      *** Interpolation DISABLED ***'
        INTERP=.FALSE.
       END IF  
      END IF
      IF (INTERP) THEN
        DLRNEW=2*PI/(NWVNO*DLWVNO)
        WRITE(6,*) 'Interpolation RANGE increment:',DLRNEW
        R1=DLRNEW
        DLRAN=DLRNEW
        IF (DEBUG) WRITE(6,*) 'R1,DR:',R1,DLRAN
        ONODLR=1E0/DLRNEW
        IRDCUR=IDEP(1)
        IRPCUR=IRCPAR(1)
        CALL GETSGF(LUGRN,IRDCUR,IRPCUR)
C *** FILL CBUF WITH PHASE FACTORS
        IF (DEBUG) WRITE(6,*) 'CALLING FLCBUF'
        CALL FLCBUF
C *** AND INTEGRATE BY MEANS OF FFT
        IF (DEBUG) WRITE(6,*) 'CALLING FFTINT'
        CALL FFTINT
      ELSE
        CALL GETGFC(LUGRN,IDEP(1),IPL1,IPL2)
        IRDCUR=1
      END IF

      DO 51 IXP=1,NXSRC,NXPRLP
      NXM=MIN(NXSRC-IXP+1,NXPRLP)

      DO 50 IRCV=1,NRCV
      if (IPRINT.NE.0) WRITE(6,*) 'IXP,IRCV:',IXP,IRCV
C *** IF NEW DEPTH GET GREEN'S FUNCTIONS
      IF (INTERP) THEN
       IF (IDEP(IRCV).NE.IRDCUR.OR.IRCPAR(IRCV).NE.IRPCUR) THEN
        IRDCUR=IDEP(IRCV)
        IRPCUR=IRCPAR(IRCV)
        CALL GETSGF(LUGRN,IRDCUR,IRPCUR)
        CALL FFTINT
       END IF
      ELSE
       IF (IDEP(IRCV).NE.IRDCUR) THEN
        CALL GETGFC(LUGRN,IDEP(IRCV),IPL1,IPL2)
        IRDCUR=IDEP(IRCV)
       END IF
      END IF
       DO 49 IXM=1,NXM
        IXSRC=IXP+IXM-1
         DO 48 IYSRC=1,NYSRC 
C
          RANGEM=SQRT((RAN(IRCV)-XSC(IXSRC))**2+
     1                (TRAN(IRCV)-YSC(IYSRC))**2)
          RANGEM=MAX(RANGEM,1E-3)
C
C     REPLICA FIELDS
C
        IF (INTERP) THEN
C *** INTERPOLATE IN FFT INTEGRALS
          IF (DEBUG) WRITE(6,*) 'CALLING CINTPL'
          CPXU=CINTPL(CFF,R1,ONODLR,NWVNO,RANGEM)
        ELSE
C *** DIRECT FILON INTEGRATION
          RST=TERM-WK0*RANGEM
          RSTP=-DLWVNO*RANGEM
          CALL VRAMP(RST,RSTP,ARG,1,NWVNO)
          CALL CVEXP(ARG,1,CBUF,2,NWVNO)
          RST=EXP(OFFIMA*RANGEM)
          CALL VSMUL(CBUF,1,RST,CBUF,1,2*NWVNO)
          FCI=GAIN(IRCV)*FNI5/SQRT(RANGEM)
          CPXU = FILON(CFF(1,IRCPAR(IRCV)),CBUF(1),ARG(1),NWVNO,FCI)
        END IF
          IF (IRTYP(IRCV).EQ.2) THEN
           CO = (XSC(IXSRC)-RAN(IRCV))/RANGEM
c      Multiplication of horizontal kernels by i added 04-18-2013
           CPXU=-CO*CPXU*ai
          ELSE IF (IRTYP(IRCV).EQ.3) THEN
           SI = (YSC(IYSRC)-TRAN(IRCV))/RANGEM
           CPXU=-SI*CPXU*ai
          ELSE
          END IF
          CFILE(IRCV+(IYSRC-1)*NRCV+(IXM-1)*NRCV*NYSRC)=CPXU
 48       CONTINUE
 49     CONTINUE
 50   CONTINUE
      DO 52 IXM=1,NXM
       DO 52 IYSRC=1,NYSRC
c        CALL WRBUF(31,CFILE(1+NRCV*(IYSRC-1+(IXM-1)*NYSRC)),2*NRCV)
        IF (ROTFLG) THEN
         CALL PUTRDA(CFILE(1+NRCV*(IYSRC-1+(IXM-1)*NYSRC)))
        END IF
c        if (debug) then
         do ircv=1,nrcv
          ii=ircv+NRCV*(IYSRC-1+(IXM-1)*NYSRC)
          write(92,*) ircv,real(cfile(ii)),rimag(cfile(ii))
         end do
c        end if
 52    CONTINUE
 51   CONTINUE
    
C *** RESTORE WAVENUMBER AND RANGE PARAMETERS
      NWVNO=NWVNSV
      R1=R1SV
      DLRAN=DLRSV

 100  FORMAT(1H ,I5,2(1X,G13.6),1X,F8.1,1X,A2,3X,A6)
      RETURN
      END
      SUBROUTINE GETSGF(IFILE,IDEP1,IPAR1)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      COMPLEX CBUFL(NRD),facsq
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      CHARACTER*6 CRTYP(4)
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/
C *** CLEAR WHOLE ARRAY
      CALL VCLR(CFF,1,2*NWVNO)
      CALL RWDBUF(IFILE)
      DO 4 JR=ICUT1,ICUT2
       if (icdr.eq.0) then
        facsq=sqrt(cmplx(wk0+(jr-1)*dlwvno,offima))
       else
        facsq=1e0
       end if
      DO 3 I=1,npar
      IF (IOUT(I).EQ.1) THEN
       CALL RDBUF(IFILE,CBUFL,2*IR)
       IF (IPAR1.EQ.I) THEN
        CFF(JR,1)=facsq*CBUFL(IDEP1)
       END IF
      END IF
 3    CONTINUE
 4    CONTINUE
C
C
      IPL1=1
      IPL2=NWVNO
      IF (ICUT1.GT.2.OR.ICUT2.LT.NWVNO) THEN
       IPL1=NWVNO
       IPL2=0
       CALL CHERMIT(CFF(1,1),NWVNO,ICUT1,ICUT2,DLWVNO,
     1              WK0,IPL1,IPL2)
       END IF
      RETURN
      END
      SUBROUTINE FLCBUF
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      NN=MIN(ICUT2+100,NP)
      TERM=PI/4E0
      RSTP=-DLWVNO*R1
      CALL VRAMP(TERM,RSTP,ARG,1,NN)
      CALL CVEXP(ARG,1,CBUF,2,NN)
      RETURN
      END
      SUBROUTINE FFTINT
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      COMPLEX CX(NP4)
      EQUIVALENCE (CX(1),CFF(1,1))
      NN=MIN(ICUT2+100,NP)
      CALL CVMUL(CFF,2,CBUF,2,CFF,2,NN,1)
      CALL CFFT(CFF,NWVNO,1)
C *** MULTIPLY BY RANGE FACTORS
      DO 10 J=1,NWVNO
       RANGEM=R1+(J-1)*DLRAN
       RR=OFFIMA*RANGEM
       RI=-RANGEM*WK0
       IF (DEBUG) WRITE(6,*) 'J,R,RR,RI:',J,RANGEM,RR,RI
       CX(J)=CX(J)*FNI5*EXP(CMPLX(RR,RI))/SQRT(RANGEM)
 10   CONTINUE
      RETURN
      END   
      SUBROUTINE PUTXSM (CORRFD,MRCV,IFR,LERR)
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
      include 'compar.f'
      include 'noiprm.f'
      INCLUDE 'recarr.f'

      CHARACTER*36 NOTE1, NOTE2
      CHARACTER*80  TITLE
      CHARACTER*80 XSMFILE
      INTEGER SRTREC, ENDREC
      INTEGER ENSEM
      INTEGER SRTPUT, SRTGET
      INTEGER IZERO
      integer idim(3)
      integer ndim
      character*80 matfile
      integer padflag, rem

      COMPLEX CORRFD (1)
      real rtyp(NRMAX)


      COMMON /RTITLE/ TITLE
      COMMON /GETPUT/ SRTPUT, SRTGET
c      COMMON /FREQS/  NFREQ, SRTFRQ, ENDFRQ, DELFRQ
      EQUIVALENCE (SRTFRQ,FREQ1),(ENDFRQ,FREQ2),(DELFRQ,DLFREQ)
C**********************************************************************
C*****  DO OPERATIONS ONLY REQUIRED FIRST TIME SUBROUTINE CALLED

      IF (SRTPUT.EQ.0)  THEN

C        *****  OPEN XSM FILE...Note that the logical unit number must 
C        be assigned a filename external to the program
         call getenv('FOR016',XSMFILE)
         if (xsmfile(1:1).eq.' ') xsmfile='fort.16'
c
c Direct access file record length
c
      call dasrec(ldaun)
         OPEN  ( UNIT       = LUN
     -,          FILE       = XSMFILE
     -,          STATUS     = 'UNKNOWN'
     -,          FORM       = 'UNFORMATTED'
     -,          ACCESS     = 'DIRECT'
     -,          RECL       = 2 * ldaun         )

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
         WRITE (LUN,REC=5) MRCV, NFREQ
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

c
c Write xsm for matlab
c
      write(matfile,'(a,i3.3,a)') 'xsm_frq_',ifr,'.mat'
      call matopen(matfile,15)
      frq= freq1 + (ifr-1)*dlfreq
      ndim=2
      idim(1)=1
      idim(2)=1
      call matsetup(ndim,idim,padflag,4,"FREQ")
      call matwrite(frq,1,padflag)
      ndim=2
      idim(1)=1
      idim(2)=nrcv
      call matsetup(ndim,idim,padflag,7,"ARRAY_X")
      call matwrite(ran(1),nrcv,padflag)
      call matsetup(ndim,idim,padflag,7,"ARRAY_Y")
      call matwrite(tran(1),nrcv,padflag)
      call matsetup(ndim,idim,padflag,7,"ARRAY_Z")
      call matwrite(dep(1),nrcv,padflag)
      do ircv = 1,nrcv
        rtyp(ircv) = irtyp(ircv)
      end do
      call matsetup(ndim,idim,padflag,4,"TYPE")
      call matwrite(rtyp(1),nrcv,padflag)
      call matsetup(ndim,idim,padflag,4,"GAIN")
      call matwrite(gain(1),nrcv,padflag)
      ndim=2
      idim(1)=nrcv
      idim(2)=nrcv*2
      call matsetup(ndim,idim,padflag,4,"DATA")
      do ircv=1,nrcv-1
        call matwrite(corrfd(1+(ircv-1)*nrcv),2*nrcv,padflag)
      end do
      call matwrite(corrfd(1+(nrcv-1)*nrcv),2*nrcv,0)

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
      SUBROUTINE PUTREP(CFILE) 
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
C*        WRITE REPLICA FILE HEADER   TO AN EXTERNAL FILE          *
C*            intended for use by main routine OASNR               *
C*                   						   *
C*  EXTERNAL FILE FORMAT : 			                   *
C*                   						   *
C*   This routine produces a formatted, sequencial file with a     *
C*   variable record length, LUN = 14. The first 2 records are     *
C*   produced from the TITLE passed through COMMON, the next 8     *
C*   records are various numerical header information, and the     *
C*   rest of the records contain the complex replica in column     *
C*   sequential form.						   *
C*                   						   *
C*  written by :   H Schmidt   October 10, 1986                    *
C*                   						   *
C*******************************************************************

      PARAMETER ( LUN= 14 )
      INCLUDE 'compar.f'
      INCLUDE 'recarr.f'

      CHARACTER*36 NOTE1, NOTE2
      CHARACTER*80  TITLE

      INTEGER NRCV,NFREQ
      INTEGER SRTREC, ENDREC
      INTEGER ENSEM
      INTEGER SRTPUT, SRTGET
      INTEGER IZERO

      REAL SRTFRQ, ENDFRQ, DELFRQ
      REAL ZERO

      COMPLEX CFILE(1)


      COMMON /RTITLE/ TITLE
      COMMON /GETPUT/ SRTPUT, SRTGET
      COMMON /REPLIC/ ZSMIN,ZSMAX,XSMIN,XSMAX,YSMIN,YSMAX,
     1                NSRCZ,NSRCX,NSRCY
      EQUIVALENCE (SRTFRQ,FREQ1),(ENDFRQ,FREQ2),(DELFRQ,DLFREQ)
C**********************************************************************
C*****  DO OPERATIONS ONLY REQUIRED FIRST TIME SUBROUTINE CALLED

C        *****  OPEN REPO FILE...Note that the logical unit number must 
C        be assigned a filename external to the program
         CALL OPFILB(14,IOER)
C         OPEN  ( UNIT       = LUN
C     -,          FILE       = 'FOR014.DAT'
C     -,          STATUS     = 'UNKNOWN'
C     -,          FORM       = 'FORMATTED' )

C        *****  INITIALIZE HEADER INFORMATION

c         NOTE1( 1:16) = TITLE( 1)//TITLE( 2)//TITLE( 3)//TITLE( 4)
c         NOTE1(17:32) = TITLE( 5)//TITLE( 6)//TITLE( 7)//TITLE( 8)
c         NOTE1(33:36) = TITLE( 9)
c         NOTE2( 1:16) = TITLE(10)//TITLE(11)//TITLE(12)//TITLE(13)
c         NOTE2(17:32) = TITLE(14)//TITLE(15)//TITLE(16)//TITLE(17)
c         NOTE2(33:36) = TITLE(18) 

         IZERO = 0

         ZERO =0.0

C        *****  WRITE HEADER

         WRITE (LUN) title
         WRITE (LUN) NRCV, NFREQ
         WRITE (LUN) SRTFRQ, ENDFRQ, DELFRQ

            WRITE (LUN) ZSMIN,ZSMAX,NSRCZ
            WRITE (LUN) XSMIN,XSMAX,NSRCX
            WRITE (LUN) YSMIN,YSMAX,NSRCY
C*****   WRITE ARRAY DATA TO FILE
         DO 10 IREC=1,NRCV
            WRITE(LUN) RAN(IREC),TRAN(IREC),DEP(IREC),IRTYP(IREC),
     &                     GAIN(IREC)
 10      CONTINUE
C        *****  ECHO HEADER TO TERMINAL

         WRITE (6,200) title
         WRITE (6,210) NRCV, NFREQ
         WRITE (6,215) SRTFRQ, ENDFRQ, DELFRQ

C*****  END WRITING HEADER
        RETURN

      ENTRY PUTRDA(CFILE)

      DO 20 IREC=1,NRCV
         WRITE (LUN) CFILE(IREC)
20    CONTINUE

      RETURN

C*****  FORMATS

100   FORMAT (A)
110   FORMAT (5(1X,I6)   )
112   FORMAT (2(1X,F12.3),1X,I5)
115   FORMAT (4(1X,F8.3) )
120   FORMAT (3(1X,F8.2),1X,I3,1X,E13.6)
130   FORMAT (2(2X,E16.9) )

200   FORMAT (//1X,'>>>>> WRITING REP TO EXTERNAL FILE'
     -        //1X,A /1X,A )

210   FORMAT (//1X,'Replica Dimension               = ',I5
     -         /1X,'Number of Frequency Lines       = ',I5)

215   FORMAT ( /1X,'       Starting Frequency       = ',F8.3,' Hz'
     -         /1X,'       Ending Frequency         = ',F8.3,' Hz'
     -         /1X,'       Delta Frequency          = ',F8.3,' Hz')

      END 
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
      include 'compar.f'
      include 'noiprm.f'

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
         call getenv('FOR015',XSMFILE)
         if (xsmfile(1:1).eq.' ') xsmfile='fort.15'
      call dasrec(ldaun)
         OPEN  ( UNIT       = LUN
     -,          FILE       = XSMFILE
     -,          STATUS     = 'OLD'
     -,          FORM       = 'UNFORMATTED'
     -,          ACCESS     = 'DIRECT'
     -,          RECL       = 2 * ldaun         )

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

      SUBROUTINE GETREP(CFILE) 
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
C*        READ REPLICA FILE HEADER FROM AN EXTERNAL FILE          *
C*            intended for use by main routine SAFARI              *
C*                   *
C*  c.f. PUTREP      *
C*                   *
C*  EXTERNAL FILE FORMAT :                  *
C*                   *
C*   This routine reads a formatted, sequencial file with a        *
C*   variable record length.          The first 2 records are      *
C*   produced from the TITLE passed through COMMON, the next 8     *
C*   records are various numerical header information, and the     *
C*   rest of the records contain the complex replica in column     *
C*   sequential form.*
C*                   *
C*  written by :   H Schmidt   October 10, 1986                    *
C*                   *
C*******************************************************************

      PARAMETER ( LUN= 13 )
      INCLUDE 'compar.f'
      INCLUDE 'recarr.f'

      CHARACTER*36 NOTE1, NOTE2
      CHARACTER*80  TITLE

      INTEGER NRCV,NFREQ
      INTEGER SRTREC, ENDREC
      INTEGER ENSEM
      INTEGER IFR, LERR
      INTEGER SRTPUT, SRTGET
      INTEGER IZERO

      REAL SRTFRQ, ENDFRQ, DELFRQ
      REAL ZERO

      DIMENSION BUF(5),IBUF(5)

      COMPLEX CFILE (1)


c      COMMON /RTITLE/ TITLE
      COMMON /GETPUT/ SRTPUT, SRTGET
      COMMON /REPLIC/ ZSMIN,ZSMAX,XSMIN,XSMAX,YSMIN,YSMAX,
     1                NSRCZ,NSRCX,NSRCY
      EQUIVALENCE (SRTFRQ,FREQ1),(ENDFRQ,FREQ2),(DELFRQ,DLFREQ)

C**********************************************************************
C*****  DO OPERATIONS ONLY REQUIRED FIRST TIME SUBROUTINE CALLED

C        *****  OPEN REPO FILE...Note that the logical unit number must 
C        be assigned a filename external to the program
         call opfilb(lun,ierr)
c         OPEN  ( UNIT       = LUN
c     -,          STATUS     = 'OLD'
c     -,          FORM       = 'FORMATTED' )

C        *****  INITIALIZE HEADER INFORMATION

         IZERO = 0

         ZERO =0.0

C        *****  read HEADER

         READ (LUN) title
         write(6,*) title
         READ (LUN) IBUF(1),IBUF(2)
         IF (IBUF(1).NE.NRCV) THEN
           STOP '>>>>>GETREP: Number of receivers does not match'
         END IF
         IF (IBUF(2).NE.NFREQ) THEN
           STOP '>>>>>GETREP: Number of frequencies does not match'
         END IF
         READ (LUn) (BUF(J),J=1,3) 
         IF ((ABS(BUF(1)-SRTFRQ).GT.1E-6*SRTFRQ).OR.
     &       (ABS(BUF(2)-ENDFRQ).GT.1E-6*ENDFRQ)    ) THEN
           STOP '>>>>>GETREP: Frequency interval discrepancy'
         END IF

C        *****  ECHO HEADER TO TERMINAL
         note1=' '
         note2=' '
         WRITE (6,200) NOTE1, NOTE2
         WRITE (6,210) IBUF(1),IBUF(2)
         WRITE (6,215) (BUF(J),J=1,3)

         WRITE(6,*) '>>>> WARNING: Replica space will be read from'
         WRITE(6,*) '              REPI-file and replace input.'

            READ (LUN) ZSMIN,ZSMAX,NSRCZ
            READ (LUN) XSMIN,XSMAX,NSRCX
            READ (LUN) YSMIN,YSMAX,NSRCY

C*****   READ ARRAY DATA FROM FILE

         DO 10 IREC=1,NRCV
            READ(LUN) BUF(1),BUF(2),BUF(3),IBUF(4),BUF(5)
            IF ((ABS(BUF(1)-RAN(IREC)) .GT.1E-3).OR.
     &          (ABS(BUF(2)-TRAN(IREC)).GT.1E-3).OR.
     &          (ABS(BUF(3)-DEP(IREC)) .GT.1E-3).OR.
     &          (IBUF(4).NE.IRTYP(IREC)        ).OR.
     &          (ABS(BUF(5)-GAIN(IREC)).GT.1E-3*BUF(5))) THEN

         WRITE(6,*) '>>>> WARNING: Array data read from REPI-file are '
         WRITE(6,*) '              inconsistent with input data for'
         WRITE(6,*) '              element no.',IREC
         WRITE(6,*) '              INPUT will be used'

            END IF

 10      CONTINUE

C*****  END READING HEADER
        RETURN

      ENTRY GETRDA(CFILE)

      DO 20 IREC=1,NRCV
         READ (LUN) CFILE(IREC)
20    CONTINUE

      RETURN

C*****  FORMATS

100   FORMAT (A)
130   FORMAT (2(2X,E16.9) )

200   FORMAT (//1X,'>>>>> READING REP FROM EXTERNAL FILE'
     -        //1X,A /1X,A )

210   FORMAT (//1X,'Replica Dimension               = ',I5
     -         /1X,'Number of Frequency Lines       = ',I5)

215   FORMAT ( /1X,'       Starting Frequency       = ',F8.3,' Hz'
     -         /1X,'       Ending Frequency         = ',F8.3,' Hz'
     -         /1X,'       Delta Frequency          = ',F8.3,' Hz')

      END 


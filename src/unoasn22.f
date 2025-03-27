      PROGRAM OASNR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                       C
C                      OASES V 2.0                      C
C                                                       C
C       Ocean Acoustic and Seismic Exploration          C
C                       Synthesis                       C
C                                                       C
C                  (C) Henrik Schmidt                      C
C         Massachusetts Institute of Technology         C
C                Cambridge, MA 02139                    C
C                                                       C
C                        1996                           C
C                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOISE AND REPLICA 
C     Version 2.0, Update 9-Apr-1996
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C          
C          
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'srccoo.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'noiprm.f'
      INTEGER SRTPUT,SRTGET                                         

      LOGICAL CRARAO,NPWBF,INTPLT,DRCONT,RRCONT
      LOGICAL ITYPLT,NFRPLT,GAMPLT,INTERP
      LOGICAL LOGBUF
      LOGICAL GETFLG, PUTFLG,RINFLG,ROTFLG,corsns

      CHARACTER*6  BOPT(10),CRAOPT(6)
      CHARACTER*6  OPTION(2)
      CHARACTER*4  TITLE(20)
      CHARACTER*40 FILENM,trcfil,brcfil

      DIMENSION IBOPT(10)
      DIMENSION X(NP2,3),FF(2,NP3),PX(MODULO)              
      DIMENSION RDUP(3),RDDOWN(3),CYAXIS(3),RDINC(3)
      DIMENSION FFS(2,NP),XS(NP2)     

      COMMON /RTITLE/ TITLE                                         
      COMMON /REPLIC/ ZSMIN,ZSMAX,XSMIN,XSMAX,YSMIN,YSMAX,
     1                NSRCZ,NSRCX,NSRCY

      EQUIVALENCE (NREC,ISPACE),(LF,NUMFR)
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))        
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))        

      DATA OPTION /'OASNR ','      '/                    
      DATA BOPT   /' BE   ',' ML   ',' TML  ',' MCM  ',6*'      '/
      DATA CRAOPT /' DX   ',' DY   ',' DZ   ',' DV   ',
     -             ' GAM  ',' LEV  ' /
          

C ********************  some FORMATS  *******************
          
200   FORMAT(20A4)                
210   FORMAT(//1H ,'OASN Version 2.0, Update 9-Apr-1996',//1H ,20A4)   
220   FORMAT(1H ,F10.2,2X,F10.3,2X,F10.3,2X,F10.1)
310   FORMAT(//1H ,'INTEGRANDS BUILT, SD: ',F12.3,' M.',
     &                ' CPU=',F12.3)
350   FORMAT(//1H ,'    DEPTH        ALPHA       BETA      ATTENA     '
     -,          '  ATTENB         RHO       ROUGHNESS'//
     -       1H ,3F12.5,2F12.8,2F12.5  )  
500   FORMAT(1H ,' ',
     &      /1H ,'SOURCE FREQUENCY:',F10.2,' HZ ',
     &      /1H ,'------------------------------')
550   FORMAT(//1H ,3X,2HLS,5X,5HICUT1,5X,5HICUT2,/(1X,I5,2I10))
551   FORMAT(//1H ,'TOTAL   CONTINUOUS  DISCRETE  EVANESCENT',
     1        /(1X,I5,3I10))
600   FORMAT(//,'  CMIN = ',G15.6,' M/S ',/,'  CMAX = ',G15.6,' M/S')
650   FORMAT(1H ,' CCUT1= ',G15.6,' M/S ',/,'  CCUT2= ',G15.6,' M/S')
6010  FORMAT(1H ,I8,10X,A40)
          
          
C ******************************************************         
          
      DEBUG=.FALSE.
c      DEBUG=.true.
      NBEAMF = 3
      INDEX0 = 0
      NUMREP = 0
      NPNT   = 0          
      INPFIL = 40
      IOUFIL = 41
      MODU   = MODULO
      ITXX   = NP2
      PI     = 4.0*ATAN(1.0)           
      AI     = CMPLX(0.,1.)             
      CNUL   = CMPLX(0.,0.)
      IR     = 1 
      LS     = 1
      LAYS(1)= 2
      DELTA  = 1.
      THETA  = 0.
      FOCDEP = 0.
      LTYP   = 1
      LINA   = 0
      NFLAG  = .FALSE.
      ICNTIN = 0

      SRTPUT = 0                                                    
      SRTGET = 0                                                    
      IDUM1  = 0                                                    
      IDUM2  = 0                                                    
c >>> Initialize Bessel function origin
      brk_0=0e0
      mbf_0=0
c >>> Initialize roughness spectra
      goff=.false.
      pierson=.false.
          
C*****
C*  Read Input File  (unit 1 = input)
C*****          
          
      CALL OPFILR(1,IOER)
      IF (IOER.GT.0) STOP '>>>> ERROR: INPUT FILE NOT FOUND <<<<'
      READ(1,200)   TITLE            
      WRITE(6,210)  TITLE           
C*****  OPEN OUTPUT FILES for NOISE COVARIANCE MATRICES
C       (unit 26 =    )

      CALL OPFILW(26,IOER)
      WRITE(26,200) TITLE

      CALL GETOPT (IPROF,ICNTIN,MFAC,IPARES,IBOPT,CRARAO,NPWBF,
     -             INTPLT,DRCONT,RRCONT,ITYPLT,NFRPLT,GAMPLT,
     -             GETFLG,PUTFLG,RINFLG,ROTFLG,INTERP,
     -             corsns)

C*****  READ IN FREQUENCY DATA

      IF (ICNTIN.GT.0) THEN
        READ(1,*) FREQ1,FREQ2,NFREQ,OFFDBIN
      ELSE
        OFFDBIN=0.0
        READ(1,*) FREQ1,FREQ2,NFREQ             
      END IF
      FREQ=0.5*(FREQ1+FREQ2)
      IF (NFREQ.GT.1) THEN
        DLFREQ=(FREQ2-FREQ1)/(NFREQ-1)
      ELSE
        DLFREQ=0.0
      END IF

C*****  READ IN ENVIRONMENTAL DATA

      CALL INENVI
      NUMI=NUML-1                 
          
C*****  READ IN COORDINATES FOR RECEIVER ARRAY

      CALL INPRCV

      IF (IR.GT.NRD) THEN
         WRITE(6,*) '*** TOO MANY RECEIVER DEPTHS ***'
         STOP
      END IF

      DO 920 JJ=1,IR
         CALL RECEIV(V,NUML,RDC(JJ),LAY(JJ),Z(JJ))                 
920   CONTINUE

C*****  READ NOISE DATA  (noise levels in dB)
      IF (CALNSE.or.trfout) THEN
       CALL NOIPAR(INTPLT)
      END IF

C*****  READ IN SOURCE COORDINATES FOR WHICH REPLICA FIELDS
C       HAVE TO BE CALCULATED

      IF (IPARES.EQ.1) THEN
         NSRCZ=0
         NSRCY=0
         NSRCX=0
         READ(1,*) ZSMIN,ZSMAX,NSRCZ
         READ(1,*) XSMIN,XSMAX,NSRCX
         READ(1,*) YSMIN,YSMAX,NSRCY
         IF (NSRCZ.GT.NSMAX.OR.NSRCX.GT.NSMAX.OR.NSRCY.GT.NSMAX) THEN
           STOP '>>> TOO MANY REPLICA POINTS <<<'
         END IF
C
C       WRITE OUT REPLICA FILE HEADER FOR OPTION ROTFLG
C
         IF (ROTFLG) THEN
           CALL PUTREP(CFILE)
         END IF
C
         IF (NSRCZ.GT.1) THEN
            DO 601 I=1,NSRCZ
               ZSC(I)=(ZSMIN+(I-1)*(ZSMAX-ZSMIN)/FLOAT(NSRCZ-1))
601         CONTINUE
         ELSE
            ZSC(1)=ZSMIN
         END IF
         IF (NPWBF) THEN
            RFC=1E3
         ELSE 
            RFC=1E0
         END IF
         IF (NSRCX.GT.1) THEN
            DO 602 I=1,NSRCX
               XSC(I)=RFC*(XSMIN+(I-1)*(XSMAX-XSMIN)/FLOAT(NSRCX-1))
602         CONTINUE
         ELSE 
            XSC(1)=RFC*XSMIN
         END IF
         IF (NSRCY.GT.1) THEN
            DO 603 I=1,NSRCY
               YSC(I)=RFC*(YSMIN+(I-1)*(YSMAX-YSMIN)/FLOAT(NSRCY-1))
603         CONTINUE
         ELSE
            YSC(1)=RFC*YSMIN
         END IF
                    
C*****  WAVENUMBER PARAMETERS FOR REPLICA

         READ(1,*)CMINSIN,CMAXSIN        
         READ(1,*)NWSIN,ICUT1S,ICUT2S
         if (nwsin.lt.0) then
          write(6,*) '>>> Automatic sampling for REPLICAS <<<'
          icntin=1
          do ix=1,nsrcx
           do iy=1,nsrcy
            rmaxs=max(rmaxs,sqrt(xsc(ix)**2+ysc(iy)**2)/rfc)
            rmins=min(rmins,sqrt(xsc(ix)**2+ysc(iy)**2)/rfc)
           end do
          end do
          write(6,*) 'Rmax=',rmaxs,' km'
          write(6,*) 'Rmin=',rmins,' km'
          OFFDB=0E0
          CALL AUTSMN(CMINSIN,CMAXSIN,RMINS,RMAXS,CMINS,CMAXS,
     &                NWVNOS,ICUT1S,ICUT2S)
         else       
          cmins=cminsin
          cmaxs=cmaxsin
          NWVNOS=MIN0(NWSIN,NP)
          ICUT2S=MIN0(NWSIN,ICUT2S)
          ICUT1S=MAX0(1,ICUT1S)
         end if

         NFLAG=.FALSE.
         IF (CMINS.EQ.0)            STOP '*** CMIN MUST BE NON-ZERO ***'
         SLOW1S = 2*PI / CMAXS
         SLOW2S = 2*PI / CMINS               
         IF (CMINS.LE.0.OR.CMINS.GT.CMAXS) 
     -                              STOP '*** CMIN/CMAX CONFLICT ***'
         WRITE(6,*)
         WRITE(6,*) 'WAVENUMBER PARAMETERS FOR REPLICAS:'
         WRITE(6,600)CMINS,CMAXS       
         WRITE(6,550)NWVNOS,ICUT1S,ICUT2S
      END IF
C
C     OPEN SCRATCH FILE FOR INTEGRAND CONTOUR DATA
C
       IF (CALNSE.AND.DRCONT.AND.INTPLT) THEN
         NDEC=NWVNON/NCONM
         IF (NDEC.GT.0) THEN
           NCON=(NWVNON-1)/NDEC+1
         ELSE
           NDEC=1
           NCON=NWVNON
         END IF
         do ii=1,3
          conmax(ii)=-200.0
         end do
        CALL OPNBUF(27,NCON,NFREQ*NRCV,100)
        CALL OPFILW(28,IOER)
        CALL OPFILW(29,IOER)
       END IF
          
C
C     CHECK WHETHER TO INVOKE DEFAULT CONTOUR OFFSET
C
      IF (ICNTIN.GT.0.AND.OFFDBIN.LT.1E-10) THEN
       IF (IPARES.GT.0) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMINS-1E0/CMAXS)/NWVNOS
       ELSE IF (NDNS.GT.0) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIND-1E0/CMAXD)/NWVNOD
       ELSE IF (SNLEVDB.GT.0.01) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(SLS(3)-SLS(2))/NWS(2)
       ELSE IF (DPLEVDB.GT.0.01) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(SLD(3)-SLD(2))/NWD(2)
       ELSE
        OFFDB=OFFDBIN
       END IF
        WRITE(6,*) 
        WRITE(6,*) 'DEFAULT CONTOUR OFFSET APPLIED,',OFFDB,
     &             ' dB/wavelength'
      ELSE
       OFFDB=OFFDBIN
      END IF

      OFFDBS=OFFDB
      OFFDB=0.0

      NWVNO=NWVNON
      CMIN=CMINN
      CMAX=CMAXN
      ICUT1=1
      ICUT2=NWVNO                      

C*****  OPEN PLOT AND CHECK FILES

      CALL OPFILW(19,IOER)
      CALL OPFILW(20,IOER)
      CALL OPFILW(21,IOER)
      WRITE(19,6010) MODU,'MODU'


C*****  PLOT OF VELOCITY PROFILE IF OPTION 'Z' WAS CHOSEN

      IF (IPROF.GT.0) THEN
         READ(1,*) VPLEFT,VPRIGHT,VPLEN,VPINC
         READ(1,*) DPUP,DPLO,DPLEN,DPINC
         CALL PLPROF(TITLE,VPLEN,DPLEN,VPLEFT,VPRIGHT,VPINC,
     2               DPUP,DPLO,DPINC)
      END IF

C*****  OPEN SCRATCH FILE FOR NOISE AND FIELD CORRELLATION MATRICES
      IF (CALNSE) THEN
        LRECN=2*NRCV*NRCV
        ISI=NFREQ
        CALL OPNBUF(32,LRECN,ISI,2*NRNR/64+1)
      END IF
  
C *****  OPEN SCRATCH FILE FOR REPLICA FIELDS
      IF (IPARES.GT.0) THEN
        LRECORD=2*NRCV
        ISI=NSRCZ*NSRCX*NSRCY*NFREQ
        CALL OPNBUF(31,LRECORD,ISI,500)
      END IF

c >>> open TRF file and write header for option T
       IF (TRFOUT) THEN 
        irsave=ir
        ir=1                   
        nplsav=nplots
        nplots=nrcv
        lxp1=nint(freq1/dlfreq)+1
        mx=nint(freq2/dlfreq)+1
        nx=512
 432    nx=nx*2
        if (nx.lt.mx) go to 432
        dt=1e0/(nx*dlfreq)
        freqs=(freq1+freq2)*0.5        
        CALL TRFHEAD('trf',TITLE,dep(1),dep(nrcv),
     &               1.0,1.0,
     &              NX,LXP1,MX,DT,FREQS,SD)
        ir=irsave    
        nplots=nplsav
       END IF


C**********************  BEGIN FREQUENCY LOOP  *************************

      TOTTIM=0.0
      CALL CLTIME
      CALL PINIT1
      IF (DEBUG) CALL PREQV(NUML,NUMI)
      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1

      IF (TABLERC) THEN
C >>>  Tabulated Top  Reflection Coefficient
       call settrc()
      ELSE IF (BOTTOMRC) THEN
C >>>  Tabulated Bottom  Reflection Coefficient
       call setbrc()
      END IF

      DO 20 IFR=1,NFREQ        

      TIMF=0
      CALL CLTIME
      FREQ=FREQ1+(IFR-1)*DLFREQ
      WRITE(6,500)FREQ  
      WRITE(26,*) FREQ,' FREQUENCY'          
      DSQ=2E0*PI*FREQ             
      CSQ=DSQ*DSQ             

      IF (TABLERC) THEN
c >>>   Read Top reflection coefficient table
       call gettrc()
      ELSE IF (BOTTOMRC) THEN
c >>>   Read Bottom reflection coefficient table
       call getbrc()
      ENDIF

C*****  NOISE CALCULATION SECTION
      IF (CALNSE.or.trfout) THEN
       CALL NOICAL(MFAC,INTPLT,DRCONT,INTERP,corsns)

C **** WRITE OUT NOISE COVARIANCE MATRIX
       if (calnse) then
        CALL PUTXSM(CORRNS,NRCV,IFR,IERR)
  
C*****  PLOT NOISE INTENSITIES VS RECEIVER NUMBER

        IF (ITYPLT.AND.NRCV.GT.1) THEN
         CALL PLNOIS(FREQ,TITLE,20.0,12.0)
        END IF
       end if
      END IF
C*****  RESTORE WAVENUMBER PARAMETERS FOR REPLICA FIELDS

      IF (IPARES.EQ.1) THEN
         ICDR = 0
         NWVNO=NWVNOS
         CMIN=CMINS
         CMAX=CMAXS
         icut1=1
         icut2=nwvno
         ICW1=ICUT1S
         ICW2=ICUT2S                      
         SLOW1=SLOW1S
         SLOW2=SLOW2S
         WK0=FREQ*SLOW1
         WKMAX=FREQ*SLOW2    
         DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )      
         FNI5=DLWVNO*FNIFAC
         OFFDB=OFFDBS
         CALL PINIT2

C        *****  CALCULATE REPLICA FIELDS 
         WRITE(6,*)
         WRITE(6,*) 'REPLICA FIELD CALCULATION'
         WRITE(6,*) '-------------------------'
         DO 703 ISRCZ=1,NSRCZ
           LS=1
           SDC(1)=ZSC(ISRCZ)
           CPHFAC(1)=PCORR
           CALL SOURCE(V,NUML,SDC(1),LAYS(1),ZUS(1),ZLS(1))
           CALL SINIT
           CALL RDTIME(T1)
           CALL CLTIME
           TIMF=TIMF+T1

c           CALL CALINT
           CALL CALIN3
           CALL CHKSOL

           CALL RDTIME(T1)
           CALL CLTIME
           TIMF=TIMF+T1
           WRITE(6,310) SDC(1),T1
C
C   *****  REPLICA FIELD INTEGRATION

           CALL RFLD(NSRCX,NSRCY,ROTFLG,INTERP)

           CALL CLSBUF(LUGRN)

           CALL RDTIME(T1)
           CALL CLTIME
           TIMF=TIMF+T1
           WRITE(6,315) T1
 315      FORMAT(1H ,'REPLICA FIELDS DONE,                  CPU=',F12.3)
703      CONTINUE
      END IF
 
      CALL RDTIME(T1)
      TIMF=TIMF+T1
      TOTTIM=TOTTIM+TIMF
      WRITE(6,311) FREQ,TIMF
311   FORMAT(1H ,'FREQ. ',F8.2,' Hz DONE,               CPU=',F12.3)

20    CONTINUE

C************************* END of FREQUENCY LOOP **********************

C*****  ENDFILE ON BUFFER FILES
      IF (CALNSE)THEN
         CALL ENFBUF(32)
      END IF
      IF (IPARES.GT.0) THEN
         CALL ENFBUF(31)
      END IF

C *** NOISE GRAPHICS

      IF (CALNSE) THEN
       IF (DRCONT.AND.INTPLT) CALL ENFBUF(27)

C*****  PLOT OF NOISE INTENSITY SPECTRA

       IF (NFRPLT.AND.(NFREQ.GT.1)) THEN
         DO 22 IRCV=1,NRCV
          CALL PLNFSP(IRCV,FREQ1,DLFREQ,NFREQ,TITLE,20.0,12.0)
22       CONTINUE
       END IF

C*****  CONTOUR PLOTS OF INTEGRANDS VERSUS FREQUENCY

       IF (DRCONT.AND.INTPLT) THEN
        DO 710 JR=1,IR
         do 710 ii=1,3
         IF (IOUT(II).NE.0) THEN
          CALL RWDBUF(27)
          XLEFT=0
          XRIGHT=1E0/CMINN
          XINC=INT(XRIGHT*1E4/5E0)*1E-4
          XSCALE=(XRIGHT-XLEFT)/20.0
          IF (FREQ2.LE.FREQ1/10E0) THEN
            YDOWN=0E0
          ELSE
            YDOWN=FREQ2
          END IF
          IF (FREQ1.LE.FREQ2/10E0) THEN
            YUP=0E0
          ELSE
            YUP=FREQ1
          END IF
          YINC=INT(ABS(YDOWN-YUP)*1E1/5E0)*1E-1
          YSCALE=ABS(YDOWN-YUP)/12.0
          ZMIN=10.0*INT(0.1*CONMAX(II))
          ZINC=1E1
          ZMAX=ZMIN-ZINC*10E0
          CALL NOIVFW(TITLE,NCON,NFREQ,NCON,NFREQ,XLEFT,XRIGHT,
     1                XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN,ZMAX,
     2                ZINC,0.0,FLOAT(II),FREQ1,FREQ2,1E0/CMAXN,
     3                XRIGHT,II)
          DO 709 IFREQ=1,NFREQ
            DO 708 JRT=1,IR
             do 708 jj=1,3
              CALL RDBUF(27,FFS,NCON)
              IF (JRT.EQ.JR.and.jj.eq.ii) THEN
                WRITE(29,'(1X,6G13.5)') (XS(JJJ),JJJ=1,NCON)
              END IF
 708        CONTINUE
 709      CONTINUE
         end if
 710    CONTINUE
        CLOSE(28)
        CLOSE(29)
       END IF

       OPTION(2)='PLTEND'
       WRITE(19,777) OPTION
       WRITE(20,777) OPTION
      END IF

      WRITE(6,9960)

 777  FORMAT(1H ,2A6)
 9960 FORMAT(//1H ,'*** OASES NOISE AND REPLICA V-1.3 FINISHED ***')

C*****  CLOSE BUFFER FILES
      IF (CALNSE) THEN
       CALL CLSBUF(32)
      END IF
      IF (IPARES.EQ.1) THEN
        CALL CLSBUF(31)
      END IF

      WRITE(6,9962) TOTTIM
 9962 FORMAT(//1H ,'*** TOTAL TIME: ',F10.3,' SECONDS ***')

      END  

C*********************** END OF OASNS MAIN ROUTINE *******************
C
      SUBROUTINE GETOPT (IPROF,ICNTIN,MFAC,IPARES,IBOPT,CRARAO,NPWBF,
     -                   INTPLT,DRCONT,RRCONT,ITYPLT,NFRPLT,GAMPLT,
     -                   GETFLG,PUTFLG,RINFLG,ROTFLG,INTERP,
     -                   corsns)

C     INPUT OF OPTIONS
C          modified by Bruce H Pasewark   September 30, 1986
C              real (i.e. experimental)  field option added
C
C     current options : B C D E F G H I J M N O P Q R S T W X Z (1-9)
C     future options  : A K L U V Y
      INCLUDE 'compar.f'
      LOGICAL CRARAO,NPWBF,INTPLT,DRCONT,RRCONT,
     -        ITYPLT,NFRPLT,GAMPLT,INTERP,corsns
      LOGICAL GETFLG, PUTFLG, RINFLG, ROTFLG                        
      LOGICAL SECCHAR
      CHARACTER*1 OPT(40)

      DIMENSION IBOPT(10)

      COMMON /MCMLM/ MAXIN,MCMDIR

      WRITE(6,300)                
 300  FORMAT(//1H ,'OPTIONS:',/)    

      MAXIN   = 1
      IPRINT  = 1
      NOUT    = 0
      IREF    = 0
      ISTYP   = 0
      KPLOT   = 0
      ICDR    = 0
      IPROF   = 0
      ICNTIN  = 0
      MFAC    = 0
      IPARES  = 0
      NBOPT   = 0

      DO 10 I=1,3                 
         IOUT(I)  = 0     
10    CONTINUE
      DO 20 I=1,10
         IBOPT(I) = 0
 20   CONTINUE
      CRARAO = .FALSE.              
      GAMPLT = .FALSE.              
      NPWBF  = .TRUE.
      INTPLT = .FALSE.
      DRCONT = .FALSE.
      RRCONT = .FALSE.
      ITYPLT = .FALSE.
      NFRPLT = .FALSE.
      GETFLG = .FALSE.                                              
      PUTFLG = .FALSE.                                              
      RINFLG = .FALSE.                                              
      ROTFLG = .FALSE.                                              
      SHEAR=.FALSE.
      SECCHAR=.FALSE.
      INTERP=.FALSE.
      CALNSE=.FALSE.
      trfout=.false.
c >>> default is uncorrelated sources
      corsns=.false.

      READ(1,200) OPT             
 200  FORMAT(40A1)                

      DO 50 I=1,40   
         IF (SECCHAR) THEN
           SECCHAR=.FALSE.
           GO TO 50             
         ELSE IF (OPT(I).EQ.'K'.OR.OPT(I).EQ.'k') THEN
            IF (INTPLT) GO TO 50
            INTPLT=.TRUE.
            WRITE(6,309)
309          FORMAT(1H ,'NOISE KERNELS PLOTTED')

         ELSE IF (OPT(I).EQ.'C'.OR.OPT(I).EQ.'c') THEN
            IF (DRCONT) GO TO 50
            DRCONT=.TRUE.
            intplt=.true.
            WRITE(6,308)
308          FORMAT(1H ,'NOISE KERNEL CONTOURS')

         ELSE IF (OPT(I).EQ.'P'.OR.OPT(I).EQ.'p') THEN
            IF (ITYPLT) GO TO 50
            ITYPLT=.TRUE.
            WRITE(6,310)
310          FORMAT(1H ,'NOISE INTENSITY PLOTS')

         ELSE IF (OPT(I).EQ.'F'.OR.OPT(I).EQ.'f') THEN
            IF (NFRPLT) GO TO 50
            NFRPLT=.TRUE.
            WRITE(6,311)
311          FORMAT(1H ,'NOISE SPECTRUM PLOTS')

         ELSE IF (OPT(I).EQ.'Z'.OR.OPT(I).EQ.'z') THEN
            IF (IPROF.GT.0) GO TO 50
            IPROF=1
            WRITE(6,314)
314          FORMAT(1H ,'PLOT OF VELOCITY PROFILES')

         ELSE IF (OPT(I).EQ.'J'.OR.OPT(I).EQ.'j') THEN
            IF (ICNTIN.GT.0) GO TO 50
            ICNTIN=1
            WRITE(6,315)
315          FORMAT(1H ,'COMPLEX INTEGRATION CONTOUR')

         ELSE IF (OPT(I).EQ.'R'.OR.OPT(I).EQ.'r') THEN
            IF (IPARES.GT.0) GO TO 50
            IPARES=1
            ROTFLG=.TRUE.
            WRITE(6,316)
316         FORMAT(1H ,'GENERATION OF REPLICA FIELDS')

         ELSE IF (OPT(I).EQ.'N'.OR.OPT(I).EQ.'n') THEN
            IF (CALNSE) GO TO 50
            CALNSE=.TRUE.
            WRITE(6,319)
319         FORMAT(1H ,'GENERATION OF NOISE COVARIANCE MATRIX')
         ELSE IF (OPT(I).EQ.'T') THEN
            IF (TRFOUT) GO TO 50
            TRFOUT=.TRUE.
            WRITE(6,321)
 321        FORMAT(1H ,'CREATING TRANSFER FUNCTION FILE')

         ELSE IF (OPT(I).EQ.'I'.OR.OPT(I).EQ.'i') THEN
            IF (INTERP) GO TO 50
            INTERP=.TRUE.
            WRITE(6,317)
317         FORMAT(1H ,'FFT INTEGRATION AND INTERPOLATION APPLIED')
         else if (opt(i).eq.'D'.or.opt(i).eq.'d') then
            doppler=.true.
            write(6,'(1H ,a,f6.1,a)') 'Sou/rec speed: ',vrec,' m/s'
         ELSE IF (OPT(I).EQ.'t') THEN
          tablerc=.true.
          WRITE(6,322)
 322      FORMAT(1H ,'TABULATED SURFACE REFLECTION COEFFICIENT')
         ELSE IF (OPT(I).EQ.'b') THEN
          bottomrc=.true.
          WRITE(6,323)
 323      FORMAT(1H ,'TABULATED BOTTOM REFLECTION COEFFICIENT')
         ELSE IF (OPT(I).EQ.'Q'.OR.OPT(I).EQ.'q') THEN
            IF (DEBUG) GO TO 50
            DEBUG=.TRUE.
            WRITE(6,318)
318         FORMAT(1H ,'>>>> DEBUGGING ENABLED <<<<')

         ELSE IF (ICHAR(OPT(I)).GT.ICHAR('0').AND.
     -            ICHAR(OPT(I)).LE.ICHAR('9')) THEN
            IF (MFAC.EQ.0) THEN
               MFAC=ICHAR(OPT(I))-ICHAR('0')
               corsns=.true.
               WRITE(6,*) 'SOURCE DIRECTIONALITY, M=',MFAC
            END IF

         END IF	

50    CONTINUE
C
      IF (MFAC.EQ.0) THEN
          MFAC=1
          WRITE(6,*) 'UNCORRELATED SURFACE NOISE SOURCES'
      END IF

      IF (NOUT.EQ.0) THEN
         IOUT(1)=1                   
         NOUT=1                      
      END IF

      RETURN                      
      END
      BLOCK DATA NOIBLK
      INCLUDE 'compar.f'
      CHARACTER*4 TITLE(20)
      COMMON /RTITLE/ TITLE    
C
C**** DEFINITION OF MAX REAL ARGUMENT TO THE EXPONENTIAL FUNCTION
      COMMON /ARGMAX/ AM
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE FPS164
CFPS  DATA AM /300./
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE VAX
      DATA AM /65./     
      DATA OMEGIM /0.0/
      DATA TITLE  /20*'    '/      
      DATA PROGNM /'OASNR '/  
      DATA LUGRN,LUTRF,LUTGRN,LUTTRF /30,35,30,35/
      DATA SHEAR,DECOMP,SCTOUT,NFLAG,PADE 
     &     /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA MSUFT,MBMAX,MBMAXI,SRCTYP,ISROW,ISINC /1,1,2,1,1,0/
c >>> source and receiver speeds defaulted to 30 knots (option d)
      data vrec,vsou /15.0,15.0/
      data bintrf /.true./
c      data bintrf /.false./
      END

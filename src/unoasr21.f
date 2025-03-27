      PROGRAM OASRC
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
C     REFLECTION COEFFICIENT VERSION
C           
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX SLOW
      CHARACTER*50 FILENM

      DIMENSION X(NP2,3),FF(2,NP3),PX(MODULO)   
      COMPLEX CTRF(3)
      DIMENSION FFS(2,NP),XS(NP2)
      CHARACTER*16 TITLEY(4)
      CHARACTER*6  OPTION(2)
      CHARACTER*4 TITLE(20)
      LOGICAL PHPLOT,CONTUR,RCLOSS
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))              
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))              
      COMPLEX CCSQ,CDSQ
      DATA TITLE /20*'    '/ 
      DATA OPTION /'FIPR  ','      '/         
      DATA TITLEY /'NORMAL STRESS  (',        
     1             'VERTICAL VEL.  (',        
     2             'HORIZONT. VEL. (',        
     3             'REF. PRESSURE  ('/        
C           
C ********************FORMATS*******************               
C           
C           
 200  FORMAT(20A4)           
 209  FORMAT(1H ,'**********************************',
     &      /1H ,'*       OASES Version ',f3.1,'       *',
     &      /1H ,'**********************************',
     &     //1H ,'Reflection/Transmission Coefficient Module OASR' )
 210  FORMAT(//1H ,20A4)         
350   FORMAT(//1H ,'     DEPTH        ALPHA       BETA      ATTENA       AT        
     1TENB         RHO      ROUGHNESS'//(1H ,3F12.5,2F12.8,2F12.5))              
C           
C           
C **********************************************               
      DEBUG=.FALSE.
C           
C           
      ITXX=NP2
       PI=4E0*ATAN(1E0)      
      AI=CMPLX(0.,1.)        
      CNUL=CMPLX(0.,0.)
      IR=1  
      LS=1
      DELTA=1.
      THETA=0.
      FOCDEP=0.
      LTYP=1
      LINA=0
      OFFDB=0E0
      srctyp=1
c >>> Initialize roughness spectra
      goff=.false.
      pierson=.false.
C           
C           
C           
C      OPEN(UNIT=1,FILE='FOR001.DAT',STATUS='OLD')
      CALL OPFILR(1,IOER)
      IF (IOER.NE.0) STOP '>>> INPUT FILE NOT FOUND <<<'
      READ(1,200)TITLE       
      write(6,209) version
      WRITE(6,210)TITLE      
      CALL GETOPT(IPARM,PHPLOT,CONTUR,IPROF,rcloss)
      TOTTIM=0.
C           
C     READ IN ENVIRONMENTAL DATA
C
      CALL INENVI
C  
C     SOURCE PLACED  ABOVE UPPERMOST INTERFACE
C
      SD=V(2,1)-1.0E-3
      SDC(1)=SD
      rdc(1)=sd
C
C     IF LOSSLESS WATER, DISABLE SCRETTING-LEROY ATTENUATION
C
      IF (ABS(V(1,3)).LT.1E-10.AND.V(1,4).LT.1E-10) THEN
        V(1,4)=1E-8
      END IF
C           
C     LAYER NUMBERS OF SOURCE AND RECEIVER ARE DETERMINED      
C           
C
C     DETERMINATION OF SOURCE LAYERS
C
      WRITE(6,908)
 908  FORMAT(//1H ,'SOURCE DATA:',//1H ,'  N0. ','   DEPTH  ',
     1       'LAYER','      ZU        ZL')
      DO 906 I=1,LS
 906  CALL SOURCE(V,NUML,SDC(I),LAYS(I),ZUS(I),ZLS(I))
      WRITE(6,907) 1,SDC(1),LAYS(1),ZUS(1),ZLS(1)
 907  FORMAT(1H ,I6,F10.1,I6,2F10.1)
        CALL RECEIV(V,NUML,RDC(1),LAY(1),Z(1))


      NUMI=NUML-1            
C           
      READ(1,*) FREQ1,FREQ2,NFREQ,IOUTF
      IF (FREQ1*FREQ2.EQ.0.0) THEN
        STOP '*** FREQUENCIES MUST BE NON-ZERO, ABORTING ***'
      ELSE IF (CONTUR) THEN
        IF (NFREQ.LE.1) THEN
          STOP '*** CONTOURS REQUIRE NRFR>1 YOU STUPID FOOL ***'
        END IF
        F1LOG=LOG(FREQ1)
        F2LOG=LOG(FREQ2)
        DFLOG=(F2LOG-F1LOG)/(NFREQ-1)
      ELSE
      END IF
      READ(1,*) ANGLE1,ANGLE2,NANG,IOUTA
      WRITE(6,1401) FREQ1,FREQ2,NFREQ,IOUTF
 1401 FORMAT(//1H ,'REFLECTION COEFFICIENTS',
     1      /1H ,'***********************',
     2      //1H ,'MINIMUM FREQUENCY:     ',G14.6,' Hz',
     3      /1H ,'MAXIMUM FREQUENCY:     ',G14.6,' Hz',
     4      /1H ,'NUMBER OF FREQUENCIES: ',I14,
     5      /1H ,'PLOT PR.:              ',I14)
      if (slowrc) then
       WRITE(6,1402) ANGLE1,ANGLE2,NANG,IOUTA
 1402  FORMAT(//1H ,'MINIMUM SLOWNESS:      ',G14.6,' deg.',
     1      /1H ,'MAXIMUM SLOWNESS:      ',G14.6,' deg.',
     2      /1H ,'NUMBER OF SLOWNSS:     ',I14,
     3      /1H ,'PLOT PR.:              ',I14)
      else
       WRITE(6,1403) ANGLE1,ANGLE2,NANG,IOUTA
 1403  FORMAT(//1H ,'MINIMUM ANGLE:         ',G14.6,' deg.',
     1      /1H ,'MAXIMUM ANGLE:         ',G14.6,' deg.',
     2      /1H ,'NUMBER OF ANGLES:      ',I14,
     3      /1H ,'PLOT PR.:              ',I14)
      end if
C
C
      NWVNO=NANG
      DLFREQ=1.
      IF (NFREQ.GT.1) THEN
      DLFREQ=(FREQ2-FREQ1)/(NFREQ-1)
      END IF
      ICUT1=1
      ICUT2=NWVNO
      NWVNO=MIN0(NWVNO,NP)
      NIPLOT=0               
      IF (IOUTF.GT.0) NIPLOT=NFREQ/IOUTF+1
      IF (NIPLOT.GT.0) THEN
      READ(1,*)XLEFT,XRIGHT,XAXIS,XINC         
      READ(1,*) YDOWN,YUP,YAXIS,YINC
      XSCALE=ABS((XRIGHT-XLEFT)/XAXIS)
      END IF
      NAPLOT=0               
      IF (IOUTA.GT.0) NAPLOT=NANG/IOUTA+1
      IF (NAPLOT.GT.0) THEN
      READ(1,*)FXLEFT,FXRIGHT,FXAXIS,FXINC         
      READ(1,*) FYDOWN,FYUP,FYAXIS,FYINC
      FXSCALE=ABS((FXRIGHT-FXLEFT)/FXAXIS)
      END IF
      IF (NANG.GT.1) THEN
        DLANGLE=(ANGLE2-ANGLE1)/(NANG-1)
      ELSE
        DLANGLE=1
      END IF
      IF (CONTUR) THEN
      READ(1,*) CXLEFT,CXRIGHT,CXAXIS,CXINC
      READ(1,*) CYDOWN,CYUP,CYAXIS,CYINC
      READ(1,*) ZMIN,ZMAX,ZINC
C      OPEN(UNIT=28,FILE='FOR028.DAT',STATUS='NEW',FORM='FORMATTED')
C      OPEN(UNIT=29,FILE='FOR029.DAT',STATUS='NEW',FORM='FORMATTED')
      CALL OPFILW(28,IOER)
      CALL OPFILW(29,IOER)
      CALL CONFAW(IPARM,NWVNO,NFREQ,ANGLE1,ANGLE2,
     1            CXLEFT,CXRIGHT,CXINC,CXAXIS,FREQ1,FREQ2,
     2            CYDOWN,CYUP,CYINC,CYAXIS,ZMIN,ZMAX,ZINC,
     3            TITLE,DLANGLE)
      END IF
C           
C     OPEN FILES
C
C      OPEN(UNIT=19,FILE='FOR019.DAT',STATUS='NEW',FORM='FORMATTED')
C      OPEN(UNIT=21,FILE='FOR021.DAT',STATUS='NEW',FORM='FORMATTED')
C      OPEN(UNIT=20,FILE='FOR020.DAT',STATUS='NEW',FORM='FORMATTED')
      CALL OPFILW(19,IOER)
      CALL OPFILW(20,IOER)
      CALL OPFILW(21,IOER)
c >>> Open file for RC table
      if (rctable) then
       CALL OPFILW(22,IOER)
       call opfilw(23,ioer)
       write(22,'(2f12.3,2i4)') freq1,freq2,nfreq,1
       write(23,'(2f12.3,2i4)') freq1,freq2,nfreq,2
      end if
      OPEN(UNIT=30,STATUS='SCRATCH',FORM='UNFORMATTED')
      OPEN(UNIT=31,STATUS='SCRATCH',FORM='UNFORMATTED')
      WRITE(19,6010) MODULO,'MODULO'
 6010 FORMAT(1H ,I8,10X,A40)
C
C     PLOT OF VELOCITY PROFILE IF OPTION 'Z' WAS CHOSEN
C
      IF (IPROF.GT.0) THEN
        READ(1,*) VPLEFT,VPRIGHT,VPLEN,VPINC
        READ(1,*) DPUP,DPLO,DPLEN,DPINC
        CALL PLPROF(TITLE,VPLEN,DPLEN,VPLEFT,VPRIGHT,VPINC,
     2              DPUP,DPLO,DPINC)
      END IF
c File for scattering rhs
      IF (SCTOUT) THEN
       CALL OPFILB(45,IOER)
       write(45) nfreq,freq1,freq2,1e0/(nfreq*dlfreq)
      END IF

C     FREQUENCY LOOP         
C     --------------         
      CALL PINIT1
C     CALL PREQV(NUML,NUMI)
C
C     SLOWNESS DIAGRAM FOR TRANS. ISOTR. MEDIA
C
      IF (NTISOL.GT.0.and.IPROF.GT.0) THEN
       CALL SNSDGM(title)
      END IF
      DO 15 JJ=1,NFREQ
      CALL CLTIME
      KPLOT=0                
      IF (NIPLOT.LE.0) GO TO 14               
      IF (MOD(JJ-1,IOUTF).EQ.0) KPLOT=1        
 14   CONTINUE               
      IF (CONTUR) THEN
        FREQ=EXP(F1LOG+(JJ-1)*DFLOG)
      ELSE
        FREQ=FREQ1+(JJ-1)*DLFREQ
      END IF
      DSQ=2E0*PI*FREQ        
      CSQ=DSQ*DSQ            
C
C     WAVENUMBER INCREMENT NEEDED FOR N-K SCATTERING
C
      DLWVNO=2*PI*FREQ/(1000.*V(1,2))
C     
      CALL PINIT2            
 9988 FORMAT(1H ,'FREQ. NO.',I4,' : ',F10.3,' HZ',             
     1       F10.3,' SECS')
      CALL REFLEC(ANGLE1,ANGLE2,IPARM)
      CALL CHKSOL
      IF (KPLOT.EQ.1) THEN
       if (rcloss) then
        CALL PLREFL(NANG,ANGLE1,DLANGLE,TITLE,IPARM,
     1           XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2           YUP,YDOWN,YINC,FREQ)
       else
        CALL PLRC(NANG,ANGLE1,DLANGLE,TITLE,IPARM,
     1           XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2           1.0,0.0,0.2,FREQ)
       end if
       IF (PHPLOT) THEN
        CALL PLREFP(NANG,ANGLE1,DLANGLE,TITLE,IPARM,
     1           XAXIS,YAXIS,XLEFT,XRIGHT,XINC,FREQ)
       END IF
      END IF
      IF (CONTUR) THEN
       CALL CONFAB(NWVNO,FREQ)
      END IF
      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1
      WRITE(6,9988) JJ,FREQ,T1
 15   CONTINUE               
      CALL CLTIME
      IF (NAPLOT.GT.0) THEN
       DO 20 I=1,NWVNO,IOUTA
        ANGLE=ANGLE1+(I-1)*DLANGLE
        if (rcloss) then
         CALL PLREFF(NFREQ,FREQ1,DLFREQ,TITLE,IPARM,
     1            FXAXIS,FYAXIS,FXLEFT,FXRIGHT,FXINC,
     2            FYUP,FYDOWN,FYINC,ANGLE,I)
        else
         CALL PLRCF(NFREQ,FREQ1,DLfreq,TITLE,IPARM,
     1           FXAXIS,FYAXIS,FXLEFT,FXRIGHT,FXINC,
     2           1.0,0.0,0.2,ANGLE,I)
        end if
        IF (PHPLOT) THEN
         CALL PLRFPH(NFREQ,FREQ1,DLFREQ,TITLE,IPARM,
     1            FXAXIS,FYAXIS,FXLEFT,FXRIGHT,FXINC,
     2            ANGLE,I)
        END IF
 20    CONTINUE
      END IF
      OPTION(2)='PLTEND'
      WRITE(19,777) OPTION
 777  FORMAT(1H ,2A6)
C           
      CLOSE(UNIT=19,STATUS='KEEP')
      CLOSE(UNIT=20,STATUS='KEEP')
      CLOSE(UNIT=21,STATUS='KEEP')
      CLOSE(UNIT=30,STATUS='DELETE')
      CLOSE(UNIT=31,STATUS='DELETE')
      IF (CONTUR) THEN
        CLOSE(UNIT=28,STATUS='KEEP')
        CLOSE(UNIT=29,STATUS='KEEP')
      END IF

C           
      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1
      WRITE(6,9960) TOTTIM
 9960 FORMAT(//1H ,'*** OASRC FINISHED ***',
     1      //1H ,'    TOTAL TIME: ',F10.3,' SECS')
      END   
C           
      SUBROUTINE GETOPT(IPARM,PHPLOT,CONTUR,IPROF,rcloss)
C           
C     INPUT OF OPTIONS       
C           
      INCLUDE 'compar.f'
      LOGICAL PHPLOT,CONTUR,rcloss
      CHARACTER*1 OPT(40)
      WRITE(6,300)           
 300  FORMAT(//1H ,'OPTIONS:',/)                
      NOUT=0
      IREF=0
      ISTYP=0                
      ICDR=0
      ISTACK=0
      IBODY=0
      ISDEP=0
      IGRP=0
      PHPLOT=.FALSE.
      CONTUR=.FALSE.
      SHEAR=.FALSE.
      RCLOSS=.false.
      IPROF=0
      DO 10 I=1,3            
 10   IOUT(I)=0              
      READ(1,200) OPT        
 200  FORMAT(40A1)           
      DO 50 I=1,40           
       IF (OPT(I).EQ.'N') THEN             
        IF (IOUT(1).GT.0) GO TO 50              
        NOUT=NOUT+1            
        IOUT(1)=1              
        IPARM=1
        WRITE(6,301)           
 301    FORMAT(1H ,'P-P COEFFICIENT')             
        GO TO 50               
       ELSE IF (OPT(I).EQ.'S') THEN             
        IF (IOUT(2).GT.0) GO TO 50              
        NOUT=NOUT+1            
        IOUT(2)=1              
        IPARM=2
        WRITE(6,302)           
 302    FORMAT(1H ,'P-SV COEFFICIENT')        
        GO TO 50               
       ELSE IF (OPT(I).EQ.'B') THEN             
        IF (IOUT(3).GT.0) GO TO 50              
        NOUT=NOUT+1            
        IOUT(3)=1              
        IPARM=3
        WRITE(6,3021)           
 3021   FORMAT(1H ,'P-Slow-wave COEFFICIENT')        
        GO TO 50               
       ELSE IF (OPT(I).EQ.'t') THEN             
        IF (transmit) GO TO 50              
        transmit=.true.            
        WRITE(6,303)           
 303    FORMAT(1H ,'Transmission Coefficients')        
        GO TO 50               
       ELSE IF (OPT(I).EQ.'P') THEN
        IF (PHPLOT) GO TO 50
        PHPLOT=.TRUE.
        WRITE(6,304)
 304    FORMAT(1H ,'PHASE PLOTS')
        GO TO 50
       ELSE IF (OPT(I).EQ.'C') THEN
        IF (CONTUR) GO TO 50
        CONTUR=.TRUE.
        WRITE(6,305)
 305    FORMAT(1H ,'CONTURS PLOTTED IN ANGLE/FREQUENCY')
        GO TO 50
       ELSE IF (OPT(I).EQ.'Z') THEN
        IF (IPROF.GT.0) GO TO 50
        IPROF=1
        WRITE(6,314)
 314    FORMAT(1H ,'PLOT OF VELOCITY PROFILES')
        GO TO 50
       ELSE IF (OPT(I).EQ.'s') THEN
        IF (SCTOUT) GO TO 50
        SCTOUT=.TRUE.
        WRITE(6,315)
 315    FORMAT(1H ,'OUTPUT OF SCATTERING DISCONTINUITIES')
        GO TO 50
       ELSE IF (OPT(I).EQ.'L') THEN
        IF (rcloss) GO TO 50
        rcloss=.TRUE.
        WRITE(6,316)
 316    FORMAT(1H ,'PLOT OF REFLECTION LOSS')
        GO TO 50
       ELSE IF (OPT(I).EQ.'Q') THEN
        IF (DEBUG) GO TO 50
        DEBUG=.TRUE.
        WRITE(6,318)
 318    FORMAT(1H ,'***** DEBUGGING *****')
       else if (opt(i).eq.'T') then
        if (rctable) go to 50
        rctable=.true.
        write(6,'(1h ,a)') 'Output RC table to filename.rco'
       else if (opt(i).eq.'p') then
        if (slowrc) go to 50
        slowrc=.true.
        write(6,'(1h ,a)') 'RC vs slowness'
       ELSE if (opt(i).ne.' ') then
        WRITE(6,320) OPT(I)
 320    FORMAT(1H ,'>>> UNKNOWN OPTION: ',A,' <<<')
       END IF
 50   CONTINUE               
      IF (NOUT.EQ.1) RETURN  
      IF (NOUT.GT.1) STOP '*** ONLY ONE FIELD PARAMETER ALLOWED***'
      IOUT(1)=1              
      NOUT=1
      IPARM=1
      WRITE(6,301)           
      RETURN
      END
      BLOCK DATA OASRBLK
      INCLUDE 'compar.f'
C
C**** DEFINITION OF MAX REAL ARGUMENT TO THE EXPONENTIAL FUNCTION
      COMMON /ARGMAX/ AM
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE FPS164
CFPS  DATA AM /300./
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE VAX
      DATA AM /65./     
      DATA OMEGIM /0.0/
      DATA PROGNM /'OASRC '/
      DATA LUGRN,LUTRF,LUTGRN,LUTTRF /30,35,30,35/
      DATA SHEAR,DECOMP,SCTOUT /.FALSE.,.FALSE.,.FALSE./
      END

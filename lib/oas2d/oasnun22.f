      SUBROUTINE INPRCV
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      INCLUDE 'combes.f'
      CHARACTER*6 CRTYP(4)
      logical*4 od
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/
C
      LS=1
      DO 1 I=2,NUML
      IF (V(I,3).LT.1E-10) THEN
        SDC(1)=V(2,1)+V(I,2)/(50.*FREQ)
        SDC(1)=MAX(SDC(1),V(I,1)+V(I,2)/(1000.*FREQ))
        GO TO 2
      END IF
 1    CONTINUE
 2    CONTINUE
C      SDC(1)=V(2,1)+V(2,2)/(50.*FREQ)
      READ(1,*) NRCV
      write(6,*) 'NRCV=',NRCV,'NRMAX=',NRMAX
      IF (NRCV.GT.NRMAX) then
         STOP 
     &   '**** NUMBER OF RECEIVERS GREATER THAN NRMAX****'
      end if
      READ(1,*) (DEP(I),RAN(I),TRAN(I),IRTYP(I),GAIN(I),I=1,NRCV)
      call opfilw(26,ioer)
      WRITE(26,*) NRCV,' NUMBER OF RECEIVERS IN ARRAY'
      IF (NRCV.LE.0) STOP '*** NO RECEIVER ARRAY SPECIFIED ***'
      IR=1
      RDC(1)=DEP(1)
      IDEP(1)=1

C *** DETERMINE MAXIMUM HORIZONTAL RECEIVER SEPARATION
      RRMAX=0E0
      DO 21 I=1,NRCV
       DO 21 J=I+1,NRCV
        RR=(RAN(I)-RAN(J))**2+(TRAN(I)-TRAN(J))**2
        RRMAX=MAX(RR,RRMAX)
 21   CONTINUE 
      RRMAX=SQRT(RRMAX)
C
C     ORDER RECEIVER DEPTHS IN INCREASING ORDER
C
      DO 10 I=2,NRCV
      DO 5 J=1,I-1

      IF (DEP(J).GT.DEP(I)+1E-6) THEN
        IR=IR+1
        DO 3 L=IR,IDEP(J)+1,-1
  3     RDC(L)=RDC(L-1)
        RDC(IDEP(I))=DEP(I)
        DO 4 L=J,I-1
  4     IDEP(L)=IDEP(L)+1
        IDEP(I)=IDEP(J)
        GO TO 10
      ELSE IF (ABS(DEP(J)-DEP(I)).LE.1E-6) THEN
        IDEP(I)=IDEP(J)
        GO TO 10
      ELSE IF (J.EQ.I-1) THEN
        IR=IR+1
        IDEP(I)=IR
        RDC(IR)=DEP(I)
        GO TO 10
      ELSE
      END IF
 5    CONTINUE
 10   CONTINUE
      WRITE(6,100) 
 100  FORMAT(1x,'RECEIVER ARRAY:',
     &      /1x ,'REC. NO.      X        Y        Z   ',
     &           '  TYPE    GAIN (dB)')   
      DO 20 I=1,NRCV
      WRITE(6,200) I,RAN(I),TRAN(I),DEP(I),CRTYP(IRTYP(I)),GAIN(I)
 200  FORMAT(1x ,I4,3X,3F9.2,3X,A6,3X,F9.2)
      WRITE(26,220) I,RAN(I),TRAN(I),DEP(I),CRTYP(IRTYP(I)),GAIN(I),
     &              ' I,X(I),Y(I),Z(I),TYPE,GAIN'
 220  FORMAT(1x ,I4,3(1X,F9.2),3X,A6,F9.2,A30)
 20   CONTINUE
      DO 30 I=1,IR
      DO 29 J=1,3
 29   IRPAR(J,I)=0
 30   CONTINUE
      NOUT=0
      DO 31 I=1,3
 31   IOUT(I)=0
      DO 40 I=1,NRCV
      II=IRTYP(I)
      GAIN(I)=10.0**(GAIN(I)/20.0)
      IF (IRTYP(I).EQ.1) THEN
        IRCPAR(I)=1
        IRPAR(1,IDEP(I))=1
        IF (IOUT(1).EQ.0) THEN
          NOUT=NOUT+1
          IOUT(1)=1
        END IF
      ELSE IF (IRTYP(I).EQ.2.OR.IRTYP(I).EQ.3) THEN
        IRCPAR(I)=3
        IRPAR(3,IDEP(I))=1
        IF (IOUT(3).EQ.0) THEN
          NOUT=NOUT+1
          IOUT(3)=1
        END IF
      ELSE IF (IRTYP(I).EQ.4) THEN
        IRCPAR(I)=2
        IRPAR(2,IDEP(I))=1
        IF (IOUT(2).EQ.0) THEN
          NOUT=NOUT+1
          IOUT(2)=1
        END IF
      ELSE
        WRITE(6,*) '*** ERROR IN TYPE OF RECEIVER NO.',I,' ***'
        STOP
      END IF
 40   CONTINUE
C      WRITE(6,*) 'NOUT,IOUT:',NOUT,(IOUT(I),I=1,3)
      RETURN
      END


      SUBROUTINE NOIPAR(INTPLT)
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'noiprm.f'
      LOGICAL INTPLT
      integer nspecs, ipspec(1000)
      real spsc(1000)
      common /spectr/ spsc,nspecs,ipspec
      DIMENSION X(ITWONP,3),FF(2,NP3),PX(MODULO)              
      DIMENSION FFS(2,NP),XS(ITWONP)     
      real spcbuf(2*NRNR)
      equivalence (spcbuf(1),corrns(1))
      EQUIVALENCE (NREC,ISPACE),(LF,NUMFR)
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))        
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))        
C ********************  some FORMATS  *******************
          
220   FORMAT(1H ,F10.2,2X,F10.3,2X,F10.3,2X,F10.1)
310   FORMAT(//1H ,'INTEGRANDS BUILT, SD: ',F12.3,' M.',
     &                ' CPU=',F12.3)
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
          
       nspecs=0
         do iundn=1,1000
          ldnlin(iundn)=.false.
         end do
      READ(1,*) SNLEVDB,WNLEVDB,DPLEVDB,NDNS
C     *****  SURFACE NOISE

      WRITE(6,*)
      IF (abs(SNLEVDB).LT.0.01) THEN
         WRITE(6,*) 'NO SURFACE GENERATED NOISE'
      ELSE IF (SNLEVDB.GE.0E0) THEN
         WRITE(6,*) 'SURFACE NOISE LEVEL:',SNLEVDB,' dB'
         IF (V(1,2).GT.500) 
     -     STOP '*** UPPER HALFSPACE MUST BE VACUUM OR AIR ***'
         WRITE(26,*) SNLEVDB, ' dB SURFACE NOISE LEVEL'
         SLEVEL=10.0**(SNLEVDB/10.0)
      ELSE if (.not.ldnlin(-nint(SNLEVDB))) then
              IUNDN=-SNLEVDB
              CALL OPFILR(IUNDN,IOER)
              IF (IOER.GT.0) THEN
               WRITE(6,2043) IUNDN
 2043          FORMAT(1H ,'>>>> ERROR: NO FILE NO.',I3,' CONTAINING ',
     &               /1H ,'>>>>        SOURCE SPECTRUM FOR SURFACE ',
     &               /1H ,'>>>>        NOISE')
               STOP
              END IF
              READ(IUNDN,*) NFRDN,FR1DN,FR2DN            
             IF (NFRDN.NE.NFREQ.OR.ABS(FREQ1-FR1DN).GT.1E-2.OR.
     &           ABS(FREQ2-FR2DN).GT.1E-2) THEN
             WRITE(6,2044) IUNDN
 2044        FORMAT(1H ,'>>>> ERROR: INCONSISTENT FREQUENCY SAMPLING'
     &            ,/1H ,'>>>>        IN SPECTRUM FILE',I3,' FOR'
     &            ,/1H ,'>>>>        SURFACE NOISE')
             STOP
             END IF
             ldnlin(iundn)=.true.
             nspecs=nspecs+1
             ipspec(iundn)=nspecs
             if (nspecs*nfreq.gt.2*nrnr) then
              stop '>>> Too many source spectra'
             end if
             do ii=1,nfreq
               read(iundn,*) dnd
               spcbuf(ii+(nspecs-1)*nfreq)=10.0**(dnd/10.0)
             end do
             close(iundn)
      END IF

C     *****  WHITE NOISE

      WRITE(6,*)
      WRITE(6,*) 'WHITE NOISE LEVEL:',WNLEVDB,' dB'
      WRITE(26,*) WNLEVDB, ' dB WHITE NOISE LEVEL'
      WNLEV=10.0**(WNLEVDB/10.0)

C     *****  DEEP NOISE

      WRITE(6,*)
      IF (DPLEVDB.LE.0.01) THEN
         WRITE(6,*) 'NO DEEP NOISE SOURCE'
      ELSE
         WRITE(6,*) 'DEEP SOURCE NOISE LEVEL:',DPLEVDB,' dB'
         WRITE(26,*) DPLEVDB, ' dB DEEP SOURCE NOISE LEVEL'
         DPLEV=10.0**(DPLEVDB/10.0)
      END IF

C     *****  DISCRETE NOISE SOURCES

      WRITE(6,*) 
      WRITE(6,*) 'DISCRETE NOISE SOURCES:',NDNS
      WRITE(6,*)

C*****  DETERMINATION OF SOURCE AND RECEIVER LAYERS

      LS=1
      SDC(1)=V(2,1)+V(1,2)/(30.*FREQ1)+ROUGH(2)
C*****  IF POSSIBLE, PLACE NOISE SOURCE IN FLUID LAYER
      DO 11 I=2,NUML
         IF (V(I,3).LT.1E-10) THEN
           SDC(1)=V(2,1)+V(I,2)/(30.*FREQ1)
           SDC(1)=MAX(SDC(1),V(I,1)+V(I,2)/(30.*FREQ1)+ROUGH(I))
           CALL SOURCE(V,NUML,SDC(1),LAYS(1),ZUS(1),ZLS(1))
           IF (V(LAYS(1),3).GE.1E-10) THEN
             DO 111 J=LAYS(1)-1,I,-1
               IF (V(J,3).LT.1E-10) THEN
                 SDC(1)=V(J,1)+0.8*(V(J+1,1)-V(J,1))
                 GO TO 12
               END IF
 111         CONTINUE
           END IF
           GO TO 12
         END IF
11    CONTINUE
12    CONTINUE

      DO 906 I=1,LS
        CALL SOURCE(V,NUML,SDC(I),LAYS(I),ZUS(I),ZLS(I))
906   CONTINUE
          
C*****  WAVENUMBER PARAMETERS FOR NOISE     

      IF (abs(SNLEVDB).GE.0.01) THEN
         READ (1,*) CMINN,CMAXN        
         READ (1,*) (NWS(JJ),JJ=1,3)
         IF (CMINN.EQ.0)        STOP '*** CMIN MUST BE NON-ZERO ***'
         IF (CMINN.LE.0.OR.CMINN.GT.CMAXN) 
     -                          STOP '*** CMIN/CMAX CONFLICT ***'
         IF (NWS(2).GT.1) THEN
          SLS(1) = 1E0 / CMAXN
          SLS(4) = 1E0 / CMINN               

C        *****  FIND MAXIMUM VEL FOR SEPARATION OF CONT AND DISC SPECT
C        *****  AND MINIMUM FOR SEPARATION OF DISCRETE AND EVANESCENT
          CMX=V(1,2)
          CMN=1E20 
          DO 1111 I=2,NUML
           CMX=MAX(CMX,V(I,2))
           CMN=MIN(CMN,V(I,2))
1111      CONTINUE
          CMX=MAX(CMINN,CMX)
          CMX=MIN(CMAXN,CMX)
          SLS(2)=1E0/CMX
          CMN=MAX(CMINN,CMN)
          CMN=MIN(CMAXN,CMN)
          SLS(3)=1E0/CMN
C      **** DISABLE INTEGRAND PLOTS FOR NON-EQUIDISTANT SAMPLING
c          IF (INTPLT) THEN
c           WRITE(6,*) '>>>> WARNING: INTEGRAND PLOTTING DISABLED'
c           INTPLT=.FALSE.
c          END IF
         ELSE
          NWS(2)=NWS(1)
          NWS(1)=0
          NWS(3)=0
          SLS(2)=1E0/CMAXN
          SLS(3)=1E0/CMINN
         END IF
         NWVNON=NWS(1)+NWS(2)+NWS(3)

         WRITE(6,*)
         WRITE(6,*) 'WAVENUMBER PARAMETERS FOR SURFACE NOISE:'
         WRITE(6,600)CMINN,CMAXN       
         WRITE(6,650)CMX,CMN
         WRITE(6,551)NWVNON,(NWS(JJ),JJ=1,3)
         IF (NWVNON.GT.NP)      STOP '*** TOO MANY SAMPLING POINTS ***'
      END IF
          
C*****  WAVENUMBER PARAMETERS FOR DEEP SOURCE NOISE     

      IF (DPLEVDB.GE.0.01) THEN
         READ(1,*) DPSD
         READ(1,*)CMINP,CMAXP        
         READ (1,*) (NWD(JJ),JJ=1,3)
         IF (CMINP.EQ.0)        STOP '*** CMIN MUST BE NON-ZERO ***'
         IF (CMINP.LE.0.OR.CMINP.GT.CMAXP) 
     -                          STOP '*** CMIN/CMAX CONFLICT ***'
         IF (NWD(2).GT.1) THEN
          SLD(1) = 1E0 / CMAXP
          SLD(4) = 1E0 / CMINP               

C        *****  FIND MAXIMUM VEL FOR SEPARATION OF CONT AND DISC SPECT
C        *****  AND MINIMUM FOR SEPARATION OF DISC AND EVANESCENT.

          CMX=V(1,2)
          CMN=1E20
          DO 1121 I=2,NUML
           CMX=MAX(CMX,V(I,2))
           CMN=MIN(CMN,V(I,2))
1121      CONTINUE
          CMX=MAX(CMINP,CMX)
          CMX=MIN(CMAXP,CMX)
          SLD(2)=1E0/CMX
          CMN=MAX(CMINP,CMN)
          CMN=MIN(CMAXP,CMN)
          SLD(3)=1E0/CMX
         ELSE
          NWD(2)=NWD(1)
          NWD(1)=0
          NWD(3)=0
          SLD(2)=1E0/CMAXP
          SLD(3)=1E0/CMINP
         END IF

         NWVNOP=NWD(1)+NWD(2)+NWD(3)

         WRITE(6,*)
         WRITE(6,*) 'PARAMETERS FOR DEEP SOURCE NOISE:'
         WRITE(6,*) 'DEPTH:',DPSD,' m'
         WRITE(6,600)CMINP,CMAXP
         WRITE(6,650)CMX,CMN
         WRITE(6,551)NWVNOP,(NWD(JJ),JJ=1,3)
         IF (NWVNOP.GT.NP)      STOP '*** TOO MANY SAMPLING POINTS ***'
      END IF
          
C*****  WAVENUMBER PARAMETERS FOR DISCRETE NOISE SOURCES

      IF (NDNS.GT.0) THEN      
         IF (NDNS.GT.NSMAX) 
     -                   STOP '*** TOO MANY DISCRETE NOISE SOURCES ***'
         WRITE(6,*)
         WRITE(6,*) 'DISCRETE NOISE SOURCES'
         WRITE(6,*)
         WRITE(6,*) '    Z (m)      X (km)       Y (km)    Level (dB)'

         DO 105 I=1,NDNS
            READ(1,*) ZDN(I),XDN(I),YDN(I),DNLEVDB(I)
            IF (DNLEVDB(I).GE.0E0) THEN
              DNLEV(I)=10**(DNLEVDB(I)/10.0)
            ELSE if (.not.ldnlin(-nint(dnlevdb(i)))) then
              IUNDN=-DNLEVDB(I)
              CALL OPFILR(IUNDN,IOER)
              IF (IOER.GT.0) THEN
               WRITE(6,1043) IUNDN,I
 1043          FORMAT(1H ,'>>>> ERROR: NO FILE NO.',I3,' CONTAINING ',
     &               /1H ,'>>>>        SOURCE SPECTRUM FOR DISCRETE ',
     &               /1H ,'>>>>        NOISE SOURCE',I3)
               STOP
              END IF
              READ(IUNDN,*) NFRDN,FR1DN,FR2DN
              
             IF (NFRDN.NE.NFREQ.OR.ABS(FREQ1-FR1DN).GT.1E-2.OR.
     &           ABS(FREQ2-FR2DN).GT.1E-2) THEN
             WRITE(6,1044) IUNDN,I
 1044        FORMAT(1H ,'>>>> ERROR: INCONSISTENT FREQUENCY SAMPLING'
     &            ,/1H ,'>>>>        IN SPECTRUM FILE',I3,' FOR'
     &            ,/1H ,'>>>>        DISCRETE NOISE SOURCE',I3)
             STOP
             END IF
             ldnlin(iundn)=.true.
             nspecs=nspecs+1
             ipspec(iundn)=nspecs
             if (nspecs*nfreq.gt.2*nrnr) then
              stop '>>> Too many source spectra'
             end if
             do ii=1,nfreq
               read(iundn,*) dnd
               spcbuf(ii+(nspecs-1)*nfreq)=10.0**(dnd/10.0)
             end do
             close(iundn)
            END IF
            WRITE(6,220) ZDN(I),XDN(I),YDN(I),DNLEVDB(I)
            XDN(I)=1.0E3*XDN(I)
            YDN(I)=1.0E3*YDN(I)
105      CONTINUE

         READ(1,*)CMINDIN,CMAXDIN        
         READ(1,*)NWDIN,ICUT1D,ICUT2D       
         if (nwdin.lt.0) then
          write(6,*) '>>> Automatic sampling for Discrete noise <<<'
          rmaxd=0
          rmind=1e8
          do i=1,ndns
           rmaxd=max(rmaxd,sqrt(xdn(1)**2+ydn(1)**2)/1e3)
           rmind=min(rmind,0.1*sqrt(xdn(1)**2+ydn(1)**2)/1e3)
          end do
          write(6,*) 'Rmax=',rmaxd,' km'
          OFFDB=0E0
          CALL AUTSMN(CMINDIN,CMAXDIN,RMIND,RMAXD,CMIND,CMAXD,
     &                NWVNOD,ICUT1D,ICUT2D)

         else       
          cmind=cmindin
          cmaxd=cmaxdin
          NWVNOD=MIN0(NWDIN,NP)
          ICUT2D=MIN0(NWDIN,ICUT2D)
          ICUT1D=MAX0(1,ICUT1D)
         end if
         NWVNOD=MIN0(NWVNOD,NP)
         ICUT2D=MIN0(NWVNOD,ICUT2D)
         ICUT1D=MAX0(1,ICUT1D)
         IF (CMIND.EQ.0)            STOP '*** CMIN MUST BE NON-ZERO ***'
         SLOW1D = 2*PI / CMAXD
         SLOW2D = 2*PI / CMIND               
         IF (CMIND.LE.0.OR.CMIND.GT.CMAXD) 
     -                              STOP '*** CMIN/CMAX CONFLICT ***'
         WRITE(6,*)
         WRITE(6,*) 'WAVENUMBER PARAMETERS FOR DISCRETE NOISE SOURCES:'
         WRITE(6,600)CMIND,CMAXD       
         WRITE(6,550)NWVNOD,ICUT1D,ICUT2D
      END IF
c
c >>> transpose source spectrum matrix and send to file 46
      if (nspecs.gt.0) then
        open(46,status='scratch',form='unformatted')
        do ii=1,nfreq
          write(46) (spcbuf(ii+(jj-1)*nfreq),jj=1,nspecs)
        end do
        rewind(46)
      end if
      RETURN                      
      END

      SUBROUTINE NOICAL(MFAC,INTPLT,DRCONT,INTERP,corsns)
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'noiprm.f'
      INCLUDE 'combes.f'
      LOGICAL INTPLT,DRCONT,INTERP,corsns
      DIMENSION X(ITWONP,3),FF(2,NP3),PX(MODULO)              
      DIMENSION FFS(2,NP),XS(ITWONP)     
      integer nspecs, ipspec(1000)
      real spsc(1000)
      common /spectr/ spsc,nspecs,ipspec
      CHARACTER*4 TITLE(20)
      COMMON /RTITLE/ TITLE                                         
      EQUIVALENCE (NREC,ISPACE),(LF,NUMFR)
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))        
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))        
220   FORMAT(1H ,F10.2,2X,F10.3,2X,F10.3,2X,F10.1)
310   FORMAT(//1H ,'INTEGRANDS BUILT, SD: ',F12.3,' M.',
     &                ' CPU=',F12.3)
500   FORMAT(1H ,' ',
     &      /1H ,'SOURCE FREQUENCY:',F10.2,' HZ ',
     &      /1H ,'------------------------------')
C ******************************************************         
C     *****  CLEAR NOISE CORRELATION MATRIX
      CALL CLNOIS
c >>> read spectral levels
      if (nspecs.gt.0) then
       read(46) (spsc(jj),jj=1,nspecs)
      end if
      IF (calnse.and.abs(SNLEVDB).GE.0.01) THEN
C     ***** SET PLANE GEOMETRY FLAG FOR GREEN'S FUNCTION GENERATOR
         ICDR = 1
         FNI5 = FNIFAC
C        *****  SURFACE NOISE SOURCES LAMBDA/30 BELOW SURFACE
         LS=1
         SDC(1)=V(2,1)+V(1,2)/(30.*FREQ)+ROUGH(2)
         DO 1 I=2,NUML
         IF (V(I,3).LT.1E-10) THEN
           SDC(1)=V(2,1)+V(I,2)/(30.*FREQ)
           SDC(1)=MAX(SDC(1),V(I,1)+V(I,2)/(30.*FREQ)+ROUGH(I))
           CALL SOURCE(V,NUML,SDC(1),LAYS(1),ZUS(1),ZLS(1))
           IF (V(LAYS(1),3).GE.1E-10) THEN
             DO 101 J=LAYS(1)-1,I,-1
               IF (V(J,3).LT.1E-10) THEN
                 SDC(1)=V(J,1)+0.8*(V(J+1,1)-V(J,1))
                 GO TO 2
               END IF
 101         CONTINUE
           END IF
           GO TO 2
         END IF
1        CONTINUE
2        CONTINUE
         SINORM=(SDC(1)-V(2,1))
         CALL SOURCE(V,NUML,SDC(1),LAYS(1),ZUS(1),ZLS(1))
         CALL SINIT

         CALL RDTIME(T1)
         CALL CLTIME
c         T1=T1*.182E-6
         TIMF=TIMF+T1
C *** MAXIMUM ARGUMENT TO BESSEL FUNCTION
        RKMAX=RRMAX*REAL(DSQ)/CMINN
C *** NUMBER OF POINTS NEEDDED FOR INTERPOLATION
        NRKMAX=MAX(NINT(20*RKMAX/(2*PI)),2)
C        IF (DEBUG) WRITE(6,*) 'RR,RK,NRK:',RRMAX,RKMAX,NRKMAX
        NRCOMB=0
        NRCOMB=(NRCV*(NRCV+1))*NWVNON/2
c        IF (NRKMAX.LT.NRCOMB.AND.NRKMAX.LT.NP) THEN
C ***     INTERPOLATION
c          BESINT=.TRUE.
c          DRK=MAX(1E-6,RKMAX)/(NRKMAX-1.001)
c          ONODRK=1E0/DRK
c          DO 10 II=1,NRKMAX
c           RARG=DRK*(II-1)
c           CALL BESJ01(RARG,BF0(II),BF1(II))
C           IF (DEBUG) WRITE(6,*) II,RARG,BF0(II)
c 10       CONTINUE
c        ELSE
C ***     DIRECT BESSEL FUNCTION CALCULATION
          BESINT=.FALSE.
c        END IF
c       *****  LOOP OVER 3 SPECTRAL REGIMES
         DO 510 ISP=1,3
         IF (NWS(ISP).GT.0) THEN

C       *****  ARTIFICIAL ATTENUATION ADDED TO ALL LAYERS

          IF (OFFDBS.GT.1E-10.AND.ISP.EQ.2) THEN
            WRITE(6,*)
            WRITE(6,*) OFFDBS,' DB/LAMBDA ATT. ADDED ALL LAYERS'
            do llay=2,numl-1
             V(llay,4)=OFFDBS+V(llay,4)
            end do
            OFFDB=0.0
            CALL PINIT2
            do llay=2,numl-1
             V(llay,4)=V(llay,4)-offdbs
            end do
          ELSE
            OFFDB=0.0
            CALL PINIT2
          END IF
c >>> remove standard SAFARI source normalization for plane geom
          cphfac(1)= cphfac(1)/SQRT(FREQ/V(LAYS((LS-1)/2+1),2))

C       ***** SET UP WAVENUMBER PARAMETERS
          WK0=DSQ*SLS(ISP)
          WKMAX=DSQ*SLS(ISP+1)
          NWVNO=NWS(ISP)
          DLWVNO=(WKMAX-WK0)/(NWVNO-1)
          ICUT1=1
          ICUT2=NWVNO
          ICW1=1
          ICW2=NWVNO

          if (snlevdb.gt.0e0) then
           slevel=10.0**(snlevdb/10.0)
          else
           iundn=-snlevdb
           slevel=spsc(ipspec(iundn))
          end if
c          write(6,*) 'call 1 CALINT/NOICAL/oasnun21.f ' 
c          CALL CALINT
          CALL CALIN3
c          write(6,*) 'exit 1 CALINT/NOICAL/oasnun21.f ' 
          CALL CHKSOL
C        *****  PLOT OF G1xG1*
C
         IF (INTPLT) THEN
            DO 509 JR=1,IR
             DO 509 II=1,3
              IF (IOUT(II).NE.0) THEN
               CALL GETGPR(JR,JR,II,II)
               if (corsns) then
C >>> multiply by correlation factors
                do jj=1,nwvno
                 wv=wk0+(jj-1)*dlwvno
                 if (mfac.ne.1) then
                  if (wv.le.real(ak(lays(1),1))) then
                   fct=mfac*(1E0-(wv/REAL(AK(LAYS(1),1)))**2)**(MFAC-1)
                  else
                   fct=0e0
                  end if
                 else
                  fct=1E0
                 end if
                 ff(1,jj)=fct*ff(1,jj)
                end do
               end if
c >>> multiply velocity kernels by 10^6
               if (ii.gt.1) then
                GFAC=1E6/SINORM
               else
                GFAC=1E0/SINORM
               end if
               IF (.NOT.DRCONT) THEN
                 CALL PLNINT(DLWVNO,WK0,NWVNO,
     -                     RDC(JR),II,GFAC,TITLE,20.0,12.0)
               ELSE
                 CALL VSMUL(FF,2,GFAC**2,FFS,1,NWVNON)
C                IF (DEBUG) WRITE(6,'(1X,G13.5)') 
C    1                      (XS(JJJ),JJJ=1,NWVNON)
                 CALL VALG10(FFS,1,FFS,1,NWVNON)
                 CALL VSMUL(FFS,1,1E1,FFS,1,NWVNON)
                 CALL VDECIM(FFS,1,FFS,1,NWVNON,NDEC,NC)
                 CALL WRBUF(27,FFS,NC)
                 CALL VMAX(FFS,1,AAA,NC)
                 CONMAX(II)=MAX(CONMAX(II),AAA)
               END IF
              END IF
509         CONTINUE
         END IF
C        *****  CALCULATE SURFACE NOISE CORRELATION FUNCTIONS
c         write(6,*) 'call NOISE/NOICAL ' 
         CALL NOISE(MFAC,SLEVEL,INTERP,corsns)
c         write(6,*) 'exit CLSBUF/NOICAL ' 
         CALL CLSBUF(LUGRN)
       END IF
 510   CONTINUE
         CALL RDTIME(T1)
         CALL CLTIME
c         T1=T1*.182E-6
         TIMF=TIMF+T1
         WRITE(6,312) T1
 312     FORMAT(1H ,'SURFACE NOISE CORRELATION DONE,       CPU=',F12.3)
      END IF

C*****  ADD WHITE NOISE

      CALL WNOISE(WNLEV)

C*****  RESTORE DEEP SOURCE NOISE WAVENUMBER PARAMETERS

      IF (calnse.and.DPLEVDB.GE.0.01) THEN

         FNI5=FNIFAC
         LS=1
         SDC(1)=DPSD
         CALL SOURCE(V,NUML,SDC(1),LAYS(1),ZUS(1),ZLS(1))
         CALL SINIT

C      ***** LOOP IN SPECTRAL REGIMES
         DO 520 ISP=1,3
          IF (NWD(ISP).GT.0) THEN

C        *****  ARTIFICIAL ATTENUATION ADDED TO all LAYERs
C               FOR DISCRETE SPECTRUM ONLY

         IF (OFFDBS.GT.1E-10.AND.ISP.EQ.2) THEN
            WRITE(6,*)
            WRITE(6,*) OFFDBS,' DB/LAMBDA ATT. ADDED TO ALL LAYERS'
            do llay=2,numl-1
             V(llay,4)=V(llay,4)+offdbs
            end do
            OFFDB=0.0
            CALL PINIT2
            do llay=2,numl-1
             V(llay,4)=V(llay,4)-offdbs
            end do
         ELSE
            OFFDB=0.0
            CALL PINIT2
         END IF
c >>> remove standard SAFARI source normalization for plane geom
          cphfac(1)= cphfac(1)/SQRT(FREQ/V(LAYS((LS-1)/2+1),2))
         
C       ***** SET UP WAVENUMBER PARAMETERS

          WK0=DSQ*SLD(ISP)
          WKMAX=DSQ*SLD(ISP+1)
          NWVNO=NWD(ISP)
          DLWVNO=(WKMAX-WK0)/(NWVNO-1)
          ICUT1=1
          ICUT2=NWVNO
c          write(6,*) 'call 2 CALINT/NOICAL/oasnun13.f ' 
c          CALL CALINT
          CALL CALIN3
          CALL CHKSOL

C        *****  CALCULATE DEEP SOURCE NOISE CORRELATION FUNCTIONS

         CALL NOISE(0,DPLEV,INTERP,corsns)

         CALL CLSBUF(LUGRN)
       END IF
 520   CONTINUE
         CALL RDTIME(T1)
         CALL CLTIME
c         T1=T1*.182E-6
         TIMF=TIMF+T1
         WRITE(6,313) T1
 313     FORMAT(1H ,'DEEP NOISE CORRELATION DONE,          CPU=',F12.3)
      END IF
c >>> Save ambient noise covariance
      do ircv=1,nrcv
       do jrcv=1,nrcv
        coramb(ircv+(jrcv-1)*nrcv)=corrns(ircv+(jrcv-1)*nrcv)
       end do
      end do
C     ***** RESTORE CYLINDRICAL GEOMETRY FOR GREEN'S FUNCTION GENERATOR

         ICDR = 0

C*****  REINTRODUCE COMPLEX CONTOUR INTEGRATION 
C       AND REMOVE ARTIFICIAL ATTENUATION

      IF (OFFDBS.GT.1E-10) then
       OFFDB=OFFDBS
      else
       offdb=0e0
      end if

C*****  DISCRETE NOISE SOURCES

      IF (NDNS.GT.0) THEN

C        *****  RESTORE DISCRETE NOISE WAVENUMBER PARAMETERS

         WRITE(6,*)
         WRITE(6,*) 'DISCRETE NOISE SOURCES'
         WRITE(6,*)
         NWVNO=NWVNOD
         CMIN=CMIND
         CMAX=CMAXD
         icut1=1
         icut2=nwvno
         ICW1=ICUT1D
         ICW2=ICUT2D                      
         SLOW1=SLOW1D
         SLOW2=SLOW2D
         WK0=FREQ*SLOW1
         WKMAX=FREQ*SLOW2    
         DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )      
         FNI5=DLWVNO*FNIFAC
         LS=1
         do ins=1,ndns
          dndone(ins)=.false.
            	IF (DNLEVDB(INS).LT.0E0) THEN
              		IUNDN=-DNLEVDB(INS)
                        dnlev(ins)=spsc(ipspec(iundn))
                else
                        dnlev(ins)=10.0**(dnlevdb(ins)/10.0)
            	END IF
        end do
         DO 610 INS=1,NDNS
	    if(.not.dndone(ins))then
		do jns=1,ndns
			if(abs(zdn(jns)-zdn(ins)).lt.1e-2)then
				dndone(jns)=.true.
				dnmask(jns)=.true.
			else
				dnmask(jns)=.false.
			endif
		enddo
            SDC(1)=ZDN(INS)
            CALL SOURCE(V,NUML,SDC(1),LAYS(1),ZUS(1),ZLS(1))
            CALL SINIT
            CALL RDTIME(T1)
            CALL CLTIME
c            T1=T1*.182E-6
            TIMF=TIMF+T1
            CALL PINIT2
            if (debug) write(6,*) 'call 3 CALIN3/NOICAL/oasnun13.f ' 
c            CALL CALINT
            CALL CALIN3
            if (debug) write(6,*) 'exit 3 CALIN3/NOICAL/oasnun13.f ' 
            CALL CHKSOL
            CALL RDTIME(T1)
            CALL CLTIME
            TIMF=TIMF+T1
            WRITE(6,310) SDC(1),T1
            do jns=1,ndns
             if (dnmask(jns)) then 
              CALL DNCORR(ZDN(jNS),XDN(jNS),YDN(jNS),DNLEV(jNS))
             end if
            end do
            CALL CLSBUF(LUGRN)
            CALL RDTIME(T1)
            CALL CLTIME
            TIMF=TIMF+T1
            WRITE(6,314) T1
         end if            
 314     FORMAT(1H ,'DISCRETE NOISE SOURCES,           CPU=',F12.3)
610      CONTINUE
      END IF
      if (calnse) then
       CALL WRNOIS
      end if

      RETURN                      
      END
      SUBROUTINE GETGPR(IDEP1,IDEP2,IPAR1,IPAR2)
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
c      LOGICAL NFLAG
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      CHARACTER*6 CRTYP(4)
      COMPLEX CC1,CC2
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/
      IF (ICUT1.GT.1) THEN
       CALL VCLR(CFF(1,1),1,2*(ICUT1-1))
      END IF
      IF (ICUT2.LT.NWVNO) THEN
       CALL VCLR(CFF(ICUT2+1,1),1,2*(NWVNO-ICUT2))
      END IF
      CALL RWDBUF(LUGRN)
      DO 4 JR=ICUT1,ICUT2
      DO 3 I=1,3
      IF (IOUT(I).EQ.0) GO TO 3
      CALL RDBUF(LUGRN,CFILE,2*IR)
      IF (IPAR1.EQ.I) then
       if (i.eq.1) then
        CC1=-CFILE(IDEP1)
       else
        CC1=CFILE(IDEP1)
       end if
      end if
      IF (IPAR2.EQ.I) then
       if (i.eq.1) then
        CC2=-CFILE(IDEP2)
       else
        CC2=CFILE(IDEP2)
       end if
      end if
 3    CONTINUE
      CFF(JR,1)=CC1*CONJG(CC2)
 4    CONTINUE
C
C
      IPL1=1
      IPL2=NWVNO
      IF (ICUT1.GT.2.OR.ICUT2.LT.NWVNO) THEN
        IPL1=NWVNO
        IPL2=0
c        DO 5 IHER=1,2
c 5      CALL HERMIT(IHER,CFF(1,1),NP,NWVNO,ICUT1,ICUT2,DLWVNO,PI,
        CALL CHERMIT(CFF(1,1),NWVNO,ICUT1,ICUT2,DLWVNO,
     1              WK0,IPL1,IPL2)
      END IF

      RETURN
      END
C



      SUBROUTINE GETGFC(IFILE,IDEP1,IPL1,IPL2)
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
c      LOGICAL NFLAG
      COMPLEX CBUFL(NRD),facsq
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      CHARACTER*6 CRTYP(4)
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/
      IF (ICUT1.GT.1) THEN
      DO 1 I=1,npar
      CALL VCLR(CFF(1,I),1,2*(ICUT1-1))
 1    CONTINUE
      END IF
      IF (ICUT2.LT.NWVNO) THEN
      DO 2 I=1,3
      CALL VCLR(CFF(ICUT2+1,I),1,2*(NWVNO-ICUT2))
 2    CONTINUE
      END IF
      CALL RWDBUF(IFILE)
      DO 4 JR=ICUT1,ICUT2
       if (icdr.eq.0) then
        facsq=sqrt(cmplx(wk0+(jr-1)*dlwvno,offima))
       else
        facsq=1e0
       end if
      DO 3 I=1,npar
      IF (IOUT(I).EQ.0) GO TO 3
      CALL RDBUF(IFILE,CBUFL,2*IR)
c for hydrophones use negative of normal stress
      if (ipar.eq.1) then
       CFF(JR,I)=-facsq*CBUFL(IDEP1)
      else
       CFF(JR,I)=facsq*CBUFL(IDEP1)
      end if
 3    CONTINUE
 4    CONTINUE
C
C
      IPL1=1
      IPL2=NWVNO
      IF (ICUT1.GT.2.OR.ICUT2.LT.NWVNO) THEN
       IPL1=NWVNO
       IPL2=0
       DO 6 I=1,npar
        IF (IOUT(I).GT.0) THEN
c        DO 5 IHER=1,2
c 5      CALL HERMIT(IHER,CFF(1,I),NP,NWVNO,ICUT1,ICUT2,DLWVNO,PI,
         CALL CHERMIT(CFF(1,I),NWVNO,ICUT1,ICUT2,DLWVNO,
     1              WK0,IPL1,IPL2)
        END IF
 6     CONTINUE
      END IF
      RETURN
      END
C


      SUBROUTINE NOISE(MFAC,SLEVEL,INTERP,corsns)
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'combes.f'
      LOGICAL INTERP,NEWGFP,corsns
      COMPLEX CARG,CINTPL,CC,KRINT
      COMPLEX ZETAIJ,VARTHI,VARTHJ,BESJ0,BESJ1,BESJ2
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      CHARACTER*6 CRTYP(4)
      CHARACTER*10 TYP(3)
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      DATA TYP /'PRESSURE  ','VERT.PART.','HOR.PART. '/
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/
C *** STATEMENT FUNCTION TO CALCULATE INDEX
      INDEX(III,JJJ)=III+(JJJ-1)*NRCV
      IRDCUR=0
      IRPCUR=0
      JRDCUR=0
      JRPCUR=0
C *** WAVENUMBER AND INTEGRATION FACTORS
      CALL VRAMP(WK0,DLWVNO,ARG,1,NWVNO)
      write(6,*) 'nwvno,mfac,lays(1),ak=',
     &            nwvno,mfac,lays(1),ak(lays(1),1)
      DO 10 JR=1,NWVNO
       IF (.not.corsns) THEN
        fac(jr)=arg(jr)
       else
        if (mfac.ne.1) then
         if (arg(jr).le.real(ak(lays(1),1))) then
c         write(6,*) 'jr,arg,ak=',jr,arg(jr),real(ak(lays(1),1))
         FAC(JR)=ARG(JR)*
     &          (1E0-(ARG(JR)/REAL(AK(LAYS(1),1)))**2)**(mfac-1)
         else
          fac(jr)=0e0
         END IF
        else
         FAC(JR)=arg(jr)
        end if
c        write(6,*) 'jr,fac=',jr,fac(jr)
       end if
 10   CONTINUE
c      write(6,*) 'NOICAL: loop 10 done'
      IF (MFAC.GT.0) THEN
       FACINT=SLEVEL*MFAC*DLWVNO
     &        *0.5E0/((SDC(1)-V(2,1))*REAL(AK(LAYS(1),1)))**2
      ELSE
       FACINT=SLEVEL*DLWVNO
     &        *8E0*PI/(REAL(AK(LAYS(1),1)))**2
      END IF
c      write(6,*) 'facint=',facint
      IF (INTERP) THEN
       DO 50 IRCV=1,NRCV
       DO 40 JRCV=IRCV,NRCV
        IJ=INDEX(IRCV,JRCV)
        JI=INDEX(JRCV,IRCV)
C
C     CORRELLATION OF RADIAL COMPONENTS SET TO ZERO TEMPORARILY
C
C       IF (((IRTYP(IRCV).EQ.2.OR.IRTYP(JRCV).EQ.3).OR.
C     &    (IRTYP(IRCV).EQ.3.OR.IRTYP(JRCV).EQ.2)).AND.
C     &    (JRCV.NE.IRCV).AND.(IRTYP(IRCV).NE.IRTYP(JRCV))) THEN
C       CORRNS(IJ)=CORRNS(IJ)+CNUL
C       CORRNS(JI)=CORRNS(JI)+CNUL
C       CC=CNUL
C      ELSE
       NEWGFP=(IRDCUR.NE.IDEP(IRCV)).OR.(IRPCUR.NE.IRCPAR(IRCV))
     &    .OR.(JRDCUR.NE.IDEP(JRCV)).OR.(JRPCUR.NE.IRCPAR(JRCV))
       IF (NEWGFP) THEN
         CALL GETGPR(IDEP(IRCV),IDEP(JRCV),IRCPAR(IRCV),IRCPAR(JRCV))
         CALL CRVMUL(CFF(1,1),2,FAC(1),1,CBUF(1),2,NWVNO)
         IRDCUR=IDEP(IRCV)
         IRPCUR=IRCPAR(IRCV)
         JRDCUR=IDEP(JRCV)
         JRPCUR=IRCPAR(JRCV)
       END IF
C
C *** HORIZONTAL DIFFERENCE BETWEEN RECEIVERS
       DELTR=SQRT((RAN(IRCV)-RAN(JRCV))**2+(TRAN(IRCV)-TRAN(JRCV))**2)
       ZETAIJ = ATAN2(TRAN(JRCV)-TRAN(IRCV),RAN(JRCV)-RAN(IRCV))
       IF (NEWGFP.OR.(.NOT.INTERP)) THEN
        IF (INTERP) THEN
          NIR=NRKMAX
          NIN=NRKMAX
          RSTP=RRMAX/(NIR-1.001)
          RST=0E0
          ONOR=1E0/RSTP
        ELSE
          NIR=1
          NIN=2
          RSTP=0E0
          RST=DELTR
          ONOR=1E0
          CFF(2,2)=CNUL
         END IF
         DO 30 III=1,NIR
          RACT=RST+(III-1)*RSTP
          DO 20 JR=1,NWVNO
           RK=RACT*ARG(JR)
           IF (BESINT) THEN
            BESJ0=RINTPL(BF0,brk_0,ONODRK,NRKMAX,RK)
            BESJ1=RINTPL(BF1,brk_0,ONODRK,NRKMAX,RK)
           ELSE
            CALL BESJ01(RK,BESJ0,BESJ1)
           END IF
					 IF (RK.GT.1E-10) THEN
					 	 BESJ2=2./RK*BESJ1-BESJ0
					 ELSE
					   BESJ2=0.
					 END IF
C           IF (IRTYP(IRCV).NE.2.AND.IRTYP(IRCV).NE.3) THEN
C            CFFS(JR)=CBUF(JR)*BESJ0
C           ELSE IF (RK.GT.1E-10) THEN
C            CFFS(JR)=2E0*CBUF(JR)*BESJ1/RK
C           ELSE
C            CFFS(JR)=CBUF(JR)
C           END IF 
	         VARTHI=0
           VARTHJ=0
           IF (IRTYP(IRCV).EQ.3) THEN
             VARTHI=PI*0.5
           END IF
           IF (IRTYP(JRCV).EQ.3) THEN
             VARTHJ=PI*0.5
           END IF
           IF (IRTYP(IRCV).EQ.1) THEN
             IF (IRTYP(JRCV).EQ.1) THEN
               KRINT=BESJ0
             END IF
             IF (IRTYP(JRCV).EQ.2.OR.IRTYP(JRCV).EQ.3) THEN
               KRINT=-COS(VARTHJ-ZETAIJ)*BESJ1
             END IF
             IF (IRTYP(JRCV).EQ.4) THEN
               KRINT=BESJ0
             END IF
           END IF
           IF (IRTYP(IRCV).EQ.2.OR.IRTYP(IRCV).EQ.3) THEN
             IF (IRTYP(JRCV).EQ.1) THEN
               KRINT=-COS(VARTHI-ZETAIJ)*BESJ1
             END IF
             IF (IRTYP(JRCV).EQ.2.OR.IRTYP(JRCV).EQ.3) THEN
             	  KRINT=COS(VARTHJ-VARTHI)*BESJ0
             	  KRINT=KRINT-COS(VARTHI+VARTHJ-2.*ZETAIJ)*BESJ2
             	 	KRINT=0.5*KRINT
             END IF
             IF (IRTYP(JRCV).EQ.4) THEN
               KRINT=-COS(VARTHI-ZETAIJ)*BESJ1
             END IF
           END IF
           IF (IRTYP(IRCV).EQ.4) THEN
             IF (IRTYP(JRCV).EQ.1) THEN
               KRINT=BESJ0
             END IF
             IF (IRTYP(JRCV).EQ.2.OR.IRTYP(JRCV).EQ.3) THEN
               KRINT=-COS(VATRTHJ-ZETAIJ)*BESJ1
             END IF
             IF (IRTYP(JRCV).EQ.4) THEN
               KRINT=BESJ0
             END IF
           END IF
           CFFS(JR)=CBUF(JR)*KRINT
 20       CONTINUE
C
C
          CALL VTRAPZ(FFS(1,1),2,RR,0,NWVNO,FACINT)
          CALL VTRAPZ(FFS(2,1),2,RI,0,NWVNO,FACINT)
          CFF(III,2)=CMPLX(RR,RI)
 30      CONTINUE
       END IF
       CC=GAIN(IRCV)*GAIN(JRCV)*CINTPL(CFF(1,2),RST,ONOR,NIN,DELTR)
       INDXNS=IRCV+(JRCV-1)*NRCV
       CORRNS(INDXNS)=CORRNS(INDXNS)+CC
       IF (JRCV.NE.IRCV) THEN
        INDXNS=JRCV+(IRCV-1)*NRCV
        CORRNS(INDXNS)=CORRNS(INDXNS)+CONJG(CC)
       END IF
C      END IF
 40   CONTINUE
 50   CONTINUE
      ELSE
C *** DIRECT SUMMATION
       write(6,*) 'Direct summation'
       CALL RWDBUF(LUGRN)
       DO 160 JR=ICUT1,ICUT2
        FF=FACINT*FAC(JR)
c        write(6,*) 'ff=',ff
        DO 110 I=1,NPAR
         IF (IOUT(I).GT.0) THEN
           CALL RDBUF(LUGRN,CFF(1,I),2*IR)
         END IF
 110    CONTINUE
        DO 150 IRCV=1,NRCV
        DO 140 JRCV=IRCV,NRCV
         GG=GAIN(IRCV)*GAIN(JRCV)
         IJ=INDEX(IRCV,JRCV)
         JI=INDEX(JRCV,IRCV)
C
C     CORRELLATION OF RADIAL COMPONENTS SET TO ZERO TEMPORARILY
C
C       IF (((IRTYP(IRCV).EQ.2.OR.IRTYP(JRCV).EQ.3).OR.
C     &    (IRTYP(IRCV).EQ.3.OR.IRTYP(JRCV).EQ.2)).AND.
C     &    (JRCV.NE.IRCV).AND.(IRTYP(IRCV).NE.IRTYP(JRCV))) THEN
C        CORRNS(IJ)=CORRNS(IJ)+CNUL
C        CORRNS(JI)=CORRNS(JI)+CNUL
C       ELSE
C
C *** HORIZONTAL DIFFERENCE BETWEEN RECEIVERS
        DELTR=SQRT((RAN(IRCV)-RAN(JRCV))**2+(TRAN(IRCV)-TRAN(JRCV))**2)
        ZETAIJ = ATAN2(TRAN(JRCV)-TRAN(IRCV),RAN(JRCV)-RAN(IRCV))
        RK=DELTR*ARG(JR)
        IF (BESINT) THEN
          BESJ0=RINTPL(BF0,brk_0,ONODRK,NRKMAX,RK)
          BESJ1=RINTPL(BF1,brk_0,ONODRK,NRKMAX,RK)
        ELSE
          CALL BESJ01(RK,BESJ0,BESJ1)
        END IF
				IF (RK.GT.1E-10) THEN
					BESJ2=2./RK*BESJ1-BESJ0
				ELSE
					BESJ2=0.
				END IF
        CC = CFF(IDEP(IRCV),IRCPAR(IRCV))*
     2       CONJG(CFF(IDEP(JRCV),IRCPAR(JRCV)))
C        IF (IRTYP(IRCV).NE.2.AND.IRTYP(IRCV).NE.3) THEN
C            CORRNS(IJ)=CORRNS(IJ)+FF*GG*CC*BESJ0
C        ELSE IF (RK.GT.1E-10) THEN
C            CORRNS(IJ)=CORRNS(IJ)+2E0*FF*GG*CC*BESJ1/RK
C        ELSE
C            CORRNS(IJ)=CORRNS(IJ)+FF*GG*CC
C        END IF 
	      VARTHI=0
	      VARTHJ=0
	      IF (IRTYP(IRCV).EQ.3) THEN
	        VARTHI=PI*0.5
	      END IF
	      IF (IRTYP(JRCV).EQ.3) THEN
	        VARTHJ=PI*0.5
	      END IF
	      IF (IRTYP(IRCV).EQ.1) THEN
	        IF (IRTYP(JRCV).EQ.1) THEN
	          KRINT=BESJ0
	        END IF
	        IF (IRTYP(JRCV).EQ.2.OR.IRTYP(JRCV).EQ.3) THEN
	          KRINT=-COS(VARTHJ-ZETAIJ)*BESJ1
	        END IF
	        IF (IRTYP(JRCV).EQ.4) THEN
	          KRINT=BESJ0
	        END IF
	      END IF
	      IF (IRTYP(IRCV).EQ.2.OR.IRTYP(IRCV).EQ.3) THEN
	      	IF (IRTYP(JRCV).EQ.1) THEN
	          KRINT=-COS(VARTHI-ZETAIJ)*BESJ1
	        END IF
	        IF (IRTYP(JRCV).EQ.2.OR.IRTYP(JRCV).EQ.3) THEN
	        	KRINT=0.5*COS(VARTHJ-VARTHI)*BESJ0
	        	KRINT=KRINT-0.5*COS(VARTHI+VARTHJ-2.*ZETAIJ)*BESJ2
	        END IF
	        IF (IRTYP(JRCV).EQ.4) THEN 
	        	KRINT=-COS(VARTHI-ZETAIJ)*BESJ1
	        END IF
	      END IF
	      IF (IRTYP(IRCV).EQ.4) THEN
	      	IF (IRTYP(JRCV).EQ.1) THEN
	      		KRINT=BESJ0
	      	END IF
	        IF (IRTYP(JRCV).EQ.2.OR.IRTYP(JRCV).EQ.3) THEN
	        	KRINT=-COS(VATRTHJ-ZETAIJ)*BESJ1
	        END IF
	        IF (IRTYP(JRCV).EQ.4) THEN
	        	KRINT=BESJ0
	        END IF
	      END IF
	      CORRNS(IJ)=CORRNS(IJ)+FF*GG*CC*KRINT
C       END IF
 140   CONTINUE
 150   CONTINUE
c       write(6,*) 'corrns=',corrns(1),corrns(2)
 160  CONTINUE
      DO 170 IRCV=1,NRCV
      DO 170 JRCV=IRCV+1,NRCV
       IJ=INDEX(IRCV,JRCV)
       JI=INDEX(JRCV,IRCV)
       CORRNS(JI)=CONJG(CORRNS(IJ))
 170  CONTINUE
      END IF
      RETURN
C
C     WHITE NOISE
      ENTRY WNOISE(WNLEV)
      DO 60 IRCV=1,NRCV
      IDIND=IRCV+(IRCV-1)*NRCV
      CORRNS(IDIND)=CORRNS(IDIND)+WNLEV
 60   CONTINUE
      RETURN
C
C     CLEAR NOISE CORRELATION MATRIX
C
      ENTRY CLNOIS
      CALL VCLR(CORRNS,1,2*NRCV*NRCV)
      RETURN
C
C     ENTRY FOR WRITING OUT ON BUFFER FILE CORRNS
C
      ENTRY WRNOIS
      if (debug) then
       WRITE(26,*) 'TOTAL NOISE CORR. MATRIX:'
       DO 70 IRCV=1,NRCV
       DO 69 JRCV=IRCV,NRCV
       CC=CORRNS(IRCV+(JRCV-1)*NRCV)
       AMPDB=1E1*LOG10(MAX(ABS(CC),1E-20))
       WRITE(26,100) IRCV,JRCV,CC,AMPDB,'dB',
     &         CRTYP(IRTYP(IRCV)),CRTYP(IRTYP(JRCV))
 100    FORMAT(1H ,2I5,2(1X,G13.6),1X,F8.1,1X,A2,3X,A6,3X,A6)
  69   CONTINUE
  70   CONTINUE
      end if
      CALL WRBUF(32,CORRNS,2*NRCV*NRCV)
      
c      do  80 ircv=1,nrcv
c       write(63,*) 'Receiver no.', ircv
c       do 80 jrcv=1,nrcv
c        indv=IRCV+(JRCV-1)*NRCV
c        indi=IRCV+(IRCV-1)*NRCV
c        indj=jRCV+(JRCV-1)*NRCV
c        coher=real(corrns(indv))
c     &        /sqrt(real(corrns(indi))*real(corrns(indj)))
c        write(63,*) coher
c 80     continue
      RETURN
      END
      SUBROUTINE DNCORR(ZSC,XSC,YSC,SLEVEL)
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      COMPLEX CC
      COMPLEX CARG,FILON
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      CHARACTER*6 CRTYP(4)
      CHARACTER*10 TYP(3)
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      DATA TYP /'PRESSURE  ','VERT.PART.','HOR.PART. '/
      DATA CRTYP /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/
C
      TERM=PI*0.25E0
      IF (IPRINT.NE.0) THEN
      WRITE(26,*) ZSC,' M SOURCE DEPTH'
      WRITE(26,*) XSC,' M SOURCE X-COORDINATE'
      WRITE(26,*) YSC,' M SOURCE Y-COORDINATE'
      WRITE(26,*) 10.*LOG10(SLEVEL),' dB SOURCE STRENGTH'
      END IF
      WKM=WK0+(NWVNO-1)*DLWVNO
      DO 50 IRCV=1,NRCV
      IF (IRCV.EQ.1) THEN
        CALL GETGFC(LUGRN,IDEP(IRCV),IPL1,IPL2)
      ELSE IF (IDEP(IRCV).NE.IDEP(IRCV-1)) THEN
        CALL GETGFC(LUGRN,IDEP(IRCV),IPL1,IPL2)
      ELSE
      END IF
C
C
      RANGEM=SQRT((RAN(IRCV)-XSC)**2+(TRAN(IRCV)-YSC)**2)
      RK=RANGEM*WKM
      IF (RK.GT.1E-3) THEN
        RST=TERM-WK0*RANGEM
        RSTP=-DLWVNO*RANGEM
        CALL VRAMP(RST,RSTP,ARG,1,NWVNO)
        CALL CVEXP(ARG,1,CBUF,2,NWVNO)
        RST=EXP(OFFIMA*RANGEM)
        FCI=GAIN(IRCV)*SQRT(SLEVEL)*FNI5*rst/SQRT(RANGEM)
        IF (IRTYP(IRCV).EQ.2) THEN
         FCI=FCI*(RAN(IRCV)-XSC)/RANGEM
        ELSE IF (IRTYP(IRCV).EQ.3) THEN
         FCI=FCI*(TRAN(IRCV)-YSC)/RANGEM
        ELSE
        END IF
        CFILE(IRCV)=FILON(CFF(1,IRCPAR(IRCV)),CBUF(1),ARG(1),NWVNO,FCI)
        if (ircpar(ircv).eq.3) then
          cfile(ircv) = cfile(ircv)*ai;
        end if
      ELSE IF (IRCPAR(IRCV).NE.3) THEN
        DO 18 I=1,NWVNO
        CBUF(I)=SQRT(CMPLX(WK0+(I-1)*DLWVNO,OFFIMA))
 18     CONTINUE
        FCI=GAIN(IRCV)*SQRT(SLEVEL)*DLWVNO
        CALL CVMUL(CFF(1,IRCPAR(IRCV)),2,CBUF,2,CFFS,2,NWVNO,1)
        CALL VTRAPZ(FFS(1,1),2,RR,0,NWVNO,FCI)
        CALL VTRAPZ(FFS(2,1),2,RI,0,NWVNO,FCI)
        CFILE(IRCV)=CMPLX(RR,RI)
      ELSE
        CFILE(IRCV)=CNUL
      END IF
C
C
 50   CONTINUE
      if (trfout) then
       write(6,*) '>>>>> Writing TRF file <<<<<'
      end if
      DO 80 IRCV=1,NRCV
       if (trfout) then
c       write(6,*)'ircv = ',ircv,'  cfile(ircv) = ',cfile(ircv)
        if (bintrf) then
         write(luttrf) real(cfile(ircv)),rimag(cfile(ircv))
        else
         write(luttrf,*) real(cfile(ircv)),rimag(cfile(ircv))
        end if
       end if
      if (debug) then
         write(91,*) ircv,real(cfile(ircv))/sqrt(slevel),
     &                    rimag(cfile(ircv))/sqrt(slevel)
      end if

      if (calnse) then
      IF (IPRINT.NE.0) THEN
        RR=REAL(cfile(ircv))
        RI=RIMAG(cfile(ircv))
         WRITE(26,110) IRCV,rr,ri,10E0*LOG10(RR*RR+RI*RI),'dB',
     &         CRTYP(IRTYP(IRCV))
      end if
      DO 70 JRCV=IRCV,NRCV
      CC=CFILE(IRCV)*CONJG(CFILE(JRCV))
      INDXNS=IRCV+(JRCV-1)*NRCV
      CORRNS(INDXNS)=CC+CORRNS(INDXNS)
      IF (JRCV.NE.IRCV) THEN
        INDXNS=JRCV+(IRCV-1)*NRCV
        CORRNS(INDXNS)=CONJG(CC)+CORRNS(INDXNS)
      END IF
      RR=REAL(CC)
      RI=RIMAG(CC)
      IF (IPRINT.NE.0) THEN
c      WRITE(26,100) RR,RI,5E0*LOG10(RR*RR+RI*RI)
      WRITE(26,100) IRCV,JRCV,RR,RI,5E0*LOG10(RR*RR+RI*RI),'dB',
     &         CRTYP(IRTYP(IRCV)),CRTYP(IRTYP(JRCV))
      WRITE(6,100) IRCV,JRCV,RR,RI,5E0*LOG10(RR*RR+RI*RI),'dB',
     &         CRTYP(IRTYP(IRCV)),CRTYP(IRTYP(JRCV))
      END IF
 100  FORMAT(1H ,2I5,2(1X,G13.6),1X,F8.1,1X,A2,3X,A6,3X,A6)
 110  FORMAT(1H ,I5,2(1X,G13.6),1X,F8.1,1X,A2,3X,A6)
 70   CONTINUE
      end if
 80   CONTINUE
      RETURN
      END

      SUBROUTINE SINIT
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'

      RLIND(1)=FLOAT(LAYS(1)*2-1)
c      RZUBUF(1)=-ZUS(1)
c      RZLBUF(1)=-ZLS(1)
c      CPHFAC(1)=1E0
C
C     DETERMINE SOURCE AND RECEIVER POINTERS 
C
      DO 350 I=1,NUML
        NOSOU(I)=0
        IFSOU(I)=NRD
        ILSOU(I)=1
 350  CONTINUE
      do 355 I=1,4
        NUMTS(I)=0
        NUMTR(I)=0
 355  CONTINUE
      DO 360 I=1,LS
        LL=LAYS(I)
        NOSOU(LL)=NOSOU(LL)+1
        IFSOU(LL)=MIN(IFSOU(LL),I)
        ILSOU(LL)=MAX(ILSOU(LL),I)
        IF (NOSOU(LL).NE.(ILSOU(LL)-IFSOU(LL)+1)) THEN
          WRITE(6,*) '*** PINIT1: Source no.',I,' out of order ***'
          STOP
        END IF
        LT=LAYTYP(LL)
        NUMTS(LT)=NUMTS(LT)+1
        NSPNT(NUMTS(LT),LT)=I
 360  CONTINUE
      DO 365 I=1,IR
        LL=LAY(I)
        LT=LAYTYP(LL)
        NUMTR(LT)=NUMTR(LT)+1
        NRPNT(NUMTR(LT),LT)=I
 365  CONTINUE
      RETURN
      END
C


      SUBROUTINE PLNINT(DLWVN1,WK0L,NWVN1,
     &                  RECD,IPAR,RGAIN,TITLE,XLEN,YLEN) 
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2
      CHARACTER*1 PAR(3)
      DATA PAR /'P','V','H'/      
      DATA OPT2 /'NINTGR'/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2
      GAIN=20E0*LOG10(RGAIN)
      RG=RGAIN*RGAIN
      RG2=RG*RG
      NN=NWVN1
      CALL vmov(CFF(1,1),2,CFFS,1,NN)
      CALL VSMUL(CFFS,1,RG,CFFS,1,NN)
      CALL VCLIP(CFFS,1,1E-20,1E30,CFFS,1,NN)
      CALL VALG10(CFFS,1,CFFS,1,NN)
      CALL VSMUL(CFFS,1,10E0,CFFS,1,NN)
      CALL VMAX(CFFS,1,YMAX,NN)
      YMIN=YMAX-80
C          
C XAXIS DEFINITION     
C
      XMAX=WK0L+(NWVN1-1)*DLWVN1                   
      XMIN=WK0L        
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C          
C  YAXIS DEFINITION               
C          
      CALL AUTOAX(YMIN,YMAX,YDOWN,YUP,YINC,YDIV,NYDIF)
      YDIV=1E0
      PTIT='SURFACE NOISE KERNEL'
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 812  FORMAT('RD:  ',F7.1,' m $')
 813  FORMAT('Par: ',4X,A1,'$')
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),812) RECD
      WRITE(LAB(3),813) PAR(IPAR)
      WRITE(XTXT,820) NXDIF
 820  FORMAT('Horizontal wavenumber (10**',I3,')$')
      WRITE(YTXT,821)
 821  FORMAT('Level (dB)$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(NN,WK0L,DLWVN1,0.,0.,CFFS,1,CFFS,1)
      RETURN                      
      END  



      SUBROUTINE PLNOIS(FREQL,TITLE,XLEN,YLEN) 
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
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'complo.f'
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      DATA OPTION /'OASNS ','NINTEN'/                    
C          
      OPTION(1)=PROGNM
C *** CALCULATE NOISE INTENSITY IN dB 
      CALL VALG10(CORRNS,2*(NRCV+1),CFFS,1,NRCV)
      CALL VSMUL(CFFS,1,10E0,CFFS,1,NRCV)      
C
C XAXIS DEFINITION     
C
      XMAX=NRCV
      XMIN=1     
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
      XDIV=1.0
C          
C  YAXIS DEFINITION               
C          
      CALL VMAX(CFFS,1,YMAX,NRCV)
      CALL VMIN(CFFS,1,YMIN,NRCV)
      CALL AUTOAX(YMIN,YMAX,YDOWN,YUP,YINC,YDIV,NYDIF)
      YDIV=1E0
      PTIT='NOISE INTENSITY'
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
      NLAB=1
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(XTXT,820) 
 820  FORMAT('Receiver number$')
      WRITE(YTXT,821)
 821  FORMAT('Level (dB)$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(NRCV,1.0,1.0,0.,0.,CFFS,1,CFFS,1)
      RETURN                      
      END  

      SUBROUTINE PLNFSP(IRCV,F0,DLF,NFR,TITLE,XLEN,YLEN) 
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
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'complo.f'
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      DATA OPTION /'OASNS ','NIFRSP'/
C
      IOF=IRCV+(IRCV-1)*NRCV           
      CALL RWDBUF(32)                 
      DO 2401 IFR=1,NFR   
       CALL RDBUF(32,CORRNS,2*NRCV*NRCV)
       CFFS(IFR) = CORRNS(IOF)
 2401 CONTINUE
      CALL VALG10(CFFS,2,CFFS,1,NFR)
      CALL VSMUL(CFFS,1,10E0,CFFS,1,NFR)
C          
C XAXIS DEFINITION     
C
      XMIN=F0
      XMAX=F0+(NFR-1)*DLF
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
      XDIV=1.0
C          
C  YAXIS DEFINITION               
C          
      CALL VMAX(CFFS,1,YMAX,NFR)
      CALL VMIN(CFFS,1,YMIN,NFR)
      CALL AUTOAX(YMIN,YMAX,YDOWN,YUP,YINC,YDIV,NYDIF)
      YDIV=1E0
      PTIT='NOISE SPECTRUM'
 810  FORMAT('Rec. no.:',I3,' $')
      NLAB=1
      WRITE(LAB(1),810) IRCV
      WRITE(XTXT,820) 
 820  FORMAT('Frequency (Hz)$')
      WRITE(YTXT,821)
 821  FORMAT('Intensity (dB)$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(NFR,F0,DLF,0.,0.,CFFS,1,CFFS,1)
      RETURN
      END
      SUBROUTINE NOIVFW(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT 
     $,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN            
     $,ZMAX,ZSTEP,FREQ,SD,RECUP,RECLO,X1,XL,IPARM)               
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SECTOR(28)                   
      CHARACTER*50 FILENM
      character*3 parc(3)
      CHARACTER*4 TITLE(20),TITLEX(20),TITLEY(20)    
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,      
     *NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/       
      DATA DUMMY /0./
      DATA TITLEX /'Slow','ness',' (s/','km) ',16*'    '/
      DATA TITLEY /'Freq','uenc','y (H','z)  ',16*'    '/
      DATA PARC   /'PRS','VER','HOR'/
C          
C   FORMATS
 401  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')           
 402  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')           
403   FORMAT(1H ,F15.4,3X,'  DIVX ' )                   
404   FORMAT(1H ,F15.4,3X,'  DIVY ' )                   
405   FORMAT(1H ,F15.4,3X,'  FLAGRC ' )                   
406   FORMAT(1H ,F15.4,3X,'  RDUP ' )                   
407   FORMAT(1H ,F15.4,3X,'  RDLO ' )         
408   FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )        
 409  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )         
 410  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )         
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )           
  412 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  413 FORMAT(1H ,F15.4,3X,'  CAY ' )                   
  414 FORMAT(1H ,F15.4,3X,'  NRNG ' )                   
  415 FORMAT(1H ,F15.4,3X,'  ZMIN ' )                    
  416 FORMAT(1H ,F15.4,3X,'  ZMAX ' )                    
  417 FORMAT(1H ,F15.4,3X,'  ZINC ' )                    
  418 FORMAT(1H ,F15.4,3X,'  X ORIGIN OF PLOT IN INCHES ' )                     
  419 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  420 FORMAT(1H ,F15.4,3X,'  Y ORIGIN OF PLOT IN INCHES ' )                     
  421 FORMAT(1H ,F15.4,3X,'  NSM   ' )                   
  422 FORMAT(1H ,F15.4,3X,'  HGTPT ' )                   
  423 FORMAT(1H ,F15.4,3X,'  HGTC ' )                    
  424 FORMAT(1H ,F15.4,3X,'  LABPT ' )                   
  425 FORMAT(1H ,F15.4,3X,'  NDIV ' )                    
  426 FORMAT(1H ,F15.4,3X,'  NARC ' )                    
  427 FORMAT(1H ,F15.4,3X,'  LABC ' )                    
  428 FORMAT(1H ,F15.4,3X,'  LWGT ' )                    
  800 FORMAT('AMBDR,',A3,',FMT,REV')         
 801  FORMAT(A50)            
  850 FORMAT(20A4)                
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,            
     *'   XSCALE',/,F15.4,4X,'  XINC')                   
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,               
     *'   YSCALE',/,F15.4,4X,'  YINC')                   
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')                     
      WRITE(28,800) PARC(IPARM)         
      WRITE(28,850)TITLE          
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX               
C          
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST                   
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR                  
       SECTOR(4)=1.0              
      WRITE(29,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6G13.5)
      INQUIRE(UNIT=29,NAME=FILENM)
      WRITE(28,801) FILENM         
      DIVX=1E0
      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(28,850)TITLEX         
      R1=X1*1.0E3                 
      R2=XL*1.0E3                 
      WRITE(28,950)R1,R2          
      AX1=XLEFT*1.0E3             
      AX2=XRIGHT*1.0E3            
      AX3=XSCALE*1.0E3            
      AX4=XINC*1.0E3              
      WRITE(28,900)AX1,AX2,AX3,AX4
      WRITE(28,850)TITLEY         
      WRITE(28,901)YUP,YDOWN,YSCALE,YINC                 
      WRITE(28,401)FLOAT(NPX)
      WRITE(28,402)FLOAT(NPY)
      WRITE(28,403)DIVX          
      WRITE(28,404)DIVY          
      WRITE(28,405)FLAGRC          
      WRITE(28,406)RECUP          
      WRITE(28,407)RECLO                 
      WRITE(28,408)SD 
C   NUMBER OF GRID POINTS ALONG THE X AXIS               
      WRITE(28,409)FLOAT(NX)      
C   NUMBER OF GRID POINTS ALONG THE Y AXIS               
      WRITE(28,410)FLOAT(NY)      
      WRITE(28,411)FREQ  
      WRITE(28,412)DUMMY          
      WRITE(28,413)CAY          
      WRITE(28,414)FLOAT(NRNG)          
      WRITE(28,415)ZMIN           
      WRITE(28,416)ZMAX           
      WRITE(28,417)ZSTEP          
C X ORIGIN  OF PLOT IN INCHES     
      WRITE(28,418)X1PL           
      WRITE(28,419)DUMMY          
C Y ORIGIN  OF PLOT IN INCHES     
      WRITE(28,420)Y1PL           
      WRITE(28,421)FLOAT(NSM)            
      WRITE(28,422)HGTPT          
      WRITE(28,423)HGTC           
      WRITE(28,424)FLOAT(LABPT)   
      WRITE(28,425)FLOAT(NDIV)    
      WRITE(28,426)FLOAT(NARC)    
      WRITE(28,427)FLOAT(LABC)    
      WRITE(28,428)FLOAT(LWGT)    
      RETURN                      
      END  

      SUBROUTINE AUTSMN(C1,C2,RMIN,RMAX,CMIN,CMAX,NW,IC1,IC2)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (RFAC=3.0)
      PARAMETER (NR=10,NKMIN=2**8)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
C *** DETERMINE WAVENUMBER SAMPLING INTERVAL
      DK=2*PI/(RFAC*RMAX*1E3)
      CREFA=1500.0
      DK=MIN(DK,2*PI*FREQ/(CREFA*NKMIN))
C *** MINIMUM WAVENUMBER INTERVAL
      DR=1E3*(RMAX-RMIN)/(NR-1)
      WM=2*PI/DR
C *** INTEGRATION LIMITS
      WN1=2*PI*FREQ/C2
      WN2=2*PI*FREQ/C1
      WNMAX=MAX(WM,1.1*(WN2-WN1)+WN1)
      WNMIN=MIN(WNMAX-WM,WN1-0.1*(WN2-WN1))
      WNMIN=MAX(WNMIN,DK*0.5)
C *** NUMBER OF WAVENUMBER SAMPLING POINTS
       NW1=(WNMAX-WNMIN)/DK+1
       NW=NKMIN/2
 1     NW=NW*2
       IF (NW.LT.NW1) GO TO 1
       WNMAX=WNMIN+(NW-1)*DK
       IC1=(WN1-WNMIN)/(WNMAX-WNMIN)*(NW-1)+1
       IC2=(WN2-WNMIN)/(WNMAX-WNMIN)*(NW-1)+1
       IC1=MAX(1,IC1)
       IC2=MIN(NW,IC2)
       CMIN=2*PI*FREQ/WNMAX
       CMAX=2*PI*FREQ/WNMIN
       RETURN
       END



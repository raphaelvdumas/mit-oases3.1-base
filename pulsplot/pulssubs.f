      SUBROUTINE SPULSE(FREQS)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      if (istyp.eq.-1) then
       CALL CVFILL(CMPLX(1E0,0E0)/(pi*(fmax-fmin)),CFFS,2,NX/2)
      ELSE
       CALL STERM(FREQS,DT,NX)
       CALL RFFT(CFFS,NX,1)
       CALL RFFTSC(CFFS,NX,1,1)
      END IF
      CALL CVFILL(CNUL,CFF,2,NX/2)
C     CALL HANN(FFS(1,LX),2,FFS(1,LX),2,MX-LX+1,1)
C     CALL HANN(FFS(2,LX),2,FFS(2,LX),2,MX-LX+1,1)
      CALL CVMOV(CFFS(LX),2,CFF(LX,1),2,MX-LX+1)
      IF (LX.EQ.2) CFF(1,1)=CFFS(1)
      if (deconv) then
c
c Shift to base band
c
       mf=nx/2
       mfh=mf/2
       mshift=2*pi*frqdec/domega
       call vclr(cff(1,2),1,2*nx)
       npsh=min(mfh,mf-mshift)
       call vmov(cff(mshift+1,1),1,cff(1,2),1,2*npsh)
       npsh=min(mfh,mshift)
       call vmov(cff(mshift+1-npsh,1),1,cff(2*mf-npsh+1,2),1,2*npsh)
       call cfft(cff(1,2),nx,-1)
       call cvmags(cff(1,2),2,cff(1,1),1,nx)
       call vsqrt(cff(1,1),1,cff(1,1),1,nx)
       call vsmul(cff(1,1),1,2.0,cff(1,1),1,nx)
      else
       CALL RFFT(CFF(1,1),NX,-1)
      end if
      RETURN
      END
      SUBROUTINE PULSE(FREQSL,DTL,NXL)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      REAL FFS(NP2)
      EQUIVALENCE (CFFS(1),FFS(1))

      if (istyp.eq.-1) then
       CALL CVFILL(CMPLX(1E0,0E0)/(pi*(fmax-fmin)),CFFS,2,NXL/2)
      ELSE
       CALL STERM(FREQSL,DTL,NXL)
       CALL RFFT(CFFS,NXL,1)
       CALL RFFTSC(CFFS,NXL,1,1)
      END IF
      RETURN
      END
      SUBROUTINE STERM(FREQS,DELTAT,NXL)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      DIMENSION FF(NP2),AA(3)
      EQUIVALENCE (CFFS(1),FF(1))
      DATA AA /-.48829,.14128,-.01168/
      IF (ISTYP.GT.0) THEN
      TPF=2.0*PI*FREQS                  
      IF (ISTYP.NE.2) THEN
      OFR=1.0/FREQS       
      ELSE
      OFR=1.55/FREQS
        SUM0=-AA(1)-4*AA(2)-9*AA(3)
      END IF              
      FF(1)=0.0     
      DO 25 M=2,NXL  
      TM=(M-1)*DELTAT                   
      FF(M)=0.0     
      GOTO (10,20,30,40,50,60),ISTYP          
 10   IF (TM.LE.OFR) FF(M)=.75-COS(TM*TPF)+.25*COS(2*TM*TPF)
      GO TO 25      
 20   IF (TM.LE.OFR) THEN
        SUM=0.
        DO 21 IHH=1,3
        SUM=SUM-AA(IHH)*IHH*IHH*COS(2*PI/OFR*IHH*TM)
 21     CONTINUE
        FF(M)=SUM
      END IF
      GO TO 25      
 30    IF (TM.LE.OFR) FF(M)=SIN(TPF*TM) 
      GO TO 25      
 40   IF (TM.LE.(4*OFR)) FF(M)=SIN(TPF*TM)*.5*(1-COS(TPF*TM/4.))          
      GO TO 25
 50   IF (TM.LE.OFR) FF(M)=SIN(TPF*TM)-.5*SIN(2*TPF*TM)
      go to 25
 60   nper=nint((freqs*4.0)/(fmax-fmin))
      IF (TM.LE.(nper*OFR)) FF(M)=SIN(TPF*TM)*.5*(1-COS(TPF*TM/nper))          
 25   CONTINUE            
      ELSE
        CALL VCLR(FF,1,NXL)
        write(6,*) '>>> Reading source pulse <<<'
        REWIND(66,ERR=61)
        READ(66,*,END=61) (FF(M),M=1,NX)
 61     CONTINUE
      END IF
c >>> Band-limit and exponential decay
      if (omegim.ne.0e0) then
       CALL RFFT(CFFS,NXL,1)
       CALL RFFTSC(CFFS,NXL,1,1)
       if (lx.gt.1) call cvfill(cnul,cffs,2,lx-1)
       if (mx.lt.nx/2) call cvfill(cnul,cffs(mx+1),2,nx/2-mx)
       call rfft(cffs,nxl,-1) 
       do i=1,nx
        ff(i)=ff(i)*exp(omegim*(i-1)*deltat)
       end do
      end if
      RETURN        
      END      
     
      SUBROUTINE TIMSPU(I,IPACT,DOMEGA,TSHIFT,JR,JD,THETA,NX,LX,MX)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      REAL FC(MMAX)
      IF (MSUFT.GT.MMAX) STOP '>>> MMAX TOO SMALL IN compar.f <<<'
      FC(1)=1E0
      do M=2,MSUFT,2
       MORD=M/2
c >>> reverse cos and sin terms for transverse components
       IF (IPACT.EQ.4.or.IPACT.eq.8) THEN
         FC(M)=SIN(MORD*THETA)
         FC(M+1)=-COS(MORD*THETA)
       ELSE
         FC(M)=COS(MORD*THETA)
         FC(M+1)=SIN(MORD*THETA)
       END IF
      end do

      CALL CVFILL(CNUL,CFF,2,LX-1)
      CALL CVFILL(CNUL,CFF(MX+1,1),2,NX/2-MX)
      CALL RWDBUF(31)
      DO J=LX,MX       
       do JJ=1,IR
        CALL RDBUF(31,CFILE,2*NOUT*NPLOTS*MSUFT)
        IF (JJ.EQ.JD) THEN
         CFF(J,1)=CNUL
         do M=1,MSUFT
          CFF(J,1)=CFF(J,1)+FC(M)*CFILE(JR+((I-1)+NOUT*(M-1))*NPLOTS)
         end do
        END IF
       end do
       FAC(J)=J-1
      end do
      CALL VMUL(FAC(LX),1,DOMEGA,0,ARG(LX),1,NUMFR)
      CALL VMUL(ARG(LX),1,TSHIFT,0,ARG(LX),1,NUMFR)
      CALL CVEXP(ARG(LX),1,CBUF(LX),2,NUMFR)
      CALL CVMUL(CBUF(LX),2,CFFS(LX),2,CBUF(LX),2,NUMFR,1)
      CALL CVMUL(CFF(LX,1),2,CBUF(LX),2,CFF(LX,1),2,NUMFR,1)
C
      IF (LX.GT.2.OR.MX.LT.NX/2) THEN
       IPL1=NX/2
       IPL2=0
       CALL CHERMIT(CFF(1,1),NX/2,LX,MX,DOMEGA,
     1              0.0,IPL1,IPL2)
      END IF
C
      if (deconv) then
c
c Shift to base band
c
       mf=nx/2
       mfh=mf/2
       mshift=2*pi*frqdec/domega
       call vclr(cff(1,2),1,2*nx)
       npsh=min(mfh,mf-mshift)
       call vmov(cff(mshift+1,1),1,cff(1,2),1,2*npsh)
       npsh=min(mfh,mshift)
       call vmov(cff(mshift+1-npsh,1),1,cff(2*mf-npsh+1,2),1,2*npsh)
       call cfft(cff(1,2),nx,-1)
       call cvmags(cff(1,2),2,cff(1,1),1,nx)
       call vsqrt(cff(1,1),1,cff(1,1),1,nx)
       call vsmul(cff(1,1),1,2.0,cff(1,1),1,nx)
      else
       CALL RFFT(CFF(1,1),NX,-1)
      end if
      IF (OMEGIM.NE.0E0) THEN
        RST=-OMEGIM*TSHIFT
        RSTP=-OMEGIM*2.*PI/(NX*DOMEGA)
        CALL VRAMP(RST,RSTP,ARG,1,NX)
        CALL VEXP(ARG,1,FAC,1,NX)
        CALL VMUL(CFF(1,1),1,FAC,1,CFF(1,1),1,NX)
      END IF
      RETURN
      END
      SUBROUTINE CHERMIT(CFF,LS,ICUT1,ICUT2,DLWVNO,WK0,   
     *IPLOT1,IPLOT2)
C
C *** COMPLEX HERMITE EXTRAPOLATOR
C     HS 89/10/3
      COMPLEX CFF(LS)
      IPLOT1=ICUT1                
      NCUT=min(100,NINT(LS*.05))
      IF(ICUT1.LE.2)   GO TO 4000       
      IPL1=MAX0(2,ICUT1-NCUT)           
      IPL2=ICUT1-1  
C                   
C   A AND B ARE THE LOWEST AND THE HIGHEST WAVE NUMBERS OF INTEREST             
      A=(WK0+(IPL1-1)*DLWVNO)     
      B=(WK0+(ICUT1-1)*DLWVNO)    
      FA=0.0        
      FB=ABS(CFF(ICUT1))
      F1A=0.0       
      F1B=(ABS(CFF(ICUT1+1))-ABS(CFF(ICUT1)))/DLWVNO         
C *** PHASES
      RR=REAL(CFF(ICUT1))
      RI=AIMAG(CFF(ICUT1))
      PB=ATAN2X(RI,RR)
      RR=REAL(CFF(ICUT1+1))
      RI=AIMAG(CFF(ICUT1+1))
      PB2=ATAN2X(RI,RR)
      DP=PB2-PB
c *** f1b must be positive
      F1B=AMAX1(0E0,F1B)
C                   
C   THE FOLLOWING STATEMENT CHECKS THE CONDITION THAT THE HERMITE               
C   FUNCTION DOES NOT CHANGE SIGN IN THE INTERVAL A-B       
      IF(((3.0*FB)/(B-A)).GE.F1B)   GO TO 1000
C   WE INCREASE A TO SATISFY THE ABOVE CONDITION            
      A=B-3*FB/F1B  
      IPL1=1.5+(A-WK0)/DLWVNO    
 1000 CONTINUE      
C *** changed 89/10/3 HS
      C0=0
      C1=0
      C2=FB
      C3=(F1B*(B-A)-2E0*FB)                 
C **********************
      DO 3000   II=IPL1,IPL2            
      X = (WK0 + FLOAT(II-1) * DLWVNO)               
C   IN THIS CASE P1 IS ALWAYS ZERO (FA=0,F1A=0)             
C     P1=((X-B)/(B-A))**2*((1.0+2.0*(X-A)/(B-A))*FA+(X-A)*F1A)             
C      P2=((X-A)/(B-A))**2*((1.0-2.0*(X-B)/(B-A))*FB+(X-B)*F1B)            
C     FF(I,II)=P1+P2
      XB=(X-B)/(B-A)
      XA=(X-A)/(B-A)
      P2=XA*XA*(C2+C3*XB)
      CFF(II)=P2*CMPLX(COS(PB+(II-ICUT1)*DP),SIN(PB+(II-ICUT1)*DP))   
 3000 CONTINUE      
       IPLOT1=IPL1  
C                   
C                   
 4000 CONTINUE      
      IPLOT2=ICUT2  
      IF(ICUT2.EQ.LS)   RETURN          
      IPL1=ICUT2+1  
      IPL2=MIN0(LS,ICUT2+NCUT)          
C                   
C   A AND B ARE THE LOWEST AND THE HIGHEST WAVE NUMBERS OF INTEREST             
      A = (WK0 + FLOAT(ICUT2-1) * DLWVNO) 
      B = (WK0+(IPL2-1)*DLWVNO)     
      FA=abs(CFF(ICUT2))
      F1A=(ABS(CFF(ICUT2))-ABS(CFF(ICUT2-1)))/DLWVNO         
      FB=0.0        
      F1B=0
C *** PHASES
      RR=REAL(CFF(ICUT2))
      RI=AIMAG(CFF(ICUT2))
      PA=ATAN2X(RI,RR)
      RR=REAL(CFF(ICUT2-1))
      RI=AIMAG(CFF(ICUT2-1))
      PA2=ATAN2X(RI,RR)
      DP=PA-PA2
C *** F1A MUST BE NEGATIVE
      F1A=AMIN1(F1A,0E0)
C                   
C   THE FOLLOWING STATEMENT CHECKS THE CONDITION THAT THE HERMITE               
C   FUNCTION DOES NOT BECOME NEGATIVE IN THE INTERVAL A-B   
      IF(((3.0*FA)/(B-A)).GE.-F1A)   GO TO 5000                   
C   WE DECREASE B TO SATISFY THE ABOVE CONDITION            
      B=(-3.0*FA)/F1A+A                 
      IPL2=1+IFIX((B-WK0)/DLWVNO)
 5000 CONTINUE      
C                   
C *** changed 89/10/3 HS
      C0=FA
      C1=F1A*(B-A)
      C2=(-F1A*(B-A)-FA)
      C3=(F1A*(B-A)+2E0*FA)                 
C **********************
      DO 7000   II=IPL1,IPL2            
      X = (WK0 + FLOAT(II-1) * DLWVNO)
C      P1=((X-B)/(B-A))**2*((1.0+2.0*(X-A)/(B-A))*FA+(X-A)*F1A)    
C   IN THIS CASE P2 IS ALWAYS ZERO (FB=0,F1B=0)             
C     P2=((X-A)/(B-A))**2*((1.0-2.0*(X-B)/(B-A))*FB+(X-B)*F1B) 
C     FF(I,II)=P1+P2
      XB=(X-B)/(B-A)
      XA=(X-A)/(B-A)
      P1=C0+XA*(C1+XA*(C2+C3*XB))
      CFF(II)=P1*CMPLX(COS(PA+(II-ICUT2)*DP),SIN(PA+(II-ICUT2)*DP))      
 7000 CONTINUE      
      IPLOT2=IPL2   
C     WRITE(6,100) ICUT1,ICUT2,IPLOT1,IPLOT2                
C     WRITE(6,300)  
      RETURN        
      END           
      FUNCTION ATAN2X(RR,RI)
       IF (RR.EQ.0.0.AND.RI.EQ.0.0) THEN
        ATAN2X=0E0
       ELSE
        ATAN2X=ATAN2(RR,RI)
       END IF
      RETURN
      END
      SUBROUTINE PLPUPU(NX,LF0,LF,TMIN,DT,TITLE,INR,PX,MODU,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,RANGE,SD,RD,C0,THETA)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION X(NP2,NPAR),PX(MODU)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(12)
      CHARACTER*80 TXT
      CHARACTER*3  XTYP,YTYP
      CHARACTER*16 LAB
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA XTYP,YTYP /'LIN','LIN'/
      DATA OPT2 /'RPULSE','ZPULSE','WPULSE','UPULSE','VPULSE',
     &           'XPULSE','KPULSE',2*'      ',
     &           'MPULSE','DPULSE','FPULSE'/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR+1)
      I=INR
      IF (INR.EQ.0) I=1
      YUP=0      
      DO 34 J=LF0,LF                    
 34   YUP=AMAX1(YUP,ABS(X(J,1)))
      if (log_ts) then
       ILOG=IFIX(ALOG10(YUP))          
       IF (YUP.LT.1) ILOG=ILOG-1       
       YUP=20.0*(ilog+1)
       YDOWN=YUP-80.0
       YINC=10
       NDIFF=0
       YDIV=1          
      else
       ILOG=IFIX(ALOG10(YUP))          
       IF (YUP.LT.1) ILOG=ILOG-1       
       IFAC=IFIX(YUP/10.**ILOG)+1      
       YUP=IFAC*10.**ILOG              
       YDOWN=-YUP 
       YFAC=IFAC
       IF (IFAC.EQ.1) YFAC=5           
       IF (IFAC.EQ.2) YFAC=4           
       IF (IFAC.GT.5) YFAC=YFAC/2.      
       YINC=IFAC*10.**ILOG/YFAC
       NDIFF=ILOG-1                    
       YDIV=10.**(-NDIFF)              
      end if
      WRITE(19,777) OPTION,PLOTOPT
      IF (INR.EQ.0) THEN
      TXT='SOURCE PULSE'
      ELSE
      TXT='RECEIVED SIGNAL'
      END IF
      WRITE(19,778) TXT
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6,',',A)
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 811  FORMAT('SD:',F8.1,' m$')
 812  FORMAT('RD:',F8.1,' m$')
 813  FORMAT('R: ',F8.1,' km$')
 814  FORMAT('AZ:',F8.1,' dg$')
      NLAB=4
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'

      if (abs(range).lt.1.0) then
       write(lab,'(a,f6.1,a)') 'Range:',1e3*range,' m$'
      else
       write(lab,'(a,f6.1,a)') 'Range:',range,' km$'
      end if

      WRITE(19,779) LAB
      WRITE(LAB,811) SD
      WRITE(19,779) LAB
      WRITE(LAB,812) RD
      WRITE(19,779) LAB
      WRITE(LAB,814) THETA
      WRITE(19,779) LAB
      WRITE(19,6020) XLEN,'XLEN'
      WRITE(19,6020) YLEN,'YLEN'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) 1.,'XDIV'
      IF (C0.GT.1E-10.AND.INR.NE.0) THEN
      WRITE(TXT,830) C0*1E-3
 830  FORMAT('Reduced time t-r/',F5.3,' (secs.)$')
      ELSE
      TXT='Time (seconds)$'
      END IF
      WRITE(19,778) TXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) YDIV,'YDIV'
      if (log_ts) then
      GOTO(901,902,903,904,905,906,907,908,909),INR+1
 901  WRITE(TXT,911)
 911  FORMAT('Pressure (dB//Pa)$')
      go to 920
 902  WRITE(TXT,912)
 912  FORMAT('Normal stress (dB//Pa)$')
      go to 920
 903  WRITE(TXT,913)
 913  FORMAT('Vert. particle vel. (dB//m/s)$')
      GO TO 920
 904  WRITE(TXT,914) 
 914  FORMAT('Rad. particle vel. (dB//m/s)$')
      GOTO 920
 905  WRITE(TXT,915) 
 915  FORMAT('Tan. particle vel. (dB//m/s)$')
      go to 920
 906  WRITE(TXT,916) 
 916  FORMAT('Radial stress (dB//Pa)$')
      go to 920
 907  WRITE(TXT,917) 
 917  FORMAT('Bulk pressure (dB//Pa)$')
      go to 920
 908  WRITE(TXT,918) 
 918  FORMAT('Rad. shear stress (dB//Pa)$')
      GOTO 920
 909  WRITE(TXT,919) 
 919  FORMAT('Tan. shear stress (dB//Pa)$')
 920  continue

      else

      GOTO(1001,1002,1003,1004,1005,1006,1007,1008,1009),INR+1
 1001  WRITE(TXT,1011) NDIFF
 1011  FORMAT('Pressure (10**',I3,' Pa)$')
      go to 1020
 1002  WRITE(TXT,1012) NDIFF
 1012  FORMAT('Normal stress (10**',I3,' Pa)$')
      go to 1020
 1003  WRITE(TXT,1013) NDIFF
 1013  FORMAT('Vert. particle vel. (10**',I3,' m/s)$')
      GO TO 1020
 1004  WRITE(TXT,1014) NDIFF
 1014  FORMAT('Rad. particle vel. (10**',I3,' m/s)$')
      GOTO 1020
 1005  WRITE(TXT,1015) NDIFF
 1015  FORMAT('Tan. particle vel. (10**',I3,' m/s)$')
      go to 1020
 1006  WRITE(TXT,1016) NDIFF
 1016  FORMAT('Radial stress (10**',I3,' Pa)$')
      go to 1020
 1007  WRITE(TXT,1017) NDIFF
 1017  FORMAT('Bulk pressure (10**',I3,' Pa)$')
      go to 1020
 1008  WRITE(TXT,1018) NDIFF
 1018  FORMAT('Rad. shear stress (10**',I3,' Pa)$')
      GOTO 1020
 1009 WRITE(TXT,1019) NDIFF
 1019  FORMAT('Tan. shear stress (10**',I3,' Pa)$')
      end if
 1020 WRITE(19,778) TXT
      WRITE(19,780) YTYP
      WRITE(19,6010) 1,'NC'
      WRITE(19,6010) LF-LF0+1,'N'
      WRITE(19,6020) TMIN,'TMIN'
      WRITE(19,6030) DT,'DT'
      WRITE(19,6030) 0.,'YMIN'
      WRITE(19,6020) 0.,'DY'
 6010 FORMAT(1H ,I8,8X,A30)
 6020 FORMAT(1H ,F15.6,1X,A30)
 6030 FORMAT(1H ,G15.6,1X,A30)
      DO 1350 K=LF0,LF,MODU           
      I1=K       
      I2=I1+MODU-1                  
      I2=MIN0(I2,LF)                  
      JK=0
      DO 1300 J=I1,I2                 
      JK=JK+1
      if (log_ts) then
       PX(JK)=20.0*alog10(max(X(J,1),1E-20))
      else
       PX(JK)=X(J,1)
      end if
 1300 CONTINUE   
      WRITE(20,444)(PX(J),J=1,JK)         
 444  FORMAT(1H ,6G13.5)
1350  CONTINUE   
      RETURN
      END
      SUBROUTINE PLSTACK(TITLE,INR,XLEN,YLEN,
     1    XLEFT,XRIGHT,XINC,YDOWN,YUP,YINC,
     2    NC,SD,RD,C0,theta,iraopt)
      INCLUDE 'compar.f'
      INCLUDE 'complo.f'
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(12)
      CHARACTER*80 TXT
      CHARACTER*3  XTYP,YTYP
      CHARACTER*16 LAB
      DATA XTYP,YTYP /'LIN','LIN'/
      DATA OPT2 /'RPSTCK','ZPSTCK','WPSTCK','UPSTCK','VPSTCK',
     &           'XPSTCK','KPSTCK',2*'      ',
     &           'MPSTCK','DPSTCK','FPSTCK'/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR+1)
      I=INR
      IF (INR.EQ.0) I=1
      WRITE(19,777) OPTION,PLOTOPT
      IF (INR.EQ.2) THEN
        TXT='VERTICAL PARTICLE VELOCITY'
      ELSE IF (INR.EQ.3) THEN
        TXT='RADIAL PARTICLE VELOCITY'
      ELSE IF (INR.EQ.4) THEN
        TXT='TANGENTIAL PARTICLE VELOCITY'
      ELSE IF (INR.EQ.5) THEN
        TXT='RADIAL STRESS'
      ELSE IF (INR.EQ.6) THEN
        TXT='BULK PRESSURE'
      ELSE IF (INR.EQ.7) THEN
        TXT='RADIAL SHEAR STRESS'
      ELSE IF (INR.EQ.8) THEN
        TXT='TANG. SHEAR STRESS'
      else
        TXT='NORMAL STRESS'
      END IF
      WRITE(19,778) TXT
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6,',',A)
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 811  FORMAT('SD:',F8.1,' m$')
 812  FORMAT('RD:',F8.1,' m$')
 813  FORMAT('AZ:',F8.1,' dg$')
 814  FORMAT('R: ',F8.1,' km$')
      NLAB=3
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'
      WRITE(LAB,811) SD
      WRITE(19,779) LAB
      WRITE(LAB,812) RD
      WRITE(19,779) LAB
      if (iraopt.eq.1) then
       WRITE(LAB,813) theta
      else if (iraopt.eq.2) then
       if (theta.lt.1.0) then
        write(lab,'(a,f8.1,a)') 'R: ',theta*1E3,' m$'
       else
        write(lab,'(a,f8.1,a)') 'R: ',theta,' km$'
       end if
      else
       lab=' $'
      end if
      WRITE(19,779) LAB
      WRITE(19,6020) XLEN,'XLEN'
      WRITE(19,6020) YLEN,'YLEN'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) 1.,'XDIV'
      IF (C0.GT.1E-10) THEN
      WRITE(TXT,830) C0*1E-3
 830  FORMAT('Reduced time t-r/',F5.3,' (secs.)$')
      ELSE
      TXT='Time (seconds)$'
      END IF
      WRITE(19,778) TXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) 1.,'YDIV'
      if (iraopt.eq.1) then
       TXT=RYTXT(1:LENSTR(RYTXT))//'$'
      else if (iraopt.eq.2) then
       TXT=AYTXT(1:LENSTR(AYTXT))//'$'
      else
       TXT=' $'
      end if
      WRITE(19,778) TXT
      WRITE(19,780) YTYP
      WRITE(19,6010) NC,'NC'
      RETURN
 6010 FORMAT(1H ,I8,8X,A30)
 6020 FORMAT(1H ,F15.6,1X,A30)
 6030 FORMAT(1H ,E15.6,1X,A30)
      END
      SUBROUTINE PLSTDEP(TITLE,INR,XLEN,YLEN,
     1    XLEFT,XRIGHT,XINC,YDOWN,YUP,YINC,
     2    NC,SD,RANGE,C0,THETA)
      INCLUDE 'compar.f'
      INCLUDE 'complo.f'
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(12)
      CHARACTER*80 TXT
      CHARACTER*3  XTYP,YTYP
      CHARACTER*16 LAB
      DATA XTYP,YTYP /'LIN','LIN'/
      DATA OPTION /'FIPP  ','      '/
      DATA OPT2 /'RPSTDP','ZPSTDP','WPSTDP','UPSTDP','VPSTDP',
     &           'XPSTDP','KPSTDP',2*'      ',
     &           'MPSTDP','DPSTDP','FPSTDP'/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR+1)
      I=INR
      IF (INR.EQ.0) I=1
      WRITE(19,777) OPTION,PLOTOPT
      IF (INR.EQ.2) THEN
        TXT='VERTICAL PARTICLE VELOCITY'
      ELSE IF (INR.EQ.3) THEN
        TXT='RADIAL PARTICLE VELOCITY'
      ELSE IF (INR.EQ.4) THEN
        TXT='TANGENTIAL PARTICLE VELOCITY'
      ELSE IF (INR.EQ.5) THEN
        TXT='RADIAL STRESS'
      ELSE IF (INR.EQ.6) THEN
        TXT='BULK PRESSURE'
      ELSE IF (INR.EQ.7) THEN
        TXT='RADIAL SHEAR STRESS'
      ELSE IF (INR.EQ.8) THEN
        TXT='TANG. SHEAR STRESS'
      ELSE
        TXT='NORMAL STRESS'
      END IF
      WRITE(19,778) TXT
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6,',',A)
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 811  FORMAT('SD:',F8.1,' m$')
 813  FORMAT('R: ',F8.1,' km$')
 814  FORMAT('AZ:',f8.1,' dg$')
      NLAB=3
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'
      WRITE(LAB,811) SD
      WRITE(19,779) LAB

      if (abs(range).lt.1.0) then
       write(lab,'(a,f6.1,a)') 'Range:',1e3*range,' m$'
      else
       write(lab,'(a,f6.1,a)') 'Range:',range,' km$'
      end if

      WRITE(19,779) LAB
      WRITE(LAB,814) THETA
      WRITE(19,779) LAB
      WRITE(19,6020) XLEN,'XLEN'
      WRITE(19,6020) YLEN,'YLEN'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) 1.,'XDIV'
      IF (C0.GT.1E-10) THEN
      WRITE(TXT,830) C0*1E-3
 830  FORMAT('Reduced time t-r/',F5.3,' (secs.)$')
      ELSE
      TXT='Time (seconds)$'
      END IF
      WRITE(19,778) TXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) 1.,'YDIV'
      TXT=DYTXT(1:LENSTR(DYTXT))//'$'
      WRITE(19,778) TXT
      WRITE(19,780) YTYP
      WRITE(19,6010) NC,'NC'
      RETURN
 6010 FORMAT(1H ,I8,8X,A30)
 6020 FORMAT(1H ,F15.6,1X,A30)
 6030 FORMAT(1H ,E15.6,1X,A30)
      END
      SUBROUTINE OUSTACK(LF0,LF,TMIN,DT,OFFSET,FACTOR,I,PX,MODU)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      DIMENSION X(NP2,NPAR),PX(MODU)
      equivalence (X(1,1),CFF(1,1))
      WRITE(19,6010) LF-LF0+1,'N'
      WRITE(19,6020) TMIN+(LF0-1)*DT,'TMIN'
      WRITE(19,6030) DT,'DT'
      WRITE(19,6030) OFFSET,'Y-OFFSET'
      WRITE(19,6020) 0.,'DY'
 6010 FORMAT(1H ,I8,8X,A30)
 6020 FORMAT(1H ,F15.6,1X,A30)
 6030 FORMAT(1H ,E15.6,1X,A30)
      DO 1350 K=LF0,LF,MODU        
      I1=K          
      I2=I1+MODU-1
      I2=MIN0(I2,LF)
      JK=0
      DO 1300 J=I1,I2              
      JK=JK+1
      if (log_ts) then
       PX(JK)=max(20.0*alog10(X(J,1))-troff,0E0)*FACTOR  
      else
       PX(JK)=X(J,1)*FACTOR  
      end if
 1300 CONTINUE      
      WRITE(20,444)(PX(J),J=1,JK)      
 444  FORMAT(1H ,6G13.5)
1350  CONTINUE      
      RETURN
      END
      SUBROUTINE PLFRSP(FRQ1,DELFRQ,NFR,TITLE,
     * WAVINT,MODU,XLEN,YLEN,RANGE,SD,RD,THETA)     
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION WAVINT(MODU)    
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      CHARACTER*80 TXT
      CHARACTER*3  XTYP,YTYP
      CHARACTER*16 LAB
      DATA XTYP,YTYP /'LIN','LIN'/
      DATA OPTION /'OASPU ','FRSPEC'/       
C       
      OPTION(1)=PROGNM
      OPTION(2)='FRSPEC'
      YMAX=0.
      DO 2401 II=1,NFR    
       YMAX=MAX(ABS(CFF(II,1)),YMAX)
 2401  CONTINUE
C       
      IPLOT1=1        
      IPLOT2=NFR        
C       
C       
C       
C XAXIS DEFINITION        
C       
      IF (IPLOT1.EQ.2) IPLOT1=1
      FRMIN=FRQ1+(IPLOT1-1)*DELFRQ
      NN=IPLOT2-IPLOT1+1
      XMAX=(FRQ1+(NFR-1)*DELFRQ)      
      XMIN=FRQ1       
c      ILOG=IFIX(ALOG10(XMAX-XMIN))
c      IF ((XMAX-XMIN).LT.1.0) ILOG=ILOG-1
c      IFAC=IFIX((XMAX-XMIN)/10.**ILOG)+1
c      XFAC=IFAC
c      IF (IFAC.EQ.1) XFAC=5.
c      IF (IFAC.EQ.2) XFAC=4.
c      IF (IFAC.GT.5) XFAC=IFAC/2.0
c      XINC=IFAC*10.**ILOG/XFAC
c      NXDIF=ILOG-1
c      XDIV=10.**(-NXDIF)
       call AUTOAX(xmin,xmax,xleft,xright,XINC,XDIV,NXDIF)

 6010 FORMAT(1H ,I8,10X,A40)
 6020 FORMAT(1H ,F15.6,3X,A40)
 6030 FORMAT(1H ,G15.6,3X,A40)
C       
C       
C  YAXIS DEFINITION       
C       
      if (log_ts) then
       ILOG=IFIX(ALOG10(YMAX))          
       IF (YMAX.LT.1) ILOG=ILOG-1       
       YUP=20.0*(ilog+1)
       YDOWN=YUP-80.0
       YINC=10
       NDIFF=0
       YDIV=1          
      else
       YMIN=0.0   
       JLOG=IFIX(ALOG10(YMAX))
       IF (YMAX.LT.1.0) JLOG=JLOG-1
       JFAC=IFIX(YMAX/10.**JLOG)+1
       YUP=JFAC*10.**JLOG
       YDOWN=0.
       YFAC=JFAC
       IF (JFAC.EQ.1) YFAC=5
       IF (JFAC.EQ.2) YFAC=4
       IF (JFAC.GT.5) YFAC=JFAC/2.0
       YINC=JFAC*10.**JLOG/YFAC
       NYDIF=JLOG-1
       YDIV=10.**(-NYDIF)
      end if
      IF(IPLOT1.EQ.2)IPLOT1=1      
      WRITE(19,777) OPTION,PLOTOPT
      TXT='FREQUENCY SPECTRUM'
      WRITE(19,778) TXT
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6,',',A)
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 811  FORMAT('SD:',F8.1,' m$')
 812  FORMAT('RD:',F8.1,' m$')
 813  FORMAT('R: ',F8.1,' km$')
 814  FORMAT('AZ:',F8.1,' dg$')
      NLAB=4
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'

      if (abs(range).lt.1.0) then
       write(lab,'(a,f6.1,a)') 'Range:',1e3*range,' m$'
      else
       write(lab,'(a,f6.1,a)') 'Range:',range,' km$'
      end if

      WRITE(19,779) LAB
      WRITE(LAB,811) SD
      WRITE(19,779) LAB
      WRITE(LAB,812) RD
      WRITE(19,779) LAB
      WRITE(LAB,814) theta
      WRITE(19,779) LAB
      WRITE(19,6030) XLEN,'XLEN'
      WRITE(19,6030) YLEN,'YLEN'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) Xleft,'XLEFT'
      WRITE(19,6030) Xright,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      IF (NXDIF.EQ.0) THEN
       WRITE(19,6030) XDIV,'XDIV'
       WRITE(TXT,819)
 819   FORMAT('Frequency (Hz)$')
      ELSE
       nxdif=3
       xdiv=10.0**(-nxdif)
       WRITE(19,6030) XDIV,'XDIV'
       write(txt,820)
 820   FORMAT('Frequency (kHz)$')
      END IF
      WRITE(19,778) TXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) YDIV,'YDIV'
      if (log_ts) then
       write(TXT,'(a)') 'Power (dB)'
      else
       WRITE(TXT,821) NYDIF
 821   FORMAT('Amplitude (10^',I3,')$')
      end if
      WRITE(19,778) TXT
      WRITE(19,780) YTYP
      WRITE(19,6010) 1,'NC'
      WRITE(19,6010) NN,'N'
      WRITE(19,6030) FRMIN,'FRMIN'
      WRITE(19,6030) DELFRQ,'DELFRQ'
      WRITE(19,6030) 0.,'YMIN'
      WRITE(19,6030) 0.,'DY'
      DO 2700   J1=IPLOT1,IPLOT2,MODU     
      J2=MIN0(J1+MODU-1,IPLOT2)  
      INDICE=0
      DO 2650   II=J1,J2  
      INDICE=INDICE+1
      if (log_ts) then
       WAVINT(INDICE)=20.0*alog10(max(abs(CFF(II,1)),1E-20))
      else
       WAVINT(INDICE)=ABS(CFF(II,1))
      end if
 2650 CONTINUE   
      WRITE(20,444)(WAVINT(II),II=1,INDICE)     
 444  FORMAT(1H ,6G13.5)
 2700 CONTINUE   
C       
      RETURN     
C       
      END        
      SUBROUTINE PLPUTMP(NX,LF0,LF,TMIN,DT,TITLE,INR,PX,MODU,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,RANGE,SD,RD,C0,FQTMP)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION X(NP2,3),PX(MODU)
      EQUIVALENCE (X(1,1),CFF(1,1))
      CHARACTER*20 TITLE
      CHARACTER*6 OPTION(2)
      CHARACTER*80 TXT
      CHARACTER*3  XTYP,YTYP
      CHARACTER*16 LAB
      DATA XTYP,YTYP /'LIN','LIN'/
      OPTION(1)=PROGNM
      OPTION(2)='      '
      I=INR
      IF (INR.EQ.0) I=1
      YUP=0
      DO 34 J=LF0,LF
 34   YUP=AMAX1(YUP,ABS(X(J,1)))
      ILOG=IFIX(ALOG10(YUP))
      IF (YUP.LT.1) ILOG=ILOG-1
      IFAC=IFIX(YUP/10.**ILOG)+1
      YUP=IFAC*10.**ILOG
      YDOWN=-YUP
      YFAC=IFAC
      IF (IFAC.EQ.1) YFAC=5
      IF (IFAC.EQ.2) YFAC=4
      IF (IFAC.GT.5) YFAC=YFAC/2.
      YINC=IFAC*10.**ILOG/YFAC
      NDIFF=ILOG-1
      YDIV=10.**(-NDIFF)
      WRITE(19,777) OPTION,PLOTOPT
      IF (INR.EQ.0) THEN
      TXT='SOURCE PULSE'
      ELSE
      TXT='RECEIVED SIGNAL'
      END IF
      WRITE(19,778) TXT
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6,',',A)
 778  FORMAT(1H ,A)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 810  FORMAT('F:',F10.1,' Hz$')
 8101 FORMAT('F:',F6.1,' kHz$')
 8102 FORMAT('F:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
 813  FORMAT('Range:',F6.2,' km$')
      NLAB=4
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'

      if (abs(range).lt.1.0) then
       write(lab,'(a,f6.1,a)') 'Range:',1e3*range,' m$'
      else
       write(lab,'(a,f6.1,a)') 'Range:',range,' km$'
      end if

      WRITE(19,779) LAB

      if (FQTMP.GE.1e6) then
         WRITE(LAB,8102) FQTMP*1e-6
      else if (FQTMP.GE.1e3) then
         WRITE(LAB,8101) FQTMP*1e-3
      else
         WRITE(LAB,810) FQTMP
      end if
      WRITE(19,779) LAB
      WRITE(LAB,811) SD
      WRITE(19,779) LAB
      WRITE(LAB,812) RD
      WRITE(19,779) LAB
      WRITE(19,6020) XLEN,'XLEN'
      WRITE(19,6020) YLEN,'YLEN'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) 1.,'XDIV'
      IF (C0.GT.1E-10.AND.INR.NE.0) THEN
      WRITE(TXT,830) C0*1E-3
 830  FORMAT('Reduced time t-r/',F5.3,' (secs.)$')
      ELSE
      TXT='Time (seconds)$'
      END IF
      WRITE(19,778) TXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) YDIV,'YDIV'
      GOTO(901,902,903,904),INR+1
 901  WRITE(TXT,911) NDIFF
 911  FORMAT('Pressure (10**',I3,' Pa)$')
      go to 905
 902  WRITE(TXT,912) NDIFF
 912  FORMAT('Normal stress (10**',I3,' Pa)$')
      go to 905
 903  WRITE(TXT,913) NDIFF
 913  FORMAT('Vert. particle vel. (10**',I3,' m/s)$')
      GO TO 905
 904  WRITE(TXT,914) NDIFF
 914  FORMAT('Hor. particle vel. (10**',I3,' m/s)$')
 905  WRITE(19,778) TXT
      WRITE(19,780) YTYP
      WRITE(19,6010) 1,'NC'
      WRITE(19,6010) LF-LF0+1,'N'
      WRITE(19,6020) TMIN,'TMIN'
      WRITE(19,6030) DT,'DT'
      WRITE(19,6030) 0.,'YMIN'
      WRITE(19,6020) 0.,'DY'
 6010 FORMAT(1H ,I8,8X,A30)
 6020 FORMAT(1H ,F15.6,1X,A30)
 6030 FORMAT(1H ,G15.6,1X,A30)
      DO 1350 K=LF0,LF,MODU
      I1=K
      I2=I1+MODU-1
      I2=MIN0(I2,LF)
      JK=0
      DO 1300 J=I1,I2
      JK=JK+1
      PX(JK)=X(J,1)
 1300 CONTINUE
      WRITE(20,444)(PX(J),J=1,JK)
 444  FORMAT(1H ,6E13.4)
1350  CONTINUE
      RETURN
      END
      SUBROUTINE PLFRTMP(FRQP1,DLFRQP,NFR,TITLE,
     1 WAVINT,MODU,XLEN,YLEN,FMIN,FMAX)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION FFS(2,NP),WAVINT(MODU)
      EQUIVALENCE (FFS(1,1),CFFS(1))

      CHARACTER*20 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      CHARACTER*80 TXT
      CHARACTER*3  XTYP,YTYP
      CHARACTER*16 LAB
      DATA XTYP,YTYP /'LIN','LIN'/

      IF ((PROGNM.EQ.'FIPP 1').OR.(PROGNM.EQ.'FIPP 2')) THEN
      OPTION(1)='  PP  '
      OPTION(2)='FRSPEC'
      ELSE
      OPTION(1)=PROGNM
      OPTION(2)='      '
      ENDIF
C
      FFSQ = 0.0
      YMAX=0.
      DO 2401 II=1,NFR
C
C
      FFSQ = FFS(1,II)**2 + FFS(2,II)**2
      IF(FFSQ.GT.YMAX)   YMAX=FFSQ
 2401  CONTINUE
C
      IPLOT1=1
      IPLOT2=NFR
C
C
C
C XAXIS DEFINITION
C
      IF (IPLOT1.EQ.2) IPLOT1=1
      FRMIN=FRQP1+(IPLOT1-1)*DLFRQP
      NN=IPLOT2-IPLOT1+1
      XMAX=(FRQP1+(NFR-1)*DLFRQP)
      XMIN=FRQP1
      call AUTOAX(xmin,xmax,xleft,xright,xINC,xDIV,NxDIF)
c      ILOG=IFIX(ALOG10(XMAX-XMIN))
c      IF ((XMAX-XMIN).LT.1.0) ILOG=ILOG-1
c      IFAC=IFIX((XMAX-XMIN)/10.**ILOG)+1
c      XFAC=IFAC
c      IF (IFAC.EQ.1) XFAC=5.
c      IF (IFAC.EQ.2) XFAC=4.
c      IF (IFAC.GT.5) XFAC=IFAC/2.0
c      XINC=IFAC*10.**ILOG/XFAC
c      NXDIF=ILOG-1
c      XDIV=10.**(-NXDIF)
 6010 FORMAT(1H ,I8,10X,A40)
 6020 FORMAT(1H ,F15.6,3X,A40)
 6030 FORMAT(1H ,G15.6,3X,A40)
C
C
C  YAXIS DEFINITION
C
      YMIN=0.0
      YMAX=SQRT(YMAX)

      if (log_ts) then
       ILOG=IFIX(ALOG10(YMAX))          
       IF (YMAX.LT.1) ILOG=ILOG-1       
       YUP=20.0*(ilog+1)
       YDOWN=YUP-80.0
       YINC=10
       NDIFF=0
       YDIV=1          
      else
       YMIN=0.0   
       JLOG=IFIX(ALOG10(YMAX))
       IF (YMAX.LT.1.0) JLOG=JLOG-1
       JFAC=IFIX(YMAX/10.**JLOG)+1
       YUP=JFAC*10.**JLOG
       YDOWN=0.
       YFAC=JFAC
       IF (JFAC.EQ.1) YFAC=5
       IF (JFAC.EQ.2) YFAC=4
       IF (JFAC.GT.5) YFAC=JFAC/2.0
       YINC=JFAC*10.**JLOG/YFAC
       NYDIF=JLOG-1
       YDIV=10.**(-NYDIF)
      end if

      IF(IPLOT1.EQ.2)IPLOT1=1
      WRITE(19,777) OPTION,PLOTOPT
      IF (PROGNM.EQ.'FIPP 2') THEN
      TXT='PULSE SPECTRUM '
      ELSE
      TXT='SOURCE SPECTRUM'
      ENDIF
      WRITE(19,778) TXT
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6,',',A)
 778  FORMAT(1H ,A)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
      NLAB=0
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'
      WRITE(19,6030) XLEN,'XLEN'
      WRITE(19,6030) YLEN,'YLEN'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) Xleft,'XLEFT'
      WRITE(19,6030) Xright,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      IF (NXDIF.EQ.0) THEN
       WRITE(19,6030) XDIV,'XDIV'
       WRITE(TXT,819)
 819   FORMAT('Frequency (Hz)$')
      ELSE
       nxdif=3
       xdiv=10.0**(-nxdif)
       WRITE(19,6030) XDIV,'XDIV'
       WRITE(TXT,820) 
 820   FORMAT('Frequency (kHz)$')
      END IF
      WRITE(19,778) TXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) YDIV,'YDIV'
      if (log_ts) then
       write(TXT,'(a)') 'Power (dB)'
      else
       WRITE(TXT,821) NYDIF
 821   FORMAT('Amplitude (10^',I3,')$')
      end if
      WRITE(19,778) TXT
      WRITE(19,780) YTYP
      WRITE(19,6010) 3,'NC'
      WRITE(19,6010) NN,'N'
      WRITE(19,6030) FRMIN,'FRMIN'
      WRITE(19,6030) DLFRQP,'DLFRQP'
      WRITE(19,6030) 0.,'YMIN'
      WRITE(19,6030) 0.,'DY'
      WRITE(19,6010) 2,'N'
      WRITE(19,6030) FMIN,'FMIN'
      WRITE(19,6030) 1E-10,'DELTA'
      WRITE(19,6030) YDOWN,'YM'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6010) 2,'N'
      WRITE(19,6030) FMAX,'FMAX'
      WRITE(19,6030) 1E-10,'DELTA'
      WRITE(19,6030) YDOWN,'YM'
      WRITE(19,6030) YUP,'YUP'
      DO 2700   J1=IPLOT1,IPLOT2,MODU
      J2=MIN0(J1+MODU-1,IPLOT2)
      INDICE=0
      DO 2650   II=J1,J2
      INDICE=INDICE+1
      if (log_ts) then
       WAVINT(INDICE)=20.0*alog10(max(abs(CFFS(II)),1E-20))
      else
       WAVINT(INDICE)=ABS(CFFS(II))
      end if
 2650 CONTINUE
      WRITE(20,444)(WAVINT(II),II=1,INDICE)
 444  FORMAT(1H ,6E13.4)
 2700 CONTINUE
C
      RETURN
C
      END
      SUBROUTINE DEMSNAPS(XLEFT,XRIGHT,XAXIS,XINC,XSCALE,
     1                     YDOWN,YUP,YAXIS,YINC,YSCALE,
     2                     ZZMIN,ZZMAX,ZZSTEP,IPACT,
     3                     XL,theta)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      DIMENSION PX(np)
      common /plbuff/ px
      logical neg_ran
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY
      NELM=NP3
      IF ((IR*NPLOTS*NOUT).GT.NELM) THEN
        WRITE(6,*) '>>> DEMSNAPS: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      neg_ran=(xleft.lt.0.0) 
    
      if (neg_ran) then
        ioff=nplots-1
      else
        ioff=0
      end if

      IF (((NPLOTS+ioff)*IR).GT.np) then
        WRITE(6,*) '>>> DEMSNAPS: ARRAY CFILE TOO SMALL <<<'
        PAUSE
        RETURN
      END IF
      

      IF (TSHIFT.NE.TSHTMP) IFLAG=0
      IF ((IFLAG.EQ.0).OR.(IPACT.NE.ITMP)) THEN
        IFLAG=1
        ITMP=ipact
        TSHTMP=TSHIFT
        ZMIN=1E20
        ZMAX=-1E20
        if (icdr.eq.0) then
         dx=(xl-r0)/(nplots-1)
         onoran=1e0/max(r0,abs(dx))
         write(6,*) 'Range reference for scaling:',max(r0,dx)
        end if

c >>> Positive ranges
c        write(6,*) 'dirsum r>0'
        CALL DIRSUM(DOMEGA,TSHIFT,U,LX,MX,theta,ipact,iparm(ipact))
c
c        write(6,*) 'exit dirsum'
        DO IDEP=1,IR
         DO IRAN=1,NPLOTS
          INDEX=IDEP+IR*((IRAN-1)+(ipact-1)*NPLOTS)
          index_c=IDEP+IR*(ioff+IRAN-1)
          if (icdr.eq.0) then
           rangekm=r0+(iran-1)*dx
           cbuf(index_c)=u(index)*rangekm*onoran
          else
           cbuf(index_c)=u(index)
          end if
c          write(6,*) iran,idep,index_c,cbuf(index_c)
          if (undfpt.and.iundef(idep+(iran-1)*ir).eq.1) then
           cbuf(index_c)=1.E30
          else
           if (deconv) then
             value=abs(cbuf(index_c))
           else
            VALUE=REAL(cbuf(index_c))
           end if
           ZMIN=MIN(ZMIN,VALUE)
           ZMAX=MAX(ZMAX,VALUE)
          end if
         end do
        end do
c        write(6,*) zmin,zmax

c >>> Negative ranges
        if (neg_ran) then
c         write(6,*) 'dirsum r>0'
         CALL DIRSUM(DOMEGA,TSHIFT,U,LX,MX,theta+pi,ipact,iparm(ipact))
         DO IDEP=1,IR
          DO IRAN=2,NPLOTS
           INDEX=IDEP+IR*((IRAN-1)+(ipact-1)*NPLOTS)
           index_c=IDEP+IR*(nplots-IRAN+1)
           if (icdr.eq.0) then
            rangekm=r0+(iran-1)*dx
            cbuf(index_c)=u(index)*rangekm*onoran
           else
            cbuf(index_c)=u(index)
           end if
           if (undfpt.and.iundef(idep+(iran-1)*ir).eq.1) then
            cbuf(index_c)=1.E30
           else
            if (deconv) then
             value=abs(cbuf(index_c))
            else
             VALUE=REAL(cbuf(index_c))
            end if
            ZMIN=MIN(ZMIN,VALUE)
            ZMAX=MAX(ZMAX,VALUE)
           end if
          end do
         end do
        end if
        
c        Find nice format for ZMIN and ZMAX
         ZTMP1=max(1E-10,ABS(ZMIN))

         IF ((ZTMP1.LT.1.0).AND.(ZMAX.LT.1.0)) THEN
          ZTMP=1.0/ZTMP1
          K1=0
2200      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K1=K1+1
            GO TO 2200
          ENDIF
          K1=K1+1
         ELSE
          ZTMP=ZTMP1
          K1=0
2210      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K1=K1+1
            GO TO 2210
          ENDIF
          K1=-K1
         ENDIF
         write(6,*) 'k1=',k1

         IF ((ZMAX.LT.1.0).AND.(ZTMP1.LT.1.0)) THEN
          ZTMP=1.0/ZMAX
          K2=0
2202      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K2=K2+1
            GO TO 2202
          ENDIF
          K2=K2+1
         ELSE
          ZTMP=ZMAX
          K2=0
2212      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K2=K2+1
            GO TO 2212
          ENDIF
          K2=-K2
         ENDIF
         write(6,*) 'k2=',k2

         IF (K1.EQ.K2) THEN
          K=K1
         ELSE
          K=MIN(K1,K2)
         ENDIF
         write(6,*) 'k=',k

         KVAL=K
         ZMINVAL=ZMIN
         ZMAXVAL=ZMAX
         SCALEVAL=10.0**K
        write(6,*) zminval,zmaxval,scaleval
         IF(KSNAPSH1.EQ.0.or.NORMK.EQ.-99) SCALE=SCALEVAL
      END IF

      LFILN=LENSTR(basename)
      filnbuf=basename(1:LFILN)//'.adr'
      OPEN(4,FILE=filnbuf,STATUS='UNKNOWN',
     &       FORM='FORMATTED')
      WRITE(4,303)TITLEC
  303 FORMAT(A80)
      WRITE(4,301)ZMINVAL,'MIN VALUE'
  301 FORMAT(1X,E12.5,1X,A30)
      WRITE(4,301)ZMAXVAL,'MAX VALUE'

      ZMINCAL=NINT(ZMINVAL*SCALEVAL)
      ZMAXCAL=NINT(ZMAXVAL*SCALEVAL)
      ABSZMIN=ABS(ZMINCAL)
      ABSZMAX=ABS(ZMAXCAL)
      ABSMAX=AMAX1(ABSZMIN,ABSZMAX)
      if (deconv) then
        zmaxcal=absmax
        zmincal=0e0
        zstepcal=absmax/real(numint)
      else
       ZSTEPCAL=2.*ABSMAX/REAL(NUMINT)
       ZMAXCAL=absmax
       zmincal=-zmaxcal
       zmaxcal=zmincal+1e-3*(nint(1e3*(zstepcal*numint)+1e0))
      end if
      WRITE(4,301)ZMINCAL,'CAL MIN LEVEL'
      WRITE(4,301)ZMAXCAL,'CAL MAX LEVEL'
      WRITE(4,304)SCALEVAL,'CAL SCALE'
  304 FORMAT(1X,1PE8.1,5X,A30)

      IF(KSNAPSH1.EQ.0.or.NORMK.EQ.-99) THEN
        NORMK=K
        NCORMK=NORMK
      END IF
      IF(KSNAPSH1.EQ.0.or.ZZMAX.EQ.-99.) THEN
        ZZMIN=ZMINCAL
        ZCMIN=ZZMIN
        ZZMAX=ZMAXCAL
        ZCMAX=ZZMAX
        ZZSTEP=ZSTEPCAL
      END IF
      KSNAPSH1=1
      SCALE=10.0**NORMK
      WRITE(4,301)ZZMIN,'INPUT MIN LEVEL'
      WRITE(4,301)ZZMAX,'INPUT MAX LEVEL'
      WRITE(4,304)SCALE,'INPUT SCALE'
      CLOSE(4,STATUS='KEEP')


      LFILN=LENSTR(basename)

      filnbuf=basename(1:LFILN)//'.bdr'
      OPEN(17,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')
      filnbuf=basename(1:LFILN)//'.cdr'
      OPEN(55,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')

      if (log_ts) then
       zzzmax=1E+1*(nint(2.0*alog10(zzmax/scale)+0.0001))
       zzzmin=zzzmax-40.0
       zzzstep=(zzzmax-zzzmin)/(numint-1)
      else
       zzzmax=zcmin+1e-3*(nint(1e3*(zzstep*numint)+1e0))
       zzzmin=zcmin
       zzzstep=zzstep
      end if

      if (neg_ran) then
       CALL CONDRWP(TITLEC,(NPLOTS+ioff),IR,(NPLOTS+ioff),IR,
     &             XLEFT,XRIGHT,XSCALE,
     1             XINC,YUP,YDOWN,YSCALE,YINC,zzzmax,zzzmin,zzzSTEP,
     2             FREQS,SD,RD,RDLOW,-xl,XL,PX)
      else
       CALL CONDRWP(TITLEC,NPLOTS,IR,NPLOTS,IR,
     &             XLEFT,XRIGHT,XSCALE,
     1             XINC,YUP,YDOWN,YSCALE,YINC,zzzmax,zzzmin,zzzSTEP,
     2             FREQS,SD,RD,RDLOW,R0,XL,PX)
      end if
      DO IDEP=1,IR
       DO IRAN=1,NPLOTS
        INDEX=IDEP+IR*(ioff+IRAN-1)
        if (iundef(idep+(iran-1)*ir).ne.1) then 
         if (deconv) then
          if (log_ts) then
           PX(IRAN+ioff)=20.0*
     &                   alog10(max(abs(cbuf(index)),1E-10))
          else
           PX(IRAN+ioff)=abs(cbuf(index))*scale
          end if
         else
          PX(IRAN+ioff)=real(cbuf(index))*scale
         end if
        else
         px(iran+ioff)=real(cbuf(index))
        end if
c >>> offset a bit to avoid MTV 0-contour
        px(iran+ioff)=px(iran+ioff)+1E-2*zzzmax
       end do
c >>> negative ranges
       if (neg_ran) then
        DO IRAN=1,NPLOTS
         INDEX=IDEP+IR*(nplots-IRAN+1)
         if (iundef(idep+(iran-1)*ir).ne.1) then 
          if (deconv) then
           if (log_ts) then
            PX(nplots+1-IRAN)=20.0*
     &         alog10(max(abs(cbuf(index)),1E-10))
           else
            PX(nplots+1-IRAN)=abs(cbuf(index))*scale
           end if
          else
           PX(nplots+1-iran)=real(cbuf(index))*scale
          end if
         else
          px(nplots+1-iran)=real(cbuf(index))
         end if
c >>> offset a bit to avoid MTV 0-contour
         if (.not.log_ts) then
          px(nplots+1-iran)=px(nplots+1-iran)+1E-2*zzzmax
         end if
        end do
       end if
       CALL CONDRBP(1,NPLOTS+ioff,NPLOTS+ioff,PX)
      end do

      RETURN
      END

      SUBROUTINE HORSNAPS(nazim_req,idep_req,shading,shdfil,
     1                 XRIGHT,XAXIS,XINC,XSCALE,
     2                 zzMin,zzMax,zzSTEP,IPACT,XL)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      character*(*) shdfil
      logical shading
      DIMENSION PX(np)
      common /plbuff/ px
      logical neg_ran
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY

      IF ((nazim_req*NPLOTS*NOUT).GT.NP3) THEN
        WRITE(6,*) '>>> HORSNAPS: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      IF (((NPLOTS)*nazim_req).GT.np) then
        WRITE(6,*) '>>> HORSNAPS: ARRAY CFILE TOO SMALL <<<'
        PAUSE
        RETURN
      END IF
      

      IF (TSHIFT.NE.TSHTMP) IFLAG=0
      IF ((IFLAG.EQ.0).OR.(IPACT.NE.ITMP)) THEN
        IFLAG=1
        ITMP=ipact
        TSHTMP=TSHIFT
        ZMIN=1E20
        ZMAX=-1E20
        if (icdr.eq.0) then
         dx=(xl-r0)/(nplots-1)
         onoran=1e0/max(r0,abs(dx))
         write(6,*) 'Range reference for scaling:',max(r0,dx)
        end if

c >>> Positive ranges
        CALL HORSUM(DOMEGA,TSHIFT,U,LX,MX,idep_req,nazim_req,
     &              ipact,iparm(ipact))
c

        DO iazim=1,nazim_req
         DO IRAN=1,NPLOTS
          INDEX=iazim+nazim_req*((IRAN-1)+(ipact-1)*NPLOTS)
          index_c=iazim+nazim_req*(IRAN-1)
          if (icdr.eq.0) then
           rangekm=r0+(iran-1)*dx
           cbuf(index_c)=u(index)*rangekm*onoran
          else
           cbuf(index_c)=u(index)
          end if
          if (undfpt.and.iundef(idep_req+(iran-1)*ir).eq.1) then
           cbuf(index_c)=1.E30
          else
           if (deconv) then
            value=abs(cbuf(index_c))
           else
            VALUE=REAL(cbuf(index_c))
           end if
           ZMIN=MIN(ZMIN,VALUE)
           ZMAX=MAX(ZMAX,VALUE)
          end if
         end do
        end do
*       Find nice format for ZMIN and ZMAX
        ZTMP1=max(1E-10,ABS(ZMIN))

        IF ((ZTMP1.LT.1.0).AND.(ZMAX.LT.1.0)) THEN
          ZTMP=1.0/ZTMP1
          K1=0
c          write(6,*) 'k1,ztmp=',k1,ztmp
2200      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K1=K1+1
            GO TO 2200
          ENDIF
          K1=K1+1
        ELSE
          ZTMP=ZTMP1
          K1=0
2210      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K1=K1+1
            GO TO 2210
          ENDIF
          K1=-K1
        ENDIF
        IF ((ZMAX.LT.1.0).AND.(ZTMP1.LT.1.0)) THEN
          ZTMP=1.0/ZMAX
          K2=0
2202      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K2=K2+1
            GO TO 2202
          ENDIF
          K2=K2+1
        ELSE
          ZTMP=ZMAX
          K2=0
2212      IF (INT(ZTMP/10.0).NE.0) THEN
            ZTMP=ZTMP/10.0
            K2=K2+1
            GO TO 2212
          ENDIF
          K2=-K2
        ENDIF
        IF (K1.EQ.K2) THEN
          K=K1
        ELSE
          K=MIN(K1,K2)
        ENDIF

       
        KVAL=K
        ZMINVAL=ZMIN
        ZMAXVAL=ZMAX
        SCALEVAL=10.0**K
c        write(6,*) zminval,zmaxval,scaleval
        IF(KSNAPSH1.EQ.0.or.NORMK.EQ.-99) SCALE=SCALEVAL
      END IF

      LFILN=LENSTR(basename)
      filnbuf=basename(1:LFILN)//'.adr'
      OPEN(4,FILE=filnbuf,STATUS='UNKNOWN',
     &       FORM='FORMATTED')
      WRITE(4,303)TITLEC
  303 FORMAT(A80)
      WRITE(4,301)ZMINVAL,'MIN VALUE'
  301 FORMAT(1X,E12.5,1X,A30)
      WRITE(4,301)ZMAXVAL,'MAX VALUE'

      ZMINCAL=NINT(ZMINVAL*SCALEVAL)
      ZMAXCAL=NINT(ZMAXVAL*SCALEVAL)
      ABSZMIN=ABS(ZMINCAL)
      ABSZMAX=ABS(ZMAXCAL)
      ABSMAX=AMAX1(ABSZMIN,ABSZMAX)
      if (deconv) then
        zmaxcal=absmax
        zmincal=0e0
        zstepcal=absmax/real(numint)
      else
       ZSTEPCAL=2.*ABSMAX/REAL(NUMINT)
       ZMAXCAL=absmax
       zmincal=-zmaxcal
       zmaxcal=zmincal+1e-3*(nint(1e3*(zstepcal*numint)+1e0))
      end if
      WRITE(4,301)ZMINCAL,'CAL MIN LEVEL'
      WRITE(4,301)ZMAXCAL,'CAL MAX LEVEL'
      WRITE(4,304)SCALEVAL,'CAL SCALE'
  304 FORMAT(1X,1PE8.1,5X,A30)
      IF(KSNAPSH1.EQ.0.or.NORMK.EQ.-99) THEN
        NORMK=K
        NCORMK=NORMK
      END IF
      IF(KSNAPSH1.EQ.0.or.ZZMAX.EQ.-99.) THEN
        ZZMIN=ZMINCAL
        ZCMIN=ZZMIN
        ZZMAX=ZMAXCAL
        ZCMAX=ZZMAX
        ZZSTEP=ZSTEPCAL
      END IF
      KSNAPSH1=1
      SCALE=10.0**NORMK
      WRITE(4,301)ZZMIN,'INPUT MIN LEVEL'
      WRITE(4,301)ZZMAX,'INPUT MAX LEVEL'
      WRITE(4,304)SCALE,'INPUT SCALE'
      CLOSE(4,STATUS='KEEP')


      LFILN=LENSTR(basename)

      filnbuf=basename(1:LFILN)//'.bdr'
      OPEN(17,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')
      filnbuf=basename(1:LFILN)//'.cdr'
      OPEN(55,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')

      if (log_ts) then
       zzzmax=1E+1*(nint(2.0*alog10(zzmax/scale)+0.0001))
       zzzmin=zzzmax-40.0
       zzzstep=(zzzmax-zzzmin)/(numint-1)
      else
       zzzmax=zcmin+1e-3*(nint(1e3*(zzstep*numint)+1e0))
       zzzmin=zcmin
       zzzstep=zzstep
      end if

       nx=151
       ny=151
       dazim=360.0/nazim_req
       azimax=(nazim_req-1)*dazim
       ttmax=azimax*pi/180.
       drazim=pi*dazim/180.0
       rmin=r0
       rmax=xl
       nrng=nplots

       CALL CONTRP(TITLEC,NPX,NPY,NX,NY,
     &             -xright,XRIGHT,XSCALE,XINC,
     &             -xright,xright,xSCALE,xINC,
     &             zzzmin,zzzMAX,zzzstep,FREQS,SD,RDC(idep_req),
     &             rmin,rmax,0.0,azimax)

        do IRAN=1,NPLOTS
         do iazim=1,nazim_req
          i0=iazim+nazim_req*(iran-1)
          INDEX=iazim+nazim_req*(IRAN-1)

          if (iundef(idep_req+(iran-1)*ir).ne.1) then 
           if (deconv) then
            if (log_ts) then
             PX(i0)=20.0*alog10(max(abs(cbuf(index)),1E-10))
            else
             PX(i0)=abs(cbuf(index))*scale
            end if
           else
            PX(i0)=real(cbuf(index))*scale
           end if
          else
           px(i0)=real(cbuf(index))
          end if
c >>> offset a bit to avoid MTV 0-contour
          if (.not.log_ts) then
           px(i0)=px(i0)+1E-2*zzzmax
          end if
         end do
        end do
        do iran=1,nplots
         i0=(iran-1)*nazim_req+1
         call condrbp(1,nazim_req,nazim_req,px(i0))
        end do
 
      if (mtvout) then
         write(6,*) 'Write plotmtv script directly !!'
         filnbuf=basename(1:LFILN)//'.mtv'
         open(77,file=filnbuf,status='unknown')
         write(77,*) '$data=contcurve'
         do ii=len(titlec),1,-1
            if (titlec(ii:ii).ne.' ') then
               iii=ii
               goto 2000
            end if
         end do
 2000    continue
         RD=RDC(idep_req)
         write(77,'(1x,a,a,a)') '%toplabel="',titlec(1:iii),'"'
         write(77,'(1x,a,f7.1,a,f6.1,a,i2.2,a)') 
     &    '%subtitle="FS=',FREQS,'Hz  RD=',RD,'m  SF=10**',normk,' "'
         write(77,*) '%contfill=true'
         write(77,*) '%cmax=',zzzmax
         write(77,*) '%cmin=',zzzmin
         write(77,*) '%cstep=',zzzstep
         if (rmax.ge.1.0) then
          write(77,*) '%xlabel="Range (km)" ylabel="Range (km)"' 
         else
          write(77,*) '%xlabel="Range (m)" ylabel="Range (m)"' 
         end if
         write(77,*) '%yflip=true'
         ttmax=azimax*(4.*atan(1.))/180.
         if (rmax.ge.1.0) then
          call polar_mtv(px,nazim_req,0.,ttmax,nazim_req,
     &        rmin,rmax,nrng,77)
         else
          call polar_mtv(px,nazim_req,0.,ttmax,nazim_req,
     &        rmin*1E3,rmax*1E3,nrng,77)
         end if
c
c >>> add shaded polygon
c
         ioff=np/2
         nshd=1
         if (shading) then
          open(72,file=shdfil,status='old',form='formatted')
          read(72,'(a)',end=8358,err=8358) cdummy 
 8355     read(72,*,end=8358,err=8358) px(nshd),px(nshd+ioff)          
          if (rmax.lt.1.0) then
           px(nshd)=px(nshd)*1E3
           px(nshd+ioff)=px(nshd+ioff)*1E3
          end if
          nshd=nshd+1
          px(nshd)=px(1)
          px(nshd+ioff)=px(1+ioff)
          go to 8355
 8358     close(72)

          do i=1,nshd-1
           write(77,'(a,4(g10.3,a))') 
     &            '@line x1=',px( i ),' y1=',px( i+ioff ),
     &                 ' x2=',px(i+1),' y2=',px(i+1+ioff),
     &            ' linetype=1 linewidth=2 linecolor=0'  
          end do
         end if
         close(77)
      end if



      RETURN
      END

      SUBROUTINE DIRSUM(DOMEGA,TSHIFT,U,LX,MX,theta,ipact,iparm)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      COMPLEX U(1),COST
      REAL FC(MMAX)
      IF (MSUFT.GT.MMAX) STOP '>>> MMAX TOO SMALL IN compar.f <<<'
      FC(1)=1E0
      DO M=2,MSUFT,2
       MORD=M/2
c >>> reverse cos and sin terms for transverse components
       IF (iparm.EQ.4.or.iparm.eq.8) THEN
         FC(M)=SIN(MORD*THETA)
         FC(M+1)=-COS(MORD*THETA)
       ELSE
         FC(M)=COS(MORD*THETA)
         FC(M+1)=SIN(MORD*THETA)
       END IF
      end do

      NELM=IR*NPLOTS*nout
      CALL CVFILL(CNUL,U,2,NELM)
      CALL RWDBUF(31)
      if (deconv) then
c >>> deconvolve with centre freq.
       omedec=frqdec*2e0*pi
      else
       omedec=0e0
      end if

      do J=LX,MX
       COST=EXP(cmplx(-OMEGIM,(J-1)*DOMEGA-omedec)*TSHIFT)*CFFS(J)
       do K=1,IR
        CALL RDBUF(31,CFILE,2*NOUT*NPLOTS*msuft)
        do L=1,NPLOTS
         INDEX=K+IR*((L-1)+(IPACT-1)*NPLOTS)
         do m=1,msuft
          mord=m/2
          U(INDEX)=U(INDEX)+
     &             2.*FC(M)*CFILE(L+((IPACT-1)+NOUT*(M-1))*NPLOTS)*COST
         end do
        end do
       end do
      end do

      RETURN
      END

      SUBROUTINE HORSUM(DOMEGA,TSHIFT,U,LX,MX,
     &                  idep_req,nazim_req,ipact,iparm)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      COMPLEX U(1),COST
      IF (MSUFT.GT.MMAX) STOP '>>> MMAX TOO SMALL IN compar.f <<<'
      NELM=nazim_req*NPLOTS*nout
      CALL CVFILL(CNUL,U,2,NELM)
      CALL RWDBUF(31)
      if (deconv) then
c >>> deconvolve with centre freq.
       omedec=frqdec*2e0*pi
      else
       omedec=0e0
      end if
 
      drazim=2*pi/nazim_req     
c >>> reverse cos and sin terms for transverse components
      do J=LX,MX
       COST=EXP(cmplx(-OMEGIM,(J-1)*DOMEGA-omedec)*TSHIFT)*CFFS(J)
       do K=1,IR
        CALL RDBUF(31,CFILE,2*NOUT*NPLOTS*msuft)
        if (k.eq.idep_req) then
         do iazim=1,nazim_req
          theta=(iazim-1)*drazim
          do L=1,NPLOTS
           INDEX=iazim+nazim_req*((L-1)+(IPACT-1)*NPLOTS)
           do m=1,msuft
            mord=m/2
            IF (m.eq.1) then
             fc=1E0
            else if (mod(m,2).eq.0) then
             if (iparm.EQ.4.or.iparm.eq.8) THEN
              FC=SIN(MORD*THETA)
             else
              FC=COS(MORD*THETA)
             end if
            else
             if (iparm.EQ.4.or.iparm.eq.8) THEN
              FC=-COS(MORD*THETA)
             else
              FC=SIN(MORD*THETA)
             end if
            end if
            U(INDEX)=U(INDEX)+
     &             2.*FC*CFILE(L+((IPACT-1)+NOUT*(M-1))*NPLOTS)*COST
           end do
          end do
         end do
        end if
       end do
      end do
      RETURN
      END

      SUBROUTINE CONDRWP(TITLEL,NPX,NPY,NXG,NYG,XLEFT,XRIGHT
     1,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN
     1,ZMAX,ZSTEP,FREQL,SDL,RECUP,RECLO,X1,XL,PX)
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
      INCLUDE 'complo.f'

      DIMENSION SECTOR(28),PX(1)
      CHARACTER*(*) TITLEL
      CHARACTER*4 TITLEX(20),TITLEY(20)

      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,
     1NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/
      DATA DUMMY /0./
      DATA TITLEX /'Rang','e (m',')   ',17*'    '/
      DATA TITLEY /'Dept','h (m',')   ',17*'    '/
C
C   FORMATS
  401 FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')
  402 FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')
  403 FORMAT(1H ,F15.4,3X,'  DIVX ' )
  404 FORMAT(1H ,F15.4,3X,'  DIVY ' )
  405 FORMAT(1H ,F15.4,3X,'  FLAGRC ' )
  406 FORMAT(1H ,F15.4,3X,'  RDUP ' )
  407 FORMAT(1H ,F15.4,3X,'  RDLO ' )
  408 FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )
  409 FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )
  410 FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )
  412 FORMAT(1H ,E15.2,3X,'  SCALE ' )
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
  800 FORMAT('CONDR,FIP,FMT,',A)
C 801 FORMAT(A50)
  801 FORMAT(A25)
  850 FORMAT(20A4)
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,1X,F15.4,3X,'  XRIGHT',/,1X,
     *       F15.4,3X,'  XSCALE',/,1X,F15.4,3X,'  XINC')
  901 FORMAT(1X,F15.4,3X,'  YUP',/,1X,F15.4,3X,'  YDOWN',/,1X,
     *       F15.4,3X,'  YSCALE',/,1X,F15.4,3X,'  YINC')
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,1H ,F15.4,1X,'    RMAX')
      WRITE(55,800) CONTOPT
      WRITE(55,860)TITLEL
  860 FORMAT(1X,A80)
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX
      sector(2)=npy
C
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR
       SECTOR(4)=1.0
       sector(6)=abs(xl-x1)*1.0E3/(npx-1)
      WRITE(17,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6E13.4)
      INQUIRE(UNIT=17,NAME=FILENBDR)
      WRITE(55,801) FILENBDR
      IF (ABS(XL-X1).LT.1.0) THEN
      TITLEX(2)='e (m'
      TITLEX(3)=')   '
      DIVX=1E0
      ELSE
      TITLEX(2)='e (k'
      TITLEX(3)='m)  '
      DIVX=1E-3
      END IF
      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(55,850)TITLEX
      R1=X1*1.0E3
      R2=XL*1.0E3
      WRITE(55,950)R1,R2
      AX1=XLEFT*1.0E3
      AX2=XRIGHT*1.0E3
      AX3=XSCALE*1.0E3
      AX4=XINC*1.0E3
      WRITE(55,900)AX1,AX2,AX3,AX4
      WRITE(55,850)TITLEY
      WRITE(55,901)YUP,YDOWN,YSCALE,YINC
      WRITE(55,401)FLOAT(NPX)
      WRITE(55,402)FLOAT(NPY)
      WRITE(55,403)DIVX
      WRITE(55,404)DIVY
      WRITE(55,405)FLAGRC
      WRITE(55,406)RECUP
      WRITE(55,407)RECLO
      WRITE(55,408)SDL
C   NUMBER OF GRID POINTS ALONG THE X AXIS
      WRITE(55,409)FLOAT(NXG)
C   NUMBER OF GRID POINTS ALONG THE Y AXIS
      WRITE(55,410)FLOAT(NYG)
      WRITE(55,411)FREQL
      WRITE(55,412)SCALE
      WRITE(55,413)CAY
      WRITE(55,414)FLOAT(NRNG)
      WRITE(55,415)ZMIN
      WRITE(55,416)ZMAX
      WRITE(55,417)ZSTEP
C X ORIGIN  OF PLOT IN INCHES
      WRITE(55,418)X1PL
      WRITE(55,419)DUMMY
C Y ORIGIN  OF PLOT IN INCHES
      WRITE(55,420)Y1PL
      WRITE(55,421)FLOAT(NSM)
      WRITE(55,422)HGTPT
      WRITE(55,423)HGTC
      WRITE(55,424)FLOAT(LABPT)
      WRITE(55,425)FLOAT(NDIV)
      WRITE(55,426)FLOAT(NARC)
      WRITE(55,427)FLOAT(LABC)
      WRITE(55,428)FLOAT(LWGT)
      RETURN
      END
       SUBROUTINE CONDRBP(NP1,NP2,NPX,PX)
       DIMENSION PX(1)
       DO 1000 I=NP1,NP2
       PX(I-NP1+1)=PX(I)
1000   CONTINUE
       WRITE(17,444) (PX(L),L=1,NPX)
 444  FORMAT(1H ,6E13.4)
       RETURN
       END

      SUBROUTINE CONTRP(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT   
     $,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN              
     $,ZMAX,ZSTEP,FREQ,SD,RD,RECUP,RECLO,X1,XL)                 
      DIMENSION SECTOR(28)      
      CHARACTER*80 FILENM
      CHARACTER*4 TITLE(20),TITLEX(20),TITLEY(20)      
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,        
     *NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/         
      DATA DUMMY /0./
      DATA TITLEX /'Rang','e (k','m)  ',17*'    '/
      DATA TITLEY /'Rang','e (k','m)  ',17*'    '/
C                
C   FORMATS      
 401  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE RNG AXIS')           
 402  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE ANG AXIS')           
403   FORMAT(1H ,F15.4,3X,'  DIVX ' )
404   FORMAT(1H ,F15.4,3X,'  DIVY ' )
405   FORMAT(1H ,F15.4,3X,'  FLAGRC ' )
406   FORMAT(1H ,F15.4,3X,'  RDUP ' )
407   FORMAT(1H ,F15.4,3X,'  RDLO ' )           
408   FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )          
 409  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )         
 410  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )         
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )             
  412 FORMAT(1H ,F15.4,3X,'  RD ' )
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
  800 FORMAT('CONTR,FIP,FMT,CPX              ')             
 801  FORMAT(A80)                
  850 FORMAT(20A4)                    
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,            
     *'   XSCALE',/,F15.4,4X,'  XINC')
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,               
     *'   YSCALE',/,F15.4,4X,'  YINC')
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')
      WRITE(55,800)             
      WRITE(55,850)TITLE              
      SECTOR(1)=NPX
      sector(2)=NPY                   
C                
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE  
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST                   
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR                  
       SECTOR(4)=1.0                  
      WRITE(17,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6G13.5)
      INQUIRE(UNIT=17,NAME=FILENM)
      WRITE(55,801) FILENM
      DIVX=1E0
      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=1.
      WRITE(55,850)TITLEX             
      R1=RECUP
      R2=RECLO
      WRITE(55,950)R1,R2              
      AX1=XLEFT                 
      AX2=XRIGHT                
      AX3=XSCALE                
      AX4=XINC                  
      WRITE(55,900)AX1,AX2,AX3,AX4    
      WRITE(55,850)TITLEY             
      WRITE(55,901)YUP,YDOWN,YSCALE,YINC                   
      WRITE(55,401)FLOAT(NPX)
      WRITE(55,402)FLOAT(NPY)
      WRITE(55,403)DIVX              
      WRITE(55,404)DIVY              
      WRITE(55,405)FLAGRC              
      WRITE(55,406) XL*3.14159/180.              
      WRITE(55,407) X1*3.14159/180.                   
      WRITE(55,408)SD     
C   NUMBER OF GRID POINTS ALONG THE X AXIS                 
      WRITE(55,409) float(nx)          
C   NUMBER OF GRID POINTS ALONG THE Y AXIS                 
      WRITE(55,410) float(ny)          
      WRITE(55,411)FREQ      
      WRITE(55,412)RD              
      WRITE(55,413)CAY              
      WRITE(55,414)FLOAT(NRNG)              
      WRITE(55,415)ZMIN               
      WRITE(55,416)ZMAX               
      WRITE(55,417)ZSTEP              
C X ORIGIN  OF PLOT IN INCHES         
      WRITE(55,418)X1PL               
      WRITE(55,419)DUMMY              
C Y ORIGIN  OF PLOT IN INCHES         
      WRITE(55,420)Y1PL               
      WRITE(55,421)FLOAT(NSM)                
      WRITE(55,422)HGTPT              
      WRITE(55,423)HGTC               
      WRITE(55,424)FLOAT(LABPT)       
      WRITE(55,425)FLOAT(NDIV)        
      WRITE(55,426)FLOAT(NARC)        
      WRITE(55,427)FLOAT(LABC)        
      WRITE(55,428)FLOAT(LWGT)        
      RETURN     
      END        



      SUBROUTINE INTERPOL(UDSP,NX,DT)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      DIMENSION AX(NP),AY(NP),UDSP(NP)
      EQUIVALENCE (AX(1),CFF(1,2)),(AY(1),CFF(1,3))

      NPO=0

      DO 400 IJ=1,NP
      READ(65,*,END=402)AX(IJ),AY(IJ)
      NPO=NPO+1
  400 CONTINUE
  402 CLOSE(65)

      TINT1=AX(1)
      TINT2=AX(2)
      UDSP(1)=AY(1)
      IJ=2
      DO 403 II=2,NX
      TINTR=FLOAT(II-1)*DT
  407 IF(ABS(TINTR-TINT2).LT.1.E-12) GO TO 404
      IF(TINTR.GT.TINT2) GO TO 405
C     WRITE(6,*)' -407-',IJ,NX,AY(IJ-1),AY(IJ)
C     WRITE(6,*)II,TINT1,TINTR,TINT2
      UDSP(II)=AY(IJ-1)+(AY(IJ)-AY(IJ-1))*(TINTR-TINT1)/(TINT2-TINT1)
C     WRITE(6,*)II,IJ,TINT1,TINTR,TINT2
C     WRITE(6,*)AY(IJ-1),UDSP(II),AY(IJ)
      IF(TINTR+DT.LT.TINT2+1.E-12) GO TO 403
      GO TO 406
  405 CONTINUE
C     WRITE(6,*)' -405-TINTR,TINT2: ',II,IJ,TINTR,TINT2
      TINT1=TINT2
      IJ=IJ+1
      IF(IJ.GT.NPO)  THEN
        IJFIN=II+1
        GO TO 408
      END IF
      TINT2=AX(IJ)
      GO TO 407
  404 UDSP(II)=AY(IJ)
C     WRITE(6,*)' -404-',II,IJ,TINT1,TINTR,TINT2
C     WRITE(6,*)AY(IJ-1),UDSP(II),AY(IJ)
  406 TINT1=TINT2
      IF(IJ.GT.NPO)  THEN
        IJFIN=II+1
        GO TO 408
      END IF
      IJ=IJ+1
      TINT2=AX(IJ)
C     WRITE(6,*)' -406-',II,IJ
  403 CONTINUE
      IJFIN=NX+1
  408 IF(IJFIN.LT.NX) THEN
        DO 409 I=IJFIN,NX
  409   UDSP(I)=UDSP(I-1)
      END IF
C     WRITE(6,*)' IJFIN: ',IJFIN,II,IJ,NPO
      DO 100 I=NX-1,1,-1
  100 UDSP(I+1)=UDSP(I)
      UDSP(1)=0.0
      RETURN
      END
c
c >>>>> transmission loss plots
c
      SUBROUTINE TL_CONT(THETA,freq_req,XLEFT,XRIGHT,XAXIS,XINC,XSCALE,
     1                     YDOWN,YUP,YAXIS,YINC,YSCALE,
     2                     ZZMIN,ZZMAX,ZZSTEP,IPLOTTP,XL)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      DIMENSION PX(np)
      common /plbuff/ px
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY

      LFILN=LENSTR(basename)

      filnbuf=basename(1:LFILN)//'.bdr'
      OPEN(17,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')
      filnbuf=basename(1:LFILN)//'.cdr'
      OPEN(55,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')

c >>> only positive ranges or plane geometry
      if (xleft.gt.-1e-6.or.icdr.eq.1.or.R0.gt.1E-6) then
       IF ((IR*NPLOTS*NOUT).GT.NP3) THEN
        WRITE(6,*) '>>> TL_CONT: ARRAY CFF TOO SMALL <<<'
        write(6,*) ir,nplots,nout,ir*nplots*nout
        PAUSE
        RETURN
       END IF
       NELM=ISIZE
       IF ((NPLOTS*NOUT).GT.NELM) THEN
        WRITE(6,*) '>>> TL_CONT: ARRAY CFILE TOO SMALL <<<'
         PAUSE
        RETURN
       END IF

       CALL TRF_SF(THETA,FREQ_REQ,U)

       CALL CONDRWP(TITLEC,NPLOTS,IR,NPLOTS,IR,XLEFT,XRIGHT,XSCALE,
     1                 XINC,YUP,YDOWN,YSCALE,YINC,ZZMIN,ZZMAX,ZZSTEP,
     2                 FREQ,SD,RD,RDLOW,R0,XL,PX)

       do IDEP=1,IR
        do IRAN=1,NPLOTS
         INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
         if (iundef(idep+(iran-1)*ir).ne.1) then 
          PX(IRAN)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
         else
          px(iran)=real(u(index))
         end if
        end do
        CALL CONDRBP(1,NPLOTS,NPLOTS,PX)
       end do
      else
c >>> negative ranges as well

       IF ((2*IR*NPLOTS*NOUT).GT.NP3) THEN
        WRITE(6,*) '>>> TL_CONT: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
       END IF
       NELM=ISIZE
       IF ((2*NPLOTS*NOUT).GT.NELM) THEN
        WRITE(6,*) '>>> TL_CONT: ARRAY CFILE TOO SMALL <<<'
         PAUSE
        RETURN
       END IF
c       Negative ranges
       npl2=nplots*2-1
       ioff=np3/2
       theta2=theta+pi
c       write(6,*) 'ioff=',ioff,'th=',theta,'th2=',theta2
       CALL TRF_SF(THETA2,FREQ_REQ,U(ioff+1))
c       Positive ranges
       CALL TRF_SF(THETA,FREQ_REQ,U(1))
c       write(6,*) U(1),U(ioff+1)

       CALL CONDRWP(TITLEC,npl2,IR,npl2,IR,XLEFT,XRIGHT,XSCALE,
     1                 XINC,YUP,YDOWN,YSCALE,YINC,ZZMIN,ZZMAX,ZZSTEP,
     2                 FREQ,SD,RD,RDLOW,-XL,XL,PX)

       do IDEP=1,IR
        do IRAN=1,NPLOTS
         INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
c        write(6,*) idep,iran,index,iran+nplots-1,nplots-iran+1
         if (iundef(idep+(iran-1)*ir).ne.1) then 
          PX(IRAN+nplots-1)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
          if (iran.ne.1) then
           PX(nplots-iran+1)=
     &         -20.0*(alog10(max(abs(U(INDEX+ioff)),1e-20)))
          end if           
         else
          px(iran+nplots-1)=real(u(index))
          if (iran.ne.1) then
           px(nplots-iran+1)=real(U(INDEX+ioff))
          end if           
         end if
        end do

        CALL CONDRBP(1,2*NPLOTS-1,2*NPLOTS-1,PX)
       end do
      end if
      RETURN
      END

      SUBROUTINE TL_H_CONT(nazim_req,freq_req,idep_req,shading,shdfil,
     &                     XRIGHT,XAXIS,XINC,XSCALE,
     &                     ZZMIN,ZZMAX,ZZSTEP,IPLOTTP,XL)
c >>> Range-Range Contours
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf,cdummy
      character*(*) shdfil
      logical shading
      DIMENSION PX(np)
      common /plbuff/ px
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY

      LFILN=LENSTR(basename)

      filnbuf=basename(1:LFILN)//'.bdr'
      OPEN(17,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')
      filnbuf=basename(1:LFILN)//'.cdr'
      OPEN(55,FILE=filnbuf,STATUS='UNKNOWN',
     &        FORM='FORMATTED')

       IF ((IR*NPLOTS*NOUT).GT.NP3) THEN
        WRITE(6,*) '>>> TL_H_CONT: ARRAY CFF TOO SMALL <<<'
        write(6,*) ir,nplots,nout,ir*nplots*nout
        PAUSE
        RETURN
       END IF
      
       IF ((NPLOTS*nazim_req).GT.np) THEN
        WRITE(6,*) '>>> TL_H_CONT: ARRAY CFILE TOO SMALL <<<'
         PAUSE
        RETURN
       END IF

       npx=nplots
       npy=nazim_req
       nx=151
       ny=151
       dazim=360.0/nazim_req
       azimax=(nazim_req-1)*dazim
       ttmax=azimax*pi/180.
       drazim=pi*dazim/180.0
       rmin=r0
       rmax=xl
       nrng=nplots

       CALL CONTRP(TITLE,NPX,NPY,NX,NY,
     &             -xright,XRIGHT,XSCALE,XINC,
     &             -xright,xright,xSCALE,xINC,
     &             zzmin,zzMAX,zzSTEP,FREQ_req,SD,RDC(idep_req),
     &             rmin,rmax,0.0,azimax)

        do iazim=1,nazim_req
         theta=(iazim-1)*drazim
         CALL TRF_SF(THETA,FREQ_REQ,U)
         do IRAN=1,NPLOTS
          i0=(iran-1)*nazim_req+iazim
          INDEX=IDEP_REQ+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
c          write(6,*) 'i0,index=',i0,index
          if (iundef(idep_req+(iran-1)*ir).ne.1) then 
           px(i0)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
          else
           px(i0)=real(u(index))
          end if
         end do
        end do
        do iran=1,nplots
         i0=(iran-1)*nazim_req+1
         call condrbp(1,nazim_req,nazim_req,px(i0))
        end do
 
      if (mtvout) then
         write(6,*) 'Write plotmtv script directly !!'
         filnbuf=basename(1:LFILN)//'.mtv'
         open(77,file=filnbuf,status='unknown')
         write(77,*) '$data=contcurve'
         do ii=len(title),1,-1
            if (title(ii:ii).ne.' ') then
               iii=ii
               goto 2000
            end if
         end do
 2000    continue
         RD=RDC(idep_req)
         write(77,'(1x,a,a,a)') '%toplabel="',title(1:iii),'"'
         write(77,'(1x,a,f7.1,a,f6.1,a,f6.1,a)') 
     &        '%subtitle="F=',FREQ,'Hz  SD=',SD,'m   RD=',RD,'m"'
         write(77,*) '%contfill=true'
         write(77,*) '%cmin=',-zzmax
         write(77,*) '%cmax=',-zzmin
         write(77,*) '%cstep=',zzstep
         if (rmax.ge.1.0) then
          write(77,*) '%xlabel="Range (km)" ylabel="Range (km)"' 
         else
          write(77,*) '%xlabel="Range (m)" ylabel="Range (m)"' 
         end if
         write(77,*) '%yflip=true'
         ttmax=azimax*(4.*atan(1.))/180.
         do i=1,nazim_req*nrng
            px(i)=-px(i)
         end do
         if (rmax.ge.1.0) then
          call polar_mtv(px,nazim_req,0.,ttmax,nazim_req,
     &        rmin,rmax,nrng,77)
         else
          call polar_mtv(px,nazim_req,0.,ttmax,nazim_req,
     &        rmin*1E3,rmax*1E3,nrng,77)
         end if
         do i=1,nazim_req*nrng
            px(i)=-px(i)
         end do
c
c >>> add shaded polygon
c
         ioff=np/2
         nshd=1
         if (shading) then
          open(72,file=shdfil,status='old',form='formatted')
          read(72,'(a)',end=8358,err=8358) cdummy 
 8355     read(72,*,end=8358,err=8358) px(nshd),px(nshd+ioff)          
          if (rmax.lt.1.0) then
           px(nshd)=px(nshd)*1E3
           px(nshd+ioff)=px(nshd+ioff)*1E3
          end if
          nshd=nshd+1
          px(nshd)=px(1)
          px(nshd+ioff)=px(1+ioff)
          go to 8355
 8358     close(72)

          do i=1,nshd-1
           write(77,'(a,4(g10.3,a))') 
     &            '@line x1=',px( i ),' y1=',px( i+ioff ),
     &                 ' x2=',px(i+1),' y2=',px(i+1+ioff),
     &            ' linetype=1 linewidth=2 linecolor=0'  
          end do
         end if
         close(77)
      end if

      RETURN
      END

      SUBROUTINE TL_freq(theta,iran_req,idep_req,
     1                    XLEFT,XRIGHT,XAXIS,XINC,
     1                    YDOWN,YUP,YAXIS,YINC,IPLOTTP)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      DIMENSION PX(np)
      common /plbuff/ px
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY
      NELM=NP3
      IF ((NX*NOUT).GT.NELM) THEN
        WRITE(6,*) '>>> TL_FREQ: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
      END IF
      NELM=ISIZE
      IF ((NPLOTS*NOUT*MSUFT).GT.NELM) THEN
        WRITE(6,*) '>>> TL_FREQ: ARRAY CFILE TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      CALL TRF_SR(THETA,iran_req,idep_req,U)
      do ifreq=lx,mx
       j=ifreq-lx+1
       freq=(ifreq-1)*domega/(2*pi)
       INDEX=ifreq+(IPLOTTP-1)*nx
       PX(j)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
c       write(6,*) ifreq,freq,px(ifreq)
      end do
      nfrq=mx-lx+1
      fr0=(lx-1)*domega/(2*pi)
      dfr=domega/(2*pi)
      rang=r0+(iran_req-1)*rspace
        call PP_PLFLOS(PX,NFRQ,fr0,dfr,TITLE,IPARM(IPLOTTP),
     1      XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RDC(idep_req),rang,theta*180./pi)

      RETURN
      END

      SUBROUTINE TL_range(theta,freq_req,idep_req,
     1                    XLEFT,XRIGHT,XAXIS,XINC,
     1                    YDOWN,YUP,YAXIS,YINC,IPLOTTP)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      logical neg_ran
      DIMENSION PX(NP)
      common /plbuff/ px
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY
      NELM=ISIZE
      IF ((NPLOTS*NOUT*MSUFT).GT.NELM) THEN
        WRITE(6,*) '>>> TL_RANGE: ARRAY CFILE TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      neg_ran = (xleft.lt.-1e-6.and.icdr.eq.0.and.r0.lt.rspace)

      if (neg_ran) then
       NELM=NP3/2
      else 
       NELM=NP3
      end if      
      IF ((IR*NPLOTS*nout).GT.NELM) THEN
        WRITE(6,*) '>>> TL_RANGE: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      CALL TRF_SF(THETA,FREQ_REQ,U)
      if (neg_ran) then
       lf=nplots*2-1
       ioff=np3/2
       theta2=theta+pi
       CALL TRF_SF(THETA2,FREQ_REQ,U(ioff+1))
      else
       lf=nplots
      end if

      if (lf.gt.np) then
       write(6,*) '>>> TL_RANGE: ARRAY PX TOO SMALL <<<'
       pause
       return
      end if

      if (neg_ran) then
       rmin=-(r0+(nplots-1)*rspace)
       do IDEP=1,IR
        if (idep.eq.idep_req) then
         do IRAN=1,NPLOTS
          INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
          PX(IRAN+nplots-1)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
          PX(nplots-iran+1)=
     &       -20.0*(alog10(max(abs(U(INDEX+ioff)),1e-20)))
          write(6,*) iran,px(iran)
         end do
         call PP_PLTLOS(PX,lf,rmin,RSPACE,TITLE,IPARM(IPLOTTP),
     1      XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RDC(idep))
        end if
       end do
      else 
       do IDEP=1,IR
        if (idep.eq.idep_req) then
         do IRAN=1,NPLOTS
          INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
          PX(IRAN)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
          write(6,*) iran,px(iran)
         end do
         call PP_PLTLOS(PX,lf,R0,RSPACE,TITLE,IPARM(IPLOTTP),
     1      XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RDC(idep))
        end if
       end do
      end if
      RETURN
      END

      SUBROUTINE TL_dav(theta,freq_req,
     1                    XLEFT,XRIGHT,XAXIS,XINC,
     1                    YDOWN,YUP,YAXIS,YINC,IPLOTTP)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      logical neg_ran
      DIMENSION PX(NP)
      common /plbuff/ px
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY
      NELM=ISIZE
      IF ((NPLOTS*NOUT*msuft).GT.NELM) THEN
        WRITE(6,*) '>>> TL_DAV: ARRAY CFILE TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      neg_ran = (xleft.lt.-1e-6.and.icdr.eq.0.and.r0.lt.rspace)

      if (neg_ran) then
       NELM=NP3/2
      else 
       NELM=NP3
      end if      
      IF ((IR*NPLOTS*nout).GT.NELM) THEN
        WRITE(6,*) '>>> TL_DAV: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      CALL TRF_SF(THETA,FREQ_REQ,U)
      if (neg_ran) then
       lf=nplots*2-1
       ioff=np3/2
       theta2=theta+pi
       CALL TRF_SF(THETA2,FREQ_REQ,U(ioff+1))
      else
       lf=nplots
      end if

      if (lf.gt.np) then
       write(6,*) '>>> TL_RANGE: ARRAY PX TOO SMALL <<<'
       pause
       return
      end if

      if (neg_ran) then
       rmin=-(r0+(nplots-1)*rspace)
       do IRAN=1,NPLOTS
        i1=iran+nplots-1
        i2=nplots-iran+1
        px(i1)=0e0
        px(i2)=0e0
        do IDEP=1,IR
         INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
         px(i1)=px(i1)+real(u(index))**2+rimag(u(index))**2
         px(i2)=px(i2)+real(u(index+ioff))**2+rimag(u(index+ioff))**2
c         write(6,*) iran,px(iran)
        end do
        PX(i1)=-10.0*(alog10(max(px(iran),1e-30)/ir))
        PX(i2)=-10.0*(alog10(max(px(iran),1e-30)/ir))
       end do

       call PP_PLTDAV(PX,lf,rmin,RSPACE,TITLE,IPARM(IPLOTTP),
     1      XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD)
      else
       do IRAN=1,NPLOTS
        px(iran)=0e0
        do IDEP=1,IR
         INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
         px(iran)=px(iran)+real(u(index))**2+rimag(u(index))**2
c         write(6,*) iran,px(iran)
        end do
        PX(IRAN)=-10.0*(alog10(max(px(iran),1e-30)/ir))
       end do

       call PP_PLTDAV(PX,nplots,R0,RSPACE,TITLE,IPARM(IPLOTTP),
     1      XAXIS,YAXIS,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD)
      end if

      RETURN
      END

      SUBROUTINE TL_depth(theta,freq_req,tlran,iran_req,
     1                    YUP,YDOWN,YAXIS,YINC,
     1                    TLEFT,TRIGHT,TAXIS,TINC,IPLOTTP)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*80 filnbuf
      DIMENSION PX(np)
      common /plbuff/ px
      COMPLEX U(NP3)
      EQUIVALENCE (U(1),CFF(1,1))

C *** CHECK SIZE OF U ARRAY
      NELM=NP3
      IF ((IR*NPLOTS*3).GT.NELM) THEN
        WRITE(6,*) '>>> TL_CONT: ARRAY CFF TOO SMALL <<<'
        PAUSE
        RETURN
      END IF
      NELM=ISIZE
      IF ((NPLOTS*NOUT).GT.NELM) THEN
        WRITE(6,*) '>>> TL_CONT: ARRAY CFILE TOO SMALL <<<'
        PAUSE
        RETURN
      END IF

      CALL TRF_SF(THETA,FREQ_REQ,U)

      do IRAN=1,NPLOTS
       if (iran.eq.iran_req) then
        do IDEP=1,IR
         INDEX=IDEP+IR*((IRAN-1)+(IPLOTTP-1)*NPLOTS)
         PX(idep)=-20.0*(alog10(max(abs(U(INDEX)),1e-20)))
         write(6,*) idep,rdc(idep),px(idep)
        end do
        call PP_PLTDEP(PX,IR,RDC(1),TITLE,IPARM(IPLOTTP),
     1      TAXIS,YAXIS,TLEFT,TRIGHT,TINC,
     2      yup,YDOWN,YINC,SD,r0+(iran_req-1)*rspace)
       end if
      end do

      RETURN
      END


      SUBROUTINE trf_sf(Theta,FREQ_req,U)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      COMPLEX U(1),COST
      REAL FC(MMAX),FCTAB(mmax)
      IF (MSUFT.GT.MMAX) STOP '>>> MMAX TOO SMALL IN compar.f <<<'
      FC(1)=1E0
      DO M=2,MSUFT,2
       MORD=M/2
       FCTAB(M)=COS(MORD*THETA)
       FCTAB(M+1)=SIN(MORD*THETA)
      end do

      NELM=IR*NPLOTS*NOUT
      omegar=freq_req*2E0*pi
      CALL CVFILL(CNUL,U,2,NELM)
      CALL RWDBUF(31)
      dif0=1e20
      do J=LX,MX
       omega=(J-1)*DOMEGA
       dif=abs(omega-omegar)
       if (dif.lt.dif0) then
        dif0=dif
        ix=j
       end if
      end do
      if (nout*nplots*msuft.gt.isize) then
       stop '*** ISIZE too small in TRF_SF ***'
      end if
      do j=lx,mx
       do K=1,IR
        CALL RDBUF(31,CFILE,2*NOUT*NPLOTS*MSUFT)
        if (j.eq.ix) then
         do I=1,NOUT
          DO M=2,MSUFT,2
c >>> reverse cos and sin terms for transverse components
           IF (IPARM(i).EQ.4.or.IPARM(i).eq.8) THEN
            FC(M)=fctab(m+1)
            FC(M+1)=-fctab(m)
           ELSE
            FC(M)=fctab(m)
            FC(M+1)=fctab(m+1)
           END IF
          end do
          do L=1,NPLOTS
           INDEX=K+IR*((L-1)+(I-1)*NPLOTS)
           U(index)=cfile(L+(I-1)*nplots)
           do M=2,MSUFT
            U(INDEX)=U(INDEX)+FC(M)*CFILE(l+((I-1)+NOUT*(M-1))*NPLOTS)
           end do
          end do
         end do     
        end if
       end do
      end do
c >>> Modify frequency
      freq=(ix-1)*domega/(2e0*pi)
      RETURN
      END

      SUBROUTINE trf_sr(Theta,iran_req,idep_req,U)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      COMPLEX U(1),COST
      REAL FC(MMAX),FCTAB(mmax)
      IF (MSUFT.GT.MMAX) STOP '>>> MMAX TOO SMALL IN compar.f <<<'
      FC(1)=1E0
      DO M=2,MSUFT,2
       MORD=M/2
       FCTAB(M)=COS(MORD*THETA)
       FCTAB(M+1)=SIN(MORD*THETA)
      end do

      NELM=IR*NPLOTS*3
      CALL CVFILL(CNUL,U,2,NELM)
      CALL RWDBUF(31)
      nfrq=mx-lx+1
      do ifreq=lx,mx
       do K=1,IR
        CALL RDBUF(31,CFILE,2*NOUT*NPLOTS*MSUFT)
        if (k.eq.idep_req) then
         do I=1,NOUT
          DO M=2,MSUFT,2
c >>> reverse cos and sin terms for transverse components
           IF (IPARM(i).EQ.4.or.IPARM(i).eq.8) THEN
            FC(M)=fctab(m+1)
            FC(M+1)=-fctab(m)
           ELSE
            FC(M)=fctab(m)
            FC(M+1)=fctab(m+1)
           END IF
          end do
          INDEX=ifreq+(I-1)*nx
          U(index)=cfile(iran_req+(I-1)*nplots)
          do M=2,MSUFT
           U(INDEX)=U(INDEX)+
     &              FC(M)*CFILE(iran_req+((I-1)+NOUT*(M-1))*NPLOTS)
          end do
         end do     
        end if
       end do
      end do
      RETURN
      END

      SUBROUTINE PP_PLFLOS(PX,LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RD,RANG,thetad)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** TRANSMISSION LOSS VS FREQUENCY     
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION PX(1)
      CHARACTER*80 TITLE,PTIT,XTXT,YTXT
      CHARACTER*16 LAB(20)
      CHARACTER*3  XTYP,YTYP
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLFRQ'
      IF (DEBUG) WRITE(6,*) 'ENTERING PLFLOS'
      PTIT='TRANSMISSION LOSS'
      I=MAX(INR,1)
      NLAB=4
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      if (abs(rang).lt.1.0) then
       WRITE(LAB(1),809) RANG*1E3
      else
       WRITE(LAB(1),810) RANG
      end if
      WRITE(LAB(4),814) thetad
      XTXT='Frequency (Hz)$'
      GOTO (901,902,903,904,905,906,907),INR
 901  YTXT='Normal stress (dB//1Pa)$'
      GO TO 909
 902  YTXT='Vert. particle velocity (dB//1m/s)$'
      go to 909
 903  YTXT='Hor. particle velocity (dB//1m/s)$'
      go to 909
 904  YTXT='Tran. particle velocity (dB//1m/s)$'
      go to 909
 905  YTXT='Radial stress (dB//1Pa)$'
      GO TO 909
 906  YTXT='Bulk pressure (dB//1Pa)$'
      GO TO 909
 907  YTXT='Shear stress (dB//1Pa)$'
 909  CONTINUE
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PP_PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
c      if (.not.rdoas) then
      CALL PP_PLTWRI(LF,RMIN,RSTEP,0.,0.,PX(1),1,PX(1),1)
c      end if
C *** FORMATS
 811  FORMAT('SD:',F8.1,' m$')
 812  FORMAT('RD:',F8.1,' m$')
 810  FORMAT('R: ',F8.1,' km$')
 809  FORMAT('R: ',F8.1,' m$')
 814  FORMAT('AZ:',F8.1,' dg$')
      RETURN
      END

      SUBROUTINE PP_PLTLOS(PX,LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RD)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** TRANSMISSION LOSS VS RANGE     
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION PX(1)
      CHARACTER*80 TITLE,PTIT,XTXT,YTXT
      CHARACTER*16 LAB(20)
      CHARACTER*3  XTYP,YTYP
      CHARACTER*6 OPTION(2)

      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLRAN'
      IF (DEBUG) WRITE(6,*) 'ENTERING PLTLOS'
      PTIT='TRANSMISSION LOSS'
      I=MAX(INR,1)
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      XTXT='Range (km)$'
      GOTO (901,902,903,904,905,906,907),INR
 901  YTXT='Normal stress (dB//1Pa)$'
      GO TO 909
 902  YTXT='Vert. particle velocity (dB//1m/s)$'
      go to 909
 903  YTXT='Hor. particle velocity (dB//1m/s)$'
      go to 909
 904  YTXT='Tran. particle velocity (dB//1m/s)$'
      go to 909
 905  YTXT='Radial stress (dB//1Pa)$'
      GO TO 909
 906  YTXT='Bulk pressure (dB//1Pa)$'
      GO TO 909
 907  YTXT='Shear stress (dB//1Pa)$'
 909  CONTINUE
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      nc=1

C *** WRITE PLP FILE
      CALL PP_PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
c      if (.not.rdoas) then
       CALL PP_PLTWRI(LF,RMIN,RSTEP,0.,0.,PX(1),1,PX(1),1)
c      end if
C *** FORMATS
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
      RETURN
      END

      SUBROUTINE PP_PLTDEP(PX,NUMD,RDA,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RD)
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
      INCLUDE 'complo.f'
      DIMENSION PX(1),RDA(1)
      CHARACTER*80 TITLE,PTIT,XTXT,YTXT
      CHARACTER*16 LAB(20)
      CHARACTER*3  XTYP,YTYP
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      IF (DEBUG) WRITE(6,*) 'ENTERING PLDEP'
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLDEP'
      I=MAX(1,INR)
      PTIT='TRANSMISSION LOSS'
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('Range:',F6.1,' km$')
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      GOTO (901,902,903,904,905,906),INR
 901  xTXT='Normal stress (dB//1Pa)$'
      GO TO 907
 902  xTXT='Vert. particle velocity (dB//1m/s)$'
      go to 907
 903  xTXT='Hor. particle velocity (dB//1m/s)$'
      go to 907
 904  xTXT='Tran. particle velocity (dB//1m/s)$'
      go to 907
 905  xTXT='Radial stress (dB//1Pa)$'
      GO TO 907
 906  xTXT='Bulk pressure (dB//1Pa)$'
 907  CONTINUE
      YTXT='Depth (m)$'
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PP_PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PP_PLTWRI(IR,0.,0.,0.,0.,PX(1),1,RDA(1),1)
      RETURN
      END

      SUBROUTINE PP_PLTDAV(PX,LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** DEPTH-AVERAGED LOSS VS RANGE
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION PX(1)
      CHARACTER*80 TITLE,PTIT,XTXT,YTXT
      CHARACTER*16 LAB(20)
      CHARACTER*3  XTYP,YTYP
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      IF (DEBUG) WRITE(6,*) 'ENTERING PLDAV'
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLDAV'
      PTIT='DEPTH AVERAGED LOSS'
      I=MAX(INR,1)
      NLAB=2
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      XTXT='Range (km)$'
      GOTO (901,902,903,904,905,906),INR
 901  YTXT='Normal stress (dB//1Pa)$'
      GO TO 907
 902  YTXT='Vert. particle velocity (dB//1m/s)$'
      go to 907
 903  YTXT='Hor. particle velocity (dB//1m/s)$'
      go to 907
 904  YTXT='Tran. particle velocity (dB//1m/s)$'
      go to 907
 905  YTXT='Radial stress (dB//1Pa)$'
      GO TO 907
 906  YTXT='Bulk pressure (dB//1Pa)$'
 907  CONTINUE
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PP_PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PP_PLTWRI(LF,RMIN,RSTEP,0.,0.,PX(1),1,PX(1),1)
C *** FORMATS
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
      RETURN
      END

      SUBROUTINE PP_PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      character*3 xopt(6)
      common /PLXOPT/ xopt
      CHARACTER*6 OPTION(2)
      CHARACTER*16 LAB(20)
      CHARACTER*80 TITLE,PTIT,XTXT,YTXT
      CHARACTER*3 XTYP,YTYP
C
C     WRITES GENERAL PLP FILE
C
      WRITE(19,777) OPTION,(xopt(j),j=1,6)
      WRITE(19,778) PTIT
      WRITE(19,778) TITLE
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'
      DO 10 ILAB=1,NLAB
 10   WRITE(19,779) LAB(ILAB)
      WRITE(19,6030) XLEN,'XLEN'
      WRITE(19,6030) YLEN,'YLEN'
      WRITE(19,6010) IGRID,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) XDIV,'XDIV'
      WRITE(19,778) XTXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YLO,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) YDIV,'YDIV'
      WRITE(19,778) YTXT
      WRITE(19,780) YTYP
      WRITE(19,6010) NC,'NC'
C *** FORMATS
 777  FORMAT(1H ,2A6,6(',',a3))
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 6010 FORMAT(1H ,I8,10X,A40)
 6020 FORMAT(1H ,F15.6,3X,A40)
 6030 FORMAT(1H ,G15.6,3X,A40)
      RETURN
      END
      SUBROUTINE PP_PLTWRI(N,XOFF,DX,YOFF,DY,X,IX,Y,IY)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION X(1),Y(1)
      WRITE(19,6010) N,'N'
      WRITE(19,6030) XOFF,'XOFF'
      WRITE(19,6030) DX,'DX'
      WRITE(19,6030) YOFF,'YOFF'
      WRITE(19,6030) DY,'DY'
      IF (DX.EQ.0E0) WRITE(20,444)(X(II),II=1,N,IX)                    
      IF (DY.EQ.0E0) WRITE(20,444)(Y(II),II=1,N,IY)                    
c 444  FORMAT(1H ,6G13.5)
 444  FORMAT(G13.6)
 6010 FORMAT(1H ,I8,10X,A40)
 6030 FORMAT(1H ,G15.6,3X,A40)
      RETURN
      END

      subroutine polar_mtv(z,ntmax,tmin,tmax,nt,rmin,rmax,nr,iunit)
      implicit none
      DOUBLE PRECISION PI2
      PARAMETER(PI2     =  0.628318530717958647692529D+01)
      integer ntmax,nt,nr,iunit
      real z(ntmax,*),tmin,tmax,rmin,rmax
      real eps,dt,dr
      real x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,r1,r2,t1,t2
      integer ir1,ir2,it1,it2
      logical norzero,tclosed
      data eps /1.e-5/
      if ((nr.gt.1).and.(nt.gt.1)) then
         dt=(tmax-tmin)/float(nt-1)
         dr=(rmax-rmin)/float(nr-1)
         if ((abs(tmax-tmin+dt-pi2)/pi2).lt.eps) then
            tclosed=.true.
         else
            tclosed=.false.
         end if
c
         do ir1=1,nr-1
            ir2=ir1+1
            r1=rmin+dr*float(ir1-1)
            r2=r1+dr
c
            if (r1.eq.0.) then
               norzero=.false.
            else
               norzero=.true.
            end if
c
            do it1=1,nt-1
               it2=it1+1
               t1=tmin+dt*float(it1-1)
               t2=t1+dt
c
               write(iunit,*) 

               x1=r1*cos(t1)
               y1=r1*sin(t1)
               z1=z(it1,ir1)
               write(iunit,1000) x1,y1,z1

               x2=r2*cos(t1)
               y2=r2*sin(t1)
               z2=z(it1,ir2)
               write(iunit,1000) x2,y2,z2

               x3=r2*cos(t2)
               y3=r2*sin(t2)
               z3=z(it2,ir2)
               write(iunit,1000) x3,y3,z3

               if (norzero) then
                  x4=r1*cos(t2)
                  y4=r1*sin(t2)
                  z4=z(it2,ir1)               
                  write(iunit,1000) x4,y4,z4
               end if
            end do
c
            if (tclosed) then
               it1=nt
               it2=1
               t1=tmax
               t2=tmin
c
               write(iunit,*) 
               x1=r1*cos(t1)
               y1=r1*sin(t1)
               z1=z(it1,ir1)
               write(iunit,1000) x1,y1,z1

               x2=r2*cos(t1)
               y2=r2*sin(t1)
               z2=z(it1,ir2)
               write(iunit,1000) x2,y2,z2
               
               x3=r2*cos(t2)
               y3=r2*sin(t2)
               z3=z(it2,ir2)
               write(iunit,1000) x3,y3,z3

               if (norzero) then
                  x4=r1*cos(t2)
                  y4=r1*sin(t2)
                  z4=z(it2,ir1)               
                  write(iunit,1000) x4,y4,z4
               end if
            end if
         end do
      else
         write(6,*) 'polar_mtv: Too small nt or nr !!'
      end if
 1000 format(1x,3e16.5)
      return
      end

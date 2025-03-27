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
     

      SUBROUTINE INENVI
C
C     SUBROUTINE FOR READING IN ENVIRONMENTAL DATA
C
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comti.f'
      INCLUDE 'comnrd.f'
C           
C ********************FORMATS*******************               
C           
C           
 350  FORMAT(1H0,'     DEPTH        ALPHA       BETA      ATTENA       AT        
     1TENB         RHO      ROUGHNESS')
 351  FORMAT(1H ,3F12.5,2F12.8,2F12.5)              
C           
C           
C **********************************************               
C           
C     DEFAULT: ISOTROPIC LAYERS
C
      NTISOL=0
      idlmax=0
C           
C     NUML IS THE NUMBER OF LAYERS INCLUDING THE HALF SPACES.  
C     NUMI IS THE NUMBER OF INTERFACES        
      DO 1 I=1,NLTYP
 1    NUMT(I)=0
      READ(1,*) NUML       
      IF (NUML.GT.NLA) THEN
      WRITE(6,*) '*** TOO MANY LAYERS ***'
      STOP
      END IF
      NUMI=NUML-1
      WRITE (6,350)
C
C     READ IN ENVIRONMENTAL DATA
C
      DO 110 M=1,NUML            
      READ(1,*) (V(M,N),N=1,6),ROUGH(M)
      disper(m)=.false.
      IF (ROUGH(M).LT.-1E-10) THEN
C
C     NON-KIRCHHOFF SCATTERING
C     BACKSPACE AND READ ALSO CORRELLATION LENGTH
        BACKSPACE(1)
        READ(1,*) (V(M,N),N=1,6),ROUGH(M),CLEN(M)
        WRITE(6,351) (V(M,N),N=1,6),ROUGH(M)
        WRITE(6,*) 'ROUGHNESS CORRELATION LENGTH (m):',CLEN(M)
      ELSE
        CLEN(M)=1E6
        WRITE(6,351) (V(M,N),N=1,6),ROUGH(M)
      END IF
C     TYPE OF LAYER
      IF (ABS(V(M,2)).LT.1E-10) THEN
        LAYTYP(M)=-1
      ELSE IF (V(M,2).GT.0E0.AND.ABS(V(M,3)).LT.1E-10) THEN
        LAYTYP(M)=1
        NUMT(1)=NUMT(1)+1
        LAYT(NUMT(1),1)=M
        IF (M.GT.2) THEN
          IF (V(M-1,3).EQ.-999.999) V(M-1,3)=-V(M,2)
        END IF
      ELSE IF (V(M,2).GT.0E0.AND.V(M,3).LT.0) THEN
        LAYTYP(M)=2
        NUMT(2)=NUMT(2)+1
        LAYT(NUMT(2),2)=M
        IF (M.GT.2) THEN
          IF (V(M-1,3).EQ.-999.999) V(M-1,3)=-V(M,2)
        END IF
      ELSE IF (V(M,2).GT.0E0.AND.V(M,3).GT.0) THEN
        LAYTYP(M)=3
        NUMT(3)=NUMT(3)+1
        LAYT(NUMT(3),3)=M
C
C     CHECK WHETHER PARAMETERS ARE PHYSICALLY MEANINGFUL
C
        GAMMA2=(V(M,3)/V(M,2))**2
        IF (GAMMA2.GT.0.75) THEN
          WRITE(6,*) '>>>>>WARNING: UNPHYSICAL SPEED RATIO, Cs/Cp>0.75'
        END IF
        IF ((GAMMA2*V(M,5)).GT.(0.75*V(M,4))) THEN
          WRITE(6,*) 
     &  '>>>>>WARNING: UNPHYSICAL ATTENUATION, (As/Ap)*(Cs/Cp)**2>0.75'
        END IF
      else if (nint(v(m,2)).eq.-3) then
c >>> dispersive elastic medium
        LAYTYP(M)=3
        disper(m)=.true.
        NUMT(3)=NUMT(3)+1
        LAYT(NUMT(3),3)=M
c >>>  read type of dispersive layer
        read(1,*) idltyp(m)
        idlmax=max(idlmax,idltyp(m))
        if (idltyp(m).lt.1) stop '>>> ERROR in dispers. layer type <<<'
        if (idltyp(m).gt.mndlt) 
     &   stop '>>> Too many dispersive layer types <<<'
      ELSE IF (V(M,2).LT.0E0) THEN
        LAYTYP(M)=4
        NUMT(4)=NUMT(4)+1
        LAYT(NUMT(4),4)=M
        NTISOL=NTISOL+1
        LAYNTI(NTISOL)=M
        WRITE(6,*) 'LAYER',M,' IS TRANSV. ISOTROPIC'
        WRITE(6,*) 'DEPTH: ',V(M,1),' M'
c *** Thinly layered medium
        IF (nint(V(M,2)).eq.-1) THEN
         READ(1,*) NSL(M)
         AHSUM=0E0
         write(6,*) 'SPECIFIED AS THINLY LAYERED MEDIUM:'
         WRITE(6,*) 'LAYERS:',NSL(M)
         WRITE(6,352)
 352  FORMAT(1H ,8X,'   ALPHA         BETA         ATTNA        ATTNB'
     &       ,'        RO       FRACTION')       
         AMEAN=0E0
         BMEAN=0E0
         DO 109 NNN=1,NSL(M)
          READ(1,*) RASP,RBSP,ATTA,ATTB,
     &              ARO(NNN,M),AH(NNN,M)
          AMEAN=AMEAN+RASP
          BMEAN=BMEAN+RBSP
          WRITE(6,353) RASP,RBSP,ATTA,ATTB,ARO(NNN,M),AH(NNN,M)
 353      FORMAT((/1H ,8X,2(3X,F9.2),2(3X,F9.6),2(3X,F9.3)))
C
C       CONVERT ATTENUATIONS TO IMAGINARY PART OF VELOCITY
C
          CIMA=RASP*ATTA/54.57512
          CIMB=RBSP*ATTB/54.57512
          ASP(NNN,M)=CMPLX(RASP,CIMA)
          BSP(NNN,M)=CMPLX(RBSP,CIMB)
          AHSUM=AHSUM+AH(NNN,M)
 109     CONTINUE
c ***  convert densities to kg/m^3
         DO 212 N=1,NSL(M)
 212     ARO(N,M)=1E3*ARO(N,M)
         V(M,2)=AMEAN/NSL(M)
         V(M,3)=BMEAN/NSL(M)
         IF (ABS(AHSUM-1E0).GT.1E-4) 
     &     STOP '*** SUM OF FRACTIONS IS NOT 1.0 ***'
         CALL FINELAY(M)
       ELSE
        READ(1,*) RR,RI
        C11=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C13=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C33=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C44=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C66=CMPLX(RR,RI)
        READ(1,*) RH
c *** convert density to kg/m^3
        RH=RH*1E3
       END IF
       SLNORM(M)=SQRT(REAL(C44)/RH)
       V(M,3)=SLNORM(M)
       V(M,2)=SQRT(REAL(C11)/RH)
       V(M,6)=RH*1E-3
       CALL DETABC()
       CALL VMOV(A,1,ELAG(1,M),1,24)
       WRITE(6,*) 'C11=',C11
       WRITE(6,*) 'C13=',C13
       WRITE(6,*) 'C33=',C33
       WRITE(6,*) 'C44=',C44
       WRITE(6,*) 'C66=',C66
       WRITE(6,*) 'RHO=',V(M,6)
      ELSE
        STOP '*** UNKNOWN LAYER TYPE ***'
      END IF
 110  CONTINUE
      ROUGH(1)=ROUGH(2)
      DO 111 M=1,NUML
      ROUGH2(M)=ROUGH(M)**2
 111  CONTINUE   
      DO 1111 M=2,NUML
      IF (ROUGH2(M).GT.1E-10) THEN
        IF (LAYTYP(M-1).EQ.2.OR.LAYTYP(M).EQ.2) THEN
          WRITE(6,*) '**** ROUGHNESS NOT ALLOWED BETWEEN LAYERS ****'
          WRITE(6,*) '**** WITH SOUND SPEED GRADIENT. INSERT    ****'
          WRITE(6,*) '**** A DUMMY ISOVELOCITY LAYER.           ****'
          STOP '>>>> EXECUTION TERMINATED <<<<'
        ELSE IF (LAYTYP(M-1).EQ.4.OR.LAYTYP(M).EQ.4) THEN
          WRITE(6,*) '**** NON-KIRCHHOFF SCATTERING NOT INCLUDED ***'
          WRITE(6,*) '**** FOR ANISOTROPIC LAYERS. KIRCHHOFF     ***'
          WRITE(6,*) '**** APPROXIMATION WILL BE APPLIED.        ***'
          CLEN(M)=1E6
        ELSE
        END IF
      END IF
 1111 CONTINUE
C
C     THE DENSITIES ARE CONVERTED FROM G/CM**3 TO KG/M**3      
C           
      DO 112 M=1,NUML
        V(M,6)=V(M,6)*1E3      
 112  CONTINUE
      RETURN
      END
      subroutine dltable(lx,mx,dlfr)
c
c >>> subroutine for setting up parameter tables for dispersive
c     media
c
      include 'compar.f'
      include 'comnla.f'
      include 'comnp.f'
      character*80 filenm
      dimension x(5,mndfr)
      equivalence (x(1,1),cff(1,1))
      if (mx.gt.mndfr) stop '>>> Too many frequencies <<<'
      inquire(unit=1,name=filenm)
      ll=index(filenm,'.')
      open(unit=3,file=filenm(1:ll)//'dis',status='old',err=5000)
      do 20 i=1,idlmax
       write(6,*) 'Dispersive medium:',i
       read(3,*) nf
       do 10 j=1,nf
        read(3,*) (x(k,j),k=1,5)
 10     continue
       do 8 j=lx,mx
        fr=(j-1)*dlfr
        do 6 k=2,nf
         if (x(1,k).gt.fr.or.k.eq.nf) then
          xx=(fr-x(1,k-1))/(x(1,k)-x(1,k-1))
          cpdl(i,j)=x(2,k-1)+xx*(x(2,k)-x(2,k-1))
          csdl(i,j)=x(3,k-1)+xx*(x(3,k)-x(3,k-1))
          apdl(i,j)=x(4,k-1)+xx*(x(4,k)-x(4,k-1))
          asdl(i,j)=x(5,k-1)+xx*(x(5,k)-x(5,k-1))
          write(6,*) fr,cpdl(i,j),csdl(i,j),apdl(i,j),asdl(i,j)
          go to 8
         end if
 6      continue
 8     continue
 20   continue
      do 30 i=1,numl
       if (disper(i)) then
        ity=idltyp(i)
        v(i,2)=cpdl(ity,lx)
        v(i,3)=csdl(ity,lx)
        v(i,4)=apdl(ity,lx)
        v(i,5)=asdl(ity,lx)
       end if
 30    continue
      return
 5000     stop '>>> Dispersion file not found <<<'
      end
      SUBROUTINE INSRC(SD)
      INCLUDE 'compar.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comnla.f'
C *** DEFAULTS
      ISROW=1
      ISINC=0
      SRCTYP=1
      LS=1
      DELTA=1.
      THETA=0.
      FOCDEP=0.
      LTYP=1      
      if (PROGNM(1:5).EQ.'OASP3') THEN
c *** source type read in for 3d version
      READ(1,*) SRCTYP
      BACKSPACE(1)
      if (trfsou) then
        read(1,*) srctyp
        srctyp=99
        write(6,*)
        write(6,'(a)') ' Source type: 99'
        write(6,'(a)') ' trf-source array'
      else IF (SRCTYP.EQ.1) THEN
        READ(1,*) SRCTYP
        write(6,*)
        write(6,'(A)') ' SOURCE TYPE: 1'
        write(6,'(A)') ' Explosive source'
        write(6,'(A,G14.6,A)') ' Seismic moment: ',1.0,' N m'  
      ELSEIF(SRCTYP.EQ.2) THEN
        READ(1,*) SRCTYP,RFOR,DHANGLE,DVANGLE
        HANGLE=DHANGLE*PI/180.
        VANGLE=DVANGLE*PI/180.
        SINH=SIN(HANGLE)
        SINV=SIN(VANGLE)
        COSH=COS(HANGLE)
        COSV=COS(VANGLE)
        IF(ABS(SINH).LE.0.0001) SINH=0.
        IF(ABS(SINV).LE.0.0001) SINV=0.
        IF(ABS(COSH).LE.0.0001) COSH=0.
        IF(ABS(COSV).LE.0.0001) COSV=0.
        F1=RFOR*COSV*COSH
        F2=RFOR*COSV*SINH
        F3=RFOR*SINV
        write(6,*)
        write(6,'(A)') ' SOURCE TYPE: 2'
        write(6,'(A)') ' Point force'
        write(6,'(A,f8.2,A)') ' Hor. angle:',dhangle,' deg'
        write(6,'(A,f8.2,A)') ' Ver. angle:',dvangle,' deg'
        write(6,'(A,G14.6,A)') ' F  = ',rfor,' N'  
        write(6,'(A,G14.6,A)') ' Fx = ',f1,' N'  
        write(6,'(A,G14.6,A)') ' Fy = ',f2,' N'  
        write(6,'(A,G14.6,A)') ' Fz = ',f3,' N'  
      ELSEIF(SRCTYP.EQ.3) THEN
        READ(1,*) SRCTYP,RMOM,ADELTA
        RDELTA=ADELTA*PI/180.
        write(6,*)
        write(6,'(A)') ' SOURCE TYPE: 3'
        write(6,'(A)') ' Dip-slip'
        write(6,'(A,G14.6,A)') ' Seismic moment: ',rmom,' N m'  
        write(6,'(A,f8.2,A)') ' Dip angle:',adelta,' deg'
      ELSEIF(SRCTYP.EQ.4) THEN
        READ(1,*) SRCTYP,RMOM,ADELTA
        RDELTA=ADELTA*PI/180.
        write(6,*)
        write(6,'(A)') ' SOURCE TYPE: 4'
        write(6,'(A)') ' Strike-slip'
        write(6,'(A,G14.6,A)') ' Seismic moment: ',rmom,' N m'  
        write(6,'(A,f8.2,A)') ' Dip angle:',adelta,' deg'
      ELSEIF(SRCTYP.EQ.5 .OR. SRCTYP.EQ.6) THEN
        READ(1,*) SRCTYP,TENMOM(1),TENMOM(2),TENMOM(3),ATENDEL(3)
        TENDEL(3)=ATENDEL(3)*PI/180.
        ATENDEL(2)=ATENDEL(3)+90.
        TENDEL(2)=ATENDEL(2)*PI/180.
        ATENDEL(1)=90.
        TENDEL(1)=ATENDEL(1)*PI/180.
      else if (srctyp.eq.7) then
        read(1,*) srctyp,m11,m12,m13,m22,m23,m33
        write(6,'(a)') 'Source type 7: Seismic moment tensor'
        write(6,'(a,g15.6)') 'm11 =',m11
        write(6,'(a,g15.6)') 'm12 =',m12
        write(6,'(a,g15.6)') 'm13 =',m13
        write(6,'(a,g15.6)') 'm22 =',m22
        write(6,'(a,g15.6)') 'm23 =',m23
        write(6,'(a,g15.6)') 'm33 =',m33

      ENDIF
      else
C *** Source type = 1 for 2d version
       SRCTYP=1
      END IF
c *** source data
      if (lina.eq.1.and.extlar) then
       call opfilr(2,ierr)
       if (ierr.ne.0) stop '>>> Source file not found <<<'
       read(1,*) sd
       read(2,*) ls
       write(6,*)
       write(6,'(a)') 'Externally specified source array:'
       write(6,*)
       write(6,'(a)') '    Depth       Delay        Strength'
       do 700 is=1,ls
        read(2,*) sdc(is),sdelay(is),sstren(is)
        write(6,'(3(1x,f10.3))') sdc(is),sdelay(is),sstren(is)
 700   continue
      else IF (LINA.EQ.1.and.(.not.trfsou)) THEN
       READ(1,*) SD,LS,DELTA,THETA,LTYP,FOCDEP
       IF (LS.GT.NRD) THEN
        WRITE(6,*) '*** TOO MANY SOURCES ***'
        STOP
       END IF
       THETA=THETA*PI/180.
       IF (LTYP.LT.1.OR.LTYP.GT.5) LTYP=1
       DELTA=ABS(DELTA)
       CALL LINARR(SD)
       WRITE(6,901) SD,LS,DELTA,THETA*180./PI,LTYP
 901   FORMAT(/1H ,'VERTICAL SOURCE ARRAY:',
     1      /1H ,'MEAN DEPTH:          ',F7.1,' M',
     2      /1H ,'NUMBER OF SOURCES:   ',I5,
     3      /1H ,'SOURCE SPACING:      ',F7.1,' M',
     4      /1H ,'MAIN LOBE ANGLE:     ',F7.1,' DEG',
     5      /1H ,'ARRAY TYPE:          ',I5)
       IF (LTYP.EQ.3.OR.LTYP.EQ.5) WRITE(6,951) FOCDEP
 951   FORMAT(1H ,'FOCAL DEPTH:         ',F7.1,' M')
      ELSE
       READ(1,*) SD    
       SDC(1)=SD
      END IF

C
C     DETERMINATION OF SOURCE LAYERS
C
      WRITE(6,908)
 908  FORMAT(/1H ,'SOURCE DATA:',//1H ,'  N0. ','   DEPTH  ',
     1       'LAYER','      ZU        ZL')
      DO 906 I=1,LS
 906  CALL SOURCE(V,NUML,SDC(I),LAYS(I),ZUS(I),ZLS(I))
      WRITE(6,907) 1,SDC(1),LAYS(1),ZUS(1),ZLS(1)
      IF (LS.GT.1) THEN
       WRITE(6,907) LS,SDC(LS),LAYS(LS),ZUS(LS),ZLS(LS)
      END IF
 907  FORMAT(1H ,I6,F10.1,I6,2F10.1)
      RETURN
      END

      SUBROUTINE INREC(RD,RDLOW,idinc)
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
      INCLUDE 'comnrd.f'
      INCLUDE 'comnla.f'
      ztilt=0.0
      atilt=0.0
      if (PROGNM(1:5).EQ.'OASTL') THEN
       READ(1,*) RD,RDLOW,IR,IDINC
      ELSE if (tilt) then
       read(1,*) rd,rdlow,ir,ztilt,dtilt
       write(6,*)
       write(6,'(1x,a,f10.2,a)') 'Array tilt: ',dtilt,' deg'
       write(6,'(1x,a,f10.2,a)') 'Ref. depth: ',ztilt,' m'      
       write(6,*)
       atilt=dtilt*atan(1e0)/45.
      else
       READ(1,*) RD,RDLOW,IR
      END IF
      IF (IR.GT.1) THEN
       RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
      ELSE
       RDSTEP=1.
      END IF
      if (iabs(ir).gt.nrd) stop '>>> Too many receiver depths <<<'
      WRITE(6,918)
 918  FORMAT(/1H ,'RECEIVER DATA:',//1H ,'  NO. ','      DEPTH  ',
     1       'LAYER','           Z          DR')
      IF (IR.GT.0) THEN
        DO 920 JJ=1,IR
        rtmp=(JJ-1)*RDSTEP+RD
        rdc(jj)=ztilt+(rtmp-ztilt)*cos(atilt)
        ofstar(jj)=(rtmp-ztilt)*sin(atilt)
        CALL RECEIV(V,NUML,RDC(JJ),LAY(JJ),Z(JJ))
        WRITE(6,907) JJ,RDC(JJ),LAY(JJ),Z(JJ),ofstar(jj)
 920    CONTINUE
      ELSE
        IR=-IR
        READ(1,*) (RDC(JJ),jj=1,ir)
        DO 921 JJ=1,IR
c        READ(1,*) RDC(JJ)
        rtmp=rdc(jj)
        rdc(jj)=ztilt+(rtmp-ztilt)*cos(atilt)
        ofstar(jj)=(rtmp-ztilt)*sin(atilt)
        CALL RECEIV(V,NUML,RDC(JJ),LAY(JJ),Z(JJ))
        WRITE(6,907) JJ,RDC(JJ),LAY(JJ),Z(JJ),ofstar(jj)
 921    CONTINUE
      END IF
 907  FORMAT(1H ,I6,F10.1,I6,3F10.1)
      return
      end


      SUBROUTINE RECEIV(VL,NUMLL,RD,LR,Z)                    
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      V(1,1)=V(2,1)                   
      DO 10 I=2,NUML                  
      IF (V(I,1)-RD) 10,10,20         
 10   CONTINUE   
      LR=NUML    
      Z=RD-V(NUML,1)                  
      RETURN     
 20   LR=I-1     
      Z=RD-V(LR,1)                    
      RETURN     
      END        
      SUBROUTINE SOURCE(VL,NUMLL,SD,LSL,ZUP,ZLO)              
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      DO 10 I=2,NUML                  
      IF (V(I,1)-SD) 10,20,20         
 10   CONTINUE   
      LSL=NUML    
      ZUP=SD-V(NUML,1)                
      ZLO=-ZUP     
      RETURN     
 20   LSL=I-1     
      ZUP=SD-V(LSL,1)                  
      ZLO=V(I,1)-SD                   
      RETURN     
      END        
      SUBROUTINE PINIT1         
C     INITIALIZATION OF VARIABLES               
C               
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
c
       if (debug) write(6,*) 'Enter PINIT1'
      IERR=0
      IOERR=0
      IFN=0
      PI=4.0*ATAN(1E0)
      CNUL=CMPLX(0.,0.)
      AI = CMPLX(0E0,1E0)         
C
C     LAYER CLASSIFICATION:
C
C     LAYTYP=1:   ISOVELOCITY FLUID: V(I,3) = 0
C
C     LAYTYP=2:   1/C**2 LINEAR:     V(I,3) < 0 
C               -V(I,3) = VEL. AT LOWER BOUNDARY
C
C     LAYTYP=3:   ISOVELOCITY SOLID: V(I,3) > 0
C
C     LAYTYP=4:   TRANSVERSE ISOTROPIC SOLID: V(I,3) > 0
C
      LSOLF=NUML+1
      LSOLL=0
C
C     UPPER AND LOWER HALF SPACES MUST BE ISOVELOCITY
C
      IF (LAYTYP(1).EQ.2) THEN
        LAYTYP(1)=1
        V(1,3)=0
      END IF
      IF (LAYTYP(NUML).EQ.2) THEN
        LAYTYP(NUML)=1
        V(NUML,3)=0
      END IF
      DO 10 I=1,NUML
C
C     FOR SMALL GRADIENTS LAYER TREATED AS ISOVELOCITY
C
      IF (ABS(V(I,2)+V(I,3)).LT.1E-3) V(I,3)=0E0
      IF (LSOLL.EQ.0.AND.LAYTYP(I).EQ.3) THEN
      LSOLF=I
      END IF
      IF (LAYTYP(I).EQ.3) THEN
      LSOLL=I
      END IF
 10   CONTINUE
C
C     NUMBER OF SOLID LAYERS
C
      NSOL=LSOLL-LSOLF+1
      DO 15 IS=1,ISROW
      DO 15 M=1,MMAX
      DO 15 J=1,NLEQ               
      DO 15 I=1,NLA            
        SS(I,J,M,IS)=CNUL              
        R(I,J,M,IS)=CNUL               
 15   CONTINUE
      DO 16 J=1,NLEQ
       DO 16 I=1,NLA
        IPS(I,J)=0
 16   CONTINUE
      DO 20 K=1,NLEQ
      DO 20 J=1,NLEQ
      DO 20 I=1,NLA               
        AUP(I,J,K)=CNUL           
        ALO(I,J,K)=CNUL           
 20   CONTINUE  
      NEQ=0     
      IF (LAYTYP(1).LT.0) GO TO 100            
      IF (LAYTYP(1).EQ.3.OR.LAYTYP(1).EQ.4) GO TO 50             
C     LIQUID HALF SPACE         
      IPS(1,4)=1
      NEQ=1     
      GO TO 100 
C     SOLID HALF SPACE          
 50   IPS(1,4)=1
      IPS(1,5)=2
      IPS(1,6)=3
      NEQ=3     
 100  IF (NUMI.EQ.1) GO TO 200  
      DO 190 I=2,NUMI           
      IF (LAYTYP(I).EQ.3.OR.LAYTYP(I).EQ.4) GO TO 150            
C     LIQUID LAYER              
      IPS(I,1)=NEQ+1            
      IPS(I,4)=NEQ+2            
      NEQ=NEQ+2 
      GO TO 190 
 150  CONTINUE  
C     SOLID LAYER               
      DO 160 J=1,NLEQ              
 160  IPS(I,J)=NEQ+J            
      NEQ=NEQ+NLEQ 
 190  CONTINUE  
 200  IF (LAYTYP(NUML).LT.0) GO TO 300         
      IF (LAYTYP(NUML).EQ.3.OR.LAYTYP(NUML).EQ.4) GO TO 250         
      IPS(NUML,1)=NEQ+1         
      NEQ=NEQ+1 
      GO TO 300 
 250  IPS(NUML,1)=NEQ+1         
      IPS(NUML,2)=NEQ+2
      IPS(NUML,3)=NEQ+3         
      NEQ=NEQ+NLEQ/2 
 300  CALL DETBW
      IBW=MIN0(NEQ-1,IBW)
      CALL DETPNT
      RCC=1.0/V(LAYS((LS-1)/2+1),6)
      CALL VSMUL(V(1,6),1,RCC,RCON1,1,NUML)

c      DO 340 I=1,LS
c 340  RLIND(I)=FLOAT(LAYS(I)*2-1)
C
C     DETERMINE SOURCE AND RECEIVER POINTERS 
C
      DO 350 I=1,NUML
        NOSOU(I)=0
        IFSOU(I)=NRD
        ILSOU(I)=1
        NORCV(I)=0
        IFRCV(I)=NRD
        ILRCV(I)=1
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
        NORCV(LL)=NORCV(LL)+1
        IFRCV(LL)=MIN(IFRCV(LL),I)
        ILRCV(LL)=MAX(ILRCV(LL),I)
        IF (NORCV(LL).NE.(ILRCV(LL)-IFRCV(LL)+1)) THEN
          WRITE(6,*) '*** PINIT1: Receiver no.',I,' out of order ***'
          STOP
        END IF
        LT=LAYTYP(LL)
        NUMTR(LT)=NUMTR(LT)+1
        NRPNT(NUMTR(LT),LT)=I
 365  CONTINUE
      RETURN    
      END       
      SUBROUTINE PINIT2         
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
C
C     AIRY FUNCTION SOLUTION ADDED 840907
C     SPEED AT LOWER INTERFACE OF LAYER I IS
C     GIVEN BY -V(I,3) (REPLACING SHEAR).
C
C     COMPLEX CONTOUR INTEGRATION ADDED AS OPTION 'J'  850321
C
      OFFIMA=FREQ*OFFDB/(8.68588964*V(LAYS((LS-1)/2+1),2))
C
       if (debug) write(6,*) 'Enter PINI21'
      DISNRM=1E0/DSQ
      DO 10 I=1,NUML
c >>> check for dispersion
       if (disper(i)) then
        ity=idltyp(i)
        v(i,2)=cpdl(ity,nactf)
        v(i,3)=csdl(ity,nactf)
        v(i,4)=apdl(ity,nactf)
        v(i,5)=asdl(ity,nactf)
        write(6,*) (v(i,k),k=1,6)
       end if
      THICK(I)=0E0            
      IF (I.GT.1.AND.I.LT.NUML) THICK(I)=V(I+1,1)-V(I,1)        
      IF (LAYTYP(I).LT.0.OR.LAYTYP(I).EQ.4) GO TO 10             
      IF (LAYTYP(I).LE.2.AND.V(I,4).LE.0) THEN
        FK=FREQ/1000.
        VV=V(I,2)*FK*1E-6*(.007+.264/(2.89+FK**2))
      ELSE
        VV=V(I,4)
      END IF
      VI4=FREQ*VV/(8.68588964*V(I,2))           
      AK(I,1)=DSQ/V(I,2)+CMPLX(0E0,-VI4)            
      AK2(I,1)=AK(I,1)*AK(I,1)  
      ALAME(I,1)=CSQ*V(I,6)/AK2(I,1)            
C
C     AIRY FUNCTION SOLUTION ADDED 840907
C     SPEED AT LOWER INTERFACE OF LAYER I IS
C     GIVEN BY -V(I,3) (REPLACING SHEAR).
C
      IF (LAYTYP(I).EQ.2) THEN
      DELTA=AIMAG(AK(I,1))/REAL(AK(I,1))
      AKU=REAL(AK(I,1))
      AKL=-DSQ/V(I,3)
      AKU2=AKU*AKU*(1E0-DELTA*DELTA)
      AKL2=AKL*AKL*(1E0-DELTA*DELTA)
      GRAD=(AKL2-AKU2)/THICK(I)
      AA=SIGN(ABS(GRAD)**0.333333333333333,GRAD)
      ACO(I)=AA*CMPLX(1E0,DELTA*0.666666666666667)
      AAM2=1E0/(AA*AA)
      CCO(I)=AAM2*CMPLX(1E0,-DELTA*1.33333333333333)
      BCO(I)=CCO(I)*AKU2*CMPLX(1E0,2*DELTA)
C     WRITE(6,876) I,ACO(I),BCO(I),CCO(I)
C876  FORMAT(1H ,I3,3(/1H ,2G20.8))
      END IF
      IF (V(I,3).LT.1E-10) GO TO 10             
      VI5=FREQ*V(I,5)/(8.68588964*V(I,3))           
      AK(I,2)=DSQ/V(I,3)+CMPLX(0E0,-VI5)            
      AK2(I,2)=AK(I,2)*AK(I,2)  
      ALAME(I,2)=CSQ*V(I,6)/AK2(I,2)            
      ALAME(I,1)=ALAME(I,1)-2*ALAME(I,2)        
 10   CONTINUE  
C
C     DETERMINE INTEGRATION SAMPLING FOR N-K SCATTERING
      DO 12 I=2,NUML
      IF (LAYTYP(I-1).LE.0) THEN
        DLWNK(I)=REAL(AK(I,1))/NSIP
      ELSE IF (LAYTYP(I).LE.0) THEN
        DLWNK(I)=REAL(AK(I-1,1))/NSIP
      ELSE IF (LAYTYP(I).NE.4.AND.LAYTYP(I-1).NE.4) THEN
       IF (LAYS((LS-1)/2+1).LT.I) THEN
        DLWNK(I)=REAL(AK(I-1,1))/NSIP
       ELSE
        DLWNK(I)=REAL(AK(I,1))/NSIP
       END IF
      ELSE
        DLWNK(I)=1E10
      END IF
      IMX(I)=8E0/(CLEN(I)*DLWNK(I))
      IF (IMX(I).GE.1.AND.IMX(I).LT.IMXMIN) THEN
        IMX(I)=IMXMIN
        DLWNK(I)=8E0/(CLEN(I)*IMX(I))
      END IF
      if (DEBUG) WRITE(6,*) I,DLWNK(I),IMX(I)
 12   CONTINUE
      PCORR=CSQ*V(LAYS((LS-1)/2+1),6) 
        if (debug) write(6,*) pcorr        
      PCORR=1E0/PCORR
      DO 13 I=1,NUMT(1)
        LL=LAYT(I,1)
        CON1(LL)=PCORR*V(LL,6)*CSQ
 13   CONTINUE
      DO 14 I=1,NUMT(2)
        LL=LAYT(I,2)
        CON1(LL)=PCORR*V(LL,6)*CSQ
 14   CONTINUE
      DO 15 I=1,NUMT(3)
        LL=LAYT(I,3)
        CON1(LL)=PCORR*ALAME(LL,2)
        con6(ll)=pcorr*ak2(ll,1)*(alame(ll,1)+0.6666667*alame(ll,2))
 15     CONTINUE
      IF (NTISOL.GT.0) THEN
c        CALL RWDBUF(41)
        DO 20 I=1,NUMT(4)
        LL=LAYT(I,4)
C        RCON1(LL)=DSQ
C        CON1(LL)=-AI*CSQ*PCORR
         RCON1(LL)=1E0
         CON1(LL)=PCORR
 20     CONTINUE
      end if
c
c *** integration constants
c
      IF (ICDR.EQ.0) THEN
       FNIFAC =  SQRT( 2.0/PI ) * 0.5    
      ELSE
       FNIFAC = 1E0
      END IF
      FNI5=DLWVNO*FNIFAC
c
c *** source phases
c
       if (debug) write(6,*) 'calling PHASES'
      if (trfsou) then
       do is=1,ls
        cphfac(is)=cmplx(1e0,0e0)
        call rdbuf(81,cfile,2*nstrf)
        do isnm=1,nstrf
         sval(is,isnm)=cfile(isnm)
        end do
       end do
      else if (extlar) then
       do 700 is=1,ls
        cphfac(is)=sstren(is)*cexp(-ai*dsq*sdelay(is))
 700    continue
      else
       CALL PHASES()
      end if
C
C *** Source normalizations
c
      DO 900 IS=1,LS
      LL=LAYS(IS)
       IF (LAYTYP(LL).LE.2) THEN
C ***  FLUID SOURCES NORMALIZED TO YIELD P=1 AT R=1 m.
       IF (ICDR.EQ.1) THEN
        IF (NWVNO.GT.1) THEN
         cphfac(is)=cphfac(is)*SQRT(FREQ/V(LAYs(is),2))
     &                         /(v(lays(is),6)*csq)
        else
         cphfac(is)=freq*cphfac(is)/(v(lays(is),6)*csq)
        END IF
       ELSE
         cphfac(is)=cphfac(is)/(v(lays(is),6)*csq)
       END IF
      ELSE
C *** SEISMIC SOURCES NORMALIZED TO UNIT VOLUME INJECTION OR
C     UNIT FORCE.
       cphfac(is)=cphfac(is)/(4E0*PI*v(lays(is),6)*csq)
      END IF
 900  continue
C
C *** PRESSURE AND PARTICLE VELOCITY CONVERSION FACTORS
C
      CPFAC=1E0/PCORR
      CWUFAC=AI*DSQ

       if (debug) write(6,*) 'Exit PINIT2'
      RETURN    
      END       
      SUBROUTINE LINARR(SD)
      INCLUDE 'compar.f'
      INCLUDE 'comnrd.f'
C
      DO 10 I=1,LS  
 10   SDC(I)=SD+(I-1-(LS-1)/2.0)*ABS(DELTA)  
      RETURN        
      END           
      SUBROUTINE PHASES
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      VMEAN=0       
      IF (SRCTYP.GT.1) THEN
        INDV=3
      ELSE 
        INDV=2
      END IF

      DO 10 I=1,LS  
 10   VMEAN=VMEAN+V(LAYS(I),INDV)/LS       
      ANG=2*PI*FREQ*ABS(DELTA)*SIN(THETA)/VMEAN                  
      HMEAN=.5*(SDC(LS)+SDC(1))
      ALEN=ABS(SDC(LS)-SDC(1))
      IF (LS.EQ.1) GO TO 15
      GO TO (15,25,35,45,55),LTYP                
 15   DO 20 I=1,LS  
 20   CPHFAC(I)=CEXP(CMPLX(0.,-(I-1-(LS-1)/2.0)*ANG))/LS       
      RETURN        
 25   DO 30 I=1,LS  
      CPHFAC(I)=CEXP(CMPLX(0.,-(I-1-(LS-1)/2.0)*ANG))       
      FAC=1.0-COS((I-1)*2.0*PI/FLOAT(LS-1))                 
      CPHFAC(I)=CPHFAC(I)*FAC/LS           
 30   CONTINUE      
      RETURN        
 35   CONTINUE
      IF (ABS(THETA).GT.1E-8) THEN
      HH=FOCDEP-HMEAN
      DM=HH/SIN(THETA)
      R2=(HH/TAN(THETA))**2
      ELSE
      DM=FOCDEP
      R2=DM*DM
      END IF
      ALS=1E0/LS
      DO 40 I=1,LS
      IF (ABS(THETA).GT.1E-8) THEN
      H=FOCDEP-SDC(I)
      ELSE
      H=HMEAN-SDC(I)
      END IF
      D=SQRT(H*H+R2)
      ANG=(D-DM)*2*PI*FREQ/VMEAN
      CPHFAC(I)=CEXP(CMPLX(0.,ANG))
      FAC=1.0-COS((I-1)*2.0*PI/FLOAT(LS-1))
      CPHFAC(I)=CPHFAC(I)*FAC*ALS
 40   CONTINUE
      RETURN
 45   CONTINUE
      FSUM=0.
      DO 48 I=1,LS  
      CPHFAC(I)=CEXP(CMPLX(0.,-(I-1-(LS-1)/2.0)*ANG))       
      FAC=EXP(-(4.*(SDC(I)-HMEAN)/ALEN)**2)
      FSUM=FSUM+FAC
      CPHFAC(I)=CPHFAC(I)*FAC           
 48   CONTINUE      
      DO 49 I=1,LS
 49   CPHFAC(I)=CPHFAC(I)/FSUM
      RETURN        
 55   CONTINUE
      IF (ABS(THETA).GT.1E-8) THEN
      HH=FOCDEP-HMEAN
      DM=HH/SIN(THETA)
      R2=(HH/TAN(THETA))**2
      ELSE
      DM=FOCDEP
      R2=DM*DM
      END IF
      FSUM=0.
      DO 58 I=1,LS
      IF (ABS(THETA).GT.1E-8) THEN
      H=FOCDEP-SDC(I)
      ELSE
      H=HMEAN-SDC(I)
      END IF
      D=SQRT(H*H+R2)
      ANG=(D-DM)*2*PI*FREQ/VMEAN
      CPHFAC(I)=CEXP(CMPLX(0.,ANG))
      FAC=EXP(-(4.*(SDC(I)-HMEAN)/ALEN)**2)
      FSUM=FSUM+FAC
      CPHFAC(I)=CPHFAC(I)*FAC
 58   CONTINUE
      DO 59 I=1,LS
 59   CPHFAC(I)=CPHFAC(I)/FSUM
      RETURN
      END           
      SUBROUTINE CALIN3    
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX FACSQ
      NS=ICUT2-ICUT1+1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=NLEQ+1
       IF (ISIZE.LT.IR*ngfils*NPAR*MSUFT*ISROW) 
     &      STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*NPAR*MSUFT*ISROW) 
     &      STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size from remaining amount of memory
      NBLOCKS=MEMLFT()/NGFILS-1 
      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR*MSUFT*ISROW,NS*NOUT,NBLOCKS)
 5    CONTINUE
      IF (SCTOUT) THEN
C *** OPEN FILE AND WRITE HEADER
       CALL OPFILW(45,IOER)
       WRITE(45,*) FREQ,LAYS(1)
      END IF
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
      NGVALS=2*IR*MSUFT*ISROW*NPAR*NGFILS
C *** WAVENUMBER LOOP
      DO 20 II=ICUT1,ICUT2         
      WVNO=CMPLX(FAC(II),OFFIMA)
      CALL INITS
      CALL BUILD                  
      CALL SOLVE
C *** DETERMINANT INVERSE
      IF (DETERM) ARG(II)=DETMNT
      IF (IERR.GT.0) RETURN

      IF (DECOMP) THEN
       CALL KERDEC(CFILE)
      ELSE
       CALL KERNEL(CFILE)
      END IF
c *** tapering
      IF (II.LT.ICW1) THEN
       TFAC=0.5*(1E0+COS((II-ICW1)*PI/(ICUT1-ICW1+1)))
       CALL VSMUL(CFILE,1,TFAC,CFILE,1,NGVALS)
      ELSE IF (II.GT.ICW2) THEN
       TFAC=0.5*(1E0+COS((II-ICW2)*PI/(ICUT2-ICW2+1)))
       CALL VSMUL(CFILE,1,TFAC,CFILE,1,NGVALS)
      END IF
      DO 10 IFC=1,NGFILS
       do 10 ipp=1,npar
       IF (IOUT(ipp).GT.0) THEN
        INDXCF=1+IR*MSUFT*ISROW*(ipp-1+NPAR*(IFC-1))
        CALL WRBUF(LUOFF+IFC,CFILE(INDXCF),2*IR*MSUFT*ISROW)
       END IF
 10   CONTINUE
C
C *** OUTPUT ROUGH SURFACE DISCONTINUITIES FOR CALCULATING SCATTERED
C     FIELD
C
      IF (SCTOUT) CALL SCTRHS(CMPLX(1E0,0E0))
 20   CONTINUE     
c
c *** Endfile on grf files
      DO 30 IFC=1,NGFILS
      CALL ENFBUF(LUOFF+IFC)
 30   CONTINUE
C                   
      RETURN        
C                   
      END           
      SUBROUTINE CHKSOL
      INCLUDE 'compar.f'

          IF (IERR.NE.0) THEN
C            CALL PREQV(NUML,NUMI)
            WRITE(6,9987) IERR
9987        FORMAT(//1H ,'**** EXECUTION TERMINATED ***',
     -            //1H ,'**** ERROR NUMBER : ',I3)
            STOP
         END IF
         IF (IOERR.NE.0) THEN
            WRITE(6,9887) IOERR
9887        FORMAT(//1H ,'**** EXECUTION TERMINATED ****',
     -            //1H ,'**** IO- ERROR NUMBER : ',I3)
            STOP
         END IF
       RETURN
       END
      SUBROUTINE GETKNL(NREC,M,IS)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      IF (DEBUG) WRITE(6,*) 'ENTER GETKNL'
C
C    READ KERNELS FROM SCRATCH FILE
C
      IF (DEBUG) WRITE(6,*) 'RWDBUF'
      CALL RWDBUF(LUTGRN)
      IF (DEBUG) WRITE(6,*) 'EXIT RWDBUF'
      NN=2*NP
      DO 2 I=1,npar
       IF (IOUT(I).GT.0) THEN
        DO 1 JR=1,ICUT1-1
 1      CFF(JR,I)=CNUL
        DO 11 JR=ICUT2+1,NWVNO
 11     CFF(JR,I)=CNUL
       END IF
 2    CONTINUE
      LREC=2*IR*MSUFT*ISROW
      INDX=NREC+((IS-1)*MSUFT+(M-1))*IR
      DO 4 JR=ICUT1,ICUT2
      DO 3 I=1,npar
      IF (IOUT(I).GT.0) THEN
       CALL RDBUF(LUTGRN,CFILE,LREC)
       CFF(JR,I)=CFILE(INDX)
      END IF
 3    CONTINUE
 4    CONTINUE
C
C
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
      PB=ATAN2(RI,RR)
      RR=REAL(CFF(ICUT1+1))
      RI=AIMAG(CFF(ICUT1+1))
      PB2=ATAN2(RI,RR)
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
      PA=ATAN2(RI,RR)
      RR=REAL(CFF(ICUT2-1))
      RI=AIMAG(CFF(ICUT2-1))
      PA2=ATAN2(RI,RR)
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
      SUBROUTINE FLLNEG
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
C
      DO 10 I=1,3
      IF (IOUT(I).NE.1) GO TO 10
      IF (I.LT.3) THEN
      CALL CVMOV(CFF(ICUT1,I),2,CFF(ICUT1-1,I),-2,NWVNO/2)
      ELSE
      CALL CVNEG(CFF(ICUT1,I),2,CFF(ICUT1-1,I),-2,NWVNO/2)
      END IF
 10   CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     DETBW : FIPM VERSION
C
C             MAY 15, 1988 JKIM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DETBW
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
c      WRITE(6,54)
c 54   FORMAT(5X,'+++++ ENTERING DETBW +++++')
      IBW=0
      DO 500 IN=1,NUMI
      IN1=IN+1
      IF (V(IN,2).GT.1E-10) GO TO 100
C
C     VACUUM UPPER HALFSPACE
C     **********************
      IF (V(IN1,3).GT.1E-10) GO TO 50
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      I=IPS(IN1,1)
      J=IPS(IN1,4)
      IF (IN1.LT.NUML) IBW=MAX0(IBW,IABS(I-J))
      GO TO 500
 50   CONTINUE
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
      I=IPS(IN1,1)
      J=IPS(IN1,3)
      IBW=MAX0(IBW,IABS(I-J))
      IF (IN1.GE.NUML) GO TO 500
      K=IPS(IN1,4)
      L=IPS(IN1,6)
      IBW=MAX0(IBW,IABS(I-K),IABS(I-L),IABS(J-K),IABS(J-L))
      GO TO 500
 100  IF (V(IN,3).GT.1E-10) GO TO 200
C
C     LAYER OR HALFSPACE OVER INTERFACE IS LIQUID
C     *******************************************
      IF (V(IN1,2).GT.1E-10) GO TO 125
C     LOWER HALFSPACE IS VACUUM
C     -------------------------
      I=IPS(IN,4)
      J=IPS(IN,1)
      IF (IN.GT.1) IBW=MAX0(IBW,IABS(I-J))
      GO TO 500
 125  IF (V(IN1,3).GT.1E-10) GO TO 150
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      I=IPS(IN,4)
      J=IPS(IN1,1)
      IBW=MAX0(IBW,IABS(I-J))
      IF (IN.GE.NUMI) GO TO 130
      K=IPS(IN1,4)
      IBW=MAX0(IBW,IABS(I-K),IABS(J-K))
 130  IF (IN.LE.1) GO TO 500
      K=IPS(IN,1)
      IBW=MAX0(IBW,IABS(I-K),IABS(J-K))
      GO TO 500
C
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
 150  CONTINUE
      I=IPS(IN,4)
      J=IPS(IN1,1)
      K=IPS(IN1,3)
      IBW=MAX0(IBW,IABS(I-J),IABS(I-K),IABS(J-K))
      IF (IN.GE.NUMI) GO TO 180
      M=IPS(IN1,4)
      L=IPS(IN1,6)
      IBW=MAX0(IBW,IABS(I-M),IABS(I-L),IABS(J-M),IABS(J-L),
     1   IABS(K-M),IABS(K-L))
 180  IF (IN.LE.1) GO TO 500
      M=IPS(IN,1)
      IBW=MAX0(IBW,IABS(I-M),IABS(J-M),IABS(K-M))
      GO TO 500
 200  CONTINUE
C     LAYER OR HALFSPACE OVER INTERFACE IS SOLID
C     *******************************************
C     LOWER HALFSPACE VACUUM
C     ----------------------
      IF (V(IN1,2).GT.1E-10) GO TO 225
      I=IPS(IN,4)
      J=IPS(IN,6)
      IBW=MAX0(IBW,IABS(I-J))
      IF (IN.LE.1) GO TO 500
      K=IPS(IN,1)
      L=IPS(IN,3)
      IBW=MAX0(IBW,IABS(I-K),IABS(I-L),IABS(J-K),IABS(J-L))
      GO TO 500
 225  IF (V(IN1,3).GT.1E-10) GO TO 250
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      I=IPS(IN,4)
      J=IPS(IN,6)
      K=IPS(IN1,1)
      IBW=MAX0(IBW,IABS(I-J),IABS(I-K),IABS(J-K))
      IF (IN.GE.NUMI) GO TO 230
      M=IPS(IN1,4)
      IBW=MAX0(IBW,IABS(I-M),IABS(J-M),IABS(K-M))
 230  IF (IN.LE.1) GO TO 500
      M=IPS(IN,1)
      N=IPS(IN,3)
      IBW=MAX0(IBW,IABS(I-M),IABS(I-N),IABS(J-M),IABS(J-N),
     1         IABS(K-M),IABS(K-N))
      GO TO 500
 250  CONTINUE
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
      I=IPS(IN,4)
      J=IPS(IN,6)
      K=IPS(IN1,1)
      L=IPS(IN1,3)
      IBW=MAX0(IBW,IABS(I-J),IABS(I-K),IABS(I-L),IABS(J-K),
     1         IABS(J-L),IABS(K-L))
      IF (IN.GE.NUMI) GO TO 280
      M=IPS(IN1,4)
      N=IPS(IN1,6)
      IBW=MAX0(IBW,IABS(I-M),IABS(I-N),IABS(J-M),IABS(J-N),
     1         IABS(K-M),IABS(K-N),IABS(L-M),IABS(L-N))
 280  IF (IN.LE.1) GO TO 500
      M=IPS(IN,1)
      N=IPS(IN,3)
      IBW=MAX0(IBW,IABS(I-M),IABS(I-N),IABS(J-M),IABS(J-N),
     1         IABS(K-M),IABS(K-N),IABS(L-M),IABS(L-N))
 500  CONTINUE
c      WRITE(6,7) 
c 7    FORMAT(5X,'+++++ EXITTING DETBW +++++')
      RETURN
      END
      SUBROUTINE DETPNT
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
C
C     DETERMINE NUMBER OF ROWS FOR EACH INTERFACE
C
c      WRITE(6,54)
c 54   FORMAT(5X,'+++++ ENTERING DETPNT +++++')
      ICNT=1
      DO 510 IN=1,NUMI
      IN1=IN+1
      IF (V(IN,2).GT.1E-10) GO TO 100
C
C     VACUUM UPPER HALFSPACE
C     **********************
      IF (V(IN1,3).GT.1E-10) GO TO 50
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      NRI(IN)=1
      GO TO 500
 50   CONTINUE
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
      NRI(IN)=3
      GO TO 500
 100  IF (V(IN,3).GT.1E-10) GO TO 200
C
C     LAYER OR HALFSPACE OVER INTERFACE IS LIQUID
C     *******************************************
      IF (V(IN1,2).GT.1E-10) GO TO 125
C     LOWER HALFSPACE IS VACUUM
C     -------------------------
      NRI(IN)=1
      GO TO 500
 125  IF (V(IN1,3).GT.1E-10) GO TO 150
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      NRI(IN)=2
      GO TO 500
C
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
 150  CONTINUE
      NRI(IN)=4
      GO TO 500
 200  CONTINUE
C     LAYER OR HALFSPACE OVER INTERFACE IS SOLID
C     *******************************************
C     LOWER HALFSPACE VACUUM
C     ----------------------
      IF (V(IN1,2).GT.1E-10) GO TO 225
      NRI(IN)=3
      GO TO 500
 225  IF (V(IN1,3).GT.1E-10) GO TO 250
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      NRI(IN)=4
      GO TO 500
 250  CONTINUE
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
      NRI(IN)=6
 500  IRST(IN)=ICNT
      ICNT=ICNT+NRI(IN)
 510  CONTINUE
      NRI(NUML)=0
C
C     DET NCL(NLA)
C
      ICNT=1
      DO 10 IL=1,NUML
      NCL(IL)=0
      IF (V(IL,2).LT.1E-10) GO TO 10
      IF (V(IL,3).GE.1E-10) GO TO 5
      NCL(IL)=1
      GO TO 8
 5    NCL(IL)=3
 8    IF (IL.NE.1.AND.IL.NE.NUML) NCL(IL)=2*NCL(IL)
      ICST(IL)=ICNT
      ICNT=ICNT+NCL(IL)
 10   CONTINUE
C
C     DETERMINE FIRST POINTER FOR EACH LAYER
C
      ISTART(1)=1
      ISTART(2)=ISTART(1)+NRI(1)*NCL(1)
      IF (NUML.LE.2) GO TO 20
      DO 19 IL=3,NUML
 19   ISTART(IL)=ISTART(IL-1)+(NRI(IL-2)+NRI(IL-1))*NCL(IL-1)
 20   CONTINUE
C
C     DETERMINE POINTERS
C
      IF (NCL(1).EQ.0) GO TO 30
      DO 25 J=1,NCL(1)
      DO 22 L=1,NRI(1)
      IRN(ISTART(1)+(J-1)*NRI(1)+L-1)=IRST(1)+L-1
 22   CONTINUE
      ICP(ICST(1)+J-1)=ISTART(1)+(J-1)*NRI(1)
      IDP(ICST(1)+J-1)=ISTART(1)+(J-1)*(NRI(1)+1)
 25   CONTINUE
 30   DO 40 IL=2,NUML
      INA=IL-1
      INB=IL
      IF (NCL(IL).EQ.0) GO TO 40
      DO 35 J=1,NCL(IL)
      NR=NRI(INA)+NRI(INB)
      DO 32 L=1,NR
      IRN(ISTART(IL)+(J-1)*NR+L-1)=IRST(INA)+L-1
 32   CONTINUE
      ICP(ICST(IL)+J-1)=ISTART(IL)+(J-1)*NR
      IDP(ICST(IL)+J-1)=ISTART(IL)+(ICST(IL)-IRST(INA))+(J-1)*(NR+1)
 35   CONTINUE
 40   CONTINUE
C
C     DETERMINE NUMBER OF SPARSE ELEMENTS
C
      NNA=ISTART(NUML)+NRI(NUMI)*NCL(NUML)-1
      ICP(NEQ+1)=NNA+1
      DO 710 I=1,NEQ
      DO 705 J=ICP(I),ICP(I+1)-1
      IIRR=IRN(J)
      IICC=I-IIRR+IBW+1
      IICC=2*(IIRR+(IICC-1)*NEQ)-1
      INDB(J)=IICC
 705  CONTINUE
 710  CONTINUE
      IRHCOL=3*IBW+2
      NNB=(IRHCOL-1)*NEQ
      CALL DETIND
      RETURN
      END
      SUBROUTINE DETIND
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
c      WRITE(6,54) 
c 54   FORMAT(10X,'+++++ ENTERING DETIND +++++') 
      DO 10 IN=1,NUMI
      IN1=IN+1
      IF (V(IN,2).GT.1E-10) GO TO 100
C
C     VACUUM UPPER HALFSPACE
C     **********************
      IF (V(IN1,3).GT.1E-10) GO TO 50
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      I=IPS(IN1,1)
      J=IPS(IN1,3)
      INDR(I)=INDX2(IN,4)
      IST=ISTART(IN1)
      INDA(IST)=INDXUP(IN1,4,1)
      IF (IN.GE.NUMI) GO TO 10
      IST=IST+NRI(IN)+NRI(IN1)
      INDA(IST)=INDXUP(IN1,4,4)
      GO TO 10
 50   CONTINUE
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
      I=IPS(IN1,1)
      J=IPS(IN1,2)
      K=IPS(IN1,3)
      INDR(I)=INDX2(IN,4)
      INDR(J)=INDX2(IN,5)
      INDR(K)=INDX2(IN,6)
      IST=ISTART(IN1)
      IF (IN.GE.NUMI) THEN
      INC=NRI(IN)
      J2=3
      ELSE
      INC=NRI(IN)+NRI(IN1)
      J2=6
      END IF
      DO 60 J=1,J2
      DO 55 I=0,2
 55   INDA(IST+I)=INDXUP(IN1,4+I,J)
 60   IST=IST+INC
      GO TO 10
 100  IF (V(IN,3).GT.1E-10) GO TO 200
C
C     LAYER OR HALFSPACE OVER INTERFACE IS LIQUID
C     *******************************************
      IF (V(IN1,2).GT.1E-10) GO TO 125
C     LOWER HALFSPACE IS VACUUM
C     -------------------------
      I=IPS(IN,4)
      INDR(I)=INDX2(IN,4)
      IF (IN.EQ.1) THEN
      J1=4
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 110 J=J1,4,3
      INDA(IST)=INDXLO(IN,4,J)
 110  IST=IST+INC
      GO TO 10
 125  IF (V(IN1,3).GT.1E-10) GO TO 150
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      I=IPS(IN,4)
      J=IPS(IN1,1)
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,4)
      IF (IN.EQ.1) THEN
      J1=4
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 130 J=J1,4,3
      INDA(IST)=INDXLO(IN,1,J)
      INDA(IST+1)=INDXLO(IN,4,J)
 130  IST=IST+INC
      IF (IN.GE.NUMI) THEN
      J2=1
      IST=ISTART(IN1)
      INC=NRI(IN)
      ELSE
      J2=4
      IST=ISTART(IN1)
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 140 J=1,J2,3
      INDA(IST)=INDXUP(IN1,1,J)
      INDA(IST+1)=INDXUP(IN1,4,J)
 140  IST=IST+INC
      GO TO 10
C
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
 150  CONTINUE
      I=IPS(IN,4)
      J=IPS(IN1,1)
      K=IPS(IN1,2)
      L=IPS(IN1,3)
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,4)
      INDR(K)=INDX2(IN,5)
      INDR(L)=INDX2(IN,6)
      IF (IN.EQ.1) THEN
      J1=4
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 170 J=J1,4,3
      INDA(IST)=INDXLO(IN,1,J)
      INDA(IST+1)=INDXLO(IN,4,J)
      INDA(IST+2)=INDXLO(IN,5,J)
      INDA(IST+3)=INDXLO(IN,6,J)
 170  IST=IST+INC
      IF (IN.GE.NUMI) THEN
      J2=3
      IST=ISTART(IN1)
      INC=NRI(IN)
      ELSE
      J2=6
      IST=ISTART(IN1)
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 180 J=1,J2
      INDA(IST)=INDXUP(IN1,1,J)
      INDA(IST+1)=INDXUP(IN1,4,J)
      INDA(IST+2)=INDXUP(IN1,5,J)
      INDA(IST+3)=INDXUP(IN1,6,J)
 180  IST=IST+INC
      GO TO 10
 200  CONTINUE
C     LAYER OR HALFSPACE OVER INTERFACE IS SOLID
C     *******************************************
C     LOWER HALFSPACE VACUUM
C     ----------------------
      IF (V(IN1,2).GT.1E-10) GO TO 225
      I=IPS(IN,4)
      J=IPS(IN,5)
      K=IPS(IN,6)
      INDR(I)=INDX2(IN,4)
      INDR(J)=INDX2(IN,5)
      INDR(K)=INDX2(IN,6)
      IF (IN.LE.1) THEN
      J1=4
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 220 J=J1,6
      INDA(IST)=INDXLO(IN,4,J)
      INDA(IST+1)=INDXLO(IN,5,J)
      INDA(IST+2)=INDXLO(IN,6,J)
 220  IST=IST+INC
      GO TO 10
 225  IF (V(IN1,3).GT.1E-10) GO TO 250
C     NEXT LAYER OR HALFSPACE IS LIQUID
C     ---------------------------------
      I=IPS(IN,4)
      J=IPS(IN,5)
      K=IPS(IN,6)
      L=IPS(IN1,1)
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,4)
      INDR(K)=INDX2(IN,5)
      INDR(L)=INDX2(IN,6)
      IF (IN.LE.1) THEN
      J1=4
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 230 J=J1,6
      INDA(IST)=INDXLO(IN,1,J)
      INDA(IST+1)=INDXLO(IN,4,J)
      INDA(IST+2)=INDXLO(IN,5,J)
      INDA(IST+3)=INDXLO(IN,6,J)
 230  IST=IST+INC
      IF (IN.GE.NUMI) THEN
      J2=1
      IST=ISTART(IN1)
      INC=NRI(IN)
      ELSE
      J2=4
      IST=ISTART(IN1)
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 240 J=1,J2,3
      INDA(IST)=INDXUP(IN1,1,J)
      INDA(IST+1)=INDXUP(IN1,4,J)
      INDA(IST+2)=INDXUP(IN1,5,J)
      INDA(IST+3)=INDXUP(IN1,6,J)
 240  IST=IST+INC
      GO TO 10
 250  CONTINUE
C     NEXT LAYER OR HALFSPACE IS SOLID
C     --------------------------------
      I=IPS(IN,4)
      J=IPS(IN,5)
      K=IPS(IN,6)
      L=IPS(IN1,1)
      M=IPS(IN1,2)
      N=IPS(IN1,3)
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,2)
      INDR(K)=INDX2(IN,3)
      INDR(L)=INDX2(IN,4)
      INDR(M)=INDX2(IN,5)
      INDR(N)=INDX2(IN,6)
      IF (IN.LE.1) THEN
      J1=4
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 260 J=J1,6
      DO 255 I=0,5
 255  INDA(IST+I)=INDXLO(IN,I+1,J)
 260  IST=IST+INC
      IST=ISTART(IN1)
      IF (IN.GE.NUMI) THEN
      J2=3
      INC=NRI(IN)
      ELSE
      J2=6
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 280 J=1,J2
      DO 275 I=0,5
 275  INDA(IST+I)=INDXUP(IN1,I+1,J)
 280  IST=IST+INC
 10   CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     CHANGE OF INDR AND INDS DUE TO MORDER AND ISROW
C     DUE TO MOVING SOURCE
C
C     MAY 14, 1988 MODIFIED BY JKIM
C
C     CHANGES
C     (1) THE DIMENSIONS OF INDR AND INDS HAVE BEEN CHANGED TO
C         INDR(NLA,6,MMAX,ISROWMAX) AND INDS(NLA,6,MMAX,ISROWMAX)
C     (2) THIS CHANGE IS MADE IN ORDER TO HANDLE THE MULTIPLE RIGHT HAND SIDE
C         ( I.E. MSUFT*ISROW ) SIMULTANEOUSLY
C     (3) EXTRA R.H.S. TERMS ARE DUE TO THE NECESSITY OF HANDLING EACH SOURCE
C         DEPTH SEPARATELY
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 300 IS1=1,ISROW
        DO 300 K=1,MSUFT
          DO 300 I=1,NEQ
            IF(IS1.EQ.1 .AND. K.EQ.1) GO TO 300
            INDR(I+(K-1)*NEQ+(IS1-1)*NEQ*MSUFT)
     &      =INDR(I)+(K-1)*NLA3*2+(IS1-1)*NLA6*2
 300  CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOW INDS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 20 IS1=1,ISROW
        DO 20 K=1,MSUFT
          DO 20 I=1,NUML
            DO 20 J=1,6
              IF(IPS(I,J).GT.0) THEN 
                INDS(IPS(I,J)+(K-1)*NEQ+(IS1-1)*NEQ*MSUFT)
     &              =INDX2(I,J)+(K-1)*2*NLA3+(IS1-1)*NLA6*2
              ENDIF
 20   CONTINUE
C
C
c      WRITE(6,22)
c 22   FORMAT(10X,'+++++ EXITTING DETIND +++++')
      RETURN
      END
      INTEGER FUNCTION INDX2(I,J)
      INCLUDE 'compar.f'
      II=I+(J-1)*NLA
      INDX2=2*II-1
      RETURN
      END
      INTEGER FUNCTION INDXLO(I,J,K)
      INCLUDE 'compar.f'
      II=I+(J-1)*NLA+(K-1)*NLA*NLEQ
      INDXLO=2*II-1
      RETURN
      END
      INTEGER FUNCTION INDXUP(I,J,K)
      INCLUDE 'compar.f'
      II=I+(J-1)*NLA+(K-1)*NLA*NLEQ+NLA*NLEQ*NLEQ
      INDXUP=2*II-1
      RETURN
      END
      SUBROUTINE SCTRHS(C)
       COMPLEX C
C
C      DUMMY SUBROUTINE IN 3d
C
      RETURN
      END 


      SUBROUTINE CALELC
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
C     CALCULATES ELASTIC PARAMETERS FOR TRANS. ISOTR. LAYERS
C
C     CHANGED TO INCLUDE ATTENUATION 120988
C
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comti.f'
C
      DUM=0E0
      DO 10 I=1,NTISOL
        LAYN=LAYNTI(I)
C        SLNORM(LAYN)=1E10
C        DO 5 J=1,NSL(LAYN)
C          SLNORM(LAYN)=MIN(SLNORM(LAYN),REAL(BSP(J,LAYN)))
C 5      CONTINUE
C        CALL FINELAY(LAYN)
        CALL FINELAY(LAYN)
        SLNORM(LAYN)=SQRT(REAL(C44/RH))
        CALL VMOV(A,1,ELAG(1,LAYN),1,24)
        WRITE(21,*) 'LAYN',LAYN
        WRITE(21,*) 'A,B1,B0,C2,C1,C0,C44,C66,C33,C13,C11,RH:'
        WRITE(21,*) (ELAG(J,LAYN),J=1,12)
        WRITE(21,*) 'COMPRESSIONAL',SQRT(ELAG(11,LAYN)/910.)
        WRITE(21,*) 'SV',SQRT(ELAG(7,LAYN)/910.)
        WRITE(21,*) 'SH',SQRT(ELAG(8,LAYN)/910.)
        WRITE(21,*) '  '
 10   CONTINUE
      RETURN
      END
      SUBROUTINE CALSLN(SLOW)
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
C     CALCULATES VERTICAL SLOWNESSES, DISPL. AND STRESS COEFF. FOR
C     HORIZONTAL SLOWNESS SLOW
C
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comti.f'
      COMPLEX SLOW
C
      COMPLEX S1
C
      S1=SLOW*DSQ
C
      DO 10 I=1,NTISOL
      LAYN=LAYNTI(I)
      CALL VMOV(ELAG(1,LAYN),1,A,1,24)
      CALL TIEIGEN(S1)
      CALL WRBUF(41,S3UP,2*NTIPAR)
 10   CONTINUE
      RETURN
      END
      SUBROUTINE FINELAY(LAYN)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C NLAY media are required (NLAY=2 or 3) consisisting of
C           ALPHA(I),BETA(I),RHO(I),H(I), I=1 to NLAY
C               where ALPHA(I) is compresional speed,
C                     BETA(I) is shear speed,
C                     RHO(I) is density,
C                     H(I) is volume fraction         of Ith layer.
C                          H(1)+H(2)+H(3)=1
C Outputs are the 5 moduli and the density of the long wavelength
C  equivalent transversely isotropic medium.
C  Note: Usual case has 2 constituents so H(2)=1-H(1)
C  CHANGED TO COVER LOSSY CASE (COMPLEX VEL.) 860124
C
      INCLUDE 'compar.f'
      INCLUDE 'comti.f'
      INCLUDE 'comnla.f'
      COMPLEX CRHO(3),X
      COMPLEX XMU(3),GAM(3),XMTG(3),XMDG(3)
C
      NLAY=NSL(LAYN)
C
      DO 3 I=1,NLAY
        CRHO(I)=CMPLX(ARO(I,LAYN),0E0)
 3    CONTINUE
      DO 5 I=1,NLAY
        XMU(I)=ARO(I,LAYN)*BSP(I,LAYN)**2
        GAM(I)=(BSP(I,LAYN)/ASP(I,LAYN))**2
        XMTG(I)=XMU(I)*GAM(I)
        XMDG(I)=XMU(I)/GAM(I)
 5    CONTINUE
      CALL AV(NLAY,XMU,AH(1,LAYN),-1,C44)
      CALL AV(NLAY,XMDG,AH(1,LAYN),-1,C33)
      CALL AV(NLAY,GAM,AH(1,LAYN),1,X)
      C13=(1.-2.*X)*C33
      CALL AV(NLAY,XMU,AH(1,LAYN),1,C66)
      CALL AV(NLAY,XMTG,AH(1,LAYN),1,X)
      C11=4.*(C66-X)+C13*C13/C33
C    
C     C11,C13,C33,C44,C66   C12=C11-2*C66
C
      CALL AV(NLAY,CRHO,AH(1,LAYN),1,RH)
      return
      end
      SUBROUTINE DETABC
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
      INCLUDE 'comti.f'
      INCLUDE 'comnla.f'
      A=C13+C44
      B1=(C13+C44)*(C13+C44)/(C33*C44)
      B1=C11/C44 + C44/C33 -B1
      B0=-RH*(1./C44+1./C33)
      C2=C11/C33
      C1=-RH*(1+C11/C44)/C33
      C0=RH*RH/C33/C44
      RETURN
      END
C
      SUBROUTINE AV(N,X,H,J,XA)      
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C     IMPLICIT NONE
      INTEGER I,J,N
      COMPLEX X(3),XA
      REAL*4 H(3),V
      XA=0.
      DO 1 I=1,N
 1      XA=XA+H(I)*X(I)**J
      V=1./J
C  SMALL IMAGINARY PART
      R=REAL(XA)
      ANG=RIMAG(XA)*V/REAL(XA)
      XA=(R**V)*EXP(CMPLX(0E0,ANG))
      RETURN
      END
      SUBROUTINE TIEIGEN(S1)
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
      INCLUDE 'comti.f'
      INTEGER J
      COMPLEX S1,SQ1
      COMPLEX SQ3(2),B,C,CROOT,SCFAC(2)
      COMPLEX CYY11,CYY21,CYY31,CYY41,cyy51,cyy61,
     &        CYY12,CYY22,CYY32,CYY42,cyy52,cyy62,
     &        CYY53,CYY63
C
C     UNLIKE S1 IN TIEIG.F, NOW S1 IN TIEIGEN.F IS HORIZONTAL WAVE NUMBER
C
      SQ1=S1**2
      B0=B0*CSQ
      C1=C1*CSQ
      C0=C0*CSQ*CSQ
      B=(SQ1*B1+B0)/2.
      C=SQ1*SQ1*C2+SQ1*C1+C0
      CROOT=B*B-C
C     
C     TAKE B-SQRT(CROOT) AS SQ3(1)
C
C     VERIFY LATER ON
C
      SQ3(1)=B+SQRT(CROOT)
c      SQ3(1)=B-SQRT(CROOT)
      SQ3(2)=2.*B-SQ3(1)
C
C     FIND VERTICAL WAVE NUMBERS
C
      S3UP(1)=SQRT(SQ3(1))
      S3DN(1)=-S3UP(1)
      S3UP(2)=SQRT(SQ3(2))
      S3DN(2)=-S3UP(2)
      S3SHUP=SQRT((C66*SQ1-RH*CSQ)/C44)
      S3SHDN=-S3SHUP
C
C     FIND EIGENVECTORS FROM NOTES TAK15 AND TAK3
C
c *** Pseudo-P
c >>> u1
      CYY11=RH*CSQ-SQ1*C44+SQ3(1)*C33
c >>> u3
      CYY21=S1*S3UP(1)*(C13+C44)
c >>> sigma33
      CYY31=C33*S3UP(1)*CYY21-S1*C13*CYY11
c >>> sigma13
      CYY41=C44*(S3UP(1)*CYY11+S1*CYY21)
c >>> sigma11
      cyy51=-c11*s1*cyy11+c13*s3up(1)*cyy21
c >>> sigma22
      cyy61=C13*S3UP(1)*CYY21-S1*(C11-2*C66)*CYY11
C
c *** Pseudo-S
c >>> u1
      CYY12=RH*CSQ-SQ1*C44+SQ3(2)*C33
c >>> u3
      CYY22=S1*S3UP(2)*(C13+C44)
c >>> sigma33
      CYY32=C33*S3UP(2)*CYY22-S1*C13*CYY12
c >>> sigma13
      CYY42=C44*(S3UP(2)*CYY12+S1*CYY22)
c >>> sigma11
      cyy52=-c11*s1*cyy12+c13*s3up(2)*cyy22
c >>> sigma22
      cyy62=C13*S3UP(2)*CYY22-S1*(C11-2*C66)*CYY12
C
c >>> u2
      CYY53=1
c >>> sigma23
      CYY63=C44*S3SHUP
C
C     NORMALIZE
C
      SCFAC(1)=A*S3UP(1)
      SCFAC(1)=1./SCFAC(1)
      SCFAC(2)=A*S3UP(2)
      SCFAC(2)=1./SCFAC(2)
C
      CYY11=CYY11*SCFAC(1)      
      CYY21=CYY21*SCFAC(1)      
      CYY31=CYY31*SCFAC(1)      
      CYY41=CYY41*SCFAC(1)
      cyy51=cyy51*scfac(1)      
      cyy61=cyy61*scfac(1)      
C
      CYY12=CYY12*SCFAC(2)      
      CYY22=CYY22*SCFAC(2)      
      CYY32=CYY32*SCFAC(2)      
      CYY42=CYY42*SCFAC(2) 
      cyy52=cyy52*scfac(2)      
      cyy62=cyy62*scfac(2)      
C  
      CYY53=CYY53*S1
      CYY63=CYY63*S1
C
C
C
      U3DN(1)=-CYY21
      U3DN(2)= CYY22
      U3UP(1)= CYY21
      U3UP(2)= CYY22
C
      U1DN(1)=-CYY11
      U1DN(2)= CYY12
      U2DN=CYY53
      U1UP(1)=-CYY11
      U1UP(2)=-CYY12
      U2UP=CYY53
C
c >>> sigma33
      SIG3DN(1)=CYY31
      SIG3DN(2)=-CYY32
      SIG3UP(1)=CYY31
      SIG3UP(2)=CYY32
c >>> sigma13
      SIG13DN(1)= CYY41
      SIG13DN(2)=-CYY42
      SIG13UP(1)=-CYY41
      SIG13UP(2)=-CYY42
c >>> sigma23
      SIG23DN=-CYY63
      SIG23UP= CYY63
c >>> sigma11
      sig1dn(1)=cyy51
      sig1dn(2)=-cyy52
      sig1up(1)=cyy51
      sig1up(2)=cyy52
c >>> sigma22
      sig2dn(1)=cyy61
      sig2dn(2)=-cyy62
      sig2up(1)=cyy61
      sig2up(2)=cyy62
c >>> Bulk stress (sigma11+sigma22+sigma33)/3
      sigbdn(1)=(cyy51+cyy61+cyy31)*0.33333333
      sigbdn(2)=-(cyy52+cyy62+cyy32)*0.33333333
      sigbup(1)=(cyy51+cyy61+cyy31)*0.33333333
      sigbup(2)=(cyy52+cyy62+cyy32)*0.33333333
C
C
C
      RETURN
      END
      SUBROUTINE GETSLN(IL,NSLOW)
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
      INCLUDE 'comti.f'

      DIMENSION X(NP2,3)
      equivalence (X(1,1),CFF(1,1))

      CALL RWDBUF(41)
      DO 10 ISL=1,NSLOW
      DO 5 I=1,NTISOL
      LAYN=LAYNTI(I)
      CALL RDBUF(41,S3UP,52)
      IF (LAYN.EQ.IL) THEN
      X(ISL,1)=ABS(RIMAG(S3UP(1)))*SLNORM(IL)
      X(ISL,2)=ABS(RIMAG(S3UP(2)))*SLNORM(IL)
      X(ISL,3)=ABS(RIMAG(S3SHUP))*SLNORM(IL)
      END IF
 5    CONTINUE
 10   CONTINUE
      RETURN
      END
      SUBROUTINE PLSLOW(NSLOW,TITLE,
     1     XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC)
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
      INCLUDE 'comti.f'
      INCLUDE 'complo.f'
      DIMENSION X(NP2,3)
      equivalence (X(1,1),CFF(1,1))
      DIMENSION XS(NP2)
      EQUIVALENCE (XS(1),CFFS(1))
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      OPTION(1)=PROGNM
      OPTION(2)='SLOWNS'
      DO 10 I=1,NTISOL
      IL=LAYNTI(I)
      CALL GETSLN(IL,NSLOW)
      PTIT='VERTICAL SLOWNESS'
 811  FORMAT('Layer:',I4,'$')
 812  FORMAT('Cr: ',F6.1,' m/s$')
      NLAB=2
      WRITE(LAB(1),811) IL
      WRITE(LAB(2),812) SLNORM(IL)
      XTXT='Norm. horizontal slowness$'
      YTXT='Norm. vertical slowness$'
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=1
      NC=3
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL VSMUL(XS,1,SLNORM(IL),ARG,1,NSLOW)
      CALL PLTWRI(NSLOW,0.,0.,0.,0.,ARG,1,X(1,1),1)
      CALL PLTWRI(NSLOW,0.,0.,0.,0.,ARG,1,X(1,2),1)
      CALL PLTWRI(NSLOW,0.,0.,0.,0.,ARG,1,X(1,3),1)
 10   CONTINUE
      RETURN
      END

      SUBROUTINE SNSDGM(TITLE)
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
      INCLUDE 'comti.f'
      INCLUDE 'complo.f'
      DIMENSION X(NP2,3)
      equivalence (X(1,1),CFF(1,1))
      DIMENSION XS(NP2)
      EQUIVALENCE (XS(1),CFFS(1))
      CHARACTER*80 TITLE
      COMPLEX SLOW
C
C     SLOWNESS DIAGRAM FOR TRANS. ISOTR. MEDIA
C
      SLOW1=1E-8
      DO 10 I=1,NUMT(4)
       LAYN=LAYT(I,4)
       AA=SQRT(REAL(ELAG(12,LAYN))/REAL(ELAG(7,LAYN)))
       SLOW2=MAX(SLOW2,AA)
 10   continue
C       SLOWIM=OFFDB/(2E0*PI*8.6859*V(LAYS((LS-1)/2+1),2)) 
       NSLP=200
        DLSLOW=(SLOW2-SLOW1)/NSLP
       CALL OPNBUF(41,2*NTIPAR,NTISOL*(ICUT2-ICUT1+1),500)
       DO 116 ISL=1,NSLP
        SLOW=SLOW1+(ISL-1)*DLSLOW
        XS(ISL-ICUT1+1)=REAL(SLOW)
        DSQ=1E0
        CSQ=1E0
        CALL CALSLN(SLOW)
 116   CONTINUE
       CALL ENFBUF(41)
C    
C       PLOT SLOWNESS DIAGRAMS
C
          CALL PLSLOW(NSLP,TITLE,15.,15.,0.,1.0,0.2,1.0,0.0,0.2)
          CALL CLSBUF(41)
       RETURN
       END

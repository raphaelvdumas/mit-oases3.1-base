      SUBROUTINE SCNFOR()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNFOR : OASES VERSION
C
C     IMPLEMENTING POINT FORCE SOURCE IN SOLID MEDIUM, 120987 BY JKIM
C     MODIFIED BY FOR MOVING SOURCE MAY 15, 1988
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3
C
      S=WVNO
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
        cc3=ak2(ln,2)/(s*beta(ln))      
cvd$  novector
        DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + is* F3*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + F3*cc2*s*betinv(ln)      
c >>> m=2
        m=2
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + F1*cc1*s*alfinv(ln)
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + is* F1*cc2
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) + F1*cc3*cc2      
c >>> m=3
        m=3
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + F2*cc1*s*alfinv(ln)
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + is* F2*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) + F2*cc3*cc2      
 40     CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END

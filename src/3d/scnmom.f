      SUBROUTINE SCNMOM()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNMOM : OASES VERSION
C
C     SEISMIC MOMENT TENSOR SOURCE IN SOLID MEDIUM
C     HS SEP 13, 1994
c     Sep 15, 1994: Changed sign of P-potential in strike slip sources
c                   Now consistent with jkim appendix. 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5,cc6,cc7
      REAL tmom(3)
      real cc(3)
C
      S=WVNO
C
      CC(1)=-1.0
      CC(2)=-1.0
      CC(3)=1.0
      TMOM(1)=M11
      TMOM(2)=M22
      TMOM(3)=M33
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
        cc3=s2-2.0*alfa(ln)*alfa(ln)
        cc4=s2+2.0*alfa(ln)*alfa(ln)
        cc5=beta(ln)*beta(ln)
        cc6=ak2(ln,2)/s
        cc7=2e0*s2-ak2(ln,2)
cvd$  novector
         DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
c >>> Normal moment tensor components
c >>> m=1
         do k=1,3
          m=1
          pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 
     &                  tmom(k)*.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
          pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + 
     &                  tmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
c >>> m=4
          if (k.lt.3) then
           rr1=1.0-cc(k)
           if (k.eq.1) rr1=-rr1
           m=4
           pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 
     &                  tmom(k)*0.25*s2*alfinv(ln)*rr1*cc1
           pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - 
     &                  tmom(k)*0.25*is*s*rr1*cc2      
           pot(iwav3,ircv,m)=pot(iwav3,ircv,m) - 
     &                  tmom(k)*0.25*(s2-cc5)*betinv(ln)*rr1*cc2
          end if
c >>> m=5
         end do
c >>> moment components M12=M21 (strike slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
c >>> m=4
c >>> m=5
         m=5
         pot(iwav1,ircv,m)=pot(iwav1,ircv,m)  
     &                      - m12*s2*alfinv(ln)*cc1
         pot(iwav2,ircv,m)=pot(iwav2,ircv,m)  
     &                      - is*m12*s*cc2      
         pot(iwav3,ircv,m)=pot(iwav3,ircv,m)  
     &                      - m12*ak2(ln,2)*betinv(ln)*cc2
c >>> moment components M13=M31 (strike slip, t=0)
c >>> m=1
c >>> m=2
         m=2
         pot(iwav1,ircv,m)=pot(iwav1,ircv,m)  
     &                      + is*m13*2.0*s*cc1
         pot(iwav2,ircv,m)=pot(iwav2,ircv,m)  
     &                      + m13*cc7*betinv(ln)*cc2      
         pot(iwav3,ircv,m)=pot(iwav3,ircv,m)  
     &                      + is*m13*cc6*cc2
c >>> m=3
c >>> m=4
c >>> m=5
c >>> moment components M23=M32 (dip slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
        m=3
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m)  
     &                      + is*m23*2.0*s*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m)  
     &                      + m23*cc7*betinv(ln)*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m)  
     &                      + is*m23*cc6*cc2
c >>> m=4
c >>> m=5
 40      CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END


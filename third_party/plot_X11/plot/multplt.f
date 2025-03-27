      SUBROUTINE MULTPLT(MARK,RDSYMB,AX,AY,KMIN,
     & NPTS,HGTC,ICURV,YMEAN)
      parameter (nbuf=1000)
      real xbuf(nbuf),ybuf(nbuf)

      INTEGER RDSYMB,jcoltab(7)
      CHARACTER*2 BDEV, FORMT
      CHARACTER*3 SYMB(20), PLOP(21), DEV, ADD, URC, ULC, NWR
      CHARACTER*11 FACTORTMP
      REAL AX(1), AY(1)

      include 'default.f'

      COMMON /SETPRM/ PLOP, ADD, BDEV, FACTORTMP,
     &  SYMB, DEV, FORMT, URC, ULC, NWR
      COMMON /XAX/ X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     &  X1PL,XDUM4,IXDUM1,XDUM5,XDUM6,XDIV,XVAL(100),NXVAL
      COMMON /YAX/ Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     &  Y1PL,YDUM4,IYDUM1,YDUM5,YDUM6,YDIV,YVAL(100),NYVAL,
     &  YBOX
      COMMON /XYLAST/ XLAST,YLAST,XLN,YLN,ISGN

      COMMON /SETARG/ TRACD, TRCMAX
      data jcoltab /6,2,7,1,3,5,4/
      IF(RDSYMB .EQ. 1)   THEN      
        CALL MARKER(ABS(MARK))
        CALL HEIGHT(HGTC)
        CALL SCLPIC(1.0)
        CALL CURVE(AX(KMIN),AY(KMIN),NPTS,SIGN(1,MARK))

      ELSE IF(SYMB(ICURV) .EQ. 'CNT') THEN
        IF(ISYMB .GT. 0)   CALL MARKER(MARK)
c        CALL CURVE(AX(KMIN),AY(KMIN),NPTS,IMARK)
       n=npts
       IF (.NOT.FILL) THEN 
        do 949 ispt=1,n,nbuf
 949       calL CURVE(AX(ispt),AY(ispt),min(nbuf+1,N-ispt+1),IMARK)
       ELSE
C    
C SHADED WIGGLES
C
        ampmax=tracd*0.5
        K=0
        KS=1
        SG0=SIGN(1.0,YUP-YDOWN)
        if (ay(1)-ymean.lt.-1e-3*trcmax) then
          sg1=-sg0
        else if (ay(1)-ymean.gt.1e-3*trcmax) then
          sg1=sg0
        else
          sg1=0
        end if
        AMPLMAX=0.0       
        ampwmax=0e0
 501    K=K+1
        if (ay(k)-ymean.lt.-1e-3*trcmax) then
          sg2=-sg0
        else if (ay(k)-ymean.gt.1e-3*trcmax) then
          sg2=sg0
        else
          sg2=0
        end if
        AMPLMAX=MAX(AMPLMAX,ABS(Ay(K)-YMEAN))
        IF (SG2.NE.SG1.OR.K.EQ.N) GO TO 502
        GO TO 501
 502    DO 503 I=KS,K-1,NBUF-2
         NN=MIN(NBUF-2,K-I)
         CALL VMOV(AX(I),1,XBUF(2),1,NN)
         CALL VMOV(Ay(I),1,YBUF(2),1,NN)
         IF (((SG1.Gt.0.AND.IPOS.GT.0).OR.
     &        (SG1.LT.0.AND.INEG.GT.0)).and.
     &        (amplmax.gt.abs(ydown-yup)*0.003).and.
     &        (nn.gt.2)) THEN
c >>> shaded wiggle
          IF (I.EQ.KS.AND.I.NE.1) THEN
           A0=Ay(I)-YMEAN
           A1=Ay(I-1)-YMEAN
           if (a0.ne.a1) then
            XBUF(1)=(A0*AX(I-1)-A1*AX(I))/(A0-A1)
           else
            xbuf(1)=ax(i)
           end if
          ELSE 
           XBUF(1)=AX(I)
          END IF
          YBUF(1)=YMEAN
          IF (NN.LT.NBUF-2.AND.K.NE.N) THEN
           I1=I+NN-1
           I2=I+NN
           A1=Ay(I1)-YMEAN
           A2=Ay(I2)-YMEAN
           if (a1.ne.a2) then
            XBUF(NN+2)=(A2*AX(I1)-A1*AX(I2))/(A2-A1)
           else
            xbuf(nn+2)=ax(i2)
           end if
          ELSE
           XBUF(NN+2)=AX(I+NN-1)
          END IF
          YBUF(NN+2)=YMEAN
C        DO 504 J=1,NN+2
C          XBUF(J)=(XBUF(J)-XLEFT)*FACX+XOR
C          YBUF(J)=(YBUF(J)-YDOWN)*FACY+YOR
C 504     CONTINUE
          if (ampmax.gt.0e0) then
           LCOL=IFIX(1.0+7*(AMPMAX-AMPLMAX*SG1)/(2*AMPMAX))
          else
           lcol=1
          end if
          LCOL=MIN(LCOL,7)
          LCOL=MAX(LCOL,1)
          IG=jcoltab(LCOL)
          IF (ICOLF.GT.0) CALL SETCOL(IG)
c          write(6,*) 'POLYGO: nn=',nn
          CALL POLYGO(XBUF,YBUF,NN+2)
          if (icolf.gt.0) then
           IF (I.NE.1) THEN
             XBUF(1)=AX(I-1)
             YBUF(1)=Ay(I-1)
           ELSE
             XBUF(1)=AX(I)
             YBUF(1)=Ay(I)
           END IF
           XBUF(NN+2)=AX(I+NN)
           YBUF(NN+2)=Ay(I+NN)
           CALL SETCOL(1)
           CALL CURVE(XBUF,YBUF,NN+2,0)
          end if
         ELSE
          IF (I.NE.1) THEN
            XBUF(1)=AX(I-1)
            YBUF(1)=Ay(I-1)
          ELSE
            XBUF(1)=AX(I)
            YBUF(1)=Ay(I)
          END IF
          XBUF(NN+2)=AX(I+NN)
          YBUF(NN+2)=Ay(I+NN)
C         DO 505 J=1,NN+2
C          XBUF(J)=(XBUF(J)-XLEFT)*FACX+XOR
C          YBUF(J)=(YBUF(J)-YDOWN)*FACY+YOR
C 505     CONTINUE
          CALL SETCOL(1)
          CALL CURVE(XBUF,YBUF,NN+2,0)
         END IF
 503    CONTINUE
        KS=K
        SG1=SG2
        AMPLMAX=0.0
        IF (K.LT.N) GO TO 501
       END IF

      ELSE IF(SYMB(ICURV) .EQ. 'DSH') THEN
        CALL DASH
        CALL CURVE(AX(KMIN),AY(KMIN),NPTS,0)
        CALL RESET('DASH')

      ELSE IF(SYMB(ICURV) .EQ. 'DOT') THEN
        CALL DOT
        CALL CURVE(AX(KMIN),AY(KMIN),NPTS,0)
        CALL RESET('DOT')

      ELSE IF(SYMB(ICURV) .EQ. 'CDS') THEN
        CALL CHNDSH
        CALL CURVE(AX(KMIN),AY(KMIN),NPTS,0)
        CALL RESET('CHNDSH')

      ELSE IF(SYMB(ICURV) .EQ. 'CDO') THEN
        CALL CHNDOT
        CALL CURVE(AX(KMIN),AY(KMIN),NPTS,0)
        CALL RESET('CHNDOT')
      END IF

      RETURN
      END          

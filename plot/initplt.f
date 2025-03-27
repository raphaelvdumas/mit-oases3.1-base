      SUBROUTINE INITPLT(XLEN,YLEN,HTXT,OPTS,LAB,NLAB,
     & TIT,TST,HGTREF,VXSTA,VYSTA,YSTA,XSTA,OPENED,NT)

      INTEGER OPENED
      REAL XF(3), YF(3)
      CHARACTER*2 BDEV, FORMT
      CHARACTER*3 PLOP(21), ADD, XBTYPE, YBTYPE, DEV,
     &             SYMB(20), URC, ULC, NWR
      CHARACTER*7 SDD
      CHARACTER*9 SC
      CHARACTER*11 FACTORTMP
      CHARACTER*12 OPTS
      CHARACTER*20 LAB(20)
      CHARACTER*80 TIT, TITX, TITY, HTXT


      COMMON /XAX/ X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     & X1PL,XDUM4,IXDUM1,XDUM5,XDUM6,XDIV,XVAL(100),NXVAL

      COMMON /XAXC/ TITX, XBTYPE

      COMMON /YAX/ Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     & Y1PL,YDUM4,IYDUM1,YDUM5,YDUM6,YDIV,YVAL(100),NYVAL,
     & YBOX

      COMMON /YAXC/ TITY, YBTYPE

      COMMON /XYLAST/ XLAST, YLAST, XLN, YLN, ISGN

      include 'default.f'

      COMMON /SCALES/ YSCALETMP, DRRATIO, YOLDLEN,
     &  SCFAC, HSCFAC, IHSC, FACTOR, FACT, FACSCALE, SCALEFACT

      COMMON /SCISS/ SCXL, SCXR, SCYUP, SCYDOWN

      COMMON /SETPRM/ PLOP, ADD, BDEV, FACTORTMP,
     &  SYMB, DEV, FORMT, URC, ULC, NWR

      DATA SDD/'SD (m)$'/

  200 FORMAT(F7.3)

      IF( (OPENED .EQ. 1) .OR. (NOP .EQ. 1)) RETURN

      DO 1000 I=80,1,-1
      NT=I
      IF (TIT(I:I) .NE. ' ') GO TO 1200
      IF (I .EQ. 1) NT=0
 1000 CONTINUE
 1200 CONTINUE

      DO 1400 I=80,1,-1
      NH=I
      IF (HTXT(I:I) .NE. ' ') GO TO 1600
      IF (I .EQ. 1) NH=0
 1400 CONTINUE
 1600 CONTINUE


C   ******* OPENING OF MINDIS PLOT PACKAGE *******


      IF (IUNI.NE.1) THEN
        CALL BGNPL(iplhs)
        iplhs=iplhs+1
        OPENED=1

        SCXL= XLEFT
        SCXR= XRIGHT
        SCYUP= YUP
        SCYDOWN= YDOWN

        CALL NOBRDR        
        CALL NOCHEK
        GOTO (2300,2400,2500,2600),ICHTYP
 2300   CALL SIMPLX
        GOTO 2700
 2400   CALL DUPLX
        GO TO 2700
 2500   CALL COMPLX
        GOTO 2700
 2600   CALL COMPLX
        CALL MX1ALF('ITALIC','}')
        GOTO 2800
 2700   CALL MX1ALF('STANDA','}')
 2800   CALL MX2ALF('SUBSCR','!')
        CALL MX3ALF('SUPERS','#')
        CALL MX4ALF('GREEK', '%')
        CALL MX5ALF('ENDSCR','"')
        CALL MX6ALF('MATHEM','^')
        CALL MX7ALF('SPECIA','&')
        CALL MX8ALF('BACKSP','@')
        XLN= XLEN / 2.54
        YLN= YLEN / 2.54
        YSTEP= (YDOWN - YUP) / 11.
        XPOS= XLN + ( 3.5 * HGTREF )
        YPOS= YLN - HGTREF
        YSTA= 0.93 * YLN + 0.03
        XSTA= XLN - 0.01
        VXSTA= (XRIGHT - XLEFT) / XLN
        VYSTA= (YUP - YDOWN) / YLN
        GMEAN= SQRT(XLEN * YLEN)
        CALL SETCOL( ICOL )

        HGTC= HGTREF*HSCFAC
        CALL HEIGHT( 1.5 * HGTC )

        if (nh.gt.0) then
         CALL PAGE(YOR+YLN+1.8,XOR+XLN+0.8)
        else
         CALL PAGE(YOR+YLN+0.4,XOR+XLN+0.8)
        end if
c        IF(XLEN.LT.YLEN) THEN
c          CALL PAGE( 10.0 , 7.0 )
c        ELSE
c          CALL PAGE( 7.0 , 10.0 )
c        END IF

        CALL PHYSOR(XOR,YOR)

        IF(XBTYPE .EQ. 'LIN' .AND. YBTYPE .EQ. 'LIN')   THEN
          CALL TITLE('    ',-1,TITX,100,TITY,100,XLN,YLN)
        ELSE  
          CALL TITLE(' <   ',-1,TITX,0,TITY,100,XLN,YLN)
        END IF

C   N.B.: CASE WITH YBTYPE =LOG TO BE INCLUDED YET
        CALL YAXANG(0.0)
        IF (IXAX.EQ.1.AND.IYAX.EQ.1) THEN
          CALL INTAXS
        ELSE IF (IXAX.EQ.1) THEN
          CALL XINTAX
        ELSE IF (IYAX.EQ.1) THEN
          CALL YINTAX
        ELSE
        END IF
        CALL XTICKS( 2 )
        CALL YTICKS( 2 )
        IF(XBTYPE .EQ. 'LIN')   THEN
          IF (KTYP.EQ.1) THEN
            CALL GRAF(XLEFT,XINC,XRIGHT,YDOWN,YINC,YUP)
          ELSE
            IF (KTYP.EQ.2) THEN
              XCYCLE=XLN/ALOG10(XRIGHT-XLEFT)
              YSTEP=(YUP-YDOWN)/YLN
C             CALL XLOG(XLEFT,XCYCLE,YDOWN,YSTEP)
              CALL GRAF(XLEFT,XINC,XRIGHT,YDOWN,YINC,YUP)
              CALL DOT
              CALL GRID(1,1)
              CALL RESET('DOT')
            END IF
          END IF
        ELSE
          ISKIP=1
          CALL GRAF(XLEFT,XRIGHT-XLEFT,XRIGHT,YDOWN,YINC,YUP)
          CALL XAXLOG(XLN,ISKIP,HGTC)
        END IF
 
            CALL MARKER(1)
            CALL SCLPIC(1.5)
        IF (ITFRAM.EQ.1) THEN
           CALL VECTOR(0.,YLN,XLN,YLN,0)
           CALL VECTOR(XLN,YLN,XLN,0.,0)
        ELSE
          XF(1)=XLEFT
          XF(2)=XRIGHT
          XF(3)=XRIGHT
          YF(1)=YUP
          YF(2)=YUP
          YF(3)=YDOWN
          CALL CURVE(XF,YF,3,0)
        END IF
        IHGT=NINT(HGTC/0.07)+1
        HTXTL=(NH*HGTC)
        IG=0

        DO 2850 J=NH,1,-1
        IG=IG+1
        CHARG=IG*HGTC
        IF((HTXTL-CHARG).LE.XLN) GOTO 2860
 2850   CONTINUE
 2860   CONTINUE
        NH=J   

        IF (NH.GT.0)   THEN
          IF ( ULC .NE. 'ULC' .and.  NWR.NE.'NWR' )   THEN
             CALL HEADIN(HTXT,NH,IHGT,1)
          ELSE if (NWR.NE.'NWR') Then
            CALL HEIGHT( 1.7*HGTC )
            CALL DUPLX
            CALL MESSAG( HTXT, NH, - 6.6 * HGTC, YLN + 1.5*HGTC )
            GOTO (4300,4400,4500,4600),ICHTYP
 4300       CALL SIMPLX
            GOTO 4700
 4400       CALL DUPLX
            GO TO 4700
 4500       CALL COMPLX
            GOTO 4700
 4600       CALL COMPLX
            CALL MX1ALF('ITALIC','}')
            GOTO 4800
 4700       CALL MX1ALF('STANDA','}')
 4800       CALL MX2ALF('SUBSCR','!')
            CALL HEIGHT( HGTC )
          END IF
        END IF


        HGTC= HGTREF * HSCFAC * 1.1
        CALL HEIGHT(HGTC)
        TSX=XLN-14*HGTC
        TSY=YLN-1.5*HGTC
        TST=1.5*HGTC

        IF(OPTS(8:12) .EQ. 'TLAVR' .OR. ADD .EQ. 'ADD')   THEN
          CALL MESSAG(LAB(1)//'$',100,TSX,TSY)
          IF(OPTS(8:12) .EQ. 'TLAVR') 
     &    CALL MESSAG(SDD,7,XLN+1.5*HGTC,TSY)
        ELSE IF(OPTS(1:6) .EQ. 'MOCASS' .AND. 
     %         (PLOP(1)(1:3) .EQ. ' P ')) THEN
          DO 2899 K =1,10
          CALL MESSAG(LAB(K)//'$',100,TSX,TSY)
          TSY=TSY-TST
 2899     CONTINUE
        ELSE IF(OPTS(1:6) .EQ. 'MOCASS' .AND.
     %    PLOP(1) .EQ. 'BP ' .OR. 
     %    PLOP(1) .EQ. 'RP ' .OR.
     %    PLOP(1) .EQ. 'TP ' ) THEN
          DO 2898 K=1,2
          CALL MESSAG(LAB(K)//'$',100,TSX,TSY)
          TSY=TSY-TST
 2898     CONTINUE
        ELSE IF(PLOP(1) .NE. 'ZP') THEN
          DO 2900 K=1,NLAB
          CALL MESSAG(LAB(K)//'$',100,TSX,TSY)
          TSY=TSY-TST
 2900     CONTINUE
        END IF

        IF( NWR .NE. 'NWR')   THEN
          IF(URC .NE. 'URC')   THEN

            IF(OPTS(1:4) .EQ. 'SNAP' .OR. OPTS(1:6) .EQ. 'COUPLE') THEN
              CALL MESSAG(OPTS//' '//PLOP(1),16,TSX-HGTC,YLN+.5*HGTC)
            ELSE IF(((OPTS( 1: 5).EQ.'PAREQ').AND.
     &             (OPTS(11:12).EQ.'TL')  ).OR. 
     &            ((OPTS( 3: 5).EQ.'IFD'  ).AND.
     &             (OPTS(11:12).EQ.'TL'   )))    THEN
              CALL MESSAG(OPTS//PLOP(1),15,TSX,YLN+.5*HGTC)
            ELSE IF((OPTS( 1: 6).EQ.'MOCASS').AND.
     &              (PLOP( 1)   .EQ.'TP ' ))      THEN
              CALL MESSAG(OPTS(1:6)//','//PLOP(1),10,TSX,YLN+.5*HGTC)
            ELSE IF((OPTS( 1: 6).EQ.'MOCASS').AND.
     &              (PLOP( 1)   .EQ.'ZP ' ))      THEN
              CALL MESSAG(OPTS(1:7)//PLOP(1),10,TSX,YLN+.5*HGTC)
            ELSE IF((OPTS( 1: 6).EQ.'MOCASS').AND.
     &              (PLOP( 1)   .EQ.'RP ' ))      THEN
              CALL MESSAG(OPTS(1:7)//PLOP(1),10,TSX,YLN+.5*HGTC)
            ELSE IF((OPTS( 1: 6).EQ.'MOCASS').AND.
     &              (PLOP( 1)   .EQ.'BP ' ))      THEN
              CALL MESSAG(OPTS(1:7)//PLOP(1),10,TSX,YLN+.5*HGTC)
            ELSE IF((OPTS( 1: 6).EQ.'MOCASS').AND.
     &              (PLOP( 1)   .EQ.' P ' ))      THEN
              CALL MESSAG(OPTS(1:6)//PLOP(1),10,TSX,YLN+.5*HGTC)
            ELSE IF (OPTS.NE.' ') THEN
              IF(SCALEFACT.EQ.1.0.AND.FACTOR.EQ.1.0) THEN
                CALL MESSAG(OPTS,12,TSX,YLN+.5*HGTC)
              ELSE
                SC(1:1)='('
                WRITE(SC(2:8),200) SCALEFACT
                SC(9:9)=')'
                CALL MESSAG(OPTS(1:6)//SC,15,TSX,YLN+.5*HGTC)
              END IF
            END IF

          ELSE

            DO 3000   I=19,1,-1
            J=I
            IF(LAB(NLAB+1)(I:I) .NE. ' ')   GO TO 3200
 3000       CONTINUE
            J=0
            GO TO 3400
 3200       CONTINUE
            IF(LAB(NLAB+1)(J:J) .EQ. '$')   THEN
              J=J-1
            ELSE
              LAB(NLAB+1)(J+1:J+1)='$'
            END IF
 3400       CONTINUE
            TSX=XLN - (J-1)*HGTC*0.9453
            CALL MESSAG(LAB(NLAB+1),J,TSX,YLN + .5*HGTC)
          END IF
        END IF

        IF (NT.GT.0.AND.PLOP(1).NE.'RAN')   
     &     CALL MESSAG(TIT,NT,TST/2.,YLN+.5*HGTC)


      END IF   
   
      RETURN
      END

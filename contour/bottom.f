      SUBROUTINE BOTTOM(*)
      parameter (nbot_max = 5000, nbm= nbot_max+20)
      CHARACTER*40 WORD
      CHARACTER*6 BOTTUM,BOT
      COMMON /BOTT/ XF(nbm),YF(nbm),NPBOTT, ISHADE, NPSH
      COMMON /MULBOTT/  LINES,numbot 

      COMMON /XAX/X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     &            X1PL,XLPL,NX,X1GRID,XLGRID,DIVX,XVAL(100),NXVAL

  100 FORMAT(1X,/,' *** WARNING:  BOTTOM POLYGON IS BEING READ **** ',/)
  200 FORMAT(A6,1X,I10)
  300 FORMAT(A40)

C
      NPBOTT=0
      READ(55,'(a6)',END=1200,ERR=1000) BOTTUM
      backspace(55)
      IF(BOTTUM(1:4) .EQ.'@EOF')  THEN
        LINES=LINES+1
        RETURN 1
      ELSE  IF( BOTTUM .EQ. 'BOTTOM')   THEN
        READ(55,200,END=1200,ERR=1000) BOTTUM,ISHADE
        write(6,*) '>>> Reading polygon, shading=',ishade
        numbot=numbot+1
        GO TO 1400
      ELSE
c        BACKSPACE 55
        RETURN 1
      END IF
  
 1000 CONTINUE
      STOP ' ERROR IN READING BOTTOM ' 

 1200 CONTINUE
      RETURN 1

 1400 CONTINUE
      LINES=LINES+1

      WRITE(6,100)

      DO 1600   I= 1, nbot_max
      read(55,'(a6)',end=2000) bot
      if (bot.eq.'BOTTOM') go to 1800
      if (bot(1:5).eq.'CONDR') then
       go to 1800
      end if
      backspace(55)
       READ(55,*,ERR=1800,END=2000) D1, D2
C      XF(I)=D1*1.0E3
c       XF(I)=D1/DIVX
       XF(I)=D1
       YF(I)=D2
       NPBOTT=I
 1600 CONTINUE
      STOP  ' TOO MANY BOTTOM POINTS'

 1800 CONTINUE
      BACKSPACE 55
      GO TO 3000

 2000 CONTINUE
      BACKSPACE 55
      READ(55,300) WORD
      IF(WORD(1:4) .EQ.'@EOF')  LINES=LINES+1

 3000 CONTINUE
      LINES=LINES+NPBOTT
C ADD CLOSING POINT
      IF(XF(1) .NE. XF(NPBOTT))   THEN
       NPBOTT=NPBOTT+1
       XF(NPBOTT)=XF(1)
       YF(NPBOTT)=YF(1)
      END IF
       
      RETURN
      END

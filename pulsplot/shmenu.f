      SUBROUTINE SHMENU(IRET)
      INCLUDE 'menucom.f'
      character*72 SCREEN(menu_max+4),CHRLIN

c *** MAKE FRAME

      DO 10 I=1,12
      I1=(I-1)*6+1
      I2=I1+5
      CHRLIN(I1:I2)='******'
 10   CONTINUE
      SCREEN(1)=CHRLIN
      SCREEN(3)=CHRLIN
      SCREEN(4+NMLINE)=CHRLIN

C *** CENTER MENU TITLE

      WRITE(CHRLIN,'(A,68X,A)') '* ',' *'
      LL=LENSTR(MENTIT)
      L1=36-LL/2
      CHRLIN(L1:L1+LL-1)=MENTIT
      SCREEN(2)=CHRLIN

C *** UPDATE MENU

 1    DO 20 I=1,NMLINE
      GOTO (11,12,13,14,15),FLDTYP(I)+1
 11    FIELD(I) = ' '
       WRITE(SCREEN(I+3),101) I,ITEM(I),FIELD(I),UNIT(I)
       GO TO 20
 12    CHRLIN=FLDTEX(I)
       FIELD(I) = CHRLIN(1:12)
       UNIT(I) = CHRLIN(13:48)
       WRITE(SCREEN(I+3),100) I,ITEM(I),FIELD(I),UNIT(I)
       GO TO 20
 13    WRITE(FIELD(I),'(F12.4)') FLTNUM(I)
       WRITE(SCREEN(I+3),101) I,ITEM(I),FIELD(I),UNIT(I)
       GO TO 20
 14    WRITE(FIELD(I),'(I8,4X)') INTNUM(I)
       WRITE(SCREEN(I+3),101) I,ITEM(I),FIELD(I),UNIT(I)
       GO TO 20
 15    WRITE(FIELD(I),'(7X,A1,4X)') FLDCHA(I)
       WRITE(SCREEN(I+3),101) I,ITEM(I),FIELD(I),UNIT(I)
 20   CONTINUE
 100  FORMAT('* ',I2,'. ',A16,A12,A36,' *')
 101  FORMAT('* ',I2,'. ',A16,A12,1X,A36,'*')

C *** SHOW MENU

      DO 25 I=NMLINE+6,menu_max
 25     WRITE(6,*)      
      WRITE(6,'(1H ,A72)') (SCREEN(I),I=1,NMLINE+4)

C *** OPTION SELECTION

 30   WRITE(6,'(1H ,A)') '     SELECT OPTION: '
      READ(5,*,ERR=30) IRET
      IF (IRET.LT.1.OR.IRET.GT.NMLINE) GO TO 30

C *** READ PARAMETERS

      GOTO (999,31,32,33,34),FLDTYP(IRET)+1
 31   WRITE(6,'(1H ,A16,A4)') ITEM(IRET),'?   '
      READ(5,'(A)') FLDTEX(IRET)
      GO TO 999
 32   WRITE(6,'(1H ,A16,A4)') ITEM(IRET),'?   '
      READ(5,*,ERR=32) FLTNUM(IRET)
      GO TO 999
 33   LL=LENSTR(UNIT(IRET))
      CHRLIN=UNIT(IRET)
      WRITE(6,'(1H ,A16,1X,2A)') ITEM(IRET),CHRLIN(1:LL),' ?'
      READ(5,*,ERR=33) INTNUM(IRET)
      IF (INTNUM(IRET).LT.INTMIN(IRET)
     &    .OR.INTNUM(IRET).GT.INTMAX(IRET)) THEN
       WRITE(6,'(1H ,A,1X)') 'NUMBER OUT OF RANGE >>'
       GO TO 33
      END IF
      GO TO 999
 34   LL=LENSTR(UNIT(IRET))
      CHRLIN=UNIT(IRET)
      WRITE(6,'(1H ,A16,1X,2A)') ITEM(IRET),CHRLIN(1:LL),' ?'
      READ(5,'(A1)') FLDCHA(IRET)
       ichr=ichar(fldcha(iret))
      if (ichr.gt.96.and.ichr.lt.123) then
       fldcha(iret)=char(ichr-32)
      end if
      DO 341 II=LL-2,3,-1
       IF (FLDCHA(IRET).NE.' '.AND.FLDCHA(IRET).EQ.CHRLIN(II:II)) THEN
        GO TO 999
       END IF
 341  CONTINUE
      WRITE(6,'(1H ,A,1X)') 'ILLEGAL CHARACTER >>'
      GO TO 34
 999  RETURN
      END
      INTEGER FUNCTION LENSTR(STR)
      CHARACTER*(*) STR
      DO 10 I=LEN(STR),1,-1
       IF (STR(I:I).NE.' ') THEN
        LENSTR=I
        RETURN
       END IF
 10   CONTINUE
      LENSTR=0
      RETURN
      END
      SUBROUTINE MENCRT(TIT,NLIN)
      INCLUDE 'menucom.f'
      CHARACTER*(*) TIT
      NMLINE=NLIN
      if (nmline.gt.menu_max) then
       stop '>>> MENCRT: Too many menu lines <<<'
      end if

      MENTIT=TIT
      DO 10 I=1,NMLINE
       ITEM(I)=' '
       FIELD(I)=' '
       UNIT(I)=' '
       FLDTYP(I)=0
       FLTNUM(I)=0
       INTNUM(I)=0
       FLDTEX(I)=' '
       FLDCHA(I)=' '
 10   CONTINUE
      RETURN
      END
      SUBROUTINE FLTFLD(LNUM,ITXT,FNUM,IUTXT)
      INCLUDE 'menucom.f'
C *** CREATES FLOATING POINT FIELD
      CHARACTER*(*) ITXT,IUTXT
      IF (LNUM.LT.1.OR.LNUM.GT.NMLINE) THEN
        STOP '>>> FLTFLD: MENU LINE OUT OF RANGE <<<'
      END IF
      FLDTYP(LNUM)=2
      ITEM(LNUM)=ITXT
      FLTNUM(LNUM)=FNUM
      UNIT(LNUM)=IUTXT
      RETURN
      END
      SUBROUTINE INTFLD(LNUM,ITXT,INUM,IMIN,IMAX)
      INCLUDE 'menucom.f'
C *** CREATES INTEGER FIELD
      CHARACTER*(*) ITXT
      IF (LNUM.LT.1.OR.LNUM.GT.NMLINE) THEN
        STOP '>>> INTFLD: MENU LINE OUT OF RANGE <<<'
      END IF
      FLDTYP(LNUM)=3
      ITEM(LNUM)=ITXT
      INTNUM(LNUM)=INUM
      INTMIN(LNUM)=IMIN
      INTMAX(LNUM)=IMAX
      WRITE(UNIT(LNUM),'(A,I5,A,I5,A)') '( ',INTMIN(LNUM),' -',
     &                                       INTMAX(LNUM),' )'
      RETURN
      END
      SUBROUTINE TXTFLD(LNUM,ITXT,IUTXT)
      INCLUDE 'menucom.f'
C *** CREATES TEXT FIELD
      CHARACTER*(*) ITXT,IUTXT
      IF (LNUM.LT.1.OR.LNUM.GT.NMLINE) THEN
        STOP '>>> TXTFLD: MENU LINE OUT OF RANGE <<<'
      END IF
      FLDTYP(LNUM)=1
      ITEM(LNUM)=ITXT
      FLDTEX(LNUM)=IUTXT
      RETURN
      END
      SUBROUTINE CHAFLD(LNUM,ITXT,ICHA,IUTXT)
      INCLUDE 'menucom.f'
C *** CREATES CHARACTER FIELD
      CHARACTER*(*) ITXT,IUTXT
      CHARACTER ICHA
      IF (LNUM.LT.1.OR.LNUM.GT.NMLINE) THEN
        STOP '>>> CHAFLD: MENU LINE OUT OF RANGE <<<'
      END IF
      FLDTYP(LNUM)=4
      ITEM(LNUM)=ITXT
      FLDCHA(LNUM)=ICHA
      MIND=MIN(LEN(IUTXT),32)
      DO 10 I=MIND,1,-1
       IF (IUTXT(I:I).NE.' ') THEN
        UNIT(LNUM)='( '//IUTXT(1:I)//' )'
        GO TO 11
       END IF
 10   CONTINUE
 11   RETURN
      END
      SUBROUTINE ACTFLD(LNUM,ITXT,IUTXT)
      INCLUDE 'menucom.f'
C *** CREATES ACTION FIELD
      CHARACTER*(*) ITXT,IUTXT
      IF (LNUM.LT.1.OR.LNUM.GT.NMLINE) THEN
        STOP '>>> ACTFLD: MENU LINE OUT OF RANGE <<<'
      END IF
      FLDTYP(LNUM)=0
      ITEM(LNUM)=ITXT
      MIND=MIN(LEN(IUTXT),32)
      DO 10 I=MIND,1,-1
       IF (IUTXT(I:I).NE.' ') THEN
        UNIT(LNUM)='( '//IUTXT(1:I)//' )'
        GO TO 11
       END IF
 10   CONTINUE
 11   RETURN
      RETURN
      END
      SUBROUTINE GETFLT(LNUM,FNUM)
      INCLUDE 'menucom.f'
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.2) THEN
        FNUM=FLTNUM(LNUM)            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> GETFLT: FIELD ',LNUM,
     &                          ' NOT FLOATING POINT <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> GETFLT: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE GETINT(LNUM,INUM)
      INCLUDE 'menucom.f'
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.3) THEN
        INUM=INTNUM(LNUM)            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> GETINT: FIELD ',LNUM,
     &                          ' NOT INTEGER <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> GETINT: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE GETTXT(LNUM,OTXT)
      INCLUDE 'menucom.f'
      CHARACTER*(*) OTXT
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.1) THEN
        OTXT=FLDTEX(LNUM)            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> GETTXT: FIELD ',LNUM,
     &                          ' NOT ALPHANUMERIC <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> GETTXT: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE GETCHA(LNUM,OTXT)
      INCLUDE 'menucom.f'
      CHARACTER OTXT
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.4) THEN
        OTXT=FLDCHA(LNUM)            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> GETCHA: FIELD ',LNUM,
     &                          ' NOT SINGLE CHARACTER <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> GETCHA: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE PUTFLT(LNUM,FNUM)
      INCLUDE 'menucom.f'
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.2) THEN
        FLTNUM(LNUM)=FNUM            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> PUTFLT: FIELD ',LNUM,
     &                          ' NOT FLOATING POINT <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> PUTFLT: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE PUTINT(LNUM,INUM)
      INCLUDE 'menucom.f'
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.3) THEN
        INTNUM(LNUM)=INUM            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> PUTINT: FIELD ',LNUM,
     &                          ' NOT INTEGER <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> PUTINT: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE PUTTXT(LNUM,OTXT)
      INCLUDE 'menucom.f'
      CHARACTER*(*) OTXT
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.1) THEN
        LL=MIN(LENSTR(OTXT),48)
        FLDTEX(LNUM)=OTXT(1:LL)            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> PUTTXT: FIELD ',LNUM,
     &                          ' NOT ALPHANUMERIC <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> PUTTXT: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END
      SUBROUTINE PUTCHA(LNUM,OTXT)
      INCLUDE 'menucom.f'
      CHARACTER OTXT
      IF (LNUM.GT.0.AND.LNUM.LE.NMLINE) THEN
       IF (FLDTYP(LNUM).EQ.4) THEN
        FLDCHA(LNUM)=OTXT            
       ELSE
        WRITE(6,'(1H ,A,I2,A)') '>>> PUTCHA: FIELD ',LNUM,
     &                          ' NOT SINGLE CHARACTER <<<'
        STOP
       END IF
      ELSE
        WRITE(6,'(1H ,A)') '>>> PUTCHA: FIELD OUT OF RANGE <<<'
      END IF
      RETURN
      END


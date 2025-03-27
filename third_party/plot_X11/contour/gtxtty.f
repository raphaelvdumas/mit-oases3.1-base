      INTEGER FUNCTION GTXTTY()
C     ***Routine to return terminal type
C
      IMPLICIT INTEGER*2(W), INTEGER*4(L)
      CHARACTER*63 NAMDES
      INTEGER*2 NAMLEN
      INTEGER*4 SYSXTRNLOG
      logical frstgt
      common /frstgt/ frstgt
      LOGICAL LNAMOK
      CHARACTER*8 TNAM1, TTYP1
      CHARACTER*8 ATERM, TTYPE
      CHARACTER*8 LCLVALUES
c      INCLUDE '(XJPIDEF)'
c      COMMON /DTXTID/ WLEN1, WCODE1, LADDR1, LLENADDR1
c      DATA WLEN1 /8/
c      DATA WCODE1 /JPIXTERMINAL/
C
c      ICODE=SYSXTRNLOG('USRXTERMTYPE',NAMLEN,NAMDES,,,%VAL(3))
c      LNAMOK=(ICODE.AND.(NAMDES(1:NAMLEN).NE.'USRXTERMTYPE'))
c
c *** UNIX. HS 4-Feb-89
       if (.not.frstgt) then
         call getenv('USRTERMTYPE',namdes)
         if (NAMDES.eq.' ') then
           LNAMOK=.FALSE.
         ELSE
           DO 101 JJ=63,1,-1
           IF (NAMDES(JJ:JJ).NE.' ') THEN
             NAMLEN=JJ
             GO TO 102
           END IF
 101       CONTINUE
 102       CONTINUE
           LNAMOK=.TRUE.
         END IF
        FRSTGT=.TRUE.
       END IF

       IF (LNAMOK) THEN
           TTYPE=NAMDES(1:NAMLEN)
       ELSE
           WRITE(6,*) 'Env. variable USRTERMTYPE not set'
           WRITE(6,*) 'Specify terminal type [X,tek4105',
     &                '(-4112 -4010 -4014)', 
     &                ',vt240]'
           read(5,'(A)') NAMDES           
         if (NAMDES.ne.' ') then
           DO 201 JJ=63,1,-1
           IF (NAMDES(JJ:JJ).NE.' ') THEN
             NAMLEN=JJ
             GO TO 202
           END IF
 201       CONTINUE
 202      CONTINUE
          TTYPE=NAMDES(1:NAMLEN)
         else
          TTYPE='EOF RECD'
         END IF
       end if
       LNAMOK=.TRUE.
C
      WRITE(6,*) ' TERM = ',TTYPE
      GTXTTY=0
      IF (TTYPE.EQ.'TEK4010'.OR.TTYPE.EQ.'tek4010') GTXTTY=4010
      IF (TTYPE.EQ.'TEK4014'.OR.TTYPE.EQ.'tek4014') GTXTTY=4014
      IF (TTYPE.EQ.'TEK4025'.OR.TTYPE.EQ.'tek4025') GTXTTY=4025
      IF (TTYPE.EQ.'TEK4027'.OR.TTYPE.EQ.'tek4027') GTXTTY=4027
      IF (TTYPE.EQ.'TEK4105'.OR.TTYPE.EQ.'tek4105') GTXTTY=4105
      IF (TTYPE.EQ.'TEK4112'.OR.TTYPE.EQ.'tek4112') GTXTTY=4112
      IF (TTYPE.EQ.'VT240'.OR.TTYPE.EQ.'vt240')   GTXTTY=240
      IF (TTYPE.EQ.'VT330'.OR.TTYPE.EQ.'vt330')   GTXTTY=240
      IF (TTYPE.EQ.'VT340'.OR.TTYPE.EQ.'vt340')   GTXTTY=240
      if (ttype.eq.'X'    .or.ttype.eq.'x'    )   GTXTTY=1011
      RETURN
C
c 990  CONTINUE
c      WRITE(6,*) ' *** Error opening terminal data file'
c      GTXTTY=0
c      RETURN
      END

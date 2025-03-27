      SUBROUTINE CHARA(CARD,N)
      CHARACTER*(*) CARD
C
      DO 1000 I=LEN(CARD)-1,1,-1
      IF(CARD(I:I).EQ.' ')   GO TO 1000
      IF(CARD(I:I).EQ.'$')   THEN
      N=I-1
      RETURN
      ELSE
        CARD(I+1:I+1)='$'
        N=I
      END IF
      RETURN
 1000 CONTINUE
C
      N=0
      CARD(1:1)='$'
      RETURN
C
      END

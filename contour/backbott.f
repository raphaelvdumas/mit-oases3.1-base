      SUBROUTINE BACKBOTT

      CHARACTER*40 WORD
      COMMON /MULBOTT/  LINES,numbot 

C  100 FORMAT(A40)
      DO 1000 I=1,numbot
 999  BACKSPACE(55)
      READ(55,'(a)') WORD
c      TYPE *,'WORD',WORD
      if (word(1:6).eq.'BOTTOM') then
       backspace(55)
       go to 1000
      else
       backspace(55)
       go to 999
      end if
 1000 CONTINUE
      LINES=0
      numbot=0 
      RETURN
      END

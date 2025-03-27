      SUBROUTINE OUTXID
C     ***DISSPLA ID ROUTINE
      LOGICAL LDONE
      COMMON /CMNXID/ LDONE
      DATA LDONE /.FALSE./
      IF (LDONE) RETURN
      LDONE=.TRUE.
      WRITE(6,100)
 100      FORMAT(//' MINDIS - Saclantcen Graph',
     X           ' Plotting Package'/)
      WRITE(6,110)
C 110      FORMAT(' Version 4.00AA  10-Feb-86 09:01:31  (Pre-release)'/)
C 110      FORMAT(' Version 4.00AB  16-May-86 11:41:31  (Pre-release)'/)
c 110      FORMAT(' Version 4.00    27-May-86 15:04:31'/)
c 110      FORMAT(' Version 4.01    15-Oct-86 17:14:49'/)
  110      FORMAT(' Version 4.02    17-Feb-87 13:01:49'/)
      RETURN
      END

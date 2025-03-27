      SUBROUTINE AXDEC(A1, A2, A3, IOUT )
      IF(IOUT .EQ. 1)   RETURN
      IF(   ( MOD(A1,1.0) .EQ. 0.0 ) .AND.
     &      ( MOD(A2,1.0) .EQ. 0.0 ) .AND.
     &      ( MOD(A3,1.0) .EQ. 0.0 )     )   IOUT= 1
      RETURN
      END

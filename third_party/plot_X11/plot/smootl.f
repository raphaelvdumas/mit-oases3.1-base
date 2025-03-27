      SUBROUTINE SMOOTL(NSP, NPOINTS, XAR, TLIN, TEMP)
      REAL*8 PSUM, TL1, TL2, TL3
      REAL*4 XAR(1), TLIN(1), TEMP(1)

C     INPUT :
C     NPOINTS : SIZE OF INPUT ARRAY
C     NSP     : NUMBER OF POINTS IN SMOOTHING WINDOW (ODD NUMBER)

C N.B.:
C     NSTART and NEND are first and last point on which the running window
C     of length NSP can be applied. A proportionally reduced window
C     will be applied on the other points.

  100 FORMAT(1X,/,'  ***  WARNING : ',/,
     & '       LENGTH OF SMOOTHING WINDOW IS LARGER THEN',
     & ' AVAILABLE DATA ',/,
     & '       NO SMOOTHING IS PERFORMED ',/,
     & '       NUMBER OF POINTS IN SMOOTHING WINDOW : ',I6,/,
     & '       NUMBER OF AVAILABLE DATA POINTS      : ',I6,/,
     & '  *** ')

      IF( (NSP .LE. 1) .OR. ( NSP .GT. NPOINTS) )   THEN
       DO 1000   J = 1, NPOINTS
       TEMP(J)= TLIN(J)
 1000  CONTINUE
       IF( NSP .GT. NPOINTS)   WRITE(6,100) NSP, NPOINTS
       RETURN
      END IF


      NSTART= NSP / 2 + 1
      NEND= NPOINTS - NSTART + 1
C  NUMBER OF POINTS IN SMOOTHING WINDOW MUST BE ODD
      NSP= (NSTART - 1) * 2 + 1

      SAVETL= TLIN(1)
      TEMP(1)= TLIN(1)
      TEMP(NPOINTS)= TLIN(NPOINTS)


C   WARNING : THE FOLLOWING DEFINITION FOR TL1 IS USED TO REMOVE 
C             NUMERICAL INSTABILITY.
C             IT COVERS THE CASE FOR TL= 0  AT RANGE= 0.
      IF( (TLIN(1) .LE. 0.0) .AND. (XAR(1) .EQ. 0.0) )   THEN
        TLIN(1)= TLIN(2)
      END IF

      DO 1400   J = 1, NPOINTS
      TL1= - TLIN(J) / 4.34294 
      TLIN(J)= DEXP( TL1 )
 1400 CONTINUE


      PSUM= TLIN( 1 )
      ICOUNT= 1


C  a reduced window is applied on points preceding point NSTART
      DO 1600   I = 2, NSTART - 1
      PSUM= PSUM  + TLIN( ICOUNT + 1 ) + TLIN( ICOUNT + 2 )
      ICOUNT= ICOUNT + 2
      TEMP(I)= - 10.0 * DLOG10(PSUM / ICOUNT)
 1600 CONTINUE


      PSUM= TLIN( NPOINTS )
      ICOUNT= 1
C  a reduced window is applied on points following point NEND
      DO 1800   I= NPOINTS - 1, NEND + 1, - 1
      PSUM= PSUM  + TLIN(NPOINTS - ICOUNT) + TLIN(NPOINTS - ICOUNT - 1)
      ICOUNT= ICOUNT + 2
      TEMP(I)= - 10.0 * DLOG10(PSUM / ICOUNT)
 1800 CONTINUE


      DO 2200 J= NSTART, NEND
      PSUM= 0.0D0

      DO 2000 I= 1, NSP
 2000 PSUM= PSUM +  TLIN( J - NSTART + I )

      TEMP(J)= - 10.0 * DLOG10(PSUM / NSP)
 2200 CONTINUE


      TLIN(1)= SAVETL      
      DO 3000   J= 2, NPOINTS
      TLIN(J)= TEMP(J)
 3000 CONTINUE

      RETURN
      END

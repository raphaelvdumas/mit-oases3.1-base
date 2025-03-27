      SUBROUTINE VCLIP(A,I,B,C,D,K,N)
      DIMENSION A(1),D(1)
      I1=1
      K1=1
      DO 10 L=1,N
       IF (A(I1).LT.B) THEN
         D(K1)=B
       ELSE IF (A(I1).GT.C) THEN
         D(K1)=C
       ELSE
         D(K1)=A(I1)
       END IF
      I1=I1+I
      K1=K1+K
10    CONTINUE
      RETURN
      END
      SUBROUTINE VMOV(A,I,C,J,N)
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(I1)
      I1=I1+I
 10   J1=J1+J
      RETURN
      END

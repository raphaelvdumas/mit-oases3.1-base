      SUBROUTINE TIMSPU(I,IPACT,DOMEGA,TSHIFT,JR,JD,THETA,NX,LX,MX)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      REAL FC(MMAX)
      IF (MSUFT.GT.MMAX) STOP '>>> MMAX TOO SMALL IN compar.f <<<'
      FC(1)=1E0
      do M=2,MSUFT,2
       MORD=M/2
c >>> reverse cos and sin terms for transverse components
       IF (IPACT.EQ.4.or.IPACT.eq.8) THEN
         FC(M)=SIN(MORD*THETA)
         FC(M+1)=-COS(MORD*THETA)
       ELSE
         FC(M)=COS(MORD*THETA)
         FC(M+1)=SIN(MORD*THETA)
       END IF
      end do

      CALL CVFILL(CNUL,CFF,2,LX-1)
      CALL CVFILL(CNUL,CFF(MX+1,1),2,NX/2-MX)
      CALL RWDBUF(31)
      DO J=LX,MX       
       do JJ=1,IR
        CALL RDBUF(31,CFILE,2*NOUT*NPLOTS*MSUFT)
        IF (JJ.EQ.JD) THEN
         CFF(J,1)=CNUL
         do M=1,MSUFT
          CFF(J,1)=CFF(J,1)+FC(M)*CFILE(JR+((I-1)+NOUT*(M-1))*NPLOTS)
         end do
        END IF
       end do
       FAC(J)=J-1
      end do
      CALL VMUL(FAC(LX),1,DOMEGA,0,ARG(LX),1,NUMFR)
      CALL VMUL(ARG(LX),1,TSHIFT,0,ARG(LX),1,NUMFR)
      CALL CVEXP(ARG(LX),1,CBUF(LX),2,NUMFR)
      CALL CVMUL(CBUF(LX),2,CFFS(LX),2,CBUF(LX),2,NUMFR,1)
      CALL CVMUL(CFF(LX,1),2,CBUF(LX),2,CFF(LX,1),2,NUMFR,1)
C
      IF (LX.GT.2.OR.MX.LT.NX/2) THEN
       IPL1=NX/2
       IPL2=0
       CALL CHERMIT(CFF(1,1),NX/2,LX,MX,DOMEGA,
     1              0.0,IPL1,IPL2)
      END IF
C
      if (deconv) then
c
c Shift to base band
c
       mf=nx/2
       mfh=mf/2
       mshift=2*pi*frqdec/domega
       call vclr(cff(1,2),1,2*nx)
       npsh=min(mfh,mf-mshift)
       call vmov(cff(mshift+1,1),1,cff(1,2),1,2*npsh)
       npsh=min(mfh,mshift)
       call vmov(cff(mshift+1-npsh,1),1,cff(2*mf-npsh+1,2),1,2*npsh)
       call cfft(cff(1,2),nx,-1)
       call cvmags(cff(1,2),2,cff(1,1),1,nx)
       call vsqrt(cff(1,1),1,cff(1,1),1,nx)
       call vsmul(cff(1,1),1,2.0,cff(1,1),1,nx)
      else
       CALL RFFT(CFF(1,1),NX,-1)
      end if
      IF (OMEGIM.NE.0E0) THEN
        RST=-OMEGIM*TSHIFT
        RSTP=-OMEGIM*2.*PI/(NX*DOMEGA)
        CALL VRAMP(RST,RSTP,ARG,1,NX)
        CALL VEXP(ARG,1,FAC,1,NX)
        CALL VMUL(CFF(1,1),1,FAC,1,CFF(1,1),1,NX)
      END IF
      RETURN
      END

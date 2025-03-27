      SUBROUTINE READTRF
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE

      COMPLEX CARR(NP3)
      EQUIVALENCE (CARR(1),CFF(1,1))

C *** STATEMENT FUNCTION FOR INDEXING CARR
      INDEXC(IND1,IND2,IND3)=IND1+IR*((IND2-1)+NPLOTS*(IND3-1))

C ***  OPEN TRF-FILE AND READ HEADING

      CALL READHEAD(ISTAT)

C ***  CHECK SIZE OF INPUT BUFFERS

        NELM=NP3
        IF ((IR*NPLOTS*NOUT).GT.NELM) THEN
         WRITE(6,*) '>>> READTRF: ARRAY CFF TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         PAUSE
         RETURN
        END IF
        NELM=ISIZE
        IF ((NPLOTS*NOUT).GT.NELM) THEN
         WRITE(6,*) '>>> READFMT: ARRAY CFILE TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         PAUSE
         RETURN
        END IF

        DLFRQP=1.0/REAL(DT*NX)
        DOMEGA=8.0*ATAN(1.0)*DLFRQP
        OPTION(1)=PROGNM
        LX=nint(FMIN/DLFRQP+1)
        LX=MAX(LX,LXTRF)
        MX=NINT(FMAX/DLFRQP+1)
        MX=MIN(MX,MXTRF)
        NUMFR=MX-LX+1
*     Open asynchronous scratch file

        CALL OPNBUF(31,2*NOUT*NPLOTS,IR*NUMFR,5000)

        DO 35 K=LXTRF,MXTRF
        DO 40 I=1,NPLOTS
          DO 50 J=1,IR
           IF (BINFILE) THEN
            READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
           ELSE 
            READ(15,*) (ARG(KK),FAC(KK),KK=1,NOUT)
           END IF
           DO 45 KK=1,NOUT
            INDX1=INDEXC(J,I,KK)
            CARR(INDX1)=CMPLX(ARG(KK),FAC(KK))
 45        CONTINUE
 50       CONTINUE
 40      CONTINUE

        IF(K.LT.LX.OR.K.GT.MX) GO TO 35

        DO 80 J=1,IR
        DO 85 L=1,NOUT
        DO 90 I=1,NPLOTS
        IF (SIGNN.EQ.'-') THEN
          CFILE(I+(L-1)*NPLOTS)=CONJG(CARR(INDEXC(J,I,L)))
        ELSE
          CFILE(I+(L-1)*NPLOTS)=CARR(INDEXC(J,I,L))
        ENDIF
90      CONTINUE
85      CONTINUE
        CALL WRBUF(31,CFILE,2*NOUT*NPLOTS)
80      CONTINUE

35      CONTINUE
        CLOSE(15)
        CALL ENFBUF(31)

        RETURN
        END
      SUBROUTINE READHEAD(ISTAT)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'


      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE

      CHARACTER*8 FILEID
      CHARACTER PARCHC(12)
      DATA PARCHC /'N','V','H','T',8*' '/
C *** STATEMENT FUNCTION FOR INDEXING CARR
      INDEXC(IND1,IND2,IND3)=IND1+IR*((IND2-1)+NPLOTS*(IND3-1))
        ISTAT=0
        BINFILE=.TRUE.
        OPEN(15,FILE=FILENAME,STATUS='OLD',FORM='UNFORMATTED',ERR=500)

        READ(15) FILEID
        IF (FILEID.NE.'PULSETRF') THEN
C         ISTAT=2
C         WRITE(6,*) 'BINARY FILE'
          GO TO 500
        END IF
        READ(15) PROGNM
        READ(15) NOUT
        READ(15) (IPARM(J),J=1,NOUT)
        READ(15) TITLE
        READ(15) SIGNN
        READ(15) FCTRF
        READ(15) SD
        READ(15) RD,RDLOW,IR
        IF (IR.GT.0) THEN
         IF (IR.GT.NRD) THEN
          WRITE(6,*) '>>> TOO MANY RECEIVERS IN FILE <<<'
          ISTAT=3
          RETURN
         END IF
         IF (IR.GT.1) THEN
          RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
         ELSE
          RDSTEP=1.0
         ENDIF
         DO 5 L=1,IR
          RDC(L)=(L-1)*RDSTEP+RD
  5      CONTINUE
        ELSE
         IR=ABS(IR)
         READ(15) (RDC(L),L=1,IR)
        END IF

        READ(15) R0,RSPACE,NPLOTS
        READ(15)NX,LXTRF,MXTRF,DT
        READ(15) ICDR
        READ(15) OMEGIM
        GO TO 600

  500   CLOSE(15,STATUS='KEEP',ERR=501)
  501   BINFILE=.FALSE.
        OPEN(15,FILE=FILENAME,STATUS='OLD',FORM='FORMATTED',ERR=995)
        READ(15,20) FILEID
        IF (FILEID.NE.'PULSETRF') THEN
          ISTAT=2
          WRITE(6,*) 'FORMATTED FILE'
          RETURN
        END IF
        READ(15,20) PROGNM
        READ(15,*) NOUT
        READ(15,*) (IPARM(J),J=1,NOUT)
        READ(15,20) TITLE
        READ(15,20) SIGNN
        READ(15,*) FCTRF
        READ(15,*) SD
        READ(15,*) RD,RDLOW,IR
        IF (IR.GT.0) THEN
         IF (IR.GT.NRD) THEN
          WRITE(6,*) '>>> TOO MANY RECEIVERS IN FILE <<<'
          ISTAT=3
          RETURN
         END IF
         IF (IR.GT.1) THEN
          RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
         ELSE
          RDSTEP=1.0
         ENDIF
         DO 105 L=1,IR
          RDC(L)=(L-1)*RDSTEP+RD
105      CONTINUE
        ELSE
         IR=ABS(IR)
         READ(15,*) (RDC(L),L=1,IR)
        END IF

        READ(15,*) R0,RSPACE,NPLOTS
        READ(15,*)NX,LXTRF,MXTRF,DT
        READ(15,*) ICDR
        READ(15,*) OMEGIM
 20     FORMAT(1X,A)

 600    DO 30 JJ=1,NOUT
 30     PCHOICE(JJ)=PARCHC(IPARM(JJ))
        RETURN

 995    ISTAT=1
        RETURN
        
        END

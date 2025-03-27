      program trfreadhead
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE
      character*80 filein,fileout

c >>> input file names
      write(6,*) 'Input file?'
      read(5,'(A)') filein
C ***  OPEN TRF-FILE AND READ HEADING
      CALL RHEAD(ISTAT,filein,fileout)

      CLOSE(15)
      END

      SUBROUTINE RHEAD(ISTAT,filein,fileout)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      character*(*) filein,fileout
      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE

      CHARACTER*8 FILEID
      CHARACTER PARCHC(12)
      DATA PARCHC /'N','V','H','T',8*' '/
        ISTAT=0
        BINFILE=.TRUE.
        OPEN(15,FILE=FILEin,STATUS='OLD',FORM='UNFORMATTED',ERR=995)

        READ(15) FILEID
        WRITE(6,*),'FILEID:',FILEID
        IF (FILEID.NE.'PULSETRF') THEN
         ISTAT=2
         RETURN
        END IF
        READ(15) PROGNM
        write(6,*),'PROGNM:',PROGNM
        READ(15) NOUT
        write(6,*),'NOUT:',NOUT
        READ(15) (IPARM(J),J=1,NOUT)
        write(6,*)('J,IPARM(J):',J,IPARM(J),j=1,NOUT)
        READ(15) TITLE
        write(6,*),'TITLE:',TITLE
        READ(15) SIGNN
        write(6,*),'SIGNN:',SIGNN
        READ(15) FCTRF
        write(6,*),'FCTRF:',FCTRF
        READ(15) SD
        write(6,*),'SD:',SD
        READ(15) RD,RDLOW,IR
        write(6,*),'RD,RDLOW,IR:',RD,RDLOW,IR
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
          READ(15) (RDC(L),L=1,(-IR))
          write(6,*)('L,RDC(L):',L,RDC(L),L=1,(-IR))
        END IF

        READ(15) R0,RSPACE,NPLOTS
        write(6,*),'R0,RSPACE,NPLOTS:',R0,RSPACE,NPLOTS
        READ(15)NX,LXTRF,MXTRF,DT
        write(6,*),'NX,LXTRF,MXTRF,DT:',NX,LXTRF,MXTRF,DT
        READ(15) ICDRIN
        write(6,*),'ICDRIN:',ICDRIN
        ICDR=ICDRIN
        READ(15) OMEGIM
        write(6,*),'OMEGIN:',OMEGIN
C *** READ HEADER EXTENSION
        READ(15) MSUFT
        write(6,*),'MSUFT:',MSUFT
        READ(15) ISROW
        write(6,*),'ISROW:',ISROW
        READ(15) INTTYP
        write(6,*),'INTTYP:',INTTYP
        DO 300 I=1,2
         READ(15) IDUMMY
        write(6,*),'IDUMMY:',IDUMMY
 300     CONTINUE
        DO 400 I=1,5
         READ(15) DUMMY
        write(6,*),'DUMMY:',DUMMY
 400     CONTINUE
 
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
        END IF

         WRITE(6,*) (RDC(L),L=1,IR)
 600    DO 30 JJ=1,NOUT
 30     PCHOICE(JJ)=PARCHC(IPARM(JJ))
        write(6,*),('JJ,PCHOICE(JJ):',JJ,' ',PCHOICE(JJ),JJ=1,NOUT)
        RETURN

 995    ISTAT=1
        RETURN
        
        END

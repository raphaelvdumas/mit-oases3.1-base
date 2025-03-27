      program trftovss
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE
      character*80 filein,fileout

c >>> input file names
      write(6,*) 'Input file?'
      read(5,'(A)') filein
      write(6,*) 'Output file?'
      read(5,'(A)') fileout
C ***  OPEN TRF-FILE AND READ HEADING

      CALL rwHEAD(ISTAT,filein,fileout)

      If (NOUT.ne.2) then
        stop '>>> NOUT not vss compatible <<<'
      end if

      DO K=LXTRF,MXTRF
       DO I=1,NPLOTS

           write(16,'(i10,a)') K,  '   # Frequency #'
           write(16,'(i10,a)') 1,  '   # Prop. dir.: 1 forw; 2 backw'
           write(16,'(i10,a)') I,  '   # Sector #'
           write(16,'(f10.3,a)') r0+(i-1)*rspace,'   # Range in km '
           write(16,'(a10,4a14)') ' Depth (m)',
     &                          '     Re(u)','     Im(u)',
     &                          '     Re(w)','     Im(w)'

        DO J=1,IR
         READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
        write(16,'(f10.3,4G14.6)') rdc(j),(ARG(KK),FAC(KK),KK=nout,1,-1)
        end do
       end do
      end do
      CLOSE(15)
      close(16)
      END

      SUBROUTINE RwHEAD(ISTAT,filein,fileout)
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
        IF (FILEID.NE.'PULSETRF') THEN
         ISTAT=2
         RETURN
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
          READ(15) (RDC(L),L=1,(-IR))
        END IF

        READ(15) R0,RSPACE,NPLOTS
        READ(15)NX,LXTRF,MXTRF,DT
        READ(15) ICDRIN
        ICDR=ICDRIN
        READ(15) OMEGIM
C *** READ HEADER EXTENSION
        READ(15) MSUFT
        READ(15) ISROW
        READ(15) INTTYP
        DO 300 I=1,2
         READ(15) IDUMMY
 300     CONTINUE
        DO 400 I=1,5
         READ(15) DUMMY
 400     CONTINUE
        write(6,*) 'omegim=',omegim
        write(6,*) 'IR=    ',ir
        write(6,*) 'nplot= ',nplots
        write(6,*) 'nout=  ',nout
        write(6,*) 'msuft= ',msuft
 
        OPEN(16,FILE=FILEout,STATUS='unknown',FORM='FORMATTED',ERR=995)

        return

 995    ISTAT=1
        RETURN
        
        END

      SUBROUTINE ASCOPN
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
      INCLUDE 'comgldwr.h'
        IUNGLD=18
        GLDNAME=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.asc'
      write(6,*) 'ASCII Trace file name?'
      read(5,'(a)') gldname
      OPEN(UNIT=IUNGLD,FILE=GLDNAME,
     &     FORM='FORMATTED',
     &     STATUS='UNKNOWN',ERR=999)
      RETURN
      ENTRY ASCCLS
      CLOSE(UNIT=IUNGLD,STATUS='KEEP')
      write(6,'(a,a)') 'Wrote ASCII trace file: ',gldname
      iplcnt=iplcnt+1
      RETURN
 999  STOP '>>> ERROR IN ASCOPN CREATING ASCII FILE <<<'
      END
      SUBROUTINE ASCMAKE(COM1,CPARM,FSMP,NPL,NR,NC)
      INCLUDE 'comgldwr.h'
      CHARACTER*(*) COM1,CPARM
      INTEGER NPL,NR,NC
      REAL FSMP
      call ascopn()
      ll=min0(len(com1),72)
      write(iungld,'(a)') com1(1:ll)
      write(iungld,'(11x,a,10x,a)') cparm,'# Parameter'
      write(iungld,'(i12,10x,a)') NPL,'# Number of planes'
      write(iungld,'(i12,10x,a)') NR,'# Number of traces'
      write(iungld,'(i12,10x,a)') NC,'# Number of samples/trace'
      write(iungld,'(G15.6,7x,a)') FSMP,'# Sampling frequency in Hz'
      return
      end

      SUBROUTINE ASCWRI(ran,dep,azi,tmin,NC,DATA)
      INCLUDE 'comgldwr.h'
      DIMENSION DATA(1)
      write(iungld,*)
      write(iungld,'(G15.6,7x,a)') ran,'# Range (m)'
      write(iungld,'(G15.6,7x,a)') dep,'# Depth (m)'
      write(iungld,'(G15.6,7x,a)') azi,'# Bearing (deg)'
      write(iungld,'(G15.6,7x,a)') tmin,'# Starting time (sec)'

      write(iungld,'((5G15.6))') (data(j), j=1,nc)  

      return
      end
c
c >>>> GLD format
c
      SUBROUTINE GLDOPN
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
      INCLUDE 'comgldwr.h'
        IUNGLD=18
        GLDNAME=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.gld'

      write(6,*) 'GLD trace file name?'
      read(5,'(a)') gldname

c >>> Use the following open statement on the Alliant and other 
c     machines where the record length is specified in bytes >>>
c      OPEN(UNIT=IUNGLD,FILE=GLDNAME,ACCESS='DIRECT',RECL=512,
c     &     FORM='UNFORMATTED',
c     &     STATUS='UNKNOWN',ERR=999)
c >>> Use the following open statement on the DEC station, specifying
c     recordlength in longwords >>>
      OPEN(UNIT=IUNGLD,FILE=GLDNAME,ACCESS='DIRECT',RECL=128,
     &     FORM='UNFORMATTED',
     &     STATUS='UNKNOWN',ERR=999)
cMS     &     FORM='BINARY',
cMS     &     MODE='WRITE',STATUS='UNKNOWN',ERR=999)
      RETURN
      ENTRY GLDCLS
      CLOSE(UNIT=IUNGLD,STATUS='KEEP')
      write(6,'(a,a)') 'Wrote GLD file: ',gldname
      iplcnt=iplcnt+1
      RETURN
 999  STOP '>>> ERROR IN MKHEAD CREATING GLD FILE <<<'
      END
      SUBROUTINE GLDMAKE(COM1,FSMP,NPL,NR,NC,PR,TY)
      INCLUDE 'comgldwr.h'
      CHARACTER PR,TY
      CHARACTER*(*) COM1
      INTEGER NPL,NR,NC
      REAL FSMP

      IF (TY.EQ.'c') THEN
       ISTR=2
      ELSE IF (TY.EQ.'r') THEN
       ISTR=1
      ELSE
       WRITE(6,*) '>>> ERROR IN MKHEAD: TYPE UNKNOWN <<<'
       STOP
      END IF

      IF (PR.EQ.'c') THEN
       ISTR=ISTR*1
      ELSE IF (PR.EQ.'i') THEN
       ISTR=ISTR*2
      ELSE IF (PR.EQ.'l') THEN
       ISTR=ISTR*4
      ELSE IF (PR.EQ.'f') THEN
       ISTR=ISTR*4
      ELSE IF (PR.EQ.'d') THEN
       ISTR=ISTR*8
      ELSE
       WRITE(6,*) '>>> ERROR IN MKHEAD: PRECISION UNKNOWN <<<'
       STOP
      END IF

      IHLEN=512
      NPLANES=NPL
      NROWS=NR
      NCOLS=NC
      CDISP=ISTR
      RDISP=CDISP*NCOLS
      PDISP=RDISP*NROWS
      FIRST=IHLEN
      FIRPL=IHLEN
      PREC=PR
      TYPE=TY
      COMM1=COM1
      COMM2=' '
      WRITE(COMM2,1000) YEAR,DAY,HOUR,MIN,SEC,FSMP
 1000 FORMAT(4I10,2F10.3)
      CALL GLDOPN
      WRITE(UNIT=IUNGLD,REC=1) (HH(II),II=1,512)
      write(6,'(1h ,a,a)') 'GLD file: ',gldname 
      write(6,'(1h ,a,i4,a,i4)') 'Rows:    ',nr,'Columns: ',nc
      RETURN
      END


      SUBROUTINE GLDWRI(IPLANE,ICHNO,DATA,IVST,IVEN)
      INCLUDE 'comgldwr.h'
        DIMENSION DATA(1)
C        WRITE(6,*) 'IPL,ICH,IVST,IVEN',IPL,ICH,IVST,IVEN
	IBST = 512+ ICHNO*RDISP + 4*IVST
        IBEN = 512+ ICHNO*RDISP + 4*IVEN
	NVALS = (IBEN - IBST)/4 + 1
C        write(6,*) 'IBST,IBEN,NVALS:',IBST,IBEN,NVALS
        IRST = IBST/512 + 1
	IREN = IBEN/512 + 1
        MODST = MOD(IBST,512)/4 + 1
	MODEN = MOD(IBEN,512)/4 + 1
	NRECS = IREN-IRST+1
C        WRITE(6,*) 'RST,REN,MODST,MODEN,NRECS:',IRST,IREN,MODST,MODEN,
C     &             NRECS
C
	JSTART = 1
	DO 400 III=1,NRECS
	 IF (III .EQ. 1) THEN
          ISTART = MODST
	 ELSE
	  ISTART = 1
	 ENDIF

C	 
	 IF (III .EQ. NRECS) THEN
	  IEND   = MODEN
	 ELSE
	  IEND = 128
	 ENDIF
	 JJ = 0
	 DO 300 II=ISTART,IEND
          BUF(II)=DATA(JSTART+JJ)
	  JJ = JJ+1
  300    CONTINUE
        JSTART = JSTART + JJ 
        WRITE (UNIT=IUNGLD,REC=IRST+III-1) (OUTBUF(II),II=1,512)
  400	CONTINUE
        RETURN
	END
c
c *** sdr trace file routines
c
      subroutine SDRPREP
      OPEN(11,STATUS='SCRATCH',FORM='UNFORMATTED')
      RETURN
      END
      SUBROUTINE SDRWRI(X,N)
      DIMENSION X(N)
      WRITE(11) (X(I),I=1,N)
      RETURN
      END
      SUBROUTINE SDRCLS(XX,ZZ,NR,ARR,NT,TSTEP)
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
      character*80 gldname
      dimension ARR(NT,NR)
        GLDNAME=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.trace'
      write(6,'(a,a)') 'Wrote trace file: ',gldname
      rewind(11)
      IF (NR*NT.GT.NP6) THEN
        STOP '>>> SDRCLS: NP TOO SMALL IN compar.f <<<'
      END IF
      DO 10 I=1,NR
       READ(11) (ARR(J,I),J=1,NT)
 10    CONTINUE
      close(11,STATUS='DELETE')
      WRITE(6,*) 'Writing file ', gldname
c      CALL WRITETRACE(GLDNAME,NR,NT,OEO,SD,TSTEP,NR,NT,
c     &                xx,zz,arr,ierr)
      IF (IERR.NE.0) THEN
       WRITE(6,*) '>>> WRITETRACE ERROR <<< CODE:',IERR
      END IF
      iplcnt=iplcnt+1
      RETURN
      END

c
c *** llnl CFS trace file routines
c
      subroutine CFSPREP
      OPEN(11,STATUS='SCRATCH',FORM='UNFORMATTED')
      RETURN
      END
      SUBROUTINE CFSWRI(X,N)
      DIMENSION X(N)
      WRITE(11) (X(I),I=1,N)
      RETURN
      END
      SUBROUTINE CFSCLS(XX,ZZ,NR,ARR,NT,TSTEP)
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
      character*80 gldname
      dimension ARR(NT,NR)
        GLDNAME=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.trace'
      rewind(11)
      IF (NR*NT.GT.NP6) THEN
        STOP '>>> SDRCLS: NP TOO SMALL IN compar.f <<<'
      END IF
      DO 10 I=1,NR
       READ(11) (ARR(J,I),J=1,NT)
 10    CONTINUE
      close(11,STATUS='DELETE')
      write(6,*) 'Trace file name?'
      read(5,'(a)') gldname
      write(6,*) 'Writing to file ',gldname
      call writecsf(gldname,nt,nr,arr,tstep)
c      WRITE(6,*) 'Here I were supposed to write file ', gldname
c      CALL WRITETRACE(GLDNAME,NR,NT,OEO,SD,TSTEP,NR,NT,
c     &                xx,zz,arr,ierr)
      IF (IERR.NE.0) THEN
       WRITE(6,*) '>>> WRITETRACE ERROR <<< CODE:',IERR
      END IF
      RETURN
      END
c                                                   writecsf
c
c  Fortran routine to write OASES/SAFARI trace data to a channel
c    sequential output file.  For use to drive ELAS, and 
c    to provide files in an intermediate format for creating SAC 
c    files for individual channels.
c
      subroutine writecsf( filename, nt, nch, array, delta )
c
        character*(*) filename
        integer nt, nch
        real*4 delta
        real*4 array( nt, nch )
c
        open( 12, file = filename, status = 'unknown',
     &            form = 'unformatted' )
c
        write(12) nch, nt, delta
        do i = 1, nt
          write(12) ( array( i, j ), j = 1, nch )
        end do
c
        close(12)
c
      return
      end
        

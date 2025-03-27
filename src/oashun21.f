      INTEGER FUNCTION MEMSIZ()
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      COMMON /SYSxMD/ USERMEM(msiz)
C
C     INTEGER FUNCTION TO RETURN OF TOTAL MEMORY SIZE ALLOCATED TO
C     IO-BUFFERS. RETURNS # BLOCKS COMPATIBLE WITH LAST PARAMETER
C     IN CALL TO OPNBUF.
       MEMSIZ=INT( (0.5*MSIZ-1200)/64)
       RETURN
       END
      INTEGER FUNCTION MEMLFT()
      PARAMETER (msiz=4500000)
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /SYSxMA/ ISOLD
C
C     INTEGER FUNCTION TO RETURN OF REMAINING MEMORY SIZE FOR
C     IO-BUFFERS. RETURNS # BLOCKS COMPATIBLE WITH LAST PARAMETER
C     IN CALL TO OPNBUF.
       MEMLFT=INT( (0.5*(MSIZ-ISOLD+1)-63)/64)
       RETURN
       END
      SUBROUTINE OPNBUF(IUN,LREC,NREC,IBUFSIZ)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      external asio
      CHARACTER*60 FILENM
      INTEGER SYSADDMEM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IUN.GT.99.OR.IUN.LT.1) THEN
      CALL AIOERR('OPNBUF',IUN,1)
      END IF
      IF (IOPN(IUN).EQ.1) THEN
      CALL AIOERR('OPNBUF',IUN,3)
      ELSE
      IOPN(IUN)=1
      END IF
      IRSIZE(IUN)=IBUFSIZ*64+63
      IF (LREC.GT.IRSIZE(IUN)) THEN
      CALL AIOERR('OPNBUF',IUN,4)
      END IF
      ISTAT=SYSADDMEM(ISTART(IUN),2*IRSIZE(IUN))
      IF (ISTAT.NE.0) STOP
      IREC(IUN)=LREC
      NTOT=LREC*NREC
      NN=IRSIZE(IUN)/LREC
      NOFR(IUN)=(NREC-1)/NN+1
      NPRREC(IUN)=LREC*NN
      ICOUNT(IUN)=1
      IRECAC(IUN)=1
      IRDIR(IUN)=0
      TOGGLE(IUN)=-1
      IB1(IUN)=INDX1(TOGGLE(IUN))
      IB2(IUN)=INDX2(TOGGLE(IUN))
      IOFF(IUN)=0
      IF (NOFR(IUN).GT.2) THEN
      NSIZE=NOFR(IUN)*IRSIZE(IUN)*1.2E-3
C      LNREC=(IRSIZE(IUN))*8
C      WRITE(FILENM,1230) IUN,NSIZE,MOD(IUN,NDISK)+20
C 1230 FORMAT('FTN0',I2.2,',SI=',I5.5,',DI=',I2)
C     WRITE(FILENM,1230) IUN,NSIZE
C1230 FORMAT('FTN0',I2.2,',SI=',I5.5)
C      OPEN(UNIT=IUN,FILE=FILENM,FORM='UNFORMATTED',
C     1     RECL=LNREC,
C     1     SYNC='ASYNCHRONOUS')
      OPEN(UNIT=IUN,STATUS='SCRATCH',FORM='UNFORMATTED')
      END IF
      RETURN
      END
C
C
      SUBROUTINE ENFBUF(IUN)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      CHARACTER*60 FILENM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
c      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IOPN(IUN).NE.1) THEN
      CALL AIOERR('ENFBUF',IUN,2)
      END IF
      IF (NOFR(IUN).GT.2) THEN
      IB1(IUN)=INDX1(TOGGLE(IUN))
      CALL ASOUT(IUN,USERMEM(IB1(IUN)),IRSIZE(IUN))
      ENDFILE(IUN)
      END IF
      NOFR(IUN)=IRECAC(IUN)
      IRDIR(IUN)=-2
      RETURN
      END
C
C
      SUBROUTINE RWDBUF(IUN)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      CHARACTER*60 FILENM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IOPN(IUN).NE.1) THEN
      CALL AIOERR('RWDBUF',IUN,2)
      END IF
      TOGGLE(IUN)=-1
      IB1(IUN)=INDX1(TOGGLE(IUN))
      IB2(IUN)=INDX2(TOGGLE(IUN))
      IOFF(IUN)=0
      ICOUNT(IUN)=1
      IRECAC(IUN)=1
      IRDIR(IUN)=1
      IF (NOFR(IUN).GT.2) THEN
      REWIND(IUN)
      CALL ASRDIN(IUN,USERMEM(IB1(IUN)),IRSIZE(IUN))
      CALL ASRDIN(IUN,USERMEM(IB2(IUN)),IRSIZE(IUN))
      END IF
      RETURN
      END
C
C
      SUBROUTINE CLSBUF(IUN)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      CHARACTER*60 FILENM
      INTEGER SYSSUBMEM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
c      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
c      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IOPN(IUN).NE.1) THEN
      CALL AIOERR('CLSBUF',IUN,2)
      ELSE
      IOPN(IUN)=0
      END IF
      IF (NOFR(IUN).GT.2) THEN
      CLOSE(UNIT=IUN,STATUS='DELETE')
      END IF
      ISTAT=SYSSUBMEM(ISTART(IUN),2*IRSIZE(IUN))
      IF (ISTAT.NE.0) STOP

      RETURN
      END
C
C
      SUBROUTINE WRBUF(IUN,C,NVAL)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      DIMENSION C(NVAL)
      CHARACTER*60 FILENM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IOPN(IUN).NE.1) THEN
      CALL AIOERR('WRBUF ',IUN,2)
      END IF
      IF (IRECAC(IUN).GT.NOFR(IUN)) THEN
      CALL AIOERR('WRBUF ',IUN,5)
      END IF
      IF (NVAL.GT.IREC(IUN)) THEN
      CALL AIOERR('WRBUF ',IUN,6)
      END IF
      IF (IRDIR(IUN).NE.0) THEN
        CALL AIOERR('WRBUF ',IUN,9)
      END IF
      IF (IOFF(IUN).GE.NPRREC(IUN)) THEN
      IOFF(IUN)=0
      TOGGLE(IUN)=-TOGGLE(IUN)
      IB1(IUN)=INDX1(TOGGLE(IUN))
      IB2(IUN)=INDX2(TOGGLE(IUN))
      IRECAC(IUN)=IRECAC(IUN)+1
      IF (NOFR(IUN).GT.2) THEN
      CALL ASOUT(IUN,USERMEM(IB2(IUN)),IRSIZE(IUN))
      END IF
      END IF
      IADDR=IOFF(IUN)+IB1(IUN)
C      CALL VMOV(C,1,USERMEM(IADDR),1,NVAL)
      DO 10 II=1,NVAL
       USERMEM(IADDR+II-1)=C(II)
 10   CONTINUE
      IOFF(IUN)=IOFF(IUN)+IREC(IUN)
      ICOUNT(IUN)=ICOUNT(IUN)+1
      RETURN
      END
C
C
      SUBROUTINE RDBUF(IUN,C,NVAL)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      DIMENSION C(NVAL)
      CHARACTER*60 FILENM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IOPN(IUN).NE.1) THEN
      CALL AIOERR('RDBUF ',IUN,2)
      END IF
      IF (IRECAC(IUN).GT.NOFR(IUN)) THEN
      CALL AIOERR('RDBUF ',IUN,7)
      END IF
      IF (NVAL.GT.IREC(IUN)) THEN
      CALL AIOERR('RDBUF ',IUN,8)
      END IF
      IF (IRDIR(IUN).NE.1) THEN
        CALL AIOERR('WRBUF ',IUN,9)
      END IF
      IF (IOFF(IUN).GE.NPRREC(IUN)) THEN
      IOFF(IUN)=0
      TOGGLE(IUN)=-TOGGLE(IUN)
      IB1(IUN)=INDX1(TOGGLE(IUN))
      IB2(IUN)=INDX2(TOGGLE(IUN))
      IRECAC(IUN)=IRECAC(IUN)+1
      IF (NOFR(IUN).GT.2) THEN
      IF (IRECAC(IUN).LT.NOFR(IUN)) THEN
      CALL ASRDIN(IUN,USERMEM(IB2(IUN)),IRSIZE(IUN))
      ELSE
C      WAIT(IUN)
      END IF
      END IF
      END IF
      IADDR=IB1(IUN)+IOFF(IUN)
C      CALL VMOV(USERMEM(IADDR),1,C,1,NVAL)
      DO 10 II=1,NVAL
       C(II)=USERMEM(IADDR+II-1)
 10   CONTINUE
      IOFF(IUN)=IOFF(IUN)+IREC(IUN)
      ICOUNT(IUN)=ICOUNT(IUN)+1
      RETURN
      END
      SUBROUTINE RBBUF(IUN,C,NVAL)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (msiz=4500000)
      dimension C(NVAL)
      CHARACTER*60 FILENM
      integer TOGGLE,LL
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      INDX1(LL)=NINT(0.5+0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
      INDX2(LL)=NINT(0.5-0.5*ISIGN(1,LL))*IRSIZE(IUN)+ISTART(IUN)
C
      IF (IOPN(IUN).NE.1) THEN
      CALL AIOERR('RBBUF ',IUN,2)
      END IF
      IF (IRECAC(IUN).LT.1) THEN
      CALL AIOERR('RBBUF ',IUN,7)
      END IF
      IF (NVAL.GT.IREC(IUN)) THEN
      CALL AIOERR('RBBUF ',IUN,8)
      END IF
C
C     IF FIRST BACKWARDS READ, POSOTION FILE FOR NEXT READ
C
      IF (IRDIR(IUN).EQ.-1) THEN
        IOFF(IUN)=IOFF(IUN)-IREC(IUN)
      ELSE IF (IRDIR(IUN).EQ.-2) THEN
        IF (NOFR(IUN).GT.2) THEN
          BACKSPACE(IUN)
          BACKSPACE(IUN)
        END IF
        IRDIR(IUN)=-1
        IOFF(IUN)=IOFF(IUN)-IREC(IUN)
      ELSE IF (IRDIR(IUN).EQ.0) THEN
        CALL AIOERR('RBBUF ',IUN,9)
      ELSE
        CALL AIOERR('RBBUF ',IUN,10)
      END IF
C
      IF (IOFF(IUN).LT.0) THEN
        IOFF(IUN)=NPRREC(IUN)-IREC(IUN)
        TOGGLE(IUN)=-TOGGLE(IUN)
        IB1(IUN)=INDX1(TOGGLE(IUN))
        IB2(IUN)=INDX2(TOGGLE(IUN))
        IRECAC(IUN)=IRECAC(IUN)-1
        IF (NOFR(IUN).GT.2) THEN
          IF (IRECAC(IUN).GT.1) THEN
            BACKSPACE(IUN)
            BACKSPACE(IUN)
            CALL ASRDIN(IUN,USERMEM(IB2(IUN)),IRSIZE(IUN))
          ELSE
C            WAIT(IUN)
          END IF
        END IF
      END IF
      IADDR=IB1(IUN)+IOFF(IUN)
C      CALL VMOV(USERMEM(IADDR),1,C,1,NVAL)
      DO 10 II=1,NVAL
       C(II)=USERMEM(IADDR+II-1)
 10   CONTINUE
      ICOUNT(IUN)=ICOUNT(IUN)-1
      RETURN
      END
      SUBROUTINE ASOUT(IUN,AA,NUM)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION AA(NUM)
C      WRITE(UNIT=IUN,WAIT=.FALSE.) AA
      WRITE(UNIT=IUN) (AA(J),J=1,NUM)
      RETURN
      END
      SUBROUTINE ASRDIN(IUN,AA,NUM)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION AA(NUM)
C      READ(UNIT=IUN,WAIT=.FALSE.) AA
      READ(UNIT=IUN) (AA(J),J=1,NUM)
      RETURN
      END
      SUBROUTINE AIOERR(TIT,IUN,IERR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      CHARACTER*6 TIT
      CHARACTER*30 MESS(10)
      DATA MESS(1) /'*** ILLEGAL LOGICAL UNIT   ***'/
      DATA MESS(2) /'*** FILE NOT OPENED        ***'/
      DATA MESS(3) /'*** FILE ALREADY OPENED    ***'/
      DATA MESS(4) /'*** BUFFER TOO SMALL       ***'/
      DATA MESS(5) /'*** WRITE PAST END OF FILE ***'/
      DATA MESS(6) /'*** WRITE PAST END OF RECD ***'/
      DATA MESS(7) /'*** READ PAST END OF FILE  ***'/
      DATA MESS(8) /'*** READ PAST END OF RECD  ***'/
      DATA MESS(9) /'*** MIXED R/W NOT ALLOWED  ***'/
      DATA MESS(10) /'*** TWO WAY READ MODE N.A. ***'/
C
C
 100  FORMAT(1H ,'*** ASYNCHRONOUS IO ERROR   ***')
 200  FORMAT(1H ,'*** SUBROUTINE: ',A6,'      ***')
 300  FORMAT(1H ,'*** UNIT NUMBER:',I6,'      ***')
 400  FORMAT(1H ,'*** ERROR NUMBER:',I5,'      ***')
 500  FORMAT(1H ,A30)
      WRITE(6,100)
      WRITE(6,200) TIT
      WRITE(6,300) IUN
      WRITE(6,400) IERR
      WRITE(6,500) MESS(IERR)
      STOP '*** EXECUTION TERMINATED   ***'
      END
      INTEGER FUNCTION SYSADDMEM(IST,ISIZ)
      PARAMETER (msiz=4500000)
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /SYSxMA/ ISOLD
      IST=ISOLD
      ISOLD=IST+ISIZ
      IF (ISOLD.GT.MSIZ+1) THEN
        WRITE(6,*) '>>> FATAL: INSUFFICIENT BUFFER SPACE <<<'
        SYSADDMEM=1
      ELSE
        SYSADDMEM=0
      END IF
      RETURN
      END
      INTEGER FUNCTION SYSSUBMEM(IST,ISIZ)
      PARAMETER (msiz=4500000)
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /SYSxMA/ ISOLD
      ISOLD=MIN(ISOLD,IST)
      IF (ISOLD.LT.1) THEN
        WRITE(6,*) '>>> FATAL: EXCESSIVE BUFFER SPACE SUBTRACTION <<<'
        SYSSUBMEM=1
      ELSE
        SYSSUBMEM=0
      END IF
      RETURN
      END
      BLOCK DATA ASIO
      PARAMETER (msiz=4500000)
      integer TOGGLE
      COMMON /SYSxMD/ USERMEM(msiz)
      COMMON /SYSxMA/ ISOLD
      COMMON /AIOBUF/ IBUF(99),NOFR(99),IRECAC(99),NPRREC(99),
     1  ICOUNT(99),TOGGLE(99),IB1(99),IB2(99),IOFF(99),IREC(99),
     2  IRSIZE(99),ISTART(99),IOPN(99),IRDIR(99)
      DATA IOPN /99*0/
      DATA ISOLD /1/
C
      END
      SUBROUTINE CLTIME
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      ! real*4 etime
      DIMENSION SECS(2)
      INTEGER(8) COUNT, COUNT_RATE, COUNT_MAX, OLDCOUNT
      COMMON /TIMECT/ OLDCOUNT 
      ! CALL ETIME(SECS, OLDSECS)
      ! CALL CPU_TIME (OLDSECS)
      CALL SYSTEM_CLOCK (COUNT, COUNT_RATE, COUNT_MAX)
      OLDCOUNT = COUNT
C      OLDSECS=SECS(1)
C      CALLSTAT=LIB$INIT_TIMER()
      RETURN
      END
      SUBROUTINE RDTIME(T1)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      ! real*4 etime
c
c Returns CPU time in secs.
c
C *** UNIX SYSTEMS
      DIMENSION SECS(2)
      INTEGER(8) COUNT, COUNT_RATE, COUNT_MAX, OLDCOUNT
      COMMON /TIMECT/ OLDCOUNT 
      REAL(4) T1
      ! CALL ETIME(SECS, T1)
      ! CALL CPU_TIME (T1)
      CALL SYSTEM_CLOCK (COUNT, COUNT_RATE, COUNT_MAX)
      T1 = dble(float(COUNT - OLDCOUNT) / float(COUNT_RATE))
C *** VMS SYSTEMS
CVMS  INTEGER*4 IT4
CVMS  REAL*8 IT8
CVMS  EQUIVALENCE (IT4,IT8)
CVMS  CALLSTAT=LIB$STAT_TIMER(2,IT4)
CVMS  T1=1E-2*IT4
      RETURN
      END
      SUBROUTINE OPFILW(IUN,IOER)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      CHARACTER*120 FILENM
      character*6 ENVVAR
      character*3 exten
      IOER=0
C
C     OPENS SEQUENCIAL ASCII FILE FOR WRITE
C
C***  VMS
C     OPEN(UNIT=IUN,STATUS='NEW',FORM='FORMATTED',ERR=300)
C***  ultrix 
      WRITE(ENVVAR,'(A3,I3.3)') 'FOR',IUN
      CALL GETENV(ENVVAR,FILENM)
      IF (FILENM.EQ.' ') THEN
       inquire(1,name=filenm)
       ii=indexs(filenm,'.')
       write(exten,'(i3.3)') iun
       OPEN(UNIT=IUN,file=filenm(1:ii)//exten,
     &      STATUS='UNKNOWN',FORM='FORMATTED',ERR=300)
      ELSE
       OPEN(UNIT=IUN,FILE=FILENM,STATUS='UNKNOWN',
     &      FORM='FORMATTED',ERR=300)
      END IF
C***  MS-DOS         
c      IF (IUN.LT.10) THEN
c        WRITE(FILENM,100) IUN
c      ELSE
c        WRITE(FILENM,200) IUN
c      END IF
c 100  FORMAT('FOR00',I1,'.DAT')
c 200  FORMAT('FOR0',I2,'.DAT')
c      OPEN(UNIT=IUN,FILE=FILENM,STATUS='UNKNOWN',FORM='FORMATTED',
c     1     ERR=300)
      RETURN
 300  IOER=1
      RETURN
C
      ENTRY OPFILR(IUN,IOER)
      IOER=0
C
C     OPENS SEQUENCIAL ASCII FILE FOR READ
C
C***  VMS
C     OPEN(UNIT=IUN,STATUS='OLD',ERR=400)
C***  ultrix 
      WRITE(ENVVAR,'(A3,I3.3)') 'FOR',IUN
      CALL GETENV(ENVVAR,FILENM)
      IF (FILENM.EQ.' ') THEN
       OPEN(UNIT=IUN,STATUS='OLD',FORM='FORMATTED',ERR=400)
      ELSE
       OPEN(UNIT=IUN,FILE=FILENM,STATUS='OLD',
     &      FORM='FORMATTED',ERR=400)
      END IF
C***  MS-DOS         
c      IF (IUN.LT.10) THEN
c        WRITE(FILENM,100) IUN
c      ELSE
c        WRITE(FILENM,200) IUN
c      END IF
c      OPEN(UNIT=IUN,FILE=FILENM,STATUS='OLD',ERR=400)
      RETURN
 400  IOER=2
      RETURN
      END
      SUBROUTINE OPFILB(IUN,IOER)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      CHARACTER*120 FILENM
      character*6 ENVVAR
      character*3 exten
      IOER=0
C
C     OPENS SEQUENCIAL ASCII FILE FOR WRITE
C
C***  VMS
C     OPEN(UNIT=IUN,STATUS='NEW',FORM='FORMATTED',ERR=300)
C***  ultrix 
      WRITE(ENVVAR,'(A3,I3.3)') 'FOR',IUN
      CALL GETENV(ENVVAR,FILENM)
      IF (FILENM.EQ.' ') THEN
       inquire(1,name=filenm)
       ii=indexs(filenm,'.')
       write(exten,'(i3.3)') iun
       OPEN(UNIT=IUN,file=filenm(1:ii)//exten,
     &      STATUS='UNKNOWN',FORM='UNFORMATTED',ERR=300)
      ELSE
       OPEN(UNIT=IUN,FILE=FILENM,STATUS='UNKNOWN',
     &      FORM='UNFORMATTED',ERR=300)
      END IF
C***  MS-DOS         
c      IF (IUN.LT.10) THEN
c        WRITE(FILENM,100) IUN
c      ELSE
c        WRITE(FILENM,200) IUN
c      END IF
c 100  FORMAT('FOR00',I1,'.DAT')
c 200  FORMAT('FOR0',I2,'.DAT')
c      OPEN(UNIT=IUN,FILE=FILENM,STATUS='UNKNOWN',FORM='UNFORMATTED',
c     1     ERR=300)
      RETURN
 300  IOER=1
      RETURN
      end
c
c The following routines are used for determining record length allocation 
c for direct access files.
C
C     NAME     : DURLM
C     PURPOSE  : DYNAMIC UNIT RECORD LENGTH MEASUREMENT SUBROUTINES.
C     AUTHOR   : Austin J Lee
C     SYNTAX   : STRICT ANSI FORTRAN 77 
C     USAGE    :
C            CALL SREC(L) ! single precision real variable
C            "L" is retrun integer variable which has 
C            unit record length for corresponding variable.
C
      SUBROUTINE DASREC(L)
      INTEGER L,LU
      REAL V(2)
      LOGICAL Q
      CALL SOPEND(LU,2)
      WRITE(UNIT=LU,REC=1,ERR=100) V(1),V(2)
      CLOSE(LU)
      L=1
      RETURN
 100  INQUIRE(UNIT=LU,OPENED=Q)
      IF (Q) CLOSE(LU)
      CALL SOPEND(LU,4)
      WRITE(UNIT=LU,REC=1,ERR=200) V(1),V(2)
      CLOSE(LU)
      L=2
      RETURN
 200  INQUIRE(UNIT=LU,OPENED=Q)
      IF (Q) CLOSE(LU)
      CALL SOPEND(LU,8)
      WRITE(UNIT=LU,REC=1,ERR=300) V(1),V(2)
      CLOSE(LU)
      L=4
      RETURN
 300  INQUIRE(UNIT=LU,OPENED=Q)
      IF (Q) CLOSE(LU)
      L=8
      RETURN
      END
C
      SUBROUTINE SOPEND(LU,LVEC)
      INTEGER LU,LVEC,LUFREE
      LU = LUFREE()
      OPEN(UNIT=LU,FORM='UNFORMATTED',STATUS='SCRATCH',ACCESS='DIRECT',
     &     RECL=LVEC,ERR=100)
      RETURN
 100  CALL DA_ERRMSG('SOPEND: FILE OPEN ERROR',6)
      END
C
      FUNCTION LUFREE()
      INTEGER LUFREE,I
      LOGICAL QOPEN
      DO 10 I=99,10,-1
         INQUIRE(UNIT=I,OPENED=QOPEN)
         IF (.NOT.QOPEN) THEN
            LUFREE = I
            RETURN
         END IF
 10   CONTINUE
      LUFREE = -1
      CALL DA_ERRMSG('lufree : no availible logical unit number',6)
      RETURN
      END
C
      SUBROUTINE DA_ERRMSG(A,LU)
      CHARACTER*(*) A
      INTEGER LU
      WRITE(LU,*) A
      STOP
      END


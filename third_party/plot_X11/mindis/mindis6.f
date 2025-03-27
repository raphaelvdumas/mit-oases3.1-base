C
C     ***MINDIS - SACLANTCEN GRAPH PLOTTING PACKAGE
C
C     *** John Steiert    -    13-JUN-83
C
C     *** Ammended:
C
C          JS    8-Nov-83   Use binary files again
C                           Allow for use of Applicon
C                           Allow for use of Tektronix 4112
C          JS   30-Aug-84   Allow for use of Digital VT240 terminal.
C	   JS   31-Dec-84   Look at symbol USRXTERMTYPE before TTYPE.DAT
C	   JS    5-Jan-85   New routine: TXCTLT
C	   JS   15-Feb-85   Use Tektronix 4691 instead of Applicon.
C	   JS   18-Feb-85   Allow for Calcomp plots via UNIVAC.
C	   JS   27-Sep-85   Allow use of Tektronix 4105
C			    Routine to read default output device
C			    Allow use of Central Printronix
C	   JS	 6-Feb-86   Use LIBXSPAWN when queueing batch runs
C			    Put MCNAME in filename
C	   JS   15-May-86   Submit/notify of batch run
C			    Add Viewgraph Option
C			    Handle asking hard-copy info
C	   JS   15-Oct-86   Remove us:[saclantlb.plots] references
C	   JS   17-Feb-87   Calcomp electrostatic plotter hardcopy
C          HS   24-Aug-87   Changed to ULTRIX operating system
C
C ---------------------------------------------------
C
C     *** Operating System dependent routines
C
C      ***       VAX ULTRIX version     ***
C
C ---------------------------------------------------
C
      SUBROUTINE GNXINI
C     ***Routine used to force loading of modules - never executed
      I=1
      J=2
      IF (I-J.EQ.J+I) GOTO 19
      RETURN
C
C     ***DUMMY CALL TO ENSURE MINDIS VERSION OF F2FRTX IS USED
 19   CONTINUE
C
c      CALL BLCKX1
c      CALL BLCKX2
c      CALL BLCKX3
c      CALL BLCKX4
c      CALL BLCKX5
c      CALL BLCKX6
      END
C
C ---------------------------------------------------
C
      SUBROUTINE GTXDAT(PDATE,PTIME)
C     ***Routine to return date and time in Ascii
      CHARACTER*(*) PDATE, PTIME
      INTEGER II(3)
c      EXTERNAL IDATE,ITIME
c      CALL IDATE(II)
c      WRITE(PDATE,100) II(2),II(1),MOD(II(3),100)
c      CALL ITIME(II)
c      WRITE(PTIME,100) II(1),II(2),II(3)
c      WRITE(6,*) ' DATE = ',PDATE
c      WRITE(6,*) ' TIME = ',PTIME
 100  FORMAT(3I2.2)
      RETURN
      END
C
C ---------------------------------------------------
C
      LOGICAL FUNCTION GTXBCH()
C     ***Routine which looks to see if we are running in batch
      LOGICAL BATCH
c      GTXBCH=BATCH()
      BATCH=.FALSE.
      GTXBCH=BATCH
C      WRITE(6,*) ' BATCH = ',GTXBCH
      RETURN
      END
C
C ---------------------------------------------------
C
      SUBROUTINE GTXRID(RID,INCHRS)
C     ***Routine to return the run id
      IMPLICIT INTEGER*2(W), INTEGER*4(L)
      CHARACTER*(*) RID
      CHARACTER*12 LCLVALUES
c      INCLUDE '(XJPIDEF)'
c      COMMON /DTXRID/ WLEN1, WCODE1, LADDR1, LLENADDR1
c      DATA WLEN1 /12/
c      DATA WCODE1 /JPIXUSERNAME/
C
c      LADDR1 = %LOC(LCLVALUES)
c      LLENADDR1=%LOC(WLEN2)
c      CALL SYSXGETJPI(,,,WLEN1,,,)
c      RID=LCLVALUES(1:6)
c      INCHRS=6
c      IF (WLEN2.LT.6) INCHRS=WLEN2
C      WRITE(6,*) ' USER NAME = ',RID
C      WRITE(6,*) ' WLEN2=',WLEN2
C      WRITE(6,*) ' USER NAME LEN = ',INCHRS
       RID=' '
       INCHRS=1
      RETURN
      END
C
C ---------------------------------------------------
C
      INTEGER FUNCTION GTXTTY()
C     ***Routine to return terminal type
C
      IMPLICIT INTEGER*2(W), INTEGER*4(L)
      CHARACTER*63 NAMDES
      INTEGER*2 NAMLEN
      INTEGER*4 SYSXTRNLOG
      logical frstgt
      common /frstgt/ frstgt
      LOGICAL LNAMOK
      CHARACTER*8 TNAM1, TTYP1
      CHARACTER*8 ATERM, TTYPE
      CHARACTER*8 LCLVALUES
c      INCLUDE '(XJPIDEF)'
c      COMMON /DTXTID/ WLEN1, WCODE1, LADDR1, LLENADDR1
c      DATA WLEN1 /8/
c      DATA WCODE1 /JPIXTERMINAL/
C
c      ICODE=SYSXTRNLOG('USRXTERMTYPE',NAMLEN,NAMDES,,,%VAL(3))
c      LNAMOK=(ICODE.AND.(NAMDES(1:NAMLEN).NE.'USRXTERMTYPE'))
c
c *** UNIX. HS 4-Feb-89
       if (.not.frstgt) then
         call getenv('USRTERMTYPE',namdes)
         if (NAMDES.eq.' ') then
           LNAMOK=.FALSE.
         ELSE
           DO 101 JJ=63,1,-1
           IF (NAMDES(JJ:JJ).NE.' ') THEN
             NAMLEN=JJ
             GO TO 102
           END IF
 101       CONTINUE
 102       CONTINUE
           LNAMOK=.TRUE.
         END IF
        FRSTGT=.TRUE.
       END IF

       IF (LNAMOK) THEN
           TTYPE=NAMDES(1:NAMLEN)
       ELSE
           WRITE(6,*) 'Env. variable USRTERMTYPE not set'
           WRITE(6,*) 'Specify terminal type [X,tek4105',
     &                '(-4112 -4010 -4014)', 
     &                ',vt240]'
           read(5,'(A)') NAMDES           
         if (NAMDES.ne.' ') then
           DO 201 JJ=63,1,-1
           IF (NAMDES(JJ:JJ).NE.' ') THEN
             NAMLEN=JJ
             GO TO 202
           END IF
 201       CONTINUE
 202      CONTINUE
          TTYPE=NAMDES(1:NAMLEN)
         else
          TTYPE='EOF RECD'
         END IF
       end if
       LNAMOK=.TRUE.
C
      WRITE(6,*) ' TERM = ',TTYPE
      GTXTTY=0
      IF (TTYPE.EQ.'TEK4010'.OR.TTYPE.EQ.'tek4010') GTXTTY=4010
      IF (TTYPE.EQ.'TEK4014'.OR.TTYPE.EQ.'tek4014') GTXTTY=4014
      IF (TTYPE.EQ.'TEK4025'.OR.TTYPE.EQ.'tek4025') GTXTTY=4025
      IF (TTYPE.EQ.'TEK4027'.OR.TTYPE.EQ.'tek4027') GTXTTY=4027
      IF (TTYPE.EQ.'TEK4105'.OR.TTYPE.EQ.'tek4105') GTXTTY=4105
      IF (TTYPE.EQ.'TEK4112'.OR.TTYPE.EQ.'tek4112') GTXTTY=4112
      IF (TTYPE.EQ.'VT240'.OR.TTYPE.EQ.'vt240')   GTXTTY=240
      IF (TTYPE.EQ.'VT330'.OR.TTYPE.EQ.'vt330')   GTXTTY=240
      IF (TTYPE.EQ.'VT340'.OR.TTYPE.EQ.'vt340')   GTXTTY=240
      if (ttype.eq.'X'    .or.ttype.eq.'x'    )   GTXTTY=1011
      RETURN
C
c 990  CONTINUE
c      WRITE(6,*) ' *** Error opening terminal data file'
c      GTXTTY=0
c      RETURN
      END
C
C ---------------------------------------------------
C
      SUBROUTINE PLXFOP
C     ***Routine to open plot files for MINDIS
      character*20 buffer
      CHARACTER*1 GTXMCN
      CHARACTER*1 PLTID
      INTEGER*4 getpid
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      common /pltich/ iplcha
c      buffer='~/minplots/'//GTXMCN()//PLTFNM(1:10)//'.PLT'
c      write(6,*) 'MINDIS filename:',buffer
        PLTID=CHAR(ICHAR('A')+iplcha)
        iplcha=iplcha+1
        ID=getpid()
        write(buffer,'(A,A,I5.5)') 'min',PLTID,ID
      OPEN(UNIT=48,FILE=buffer,STATUS='UNKNOWN',FORM='UNFORMATTED')
      RETURN
      END
C
C ---------------------------------------------------
C
      SUBROUTINE PLXFCL(HARDC,IDEV)
C     ***Routine to close plot files and produce hardcopy if necessary
      LOGICAL HARDC
      CHARACTER*1 MCID,GTXMCN,PLTID
      CHARACTER*120 BUFFER
      CHARACTER*20 COMFILE
      INTEGER*4 STATUS,LIBXSPAWN,getpid
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
C
C     IDEV Types:
C          1 = Calcomp Pen Plotter
C          2 = Printronix (User Area)
C          3 = Tektronix 4691 A3 Paper
C          4 = Printronix (Central)
C          5 = Tektronix 4691 A4 Viewgraph
C          6 = Calcomp Electrostatic Plotter
C          7 = Printronix (Default)
C          8 = Laser Printer (Default)
C
cX	PRNT=FXLOGICAL("USRXLASER")
cX	POS=FXLOCATE("X",PRNT)
cX	LEN=FXLENGTH(PRNT)
cX	PRNT1=PRNT
cX	IF POS .NE. LEN THEN PRNT1=FXEXTRACT(POS+1,LEN-POS,PRNT)
cX	WRITE SYSXOUTPUT "Plot will be queued to default printer (''PRNT1')"
c
        PLTID=CHAR(ICHAR('A')+PLTNO)
        ID=getpid()
      MCID=GTXMCN()
      IDEV1=IDEV
      IF (HARDC) THEN
           IF (IDEV1.EQ.1) THEN
C
C               ***GENERATE CALCOMP COPY VIA BATCH JOB
                WRITE(48) 99, 0.0, 0.0
                INQUIRE(48,NAME=BUFFER)
                CLOSE(48,STATUS='keep')
C *** MAKE COMMAND FILE
                write(COMFILE,'(A3,A1,I5.5)') 'com',PLTID,ID
                OPEN(49,FILE=COMFILE,STATUS='NEW')
                WRITE(49,'(A)') '#'
                WRITE(49,'(A,1X,A)') 'setenv FOR048',BUFFER
                WRITE(49,'(A)') 'mintopost'
                WRITE(49,'(A,A)') 'rm ',comfile
                CLOSE(49,STATUS='KEEP')
c                call chmod(COMFILE,'00755')
                call system('chmod 755 '//COMFILE)
                call system('csh '//COMFILE//' &')
            ELSE IF (IDEV1.EQ.2) THEN
C
C               ***GENERATE TEKTRONIX 4696 COPY VIA UNIRAS JOB
                WRITE(48) 99, 0.0, 0.0
                INQUIRE(48,NAME=BUFFER)
                CLOSE(48,STATUS='keep')
C *** MAKE COMMAND FILE
                write(COMFILE,'(A3,A1,I5.5)') 'com',PLTID,ID
                OPEN(49,FILE=COMFILE,STATUS='NEW')
                WRITE(49,'(A)') '#'
                WRITE(49,'(A,1X,A)') 'setenv FOR048',BUFFER
                WRITE(49,'(A)') 'mintotek'
                WRITE(49,'(A,A)') 'rm ',comfile
                CLOSE(49,STATUS='KEEP')
c                call chmod(COMFILE,'00755')
                call system('chmod 755 '//COMFILE)
c                call system('csh '//COMFILE//' >& uniplotlog &')
                call system('csh '//COMFILE//' &')

c                OPEN(UNIT=99,STATUS='NEW',FILE='SYSXMINDISCALC:'
c     X			//MCID//PLTFNM(1:10)//'.COM')
c                WRITE(99,305) MCID//PLTFNM(1:10)
c 305                FORMAT('X ASSIGN SYSXMINDISPLOTS:',1A11,
c     #                     '.PLT FOR048')
c		WRITE(99,306) PLTRID
c 306		    FORMAT('X ASSIGN ',1A6,' RUNID')
c                CLOSE(UNIT=99)
C           ELSEIF (IDEV1.EQ.2.OR.IDEV1.EQ.4) THEN
C
C               ***GENERATE PRINTRONIX COPY VIA BATCH JOB
c                WRITE(48) 99, 0.0, 0.0
c                CLOSE(48)
c                OPEN(UNIT=99,STATUS='NEW',FILE='SYSXMINDISPLOTS:'
c     X			//MCID//PLTFNM(1:10)//'.COM')
c		IF (IDEV1.EQ.2) THEN
c			WRITE(99,310) MCID//PLTFNM(1:10),'PRUSAREA'
c 310			    FORMAT('X MINDPRX "',1A11,'" "',1A8,'"')
c		ELSE
c			WRITE(99,310) MCID//PLTFNM(1:10),'PRCENTRL'
c			ENDIF
c                CLOSE(UNIT=99)
c		STATUS=LIBXSPAWN('SUBMIT/NOTIFY/QUE=SYSXGRAPHICS/NOLOG '//
c     X		    'SYSXMINDISPLOTS:'//MCID//PLTFNM(1:10)//'.COM')
c		IF (.NOT.STATUS) THEN
c			WRITE(5,*) 'PLXFCL: spawn error'
c			CALL LIBXSTOP(%VAL(STATUS))
c			ENDIF
           ELSEIF (IDEV1.EQ.3) THEN
C
C               ***GENERATE Tektronix 4691 COPY VIA BATCH JOB
C			3 = A3 Paper	5 = A4 Viewgraph
                WRITE(48) 99, 0.0, 0.0
                CLOSE(48)
c                OPEN(UNIT=99,STATUS='NEW',FILE='SYSXMINDISPLOTS:'
c     X			//MCID//PLTFNM(1:10)//'.COM')
c		IF (IDEV1.EQ.3) THEN
c			WRITE(99,320) MCID//PLTFNM(1:10),'A3'
c 320			    FORMAT('X MINDT46 "',1A11,'" "',1A2,'"')
c		ELSE
c			WRITE(99,320) MCID//PLTFNM(1:10),'VG'
c			ENDIF
c                CLOSE(UNIT=99)
c		STATUS=LIBXSPAWN('SUBMIT/NOTIFY/QUE=SYSXGRAPHICS/NOLOG '//
c     X		    'SYSXMINDISPLOTS:'//MCID//PLTFNM(1:10)//'.COM')
c		IF (.NOT.STATUS) THEN
c			WRITE(5,*) 'PLXFCL: spawn error'
c			CALL LIBXSTOP(%VAL(STATUS))
c			ENDIF
           ELSEIF (IDEV1.EQ.5) THEN
c >>> make postscript file
                WRITE(48) 99, 0.0, 0.0
                INQUIRE(48,NAME=BUFFER)
                CLOSE(48,STATUS='keep')
C *** MAKE COMMAND FILE
                write(COMFILE,'(A3,A1,I5.5)') 'com',PLTID,ID
                OPEN(49,FILE=COMFILE,STATUS='NEW')
                WRITE(49,'(A)') '#'
                WRITE(49,'(A,1X,A)') 'setenv FOR048',BUFFER
                WRITE(49,'(A)') 'mintops'
                WRITE(49,'(A,A)') 'rm ',comfile
                CLOSE(49,STATUS='KEEP')
c                call chmod(COMFILE,'00755')
                call system('chmod 755 '//COMFILE)
                call system('csh '//COMFILE//' &')
           ELSEIF (IDEV1.EQ.6) THEN
C
C               SAVE IN MINDIS FILE FOR LATER PROCESSING
                WRITE(48) 99, 0.0, 0.0
                INQUIRE(48,NAME=BUFFER)
                CLOSE(48,STATUS='KEEP')
                WRITE(6,*) 'Saved in: ',buffer(1:60)
c                OPEN(UNIT=99,STATUS='NEW',FILE='SYSXMINDISPLOTS:'
c     X			//MCID//PLTFNM(1:10)//'.COM')
c		WRITE(99,330) MCID//PLTFNM(1:10)
c 330		    FORMAT('X MINDCEL "',1A11,'"')
c                CLOSE(UNIT=99)
c		STATUS=LIBXSPAWN('SUBMIT/NOTIFY/QUE=SYSXGRAPHICS/NOLOG '//
c     X		    'SYSXMINDISPLOTS:'//MCID//PLTFNM(1:10)//'.COM')
c		IF (.NOT.STATUS) THEN
c			WRITE(5,*) 'PLXFCL: spawn error'
c			CALL LIBXSTOP(%VAL(STATUS))
c			ENDIF
           ELSE
                WRITE(6,*) ' Invalid Hard copy device (IDEV=',IDEV
                ENDIF
      ELSE
           CLOSE(48,STATUS='DELETE')
           ENDIF
      RETURN
      END
C
C ---------------------------------------------------
C
      SUBROUTINE TXINIT
C     ***Routine to perform terminal initialisations
      RETURN
      END
C
C ---------------------------------------------------
C
      SUBROUTINE TXCTLT(LSET)
C     ***Routine to set/unset operating system action for CTL-T
C      LOGICAL LSET
C      DATA ICTLT,ICTLY / '00100000'X, '02000000'X /
C      INTEGER*4 LIBXDISABLECTRL, LIBXENABLECTRL
C
c      IF (LSET) THEN
c           ISTAT=LIBXENABLECTRL(ICTLT,)
c      ELSE
c           ISTAT=LIBXDISABLECTRL(ICTLT,)
c           ENDIF
C      WRITE(6,100) ISTAT,ISTAT
C 100      FORMAT(' TXCTLT: Status = ',O20,I20)
      RETURN
      END
C
C ---------------------------------------------------
C
      CHARACTER*5 FUNCTION GTXMOD()
C     ***Routine to return default output device
C
c      IMPLICIT INTEGER*2(W), INTEGER*4(L)
c      CHARACTER*63 NAMDES
c      INTEGER*2 NAMLEN
c      INTEGER*4 SYSXTRNLOG
      LOGICAL LNAMOK
c      CHARACTER*8 LCLVALUES
c      INCLUDE '(XJPIDEF)'
c      COMMON /DTXTID/ WLEN1, WCODE1, LADDR1, LLENADDR1
c      DATA WLEN1 /8/
c      DATA WCODE1 /JPIXTERMINAL/
C
c      ICODE=SYSXTRNLOG('USRXMINDDOD',NAMLEN,NAMDES,,,%VAL(3))
c      LNAMOK=(ICODE.AND.(NAMDES(1:NAMLEN).NE.'USRXMINDDOD'))
c     WRITE(5,*) 'ICODE = ',ICODE,' NAMDES = ',NAMDES
c      IF (LNAMOK) THEN
c           GTXMOD=NAMDES(1:NAMLEN)
c      ELSE
           GTXMOD='PC'
c      ENDIF
      RETURN
      END
C
C ---------------------------------------------------
C
	CHARACTER*1 FUNCTION GTXMCN()
C	*** Routine to return mcname
	CHARACTER*20 SYMVAL
c	INTEGER*4 LIBXGETSYMBOL,STATUS,VALLEN,TABLE
C
c	STATUS=LIBXGETSYMBOL('MCNAME',SYMVAL,VALLEN,TABLE)
c	IF (.NOT.STATUS) THEN
c		WRITE(5,*) 'GTXMCN: Error getting MCNAME'
c		CALL LIBXSTOP(%VAL(STATUS))
c		ENDIF
c	IF (VALLEN.NE.1) WRITE(5,*)
c     #    'GTXMCN: MCNAME is not single char: '//SYMVAL(1:VALLEN)
c	GTXMCN=SYMVAL(1:1)
        GTXMCN='M'
	RETURN
	END
C
C ---------------------------------------------------
C
      SUBROUTINE  HCXCHK
C     ***Routine to handle end of plot and queueing hardcopy
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      CHARACTER*10 REPLY
      character*40 outdev(10)
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' HCXCHK called')
C
C                12345678901234567890
      OUTDEV(1)='Laser printer'
      OUTDEV(2)='Tektronix 4696'
      OUTDEV(3)='Tektronix paper'
      OUTDEV(4)='Central printronix'
      OUTDEV(5)='PostScript file'
      OUTDEV(6)='MINDIS file'
C
      IF (.NOT.LBATCH) THEN
           WRITE(6,*) 'End of Plot'
           WRITE(6,*) ' Do you want a hardcopy?'
           write(6,*) ' L=Laser printer, T=Tektronix 4696,',
     #                ' F=MINDIS file, P=PostScript,',
     #		      ' N=None)'
           READ(5,221,end=222) REPLY
 221           FORMAT(1A10)
           go to 223
 222       REPLY='F    '
 223       CONTINUE
      ELSE
           REPLY=DEFHCD
           IF (REPLY(1:2).EQ.'XX') REPLY='F    '
           WRITE(6,115) REPLY
 115           FORMAT(' MINDIS: Batch plot output on ',1A10)
           ENDIF
      IPLOTR=0
      IF (REPLY(1:1).EQ.'L') IPLOTR=1
      IF (REPLY(1:1).EQ.'l') IPLOTR=1
      IF (REPLY(1:1).EQ.'T') IPLOTR=2
      IF (REPLY(1:1).EQ.'t') IPLOTR=2
      IF (REPLY(1:1).EQ.'P') IPLOTR=5
      IF (REPLY(1:1).EQ.'p') IPLOTR=5
c      IF (REPLY(1:2).EQ.'PC') IPLOTR=4
c      IF (REPLY(1:2).EQ.'pc') IPLOTR=4
c      IF (REPLY(1:1).EQ.'V') IPLOTR=5
c      IF (REPLY(1:1).EQ.'v') IPLOTR=5
      IF (REPLY(1:1).EQ.'F') IPLOTR=6
      IF (REPLY(1:1).EQ.'f') IPLOTR=6
      IF (IPLOTR.EQ.0) THEN
C           WRITE(6,*)
C     X		'No hardcopy selected or Undefined hardcopy device: ',reply
      else
           write(6,120) outdev(iplotr)
 120           format(' Hardcopy will be output on ',1a40)
           endif
      CALL PLXFCL((IPLOTR.NE.0),IPLOTR)
      RETURN
      END
C
C ---------------------------------------------------
C


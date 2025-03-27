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

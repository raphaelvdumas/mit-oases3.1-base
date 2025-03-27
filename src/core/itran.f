      Integer function Itran()
c
c     returns an integer between 0 and 9999 dependent on date and time,
c     to be used as a 'random' random number generator seed
c
      character*30 dum
      call system('date > dum.dum ')
      open(10,file='dum.dum',form='formatted',status='old')
      read(10,'(a)') dum
      close(10,status='delete')
c      write(6,*) dum
      itran=0
      do i=1,len(dum)
       itran=itran+i*(30-i)*ichar(dum(i:i))
      end do
c      write(6,*) itran
      itran=mod(itran,10000)
      return
      end


      dimension a(64,152)
      do 10 i=1,4
       read(11,*) (a(i,j),j=1,152)
 10    continue
      do 20 j=1,152
       call rfft(a(1,j),4,1)
       call vclr(a(9,j),1,56)
       call rfft(a(1,j),64,-1)
c       call rfftsc(a(1,j),64,1,1)
c       call vsmul(a,1,16.0,a,1,64*152)
 20    continue
      do 30 j=1,152
       write(12,*) (a(i,j)/8.0,i=1,64)
 30    continue
      end

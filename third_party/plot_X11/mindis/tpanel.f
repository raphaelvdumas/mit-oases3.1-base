      character*5 csi
      character*2 st
      write(6,*) ' color code?'
      read(5,*) icol
      csi=' '//char(27)//'[5|'
      st=char(27)//'\'
      write(6,'(a)') csi//'CLS'//st
      write(6,'(a,I2,A)') csi//'FILLPAT ',ICOL,st
      write(6,'(a,2I5,A,I1,A)') 
     &    csi//'BEGINPAN ',100,100,',',0,st
      write(6,'(a,2I5,A)') csi//'MOV ',100,900,st
      write(6,'(a,2I5,A)') csi//'MOV ',900,900,st
      write(6,'(a,2I5,A)') csi//'MOV ',900,100,st
      WRITE(6,'(A)') csi//'ENDPA'//st
      STOP
      END

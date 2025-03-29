      Parameter (neqmax=1000)
      parameter (nrhsmax=100 )
      complex*16 dmat(neqmax*neqmax),drhs(neqmax*nrhsmax)
      integer ipiv(neqmax)
      common /dblbuf/ dmat,drhs,ipiv

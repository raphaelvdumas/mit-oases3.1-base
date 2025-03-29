c >>> field buffers for Cramer-Rao bounds
      complex afield(nrmax),xfield(nrmax),zfield(nrmax)
c >>> Components of Fisher info matrix
      complex cli(2,nsmax,nsmax),clij(2,2,nsmax,nsmax)
      real ad2(nsmax,nsmax),agam(nsmax,nsmax),afish(2,2,nsmax,nsmax)
      complex cli_f(2),clij_f(2,2)
      real ad2_f,agam_f,afish_f(2,2)
      common /INVVAR/ afield,xfield,zfield,cli,clij,ad2,agam,afish,
     &                cli_f,clij_f,ad2_f,agam_f,afish_f


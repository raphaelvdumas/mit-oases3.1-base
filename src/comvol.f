c sizes moved to compar.f 051806 HS
c      integer nnkr,nnkz
c      Parameter (nnkz=512,nnkr=8192)
c      Parameter (nnkz=256,nnkr=8192)
c      Parameter (nnkz=128,nnkr=16384)

      complex deta_kx_z(2*nnkr*nnkz),peta_kx_z(2*nnkr*nnkz)
      real    eta_x_z(2*nnkr*nnkz),eta_x_dz(2*nnkr*nnkz)
      real    eta_dx_z(2*nnkr*nnkz)
      real    temp_kx_kz(nnkz*nnkr*4)

      common /COMVOL/ deta_kx_z, peta_kx_z, 
     &                eta_x_z, eta_dx_z, eta_x_dz

      equivalence (deta_kx_z(1), temp_kx_kz(1))
c
c  variables and arrays for small argument full Bessel integrations
c
      common /BF_J01/ bf_J_0(np), bf_J_1(np), 
     &                odrk_bf, rkrm_bf, nrkm_bf



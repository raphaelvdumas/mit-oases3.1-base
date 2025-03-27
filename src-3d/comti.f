      COMPLEX ELAG(12,NLA)
      COMMON /ELAPAR/ ELAG
      COMPLEX A,B1,B0,C2,C1,C0,C44,C66,C33,C13,C11
      REAL  RH,DUM
      COMPLEX S3UP(2),S3DN(2),S3SHDN,S3SHUP
      COMPLEX U1DN(2),U3DN(2),U1UP(2),U3UP(2),U2DN,U2UP
      COMPLEX SIG3DN(2),SIG13DN(2),SIG3UP(2),SIG13UP(2)
      COMPLEX SIG23DN,SIG23UP,sig1up(2),sig1dn(2),
     &        sig2up(2),sig2dn(2),sigbup(2),sigbdn(2)
      COMMON/TI/A,B1,B0,C2,C1,C0,C44,C66,C33,C13,C11,RH,DUM
c >>> The following is put into the array ANSTD, and the order
c     should not be changed.
      COMMON/OUT/S3UP,S3DN,U1DN,U3DN,U1UP,U3UP,
     1           SIG3DN,SIG13DN,SIG3UP,SIG13UP,
     2           S3SHUP,S3SHDN,U2UP,U2DN,SIG23UP,SIG23DN,
     3           sig1up,sig1dn,sig2up,sig2dn,sigbup,sigbdn
      
      COMMON /SLNORM/ SLNORM(NLA)


C................................................................       FSU   10
      SUBROUTINE FSUB(X, Z, F)                                          FSU   20
      implicit real*8 (a-h, o-z)
      DIMENSION Z(4), F(2)
      COMMON EPS, DMU, EPS4MU, GAMMA, XT
      F(1) = Z(1)/X/X - Z(2)/X + (Z(1)-Z(3)*(1.-Z(1)/X)-GAMMA*X*(1.-X*X/
     * 2.))/EPS4MU
      F(2) = Z(3)/X/X - Z(4)/X + Z(1)*(1.-Z(1)/2./X)/DMU
      RETURN
      END

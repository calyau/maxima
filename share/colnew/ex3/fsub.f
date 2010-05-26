C................................................................       FSU   10
      SUBROUTINE FSUB(X, Z, F)                                          FSU   20
      implicit real*8 (a-h, o-z)
      REAL*8 Z(1), F(1)
      COMMON EN, S, EL, CONS
      F(1) = -EL*(CONS*Z(3)*Z(2)+(EN-1.)*Z(4)*Z(1)) + EL**2*S*(Z(1)-1.)
      F(2) = -EL*(CONS*Z(3)*Z(5)+EN*Z(4)**2) + EL**2*S*Z(4) +
     * EL**3*(1.-Z(1)**2)
      RETURN
      END

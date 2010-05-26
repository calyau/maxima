C................................................................       SOL   10
      SUBROUTINE SOLUTN(X, Z, DMVAL)                                    SOL   20
      implicit real*8 (a-h, o-z)
      COMMON EN, S, EL, CONS
      REAL*8 Z(5), DMVAL(2)
      EX = EXP(-EL*X)
      Z(1) = 1. - EX
      Z(2) = EL*EX
      Z(3) = -EL**2*X**2*EX
      Z(4) = (EL**3*X**2-2.*EL**2*X)*EX
      Z(5) = (-EL**4*X**2+4.*EL**3*X-2.*EL**2)*EX
      DMVAL(1) = -EL*Z(2)
      DMVAL(2) = (EL**5*X*X-6.*EL**4*X+6.*EL**3)*EX
      RETURN
      END

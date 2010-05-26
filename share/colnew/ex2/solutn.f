C................................................................       SOL   10
      SUBROUTINE SOLUTN(X, Z, DMVAL)                                    SOL   20
      implicit real*8 (a-h, o-z)
      COMMON EPS, DMU, EPS4MU, GAMMA, XT
      DIMENSION Z(4), DMVAL(2)
      CONS = GAMMA*X*(1.-.5*X*X)
      DCONS = GAMMA*(1.-1.5*X*X)
      D2CONS = -3.*GAMMA*X
      IF (X.GT.XT) GO TO 10
      Z(1) = 2.*X
      Z(2) = 2.
      Z(3) = -2.*X + CONS
      Z(4) = -2. + DCONS
      DMVAL(2) = D2CONS
      GO TO 20
   10 Z(1) = 0.
      Z(2) = 0.
      Z(3) = -CONS
      Z(4) = -DCONS
      DMVAL(2) = -D2CONS
   20 DMVAL(1) = 0.
      RETURN
      END

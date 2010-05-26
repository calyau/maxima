C................................................................       DFS   10
      SUBROUTINE DFSUB(X, Z, DF)                                        DFS   20
      implicit real*8 (a-h, o-z)
      DIMENSION Z(4), DF(2,4)
      COMMON EPS, DMU, EPS4MU, GAMMA, XT
      DF(1,1) = 1./X/X + (1.+Z(3)/X)/EPS4MU
      DF(1,2) = -1./X
      DF(1,3) = -(1.-Z(1)/X)/EPS4MU
      DF(1,4) = 0.
      DF(2,1) = (1.-Z(1)/X)/DMU
      DF(2,2) = 0.
      DF(2,3) = 1./X/X
      DF(2,4) = -1./X
      RETURN
      END

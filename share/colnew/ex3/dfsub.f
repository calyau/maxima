C................................................................       DFS   10
      SUBROUTINE DFSUB(X, Z, DF)                                        DFS   20
      implicit real*8 (a-h, o-z)
      REAL*8 Z(1), DF(2,1)
      COMMON EN, S, EL, CONS
      DF(1,1) = -EL*(EN-1.)*Z(4) + EL**2*S
      DF(1,2) = -EL*CONS*Z(3)
      DF(1,3) = -EL*CONS*Z(2)
      DF(1,4) = -EL*(EN-1.)*Z(1)
      DF(1,5) = 0.
      DF(2,1) = -EL**3*2.*Z(1)
      DF(2,2) = 0.
      DF(2,3) = -EL*CONS*Z(5)
      DF(2,4) = -EL*EN*2.*Z(4) + EL**2*S
      DF(2,5) = -EL*CONS*Z(3)
      RETURN
      END

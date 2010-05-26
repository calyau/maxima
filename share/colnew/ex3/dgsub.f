C................................................................       DGS   10
      SUBROUTINE DGSUB(I, Z, DG)                                        DGS   20
      implicit real*8 (a-h, o-z)
      REAL*8 Z(1), DG(1)
      DO 10 J=1,5
        DG(J) = 0.
   10 CONTINUE
      GO TO (20, 30, 40, 20, 40), I
   20 DG(1) = 1.
      RETURN
   30 DG(3) = 1.
      RETURN
   40 DG(4) = 1.
      RETURN
      END

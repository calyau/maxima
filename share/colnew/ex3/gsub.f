C................................................................       GSU   10
      SUBROUTINE GSUB(I, Z, G)                                          GSU   20
      implicit real*8 (a-h, o-z)
      REAL*8 Z(1), G
      GO TO (10, 20, 30, 40, 30), I
   10 G = Z(1)
      RETURN
   20 G = Z(3)
      RETURN
   30 G = Z(4)
      RETURN
   40 G = Z(1) - 1.
      RETURN
      END

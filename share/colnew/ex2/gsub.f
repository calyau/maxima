C................................................................       GSU   10
      SUBROUTINE GSUB(I, Z, G)                                          GSU   20
      implicit real*8 (a-h, o-z)
      DIMENSION Z(4)
      GO TO (10, 20, 10, 30), I
   10 G = Z(1)
      RETURN
   20 G = Z(3)
      RETURN
   30 G = Z(4) - .3*Z(3) + .7
      RETURN
      END

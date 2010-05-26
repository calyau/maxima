C................................................................       GSU   10
      SUBROUTINE GSUB(I, Z, G)                                          GSU   20
      REAL*8 Z(4), G
      GO TO (10, 20, 10, 20), I
   10 G = Z(1) - 0.
      RETURN
   20 G = Z(3) - 0.
      RETURN
      END

      SUBROUTINE DMZSOL (KD, MSTAR, N, V, Z, DMZ)
C
C**********************************************************************
C
C   purpose
C          compute dmz in a blockwise manner
C          dmz(i) = dmz(i)  +  v(i) * z(i), i = 1,...,n
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION V(KD,1), DMZ(KD,1), Z(1)
C
      JZ = 1
      DO 30 I = 1, N
         DO 20 J = 1, MSTAR
            FACT = Z(JZ)
            DO 10 L = 1, KD
               DMZ(L,I) = DMZ(L,I)  +  FACT * V(L,JZ)
   10       CONTINUE
            JZ = JZ + 1
   20    CONTINUE
   30 CONTINUE
      RETURN
      END

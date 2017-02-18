*DECK CNTNZU
      SUBROUTINE CNTNZU (N, IA, JA, NZSUT)
      INTEGER N, IA, JA, NZSUT
      DIMENSION IA(*), JA(*)
C-----------------------------------------------------------------------
C This routine counts the number of nonzero elements in the strict
C upper triangle of the matrix M + M(transpose), where the sparsity
C structure of M is given by pointer arrays IA and JA.
C This is needed to compute the storage requirements for the
C sparse matrix reordering operation in ODRV.
C-----------------------------------------------------------------------
      INTEGER II, JJ, J, JMIN, JMAX, K, KMIN, KMAX, NUM
C
      NUM = 0
      DO 50 II = 1,N
        JMIN = IA(II)
        JMAX = IA(II+1) - 1
        IF (JMIN .GT. JMAX) GO TO 50
        DO 40 J = JMIN,JMAX
          IF (JA(J) - II) 10, 40, 30
 10       JJ =JA(J)
          KMIN = IA(JJ)
          KMAX = IA(JJ+1) - 1
          IF (KMIN .GT. KMAX) GO TO 30
          DO 20 K = KMIN,KMAX
            IF (JA(K) .EQ. II) GO TO 40
 20         CONTINUE
 30       NUM = NUM + 1
 40       CONTINUE
 50     CONTINUE
      NZSUT = NUM
      RETURN
C----------------------- End of Subroutine CNTNZU ----------------------
      END

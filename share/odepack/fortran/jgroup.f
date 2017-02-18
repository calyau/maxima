*DECK JGROUP
      SUBROUTINE JGROUP (N,IA,JA,MAXG,NGRP,IGP,JGP,INCL,JDONE,IER)
      INTEGER N, IA, JA, MAXG, NGRP, IGP, JGP, INCL, JDONE, IER
      DIMENSION IA(*), JA(*), IGP(*), JGP(*), INCL(*), JDONE(*)
C-----------------------------------------------------------------------
C This subroutine constructs groupings of the column indices of
C the Jacobian matrix, used in the numerical evaluation of the
C Jacobian by finite differences.
C
C Input:
C N      = the order of the matrix.
C IA,JA  = sparse structure descriptors of the matrix by rows.
C MAXG   = length of available storage in the IGP array.
C
C Output:
C NGRP   = number of groups.
C JGP    = array of length N containing the column indices by groups.
C IGP    = pointer array of length NGRP + 1 to the locations in JGP
C          of the beginning of each group.
C IER    = error indicator.  IER = 0 if no error occurred, or 1 if
C          MAXG was insufficient.
C
C INCL and JDONE are working arrays of length N.
C-----------------------------------------------------------------------
      INTEGER I, J, K, KMIN, KMAX, NCOL, NG
C
      IER = 0
      DO 10 J = 1,N
 10     JDONE(J) = 0
      NCOL = 1
      DO 60 NG = 1,MAXG
        IGP(NG) = NCOL
        DO 20 I = 1,N
 20       INCL(I) = 0
        DO 50 J = 1,N
C Reject column J if it is already in a group.--------------------------
          IF (JDONE(J) .EQ. 1) GO TO 50
          KMIN = IA(J)
          KMAX = IA(J+1) - 1
          DO 30 K = KMIN,KMAX
C Reject column J if it overlaps any column already in this group.------
            I = JA(K)
            IF (INCL(I) .EQ. 1) GO TO 50
 30         CONTINUE
C Accept column J into group NG.----------------------------------------
          JGP(NCOL) = J
          NCOL = NCOL + 1
          JDONE(J) = 1
          DO 40 K = KMIN,KMAX
            I = JA(K)
 40         INCL(I) = 1
 50       CONTINUE
C Stop if this group is empty (grouping is complete).-------------------
        IF (NCOL .EQ. IGP(NG)) GO TO 70
 60     CONTINUE
C Error return if not all columns were chosen (MAXG too small).---------
      IF (NCOL .LE. N) GO TO 80
      NG = MAXG
 70   NGRP = NG - 1
      RETURN
 80   IER = 1
      RETURN
C----------------------- End of Subroutine JGROUP ----------------------
      END

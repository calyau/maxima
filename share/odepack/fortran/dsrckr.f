*DECK DSRCKR
      SUBROUTINE DSRCKR (RSAV, ISAV, JOB)
C-----------------------------------------------------------------------
C This routine saves or restores (depending on JOB) the contents of
C the Common blocks DLS001, DLS002, DLSR01, DLPK01, which
C are used internally by the DLSODKR solver.
C
C RSAV = real array of length 228 or more.
C ISAV = integer array of length 63 or more.
C JOB  = flag indicating to save or restore the Common blocks:
C        JOB  = 1 if Common is to be saved (written to RSAV/ISAV)
C        JOB  = 2 if Common is to be restored (read from RSAV/ISAV)
C        A call with JOB = 2 presumes a prior call with JOB = 1.
C-----------------------------------------------------------------------
      INTEGER ISAV, JOB
      INTEGER ILS, ILS2, ILSR, ILSP
      INTEGER I, IOFF, LENILP, LENRLP, LENILS, LENRLS, LENILR, LENRLR
      DOUBLE PRECISION RSAV,   RLS, RLS2, RLSR, RLSP
      DIMENSION RSAV(*), ISAV(*)
      SAVE LENRLS, LENILS, LENRLP, LENILP, LENRLR, LENILR
      COMMON /DLS001/ RLS(218), ILS(37)
      COMMON /DLS002/ RLS2, ILS2(4)
      COMMON /DLSR01/ RLSR(5), ILSR(9)
      COMMON /DLPK01/ RLSP(4), ILSP(13)
      DATA LENRLS/218/, LENILS/37/, LENRLP/4/, LENILP/13/
      DATA LENRLR/5/, LENILR/9/
C
      IF (JOB .EQ. 2) GO TO 100
      CALL DCOPY (LENRLS, RLS, 1, RSAV, 1)
      RSAV(LENRLS+1) = RLS2
      CALL DCOPY (LENRLR, RLSR, 1, RSAV(LENRLS+2), 1)
      CALL DCOPY (LENRLP, RLSP, 1, RSAV(LENRLS+LENRLR+2), 1)
      DO 20 I = 1,LENILS
 20     ISAV(I) = ILS(I)
      ISAV(LENILS+1) = ILS2(1)
      ISAV(LENILS+2) = ILS2(2)
      ISAV(LENILS+3) = ILS2(3)
      ISAV(LENILS+4) = ILS2(4)
      IOFF = LENILS + 2
      DO 30 I = 1,LENILR
 30     ISAV(IOFF+I) = ILSR(I)
      IOFF = IOFF + LENILR
      DO 40 I = 1,LENILP
 40     ISAV(IOFF+I) = ILSP(I)
      RETURN
C
 100  CONTINUE
      CALL DCOPY (LENRLS, RSAV, 1, RLS, 1)
      RLS2 = RSAV(LENRLS+1)
      CALL DCOPY (LENRLR, RSAV(LENRLS+2), 1, RLSR, 1)
      CALL DCOPY (LENRLP, RSAV(LENRLS+LENRLR+2), 1, RLSP, 1)
      DO 120 I = 1,LENILS
 120    ILS(I) = ISAV(I)
      ILS2(1) = ISAV(LENILS+1)
      ILS2(2) = ISAV(LENILS+2)
      ILS2(3) = ISAV(LENILS+3)
      ILS2(4) = ISAV(LENILS+4)
      IOFF = LENILS + 2
      DO 130 I = 1,LENILR
 130    ILSR(I) = ISAV(IOFF+I)
      IOFF = IOFF + LENILR
      DO 140 I = 1,LENILP
 140    ILSP(I) = ISAV(IOFF+I)
      RETURN
C----------------------- End of Subroutine DSRCKR ----------------------
      END

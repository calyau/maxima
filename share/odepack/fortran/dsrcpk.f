*DECK DSRCPK
      SUBROUTINE DSRCPK (RSAV, ISAV, JOB)
C-----------------------------------------------------------------------
C This routine saves or restores (depending on JOB) the contents of
C the Common blocks DLS001, DLPK01, which are used
C internally by the DLSODPK solver.
C
C RSAV = real array of length 222 or more.
C ISAV = integer array of length 50 or more.
C JOB  = flag indicating to save or restore the Common blocks:
C        JOB  = 1 if Common is to be saved (written to RSAV/ISAV)
C        JOB  = 2 if Common is to be restored (read from RSAV/ISAV)
C        A call with JOB = 2 presumes a prior call with JOB = 1.
C-----------------------------------------------------------------------
      INTEGER ISAV, JOB
      INTEGER ILS, ILSP
      INTEGER I, LENILP, LENRLP, LENILS, LENRLS
      DOUBLE PRECISION RSAV,   RLS, RLSP
      DIMENSION RSAV(*), ISAV(*)
      SAVE LENRLS, LENILS, LENRLP, LENILP
      COMMON /DLS001/ RLS(218), ILS(37)
      COMMON /DLPK01/ RLSP(4), ILSP(13)
      DATA LENRLS/218/, LENILS/37/, LENRLP/4/, LENILP/13/
C
      IF (JOB .EQ. 2) GO TO 100
      CALL DCOPY (LENRLS, RLS, 1, RSAV, 1)
      CALL DCOPY (LENRLP, RLSP, 1, RSAV(LENRLS+1), 1)
      DO 20 I = 1,LENILS
 20     ISAV(I) = ILS(I)
      DO 40 I = 1,LENILP
 40     ISAV(LENILS+I) = ILSP(I)
      RETURN
C
 100  CONTINUE
      CALL DCOPY (LENRLS, RSAV, 1, RLS, 1)
      CALL DCOPY (LENRLP, RSAV(LENRLS+1), 1, RLSP, 1)
      DO 120 I = 1,LENILS
 120     ILS(I) = ISAV(I)
      DO 140 I = 1,LENILP
 140     ILSP(I) = ISAV(LENILS+I)
      RETURN
C----------------------- End of Subroutine DSRCPK ----------------------
      END

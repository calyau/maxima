*DECK DSRCAR
      SUBROUTINE DSRCAR (RSAV, ISAV, JOB)
C-----------------------------------------------------------------------
C This routine saves or restores (depending on JOB) the contents of
C the Common blocks DLS001, DLSA01, DLSR01, which are used
C internally by one or more ODEPACK solvers.
C
C RSAV = real array of length 245 or more.
C ISAV = integer array of length 55 or more.
C JOB  = flag indicating to save or restore the Common blocks:
C        JOB  = 1 if Common is to be saved (written to RSAV/ISAV)
C        JOB  = 2 if Common is to be restored (read from RSAV/ISAV)
C        A call with JOB = 2 presumes a prior call with JOB = 1.
C-----------------------------------------------------------------------
      INTEGER ISAV, JOB
      INTEGER ILS, ILSA, ILSR
      INTEGER I, IOFF, LENRLS, LENILS, LENRLA, LENILA, LENRLR, LENILR
      DOUBLE PRECISION RSAV
      DOUBLE PRECISION RLS, RLSA, RLSR
      DIMENSION RSAV(*), ISAV(*)
      SAVE LENRLS, LENILS, LENRLA, LENILA, LENRLR, LENILR
      COMMON /DLS001/ RLS(218), ILS(37)
      COMMON /DLSA01/ RLSA(22), ILSA(9)
      COMMON /DLSR01/ RLSR(5), ILSR(9)
      DATA LENRLS/218/, LENILS/37/, LENRLA/22/, LENILA/9/
      DATA LENRLR/5/, LENILR/9/
C
      IF (JOB .EQ. 2) GO TO 100
      DO 10 I = 1,LENRLS
 10     RSAV(I) = RLS(I)
       DO 15 I = 1,LENRLA
 15     RSAV(LENRLS+I) = RLSA(I)
      IOFF = LENRLS + LENRLA
      DO 20 I = 1,LENRLR
 20     RSAV(IOFF+I) = RLSR(I)
C
      DO 30 I = 1,LENILS
 30     ISAV(I) = ILS(I)
      DO 35 I = 1,LENILA
 35     ISAV(LENILS+I) = ILSA(I)
      IOFF = LENILS + LENILA
      DO 40 I = 1,LENILR
 40     ISAV(IOFF+I) = ILSR(I)
C
      RETURN
C
 100  CONTINUE
      DO 110 I = 1,LENRLS
 110     RLS(I) = RSAV(I)
      DO 115 I = 1,LENRLA
 115     RLSA(I) = RSAV(LENRLS+I)
      IOFF = LENRLS + LENRLA
      DO 120 I = 1,LENRLR
 120     RLSR(I) = RSAV(IOFF+I)
C
      DO 130 I = 1,LENILS
 130     ILS(I) = ISAV(I)
      DO 135 I = 1,LENILA
 135     ILSA(I) = ISAV(LENILS+I)
      IOFF = LENILS + LENILA
      DO 140 I = 1,LENILR
 140     ILSR(I) = ISAV(IOFF+I)
C
      RETURN
C----------------------- End of Subroutine DSRCAR ----------------------
      END

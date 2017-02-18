*DECK DSRCMA
      SUBROUTINE DSRCMA (RSAV, ISAV, JOB)
C-----------------------------------------------------------------------
C This routine saves or restores (depending on JOB) the contents of
C the Common blocks DLS001, DLSA01, which are used
C internally by one or more ODEPACK solvers.
C
C RSAV = real array of length 240 or more.
C ISAV = integer array of length 46 or more.
C JOB  = flag indicating to save or restore the Common blocks:
C        JOB  = 1 if Common is to be saved (written to RSAV/ISAV)
C        JOB  = 2 if Common is to be restored (read from RSAV/ISAV)
C        A call with JOB = 2 presumes a prior call with JOB = 1.
C-----------------------------------------------------------------------
      INTEGER ISAV, JOB
      INTEGER ILS, ILSA
      INTEGER I, LENRLS, LENILS, LENRLA, LENILA
      DOUBLE PRECISION RSAV
      DOUBLE PRECISION RLS, RLSA
      DIMENSION RSAV(*), ISAV(*)
      SAVE LENRLS, LENILS, LENRLA, LENILA
      COMMON /DLS001/ RLS(218), ILS(37)
      COMMON /DLSA01/ RLSA(22), ILSA(9)
      DATA LENRLS/218/, LENILS/37/, LENRLA/22/, LENILA/9/
C
      IF (JOB .EQ. 2) GO TO 100
      DO 10 I = 1,LENRLS
 10     RSAV(I) = RLS(I)
      DO 15 I = 1,LENRLA
 15     RSAV(LENRLS+I) = RLSA(I)
C
      DO 20 I = 1,LENILS
 20     ISAV(I) = ILS(I)
      DO 25 I = 1,LENILA
 25     ISAV(LENILS+I) = ILSA(I)
C
      RETURN
C
 100  CONTINUE
      DO 110 I = 1,LENRLS
 110     RLS(I) = RSAV(I)
      DO 115 I = 1,LENRLA
 115     RLSA(I) = RSAV(LENRLS+I)
C
      DO 120 I = 1,LENILS
 120     ILS(I) = ISAV(I)
      DO 125 I = 1,LENILA
 125     ILSA(I) = ISAV(LENILS+I)
C
      RETURN
C----------------------- End of Subroutine DSRCMA ----------------------
      END

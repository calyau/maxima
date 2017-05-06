*DECK DSRCOM
      SUBROUTINE DSRCOM (RSAV, ISAV, JOB)
C***BEGIN PROLOGUE  DSRCOM
C***SUBSIDIARY
C***PURPOSE  Save/restore ODEPACK COMMON blocks.
C***TYPE      DOUBLE PRECISION (SSRCOM-S, DSRCOM-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  This routine saves or restores (depending on JOB) the contents of
C  the COMMON block DLS001, which is used internally
C  by one or more ODEPACK solvers.
C
C  RSAV = real array of length 218 or more.
C  ISAV = integer array of length 37 or more.
C  JOB  = flag indicating to save or restore the COMMON blocks:
C         JOB  = 1 if COMMON is to be saved (written to RSAV/ISAV)
C         JOB  = 2 if COMMON is to be restored (read from RSAV/ISAV)
C         A call with JOB = 2 presumes a prior call with JOB = 1.
C
C***SEE ALSO  DLSODE
C***ROUTINES CALLED  (NONE)
C***COMMON BLOCKS    DLS001
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   921116  Deleted treatment of block /EH0001/.  (ACH)
C   930801  Reduced Common block length by 2.  (ACH)
C   930809  Renamed to allow single/double precision versions. (ACH)
C   010418  Reduced Common block length by 209+12. (ACH)
C   031105  Restored 'own' variables to Common block /DLS001/, to
C           enable interrupt/restart feature. (ACH)
C   031112  Added SAVE statement for data-loaded constants.
C***END PROLOGUE  DSRCOM
C**End
      INTEGER ISAV, JOB
      INTEGER ILS
      INTEGER I, LENILS, LENRLS
      DOUBLE PRECISION RSAV,   RLS
      DIMENSION RSAV(*), ISAV(*)
      SAVE LENRLS, LENILS
      COMMON /DLS001/ RLS(218), ILS(37)
      DATA LENRLS/218/, LENILS/37/
C
C***FIRST EXECUTABLE STATEMENT  DSRCOM
      IF (JOB .EQ. 2) GO TO 100
C
      DO 10 I = 1,LENRLS
 10     RSAV(I) = RLS(I)
      DO 20 I = 1,LENILS
 20     ISAV(I) = ILS(I)
      RETURN
C
 100  CONTINUE
      DO 110 I = 1,LENRLS
 110     RLS(I) = RSAV(I)
      DO 120 I = 1,LENILS
 120     ILS(I) = ISAV(I)
      RETURN
C----------------------- END OF SUBROUTINE DSRCOM ----------------------
      END

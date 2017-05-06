*DECK DRCHEK
      SUBROUTINE DRCHEK (JOB, G, NEQ, Y, YH,NYH, G0, G1, GX, JROOT, IRT)
      EXTERNAL G
      INTEGER JOB, NEQ, NYH, JROOT, IRT
      DOUBLE PRECISION Y, YH, G0, G1, GX
      DIMENSION NEQ(*), Y(*), YH(NYH,*), G0(*), G1(*), GX(*), JROOT(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER IOWND3, IOWNR3, IRFND, ITASKC, NGC, NGE
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION ROWNR3, T0, TLAST, TOUTC
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSR01/ ROWNR3(2), T0, TLAST, TOUTC,
     1   IOWND3(3), IOWNR3(2), IRFND, ITASKC, NGC, NGE
      INTEGER I, IFLAG, JFLAG
      DOUBLE PRECISION HMING, T1, TEMP1, TEMP2, X
      LOGICAL ZROOT
C-----------------------------------------------------------------------
C This routine checks for the presence of a root in the
C vicinity of the current T, in a manner depending on the
C input flag JOB.  It calls Subroutine DROOTS to locate the root
C as precisely as possible.
C
C In addition to variables described previously, DRCHEK
C uses the following for communication:
C JOB    = integer flag indicating type of call:
C          JOB = 1 means the problem is being initialized, and DRCHEK
C                  is to look for a root at or very near the initial T.
C          JOB = 2 means a continuation call to the solver was just
C                  made, and DRCHEK is to check for a root in the
C                  relevant part of the step last taken.
C          JOB = 3 means a successful step was just taken, and DRCHEK
C                  is to look for a root in the interval of the step.
C G0     = array of length NG, containing the value of g at T = T0.
C          G0 is input for JOB .ge. 2, and output in all cases.
C G1,GX  = arrays of length NG for work space.
C IRT    = completion flag:
C          IRT = 0  means no root was found.
C          IRT = -1 means JOB = 1 and a root was found too near to T.
C          IRT = 1  means a legitimate root was found (JOB = 2 or 3).
C                   On return, T0 is the root location, and Y is the
C                   corresponding solution vector.
C T0     = value of T at one endpoint of interval of interest.  Only
C          roots beyond T0 in the direction of integration are sought.
C          T0 is input if JOB .ge. 2, and output in all cases.
C          T0 is updated by DRCHEK, whether a root is found or not.
C TLAST  = last value of T returned by the solver (input only).
C TOUTC  = copy of TOUT (input only).
C IRFND  = input flag showing whether the last step taken had a root.
C          IRFND = 1 if it did, = 0 if not.
C ITASKC = copy of ITASK (input only).
C NGC    = copy of NG (input only).
C-----------------------------------------------------------------------
      IRT = 0
      DO 10 I = 1,NGC
 10     JROOT(I) = 0
      HMING = (ABS(TN) + ABS(H))*UROUND*100.0D0
C
      GO TO (100, 200, 300), JOB
C
C Evaluate g at initial T, and check for zero values. ------------------
 100  CONTINUE
      T0 = TN
      CALL G (NEQ, T0, Y, NGC, G0)
      NGE = 1
      ZROOT = .FALSE.
      DO 110 I = 1,NGC
 110    IF (ABS(G0(I)) .LE. 0.0D0) ZROOT = .TRUE.
      IF (.NOT. ZROOT) GO TO 190
C g has a zero at T.  Look at g at T + (small increment). --------------
      TEMP1 = SIGN(HMING,H)
      T0 = T0 + TEMP1
      TEMP2 = TEMP1/H
      DO 120 I = 1,N
 120    Y(I) = Y(I) + TEMP2*YH(I,2)
      CALL G (NEQ, T0, Y, NGC, G0)
      NGE = NGE + 1
      ZROOT = .FALSE.
      DO 130 I = 1,NGC
 130    IF (ABS(G0(I)) .LE. 0.0D0) ZROOT = .TRUE.
      IF (.NOT. ZROOT) GO TO 190
C g has a zero at T and also close to T.  Take error return. -----------
      IRT = -1
      RETURN
C
 190  CONTINUE
      RETURN
C
C
 200  CONTINUE
      IF (IRFND .EQ. 0) GO TO 260
C If a root was found on the previous step, evaluate G0 = g(T0). -------
      CALL DINTDY (T0, 0, YH, NYH, Y, IFLAG)
      CALL G (NEQ, T0, Y, NGC, G0)
      NGE = NGE + 1
      ZROOT = .FALSE.
      DO 210 I = 1,NGC
 210    IF (ABS(G0(I)) .LE. 0.0D0) ZROOT = .TRUE.
      IF (.NOT. ZROOT) GO TO 260
C g has a zero at T0.  Look at g at T + (small increment). -------------
      TEMP1 = SIGN(HMING,H)
      T0 = T0 + TEMP1
      IF ((T0 - TN)*H .LT. 0.0D0) GO TO 230
      TEMP2 = TEMP1/H
      DO 220 I = 1,N
 220    Y(I) = Y(I) + TEMP2*YH(I,2)
      GO TO 240
 230  CALL DINTDY (T0, 0, YH, NYH, Y, IFLAG)
 240  CALL G (NEQ, T0, Y, NGC, G0)
      NGE = NGE + 1
      ZROOT = .FALSE.
      DO 250 I = 1,NGC
        IF (ABS(G0(I)) .GT. 0.0D0) GO TO 250
        JROOT(I) = 1
        ZROOT = .TRUE.
 250    CONTINUE
      IF (.NOT. ZROOT) GO TO 260
C g has a zero at T0 and also close to T0.  Return root. ---------------
      IRT = 1
      RETURN
C G0 has no zero components.  Proceed to check relevant interval. ------
 260  IF (TN .EQ. TLAST) GO TO 390
C
 300  CONTINUE
C Set T1 to TN or TOUTC, whichever comes first, and get g at T1. -------
      IF (ITASKC.EQ.2 .OR. ITASKC.EQ.3 .OR. ITASKC.EQ.5) GO TO 310
      IF ((TOUTC - TN)*H .GE. 0.0D0) GO TO 310
      T1 = TOUTC
      IF ((T1 - T0)*H .LE. 0.0D0) GO TO 390
      CALL DINTDY (T1, 0, YH, NYH, Y, IFLAG)
      GO TO 330
 310  T1 = TN
      DO 320 I = 1,N
 320    Y(I) = YH(I,1)
 330  CALL G (NEQ, T1, Y, NGC, G1)
      NGE = NGE + 1
C Call DROOTS to search for root in interval from T0 to T1. ------------
      JFLAG = 0
 350  CONTINUE
      CALL DROOTS (NGC, HMING, JFLAG, T0, T1, G0, G1, GX, X, JROOT)
      IF (JFLAG .GT. 1) GO TO 360
      CALL DINTDY (X, 0, YH, NYH, Y, IFLAG)
      CALL G (NEQ, X, Y, NGC, GX)
      NGE = NGE + 1
      GO TO 350
 360  T0 = X
      CALL DCOPY (NGC, GX, 1, G0, 1)
      IF (JFLAG .EQ. 4) GO TO 390
C Found a root.  Interpolate to X and return. --------------------------
      CALL DINTDY (X, 0, YH, NYH, Y, IFLAG)
      IRT = 1
      RETURN
C
 390  CONTINUE
      RETURN
C----------------------- End of Subroutine DRCHEK ----------------------
      END

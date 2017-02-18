*DECK DROOTS
      SUBROUTINE DROOTS (NG, HMIN, JFLAG, X0, X1, G0, G1, GX, X, JROOT)
      INTEGER NG, JFLAG, JROOT
      DOUBLE PRECISION HMIN, X0, X1, G0, G1, GX, X
      DIMENSION G0(NG), G1(NG), GX(NG), JROOT(NG)
      INTEGER IOWND3, IMAX, LAST, IDUM3
      DOUBLE PRECISION ALPHA, X2, RDUM3
      COMMON /DLSR01/ ALPHA, X2, RDUM3(3),
     1   IOWND3(3), IMAX, LAST, IDUM3(4)
C-----------------------------------------------------------------------
C This subroutine finds the leftmost root of a set of arbitrary
C functions gi(x) (i = 1,...,NG) in an interval (X0,X1).  Only roots
C of odd multiplicity (i.e. changes of sign of the gi) are found.
C Here the sign of X1 - X0 is arbitrary, but is constant for a given
C problem, and -leftmost- means nearest to X0.
C The values of the vector-valued function g(x) = (gi, i=1...NG)
C are communicated through the call sequence of DROOTS.
C The method used is the Illinois algorithm.
C
C Reference:
C Kathie L. Hiebert and Lawrence F. Shampine, Implicitly Defined
C Output Points for Solutions of ODEs, Sandia Report SAND80-0180,
C February 1980.
C
C Description of parameters.
C
C NG     = number of functions gi, or the number of components of
C          the vector valued function g(x).  Input only.
C
C HMIN   = resolution parameter in X.  Input only.  When a root is
C          found, it is located only to within an error of HMIN in X.
C          Typically, HMIN should be set to something on the order of
C               100 * UROUND * MAX(ABS(X0),ABS(X1)),
C          where UROUND is the unit roundoff of the machine.
C
C JFLAG  = integer flag for input and output communication.
C
C          On input, set JFLAG = 0 on the first call for the problem,
C          and leave it unchanged until the problem is completed.
C          (The problem is completed when JFLAG .ge. 2 on return.)
C
C          On output, JFLAG has the following values and meanings:
C          JFLAG = 1 means DROOTS needs a value of g(x).  Set GX = g(X)
C                    and call DROOTS again.
C          JFLAG = 2 means a root has been found.  The root is
C                    at X, and GX contains g(X).  (Actually, X is the
C                    rightmost approximation to the root on an interval
C                    (X0,X1) of size HMIN or less.)
C          JFLAG = 3 means X = X1 is a root, with one or more of the gi
C                    being zero at X1 and no sign changes in (X0,X1).
C                    GX contains g(X) on output.
C          JFLAG = 4 means no roots (of odd multiplicity) were
C                    found in (X0,X1) (no sign changes).
C
C X0,X1  = endpoints of the interval where roots are sought.
C          X1 and X0 are input when JFLAG = 0 (first call), and
C          must be left unchanged between calls until the problem is
C          completed.  X0 and X1 must be distinct, but X1 - X0 may be
C          of either sign.  However, the notion of -left- and -right-
C          will be used to mean nearer to X0 or X1, respectively.
C          When JFLAG .ge. 2 on return, X0 and X1 are output, and
C          are the endpoints of the relevant interval.
C
C G0,G1  = arrays of length NG containing the vectors g(X0) and g(X1),
C          respectively.  When JFLAG = 0, G0 and G1 are input and
C          none of the G0(i) should be zero.
C          When JFLAG .ge. 2 on return, G0 and G1 are output.
C
C GX     = array of length NG containing g(X).  GX is input
C          when JFLAG = 1, and output when JFLAG .ge. 2.
C
C X      = independent variable value.  Output only.
C          When JFLAG = 1 on output, X is the point at which g(x)
C          is to be evaluated and loaded into GX.
C          When JFLAG = 2 or 3, X is the root.
C          When JFLAG = 4, X is the right endpoint of the interval, X1.
C
C JROOT  = integer array of length NG.  Output only.
C          When JFLAG = 2 or 3, JROOT indicates which components
C          of g(x) have a root at X.  JROOT(i) is 1 if the i-th
C          component has a root, and JROOT(i) = 0 otherwise.
C-----------------------------------------------------------------------
      INTEGER I, IMXOLD, NXLAST
      DOUBLE PRECISION T2, TMAX, ZERO
      LOGICAL ZROOT, SGNCHG, XROOT
      SAVE ZERO
      DATA ZERO/0.0D0/
C
      IF (JFLAG .EQ. 1) GO TO 200
C JFLAG .ne. 1.  Check for change in sign of g or zero at X1. ----------
      IMAX = 0
      TMAX = ZERO
      ZROOT = .FALSE.
      DO 120 I = 1,NG
        IF (ABS(G1(I)) .GT. ZERO) GO TO 110
        ZROOT = .TRUE.
        GO TO 120
C At this point, G0(i) has been checked and cannot be zero. ------------
 110    IF (SIGN(1.0D0,G0(I)) .EQ. SIGN(1.0D0,G1(I))) GO TO 120
          T2 = ABS(G1(I)/(G1(I)-G0(I)))
          IF (T2 .LE. TMAX) GO TO 120
            TMAX = T2
            IMAX = I
 120    CONTINUE
      IF (IMAX .GT. 0) GO TO 130
      SGNCHG = .FALSE.
      GO TO 140
 130  SGNCHG = .TRUE.
 140  IF (.NOT. SGNCHG) GO TO 400
C There is a sign change.  Find the first root in the interval. --------
      XROOT = .FALSE.
      NXLAST = 0
      LAST = 1
C
C Repeat until the first root in the interval is found.  Loop point. ---
 150  CONTINUE
      IF (XROOT) GO TO 300
      IF (NXLAST .EQ. LAST) GO TO 160
      ALPHA = 1.0D0
      GO TO 180
 160  IF (LAST .EQ. 0) GO TO 170
      ALPHA = 0.5D0*ALPHA
      GO TO 180
 170  ALPHA = 2.0D0*ALPHA
 180  X2 = X1 - (X1-X0)*G1(IMAX)/(G1(IMAX) - ALPHA*G0(IMAX))
      IF ((ABS(X2-X0) .LT. HMIN) .AND.
     1   (ABS(X1-X0) .GT. 10.0D0*HMIN)) X2 = X0 + 0.1D0*(X1-X0)
      JFLAG = 1
      X = X2
C Return to the calling routine to get a value of GX = g(X). -----------
      RETURN
C Check to see in which interval g changes sign. -----------------------
 200  IMXOLD = IMAX
      IMAX = 0
      TMAX = ZERO
      ZROOT = .FALSE.
      DO 220 I = 1,NG
        IF (ABS(GX(I)) .GT. ZERO) GO TO 210
        ZROOT = .TRUE.
        GO TO 220
C Neither G0(i) nor GX(i) can be zero at this point. -------------------
 210    IF (SIGN(1.0D0,G0(I)) .EQ. SIGN(1.0D0,GX(I))) GO TO 220
          T2 = ABS(GX(I)/(GX(I) - G0(I)))
          IF (T2 .LE. TMAX) GO TO 220
            TMAX = T2
            IMAX = I
 220    CONTINUE
      IF (IMAX .GT. 0) GO TO 230
      SGNCHG = .FALSE.
      IMAX = IMXOLD
      GO TO 240
 230  SGNCHG = .TRUE.
 240  NXLAST = LAST
      IF (.NOT. SGNCHG) GO TO 250
C Sign change between X0 and X2, so replace X1 with X2. ----------------
      X1 = X2
      CALL DCOPY (NG, GX, 1, G1, 1)
      LAST = 1
      XROOT = .FALSE.
      GO TO 270
 250  IF (.NOT. ZROOT) GO TO 260
C Zero value at X2 and no sign change in (X0,X2), so X2 is a root. -----
      X1 = X2
      CALL DCOPY (NG, GX, 1, G1, 1)
      XROOT = .TRUE.
      GO TO 270
C No sign change between X0 and X2.  Replace X0 with X2. ---------------
 260  CONTINUE
      CALL DCOPY (NG, GX, 1, G0, 1)
      X0 = X2
      LAST = 0
      XROOT = .FALSE.
 270  IF (ABS(X1-X0) .LE. HMIN) XROOT = .TRUE.
      GO TO 150
C
C Return with X1 as the root.  Set JROOT.  Set X = X1 and GX = G1. -----
 300  JFLAG = 2
      X = X1
      CALL DCOPY (NG, G1, 1, GX, 1)
      DO 320 I = 1,NG
        JROOT(I) = 0
        IF (ABS(G1(I)) .GT. ZERO) GO TO 310
          JROOT(I) = 1
          GO TO 320
 310    IF (SIGN(1.0D0,G0(I)) .NE. SIGN(1.0D0,G1(I))) JROOT(I) = 1
 320    CONTINUE
      RETURN
C
C No sign change in the interval.  Check for zero at right endpoint. ---
 400  IF (.NOT. ZROOT) GO TO 420
C
C Zero value at X1 and no sign change in (X0,X1).  Return JFLAG = 3. ---
      X = X1
      CALL DCOPY (NG, G1, 1, GX, 1)
      DO 410 I = 1,NG
        JROOT(I) = 0
        IF (ABS(G1(I)) .LE. ZERO) JROOT (I) = 1
 410  CONTINUE
      JFLAG = 3
      RETURN
C
C No sign changes in this interval.  Set X = X1, return JFLAG = 4. -----
 420  CALL DCOPY (NG, G1, 1, GX, 1)
      X = X1
      JFLAG = 4
      RETURN
C----------------------- End of Subroutine DROOTS ----------------------
      END

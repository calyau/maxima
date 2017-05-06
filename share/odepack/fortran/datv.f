*DECK DATV
      SUBROUTINE DATV (NEQ, Y, SAVF, V, WGHT, FTEM, F, PSOL, Z, VTEM,
     1                WP, IWP, HL0, JPRE, IER, NPSL)
      EXTERNAL F, PSOL
      INTEGER NEQ, IWP, JPRE, IER, NPSL
      DOUBLE PRECISION Y, SAVF, V, WGHT, FTEM, Z, VTEM, WP, HL0
      DIMENSION NEQ(*), Y(*), SAVF(*), V(*), WGHT(*), FTEM(*), Z(*),
     1   VTEM(*), WP(*), IWP(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
C-----------------------------------------------------------------------
C This routine computes the product
C
C   (D-inverse)*(P1-inverse)*(I - hl0*df/dy)*(P2-inverse)*(D*v),
C
C where D is a diagonal scaling matrix, and P1 and P2 are the
C left and right preconditioning matrices, respectively.
C v is assumed to have WRMS norm equal to 1.
C The product is stored in z.  This is computed by a
C difference quotient, a call to F, and two calls to PSOL.
C-----------------------------------------------------------------------
C
C      On entry
C
C          NEQ = problem size, passed to F and PSOL (NEQ(1) = N).
C
C            Y = array containing current dependent variable vector.
C
C         SAVF = array containing current value of f(t,y).
C
C            V = real array of length N (can be the same array as Z).
C
C         WGHT = array of length N containing scale factors.
C                1/WGHT(i) are the diagonal elements of the matrix D.
C
C         FTEM = work array of length N.
C
C         VTEM = work array of length N used to store the
C                unscaled version of V.
C
C           WP = real work array used by preconditioner PSOL.
C
C          IWP = integer work array used by preconditioner PSOL.
C
C          HL0 = current value of (step size h) * (coefficient l0).
C
C         JPRE = preconditioner type flag.
C
C
C      On return
C
C            Z = array of length N containing desired scaled
C                matrix-vector product.
C
C          IER = error flag from PSOL.
C
C         NPSL = the number of calls to PSOL.
C
C In addition, this routine uses the Common variables TN, N, NFE.
C-----------------------------------------------------------------------
      INTEGER I
      DOUBLE PRECISION FAC, RNORM, DNRM2, TEMPN
C
C Set VTEM = D * V.
      DO 10 I = 1,N
 10     VTEM(I) = V(I)/WGHT(I)
      IER = 0
      IF (JPRE .GE. 2) GO TO 30
C
C JPRE = 0 or 1.  Save Y in Z and increment Y by VTEM.
      CALL DCOPY (N, Y, 1, Z, 1)
      DO 20 I = 1,N
 20     Y(I) = Z(I) + VTEM(I)
      FAC = HL0
      GO TO 60
C
C JPRE = 2 or 3.  Apply inverse of right preconditioner to VTEM.
 30   CONTINUE
      CALL PSOL (NEQ, TN, Y, SAVF, FTEM, HL0, WP, IWP, VTEM, 2, IER)
      NPSL = NPSL + 1
      IF (IER .NE. 0) RETURN
C Calculate L-2 norm of (D-inverse) * VTEM.
      DO 40 I = 1,N
 40     Z(I) = VTEM(I)*WGHT(I)
      TEMPN = DNRM2 (N, Z, 1)
      RNORM = 1.0D0/TEMPN
C Save Y in Z and increment Y by VTEM/norm.
      CALL DCOPY (N, Y, 1, Z, 1)
      DO 50 I = 1,N
 50     Y(I) = Z(I) + VTEM(I)*RNORM
      FAC = HL0*TEMPN
C
C For all JPRE, call F with incremented Y argument, and restore Y.
 60   CONTINUE
      CALL F (NEQ, TN, Y, FTEM)
      NFE = NFE + 1
      CALL DCOPY (N, Z, 1, Y, 1)
C Set Z = (identity - hl0*Jacobian) * VTEM, using difference quotient.
      DO 70 I = 1,N
 70     Z(I) = FTEM(I) - SAVF(I)
      DO 80 I = 1,N
 80     Z(I) = VTEM(I) - FAC*Z(I)
C Apply inverse of left preconditioner to Z, if nontrivial.
      IF (JPRE .EQ. 0 .OR. JPRE .EQ. 2) GO TO 85
      CALL PSOL (NEQ, TN, Y, SAVF, FTEM, HL0, WP, IWP, Z, 1, IER)
      NPSL = NPSL + 1
      IF (IER .NE. 0) RETURN
 85   CONTINUE
C Apply D-inverse to Z and return.
      DO 90 I = 1,N
 90     Z(I) = Z(I)*WGHT(I)
      RETURN
C----------------------- End of Subroutine DATV ------------------------
      END

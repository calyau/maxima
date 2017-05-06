*DECK DATP
      SUBROUTINE DATP (NEQ, Y, SAVF, P, WGHT, HL0, WK, F, W)
      EXTERNAL F
      INTEGER NEQ
      DOUBLE PRECISION Y, SAVF, P, WGHT, HL0, WK, W
      DIMENSION NEQ(*), Y(*), SAVF(*), P(*), WGHT(*), WK(*), W(*)
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
C              w = (I - hl0*df/dy)*p
C
C This is computed by a call to F and a difference quotient.
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
C            P = real array of length N.
C
C         WGHT = array of length N containing scale factors.
C                1/WGHT(i) are the diagonal elements of the matrix D.
C
C           WK = work array of length N.
C
C      On return
C
C
C            W = array of length N containing desired
C                matrix-vector product.
C
C In addition, this routine uses the Common variables TN, N, NFE.
C-----------------------------------------------------------------------
      INTEGER I
      DOUBLE PRECISION FAC, PNRM, RPNRM, DVNORM
C
      PNRM = DVNORM (N, P, WGHT)
      RPNRM = 1.0D0/PNRM
      CALL DCOPY (N, Y, 1, W, 1)
      DO 20 I = 1,N
 20     Y(I) = W(I) + P(I)*RPNRM
      CALL F (NEQ, TN, Y, WK)
      NFE = NFE + 1
      CALL DCOPY (N, W, 1, Y, 1)
      FAC = HL0*PNRM
      DO 40 I = 1,N
 40     W(I) = P(I) - FAC*(WK(I) - SAVF(I))
      RETURN
C----------------------- End of Subroutine DATP ------------------------
      END

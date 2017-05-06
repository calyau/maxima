*DECK DSOLPK
      SUBROUTINE DSOLPK (NEQ, Y, SAVF, X, EWT, WM, IWM, F, PSOL)
      EXTERNAL F, PSOL
      INTEGER NEQ, IWM
      DOUBLE PRECISION Y, SAVF, X, EWT, WM
      DIMENSION NEQ(*), Y(*), SAVF(*), X(*), EWT(*), WM(*), IWM(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER JPRE, JACFLG, LOCWP, LOCIWP, LSAVX, KMP, MAXL, MNEWT,
     1   NNI, NLI, NPS, NCFN, NCFL
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION DELT, EPCON, SQRTN, RSQRTN
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLPK01/ DELT, EPCON, SQRTN, RSQRTN,
     1   JPRE, JACFLG, LOCWP, LOCIWP, LSAVX, KMP, MAXL, MNEWT,
     2   NNI, NLI, NPS, NCFN, NCFL
C-----------------------------------------------------------------------
C This routine interfaces to one of DSPIOM, DSPIGMR, DPCG, DPCGS, or
C DUSOL, for the solution of the linear system arising from a Newton
C iteration.  It is called if MITER .ne. 0.
C In addition to variables described elsewhere,
C communication with DSOLPK uses the following variables:
C WM    = real work space containing data for the algorithm
C         (Krylov basis vectors, Hessenberg matrix, etc.)
C IWM   = integer work space containing data for the algorithm
C X     = the right-hand side vector on input, and the solution vector
C         on output, of length N.
C IERSL = output flag (in Common):
C         IERSL =  0 means no trouble occurred.
C         IERSL =  1 means the iterative method failed to converge.
C                    If the preconditioner is out of date, the step
C                    is repeated with a new preconditioner.
C                    Otherwise, the stepsize is reduced (forcing a
C                    new evaluation of the preconditioner) and the
C                    step is repeated.
C         IERSL = -1 means there was a nonrecoverable error in the
C                    iterative solver, and an error exit occurs.
C This routine also uses the Common variables TN, EL0, H, N, MITER,
C DELT, EPCON, SQRTN, RSQRTN, MAXL, KMP, MNEWT, NNI, NLI, NPS, NCFL,
C LOCWP, LOCIWP.
C-----------------------------------------------------------------------
      INTEGER IFLAG, LB, LDL, LHES, LIOM, LGMR, LPCG, LP, LQ, LR,
     1   LV, LW, LWK, LZ, MAXLP1, NPSL
      DOUBLE PRECISION DELTA, HL0
C
      IERSL = 0
      HL0 = H*EL0
      DELTA = DELT*EPCON
      GO TO (100, 200, 300, 400, 900, 900, 900, 900, 900), MITER
C-----------------------------------------------------------------------
C Use the SPIOM algorithm to solve the linear system P*x = -f.
C-----------------------------------------------------------------------
 100  CONTINUE
      LV = 1
      LB = LV + N*MAXL
      LHES = LB + N
      LWK = LHES + MAXL*MAXL
      CALL DCOPY (N, X, 1, WM(LB), 1)
      CALL DSCAL (N, RSQRTN, EWT, 1)
      CALL DSPIOM (NEQ, TN, Y, SAVF, WM(LB), EWT, N, MAXL, KMP, DELTA,
     1   HL0, JPRE, MNEWT, F, PSOL, NPSL, X, WM(LV), WM(LHES), IWM,
     2   LIOM, WM(LOCWP), IWM(LOCIWP), WM(LWK), IFLAG)
      NNI = NNI + 1
      NLI = NLI + LIOM
      NPS = NPS + NPSL
      CALL DSCAL (N, SQRTN, EWT, 1)
      IF (IFLAG .NE. 0) NCFL = NCFL + 1
      IF (IFLAG .GE. 2) IERSL = 1
      IF (IFLAG .LT. 0) IERSL = -1
      RETURN
C-----------------------------------------------------------------------
C Use the SPIGMR algorithm to solve the linear system P*x = -f.
C-----------------------------------------------------------------------
 200  CONTINUE
      MAXLP1 = MAXL + 1
      LV = 1
      LB = LV + N*MAXL
      LHES = LB + N + 1
      LQ = LHES + MAXL*MAXLP1
      LWK = LQ + 2*MAXL
      LDL = LWK + MIN(1,MAXL-KMP)*N
      CALL DCOPY (N, X, 1, WM(LB), 1)
      CALL DSCAL (N, RSQRTN, EWT, 1)
      CALL DSPIGMR (NEQ, TN, Y, SAVF, WM(LB), EWT, N, MAXL, MAXLP1, KMP,
     1   DELTA, HL0, JPRE, MNEWT, F, PSOL, NPSL, X, WM(LV), WM(LHES),
     2   WM(LQ), LGMR, WM(LOCWP), IWM(LOCIWP), WM(LWK), WM(LDL), IFLAG)
      NNI = NNI + 1
      NLI = NLI + LGMR
      NPS = NPS + NPSL
      CALL DSCAL (N, SQRTN, EWT, 1)
      IF (IFLAG .NE. 0) NCFL = NCFL + 1
      IF (IFLAG .GE. 2) IERSL = 1
      IF (IFLAG .LT. 0) IERSL = -1
      RETURN
C-----------------------------------------------------------------------
C Use DPCG to solve the linear system P*x = -f
C-----------------------------------------------------------------------
 300  CONTINUE
      LR = 1
      LP = LR + N
      LW = LP + N
      LZ = LW + N
      LWK = LZ + N
      CALL DCOPY (N, X, 1, WM(LR), 1)
      CALL DPCG (NEQ, TN, Y, SAVF, WM(LR), EWT, N, MAXL, DELTA, HL0,
     1          JPRE, MNEWT, F, PSOL, NPSL, X, WM(LP), WM(LW), WM(LZ),
     2          LPCG, WM(LOCWP), IWM(LOCIWP), WM(LWK), IFLAG)
      NNI = NNI + 1
      NLI = NLI + LPCG
      NPS = NPS + NPSL
      IF (IFLAG .NE. 0) NCFL = NCFL + 1
      IF (IFLAG .GE. 2) IERSL = 1
      IF (IFLAG .LT. 0) IERSL = -1
      RETURN
C-----------------------------------------------------------------------
C Use DPCGS to solve the linear system P*x = -f
C-----------------------------------------------------------------------
 400  CONTINUE
      LR = 1
      LP = LR + N
      LW = LP + N
      LZ = LW + N
      LWK = LZ + N
      CALL DCOPY (N, X, 1, WM(LR), 1)
      CALL DPCGS (NEQ, TN, Y, SAVF, WM(LR), EWT, N, MAXL, DELTA, HL0,
     1           JPRE, MNEWT, F, PSOL, NPSL, X, WM(LP), WM(LW), WM(LZ),
     2           LPCG, WM(LOCWP), IWM(LOCIWP), WM(LWK), IFLAG)
      NNI = NNI + 1
      NLI = NLI + LPCG
      NPS = NPS + NPSL
      IF (IFLAG .NE. 0) NCFL = NCFL + 1
      IF (IFLAG .GE. 2) IERSL = 1
      IF (IFLAG .LT. 0) IERSL = -1
      RETURN
C-----------------------------------------------------------------------
C Use DUSOL, which interfaces to PSOL, to solve the linear system
C (no Krylov iteration).
C-----------------------------------------------------------------------
 900  CONTINUE
      LB = 1
      LWK = LB + N
      CALL DCOPY (N, X, 1, WM(LB), 1)
      CALL DUSOL (NEQ, TN, Y, SAVF, WM(LB), EWT, N, DELTA, HL0, MNEWT,
     1   PSOL, NPSL, X, WM(LOCWP), IWM(LOCIWP), WM(LWK), IFLAG)
      NNI = NNI + 1
      NPS = NPS + NPSL
      IF (IFLAG .NE. 0) NCFL = NCFL + 1
      IF (IFLAG .EQ. 3) IERSL = 1
      IF (IFLAG .LT. 0) IERSL = -1
      RETURN
C----------------------- End of Subroutine DSOLPK ----------------------
      END

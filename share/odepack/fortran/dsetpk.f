*DECK DSETPK
      SUBROUTINE DSETPK (NEQ, Y, YSV, EWT, FTEM, SAVF, JOK, WM, IWM,
     1                  F, JAC)
      EXTERNAL F, JAC
      INTEGER NEQ, JOK, IWM
      DOUBLE PRECISION Y, YSV, EWT, FTEM, SAVF, WM
      DIMENSION NEQ(*), Y(*), YSV(*), EWT(*), FTEM(*), SAVF(*),
     1   WM(*), IWM(*)
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
C DSETPK is called by DSTOKA to interface with the user-supplied
C routine JAC, to compute and process relevant parts of
C the matrix P = I - H*EL(1)*J , where J is the Jacobian df/dy,
C as need for preconditioning matrix operations later.
C
C In addition to variables described previously, communication
C with DSETPK uses the following:
C Y     = array containing predicted values on entry.
C YSV   = array containing predicted y, to be saved (YH1 in DSTOKA).
C FTEM  = work array of length N (ACOR in DSTOKA).
C SAVF  = array containing f evaluated at predicted y.
C JOK   = input flag showing whether it was judged that Jacobian matrix
C         data need not be recomputed (JOK = 1) or needs to be
C         (JOK = -1).
C WM    = real work space for matrices.
C         Space for preconditioning data starts at WM(LOCWP).
C IWM   = integer work space.
C         Space for preconditioning data starts at IWM(LOCIWP).
C IERPJ = output error flag,  = 0 if no trouble, .gt. 0 if
C         JAC returned an error flag.
C JCUR  = output flag to indicate whether the matrix data involved
C         is now current (JCUR = 1) or not (JCUR = 0).
C This routine also uses Common variables EL0, H, TN, IERPJ, JCUR, NJE.
C-----------------------------------------------------------------------
      INTEGER IER
      DOUBLE PRECISION HL0
C
      IERPJ = 0
      JCUR = 0
      IF (JOK .EQ. -1) JCUR = 1
      HL0 = EL0*H
      CALL JAC (F, NEQ, TN, Y, YSV, EWT, SAVF, FTEM, HL0, JOK,
     1   WM(LOCWP), IWM(LOCIWP), IER)
      NJE = NJE + 1
      IF (IER .EQ. 0) RETURN
      IERPJ = 1
      RETURN
C----------------------- End of Subroutine DSETPK ----------------------
      END

*DECK DSOLSS
      SUBROUTINE DSOLSS (WK, IWK, X, TEM)
      INTEGER IWK
      DOUBLE PRECISION WK, X, TEM
      DIMENSION WK(*), IWK(*), X(*), TEM(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     1   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     2   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     3   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION RLSS
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSS01/ RLSS(6),
     1   IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     2   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     3   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     4   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      INTEGER I
      DOUBLE PRECISION DI, HL0, PHL0, R
C-----------------------------------------------------------------------
C This routine manages the solution of the linear system arising from
C a chord iteration.  It is called if MITER .ne. 0.
C If MITER is 1 or 2, it calls CDRV to accomplish this.
C If MITER = 3 it updates the coefficient H*EL0 in the diagonal
C matrix, and then computes the solution.
C communication with DSOLSS uses the following variables:
C WK    = real work space containing the inverse diagonal matrix if
C         MITER = 3 and the LU decomposition of the matrix otherwise.
C         Storage of matrix elements starts at WK(3).
C         WK also contains the following matrix-related data:
C         WK(1) = SQRT(UROUND) (not used here),
C         WK(2) = HL0, the previous value of H*EL0, used if MITER = 3.
C IWK   = integer work space for matrix-related data, assumed to
C         be equivalenced to WK.  In addition, WK(IPRSP) and IWK(IPISP)
C         are assumed to have identical locations.
C X     = the right-hand side vector on input, and the solution vector
C         on output, of length N.
C TEM   = vector of work space of length N, not used in this version.
C IERSL = output flag (in Common).
C         IERSL = 0  if no trouble occurred.
C         IERSL = -1 if CDRV returned an error flag (MITER = 1 or 2).
C                    This should never occur and is considered fatal.
C         IERSL = 1  if a singular matrix arose with MITER = 3.
C This routine also uses other variables in Common.
C-----------------------------------------------------------------------
      IERSL = 0
      GO TO (100, 100, 300), MITER
 100  CALL CDRV (N,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1   WK(IPA),X,X,NSP,IWK(IPISP),WK(IPRSP),IESP,4,IERSL)
      IF (IERSL .NE. 0) IERSL = -1
      RETURN
C
 300  PHL0 = WK(2)
      HL0 = H*EL0
      WK(2) = HL0
      IF (HL0 .EQ. PHL0) GO TO 330
      R = HL0/PHL0
      DO 320 I = 1,N
        DI = 1.0D0 - R*(1.0D0 - 1.0D0/WK(I+2))
        IF (ABS(DI) .EQ. 0.0D0) GO TO 390
 320    WK(I+2) = 1.0D0/DI
 330  DO 340 I = 1,N
 340    X(I) = WK(I+2)*X(I)
      RETURN
 390  IERSL = 1
      RETURN
C
C----------------------- End of Subroutine DSOLSS ----------------------
      END

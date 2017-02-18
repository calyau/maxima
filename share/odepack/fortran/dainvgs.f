*DECK DAINVGS
      SUBROUTINE DAINVGS (NEQ, T, Y, WK, IWK, TEM, YDOT, IER, RES, ADDA)
      EXTERNAL RES, ADDA
      INTEGER NEQ, IWK, IER
      INTEGER IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     1   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     2   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     3   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      INTEGER I, IMUL, J, K, KMIN, KMAX
      DOUBLE PRECISION T, Y, WK, TEM, YDOT
      DOUBLE PRECISION RLSS
      DIMENSION Y(*), WK(*), IWK(*), TEM(*), YDOT(*)
      COMMON /DLSS01/ RLSS(6),
     1   IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     2   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     3   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     4   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
C-----------------------------------------------------------------------
C This subroutine computes the initial value of the vector YDOT
C satisfying
C     A * YDOT = g(t,y)
C when A is nonsingular.  It is called by DLSODIS for initialization
C only, when ISTATE = 0.  The matrix A is subjected to LU
C decomposition in CDRV.  Then the system A*YDOT = g(t,y) is solved
C in CDRV.
C In addition to variables described previously, communication
C with DAINVGS uses the following:
C Y     = array of initial values.
C WK    = real work space for matrices.  On output it contains A and
C         its LU decomposition.  The LU decomposition is not entirely
C         sparse unless the structure of the matrix A is identical to
C         the structure of the Jacobian matrix dr/dy.
C         Storage of matrix elements starts at WK(3).
C         WK(1) = SQRT(UROUND), not used here.
C IWK   = integer work space for matrix-related data, assumed to
C         be equivalenced to WK.  In addition, WK(IPRSP) and WK(IPISP)
C         are assumed to have identical locations.
C TEM   = vector of work space of length N (ACOR in DSTODI).
C YDOT  = output vector containing the initial dy/dt. YDOT(i) contains
C         dy(i)/dt when the matrix A is non-singular.
C IER   = output error flag with the following values and meanings:
C       = 0  if DAINVGS was successful.
C       = 1  if the A-matrix was found to be singular.
C       = 2  if RES returned an error flag IRES = IER = 2.
C       = 3  if RES returned an error flag IRES = IER = 3.
C       = 4  if insufficient storage for CDRV (should not occur here).
C       = 5  if other error found in CDRV (should not occur here).
C-----------------------------------------------------------------------
C
      DO 10 I = 1,NNZ
 10     WK(IBA+I) = 0.0D0
C
      IER = 1
      CALL RES (NEQ, T, Y, WK(IPA), YDOT, IER)
      IF (IER .GT. 1) RETURN
C
      KMIN = IWK(IPIAN)
      DO 30 J = 1,NEQ
        KMAX = IWK(IPIAN+J) - 1
        DO 15 K = KMIN,KMAX
          I = IWK(IBJAN+K)
 15       TEM(I) = 0.0D0
        CALL ADDA (NEQ, T, Y, J, IWK(IPIAN), IWK(IPJAN), TEM)
        DO 20 K = KMIN,KMAX
          I = IWK(IBJAN+K)
 20       WK(IBA+K) = TEM(I)
        KMIN = KMAX + 1
 30   CONTINUE
      NLU = NLU + 1
      IER = 0
      DO 40 I = 1,NEQ
 40     TEM(I) = 0.0D0
C
C Numerical factorization of matrix A. ---------------------------------
      CALL CDRV (NEQ,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1  WK(IPA),TEM,TEM,NSP,IWK(IPISP),WK(IPRSP),IESP,2,IYS)
      IF (IYS .EQ. 0) GO TO 50
      IMUL = (IYS - 1)/NEQ
      IER = 5
      IF (IMUL .EQ. 8) IER = 1
      IF (IMUL .EQ. 10) IER = 4
      RETURN
C
C Solution of the linear system. ---------------------------------------
 50   CALL CDRV (NEQ,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1  WK(IPA),YDOT,YDOT,NSP,IWK(IPISP),WK(IPRSP),IESP,4,IYS)
      IF (IYS .NE. 0) IER = 5
      RETURN
C----------------------- End of Subroutine DAINVGS ---------------------
      END

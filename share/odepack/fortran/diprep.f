*DECK DIPREP
      SUBROUTINE DIPREP (NEQ, Y, RWORK, IA, JA, IPFLAG, F, JAC)
      EXTERNAL F, JAC
      INTEGER NEQ, IA, JA, IPFLAG
      DOUBLE PRECISION Y, RWORK
      DIMENSION NEQ(*), Y(*), RWORK(*), IA(*), JA(*)
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
      INTEGER I, IMAX, LEWTN, LYHD, LYHN
C-----------------------------------------------------------------------
C This routine serves as an interface between the driver and
C Subroutine DPREP.  It is called only if MITER is 1 or 2.
C Tasks performed here are:
C  * call DPREP,
C  * reset the required WM segment length LENWK,
C  * move YH back to its final location (following WM in RWORK),
C  * reset pointers for YH, SAVF, EWT, and ACOR, and
C  * move EWT to its new position if ISTATE = 1.
C IPFLAG is an output error indication flag.  IPFLAG = 0 if there was
C no trouble, and IPFLAG is the value of the DPREP error flag IPPER
C if there was trouble in Subroutine DPREP.
C-----------------------------------------------------------------------
      IPFLAG = 0
C Call DPREP to do matrix preprocessing operations. --------------------
      CALL DPREP (NEQ, Y, RWORK(LYH), RWORK(LSAVF), RWORK(LEWT),
     1   RWORK(LACOR), IA, JA, RWORK(LWM), RWORK(LWM), IPFLAG, F, JAC)
      LENWK = MAX(LREQ,LWMIN)
      IF (IPFLAG .LT. 0) RETURN
C If DPREP was successful, move YH to end of required space for WM. ----
      LYHN = LWM + LENWK
      IF (LYHN .GT. LYH) RETURN
      LYHD = LYH - LYHN
      IF (LYHD .EQ. 0) GO TO 20
      IMAX = LYHN - 1 + LENYHM
      DO 10 I = LYHN,IMAX
 10     RWORK(I) = RWORK(I+LYHD)
      LYH = LYHN
C Reset pointers for SAVF, EWT, and ACOR. ------------------------------
 20   LSAVF = LYH + LENYH
      LEWTN = LSAVF + N
      LACOR = LEWTN + N
      IF (ISTATC .EQ. 3) GO TO 40
C If ISTATE = 1, move EWT (left) to its new position. ------------------
      IF (LEWTN .GT. LEWT) RETURN
      DO 30 I = 1,N
 30     RWORK(I+LEWTN-1) = RWORK(I+LEWT-1)
 40   LEWT = LEWTN
      RETURN
C----------------------- End of Subroutine DIPREP ----------------------
      END

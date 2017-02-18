*DECK DPREP
      SUBROUTINE DPREP (NEQ, Y, YH, SAVF, EWT, FTEM, IA, JA,
     1                     WK, IWK, IPPER, F, JAC)
      EXTERNAL F,JAC
      INTEGER NEQ, IA, JA, IWK, IPPER
      DOUBLE PRECISION Y, YH, SAVF, EWT, FTEM, WK
      DIMENSION NEQ(*), Y(*), YH(*), SAVF(*), EWT(*), FTEM(*),
     1   IA(*), JA(*), WK(*), IWK(*)
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
      DOUBLE PRECISION CON0, CONMIN, CCMXJ, PSMALL, RBIG, SETH
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSS01/ CON0, CONMIN, CCMXJ, PSMALL, RBIG, SETH,
     1   IPLOST, IESP, ISTATC, IYS, IBA, IBIAN, IBJAN, IBJGP,
     2   IPIAN, IPJAN, IPJGP, IPIGP, IPR, IPC, IPIC, IPISP, IPRSP, IPA,
     3   LENYH, LENYHM, LENWK, LREQ, LRAT, LREST, LWMIN, MOSS, MSBJ,
     4   NSLJ, NGP, NLU, NNZ, NSP, NZL, NZU
      INTEGER I, IBR, IER, IPIL, IPIU, IPTT1, IPTT2, J, JFOUND, K,
     1   KNEW, KMAX, KMIN, LDIF, LENIGP, LIWK, MAXG, NP1, NZSUT
      DOUBLE PRECISION DQ, DYJ, ERWT, FAC, YJ
C-----------------------------------------------------------------------
C This routine performs preprocessing related to the sparse linear
C systems that must be solved if MITER = 1 or 2.
C The operations that are performed here are:
C  * compute sparseness structure of Jacobian according to MOSS,
C  * compute grouping of column indices (MITER = 2),
C  * compute a new ordering of rows and columns of the matrix,
C  * reorder JA corresponding to the new ordering,
C  * perform a symbolic LU factorization of the matrix, and
C  * set pointers for segments of the IWK/WK array.
C In addition to variables described previously, DPREP uses the
C following for communication:
C YH     = the history array.  Only the first column, containing the
C          current Y vector, is used.  Used only if MOSS .ne. 0.
C SAVF   = a work array of length NEQ, used only if MOSS .ne. 0.
C EWT    = array of length NEQ containing (inverted) error weights.
C          Used only if MOSS = 2 or if ISTATE = MOSS = 1.
C FTEM   = a work array of length NEQ, identical to ACOR in the driver,
C          used only if MOSS = 2.
C WK     = a real work array of length LENWK, identical to WM in
C          the driver.
C IWK    = integer work array, assumed to occupy the same space as WK.
C LENWK  = the length of the work arrays WK and IWK.
C ISTATC = a copy of the driver input argument ISTATE (= 1 on the
C          first call, = 3 on a continuation call).
C IYS    = flag value from ODRV or CDRV.
C IPPER  = output error flag with the following values and meanings:
C          0  no error.
C         -1  insufficient storage for internal structure pointers.
C         -2  insufficient storage for JGROUP.
C         -3  insufficient storage for ODRV.
C         -4  other error flag from ODRV (should never occur).
C         -5  insufficient storage for CDRV.
C         -6  other error flag from CDRV.
C-----------------------------------------------------------------------
      IBIAN = LRAT*2
      IPIAN = IBIAN + 1
      NP1 = N + 1
      IPJAN = IPIAN + NP1
      IBJAN = IPJAN - 1
      LIWK = LENWK*LRAT
      IF (IPJAN+N-1 .GT. LIWK) GO TO 210
      IF (MOSS .EQ. 0) GO TO 30
C
      IF (ISTATC .EQ. 3) GO TO 20
C ISTATE = 1 and MOSS .ne. 0.  Perturb Y for structure determination. --
      DO 10 I = 1,N
        ERWT = 1.0D0/EWT(I)
        FAC = 1.0D0 + 1.0D0/(I + 1.0D0)
        Y(I) = Y(I) + FAC*SIGN(ERWT,Y(I))
 10     CONTINUE
      GO TO (70, 100), MOSS
C
 20   CONTINUE
C ISTATE = 3 and MOSS .ne. 0.  Load Y from YH(*,1). --------------------
      DO 25 I = 1,N
 25     Y(I) = YH(I)
      GO TO (70, 100), MOSS
C
C MOSS = 0.  Process user's IA,JA.  Add diagonal entries if necessary. -
 30   KNEW = IPJAN
      KMIN = IA(1)
      IWK(IPIAN) = 1
      DO 60 J = 1,N
        JFOUND = 0
        KMAX = IA(J+1) - 1
        IF (KMIN .GT. KMAX) GO TO 45
        DO 40 K = KMIN,KMAX
          I = JA(K)
          IF (I .EQ. J) JFOUND = 1
          IF (KNEW .GT. LIWK) GO TO 210
          IWK(KNEW) = I
          KNEW = KNEW + 1
 40       CONTINUE
        IF (JFOUND .EQ. 1) GO TO 50
 45     IF (KNEW .GT. LIWK) GO TO 210
        IWK(KNEW) = J
        KNEW = KNEW + 1
 50     IWK(IPIAN+J) = KNEW + 1 - IPJAN
        KMIN = KMAX + 1
 60     CONTINUE
      GO TO 140
C
C MOSS = 1.  Compute structure from user-supplied Jacobian routine JAC.
 70   CONTINUE
C A dummy call to F allows user to create temporaries for use in JAC. --
      CALL F (NEQ, TN, Y, SAVF)
      K = IPJAN
      IWK(IPIAN) = 1
      DO 90 J = 1,N
        IF (K .GT. LIWK) GO TO 210
        IWK(K) = J
        K = K + 1
        DO 75 I = 1,N
 75       SAVF(I) = 0.0D0
        CALL JAC (NEQ, TN, Y, J, IWK(IPIAN), IWK(IPJAN), SAVF)
        DO 80 I = 1,N
          IF (ABS(SAVF(I)) .LE. SETH) GO TO 80
          IF (I .EQ. J) GO TO 80
          IF (K .GT. LIWK) GO TO 210
          IWK(K) = I
          K = K + 1
 80       CONTINUE
        IWK(IPIAN+J) = K + 1 - IPJAN
 90     CONTINUE
      GO TO 140
C
C MOSS = 2.  Compute structure from results of N + 1 calls to F. -------
 100  K = IPJAN
      IWK(IPIAN) = 1
      CALL F (NEQ, TN, Y, SAVF)
      DO 120 J = 1,N
        IF (K .GT. LIWK) GO TO 210
        IWK(K) = J
        K = K + 1
        YJ = Y(J)
        ERWT = 1.0D0/EWT(J)
        DYJ = SIGN(ERWT,YJ)
        Y(J) = YJ + DYJ
        CALL F (NEQ, TN, Y, FTEM)
        Y(J) = YJ
        DO 110 I = 1,N
          DQ = (FTEM(I) - SAVF(I))/DYJ
          IF (ABS(DQ) .LE. SETH) GO TO 110
          IF (I .EQ. J) GO TO 110
          IF (K .GT. LIWK) GO TO 210
          IWK(K) = I
          K = K + 1
 110      CONTINUE
        IWK(IPIAN+J) = K + 1 - IPJAN
 120    CONTINUE
C
 140  CONTINUE
      IF (MOSS .EQ. 0 .OR. ISTATC .NE. 1) GO TO 150
C If ISTATE = 1 and MOSS .ne. 0, restore Y from YH. --------------------
      DO 145 I = 1,N
 145    Y(I) = YH(I)
 150  NNZ = IWK(IPIAN+N) - 1
      LENIGP = 0
      IPIGP = IPJAN + NNZ
      IF (MITER .NE. 2) GO TO 160
C
C Compute grouping of column indices (MITER = 2). ----------------------
      MAXG = NP1
      IPJGP = IPJAN + NNZ
      IBJGP = IPJGP - 1
      IPIGP = IPJGP + N
      IPTT1 = IPIGP + NP1
      IPTT2 = IPTT1 + N
      LREQ = IPTT2 + N - 1
      IF (LREQ .GT. LIWK) GO TO 220
      CALL JGROUP (N, IWK(IPIAN), IWK(IPJAN), MAXG, NGP, IWK(IPIGP),
     1   IWK(IPJGP), IWK(IPTT1), IWK(IPTT2), IER)
      IF (IER .NE. 0) GO TO 220
      LENIGP = NGP + 1
C
C Compute new ordering of rows/columns of Jacobian. --------------------
 160  IPR = IPIGP + LENIGP
      IPC = IPR
      IPIC = IPC + N
      IPISP = IPIC + N
      IPRSP = (IPISP - 2)/LRAT + 2
      IESP = LENWK + 1 - IPRSP
      IF (IESP .LT. 0) GO TO 230
      IBR = IPR - 1
      DO 170 I = 1,N
 170    IWK(IBR+I) = I
      NSP = LIWK + 1 - IPISP
      CALL ODRV (N, IWK(IPIAN), IWK(IPJAN), WK, IWK(IPR), IWK(IPIC),
     1   NSP, IWK(IPISP), 1, IYS)
      IF (IYS .EQ. 11*N+1) GO TO 240
      IF (IYS .NE. 0) GO TO 230
C
C Reorder JAN and do symbolic LU factorization of matrix. --------------
      IPA = LENWK + 1 - NNZ
      NSP = IPA - IPRSP
      LREQ = MAX(12*N/LRAT, 6*N/LRAT+2*N+NNZ) + 3
      LREQ = LREQ + IPRSP - 1 + NNZ
      IF (LREQ .GT. LENWK) GO TO 250
      IBA = IPA - 1
      DO 180 I = 1,NNZ
 180    WK(IBA+I) = 0.0D0
      IPISP = LRAT*(IPRSP - 1) + 1
      CALL CDRV (N,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1   WK(IPA),WK(IPA),WK(IPA),NSP,IWK(IPISP),WK(IPRSP),IESP,5,IYS)
      LREQ = LENWK - IESP
      IF (IYS .EQ. 10*N+1) GO TO 250
      IF (IYS .NE. 0) GO TO 260
      IPIL = IPISP
      IPIU = IPIL + 2*N + 1
      NZU = IWK(IPIL+N) - IWK(IPIL)
      NZL = IWK(IPIU+N) - IWK(IPIU)
      IF (LRAT .GT. 1) GO TO 190
      CALL ADJLR (N, IWK(IPISP), LDIF)
      LREQ = LREQ + LDIF
 190  CONTINUE
      IF (LRAT .EQ. 2 .AND. NNZ .EQ. N) LREQ = LREQ + 1
      NSP = NSP + LREQ - LENWK
      IPA = LREQ + 1 - NNZ
      IBA = IPA - 1
      IPPER = 0
      RETURN
C
 210  IPPER = -1
      LREQ = 2 + (2*N + 1)/LRAT
      LREQ = MAX(LENWK+1,LREQ)
      RETURN
C
 220  IPPER = -2
      LREQ = (LREQ - 1)/LRAT + 1
      RETURN
C
 230  IPPER = -3
      CALL CNTNZU (N, IWK(IPIAN), IWK(IPJAN), NZSUT)
      LREQ = LENWK - IESP + (3*N + 4*NZSUT - 1)/LRAT + 1
      RETURN
C
 240  IPPER = -4
      RETURN
C
 250  IPPER = -5
      RETURN
C
 260  IPPER = -6
      LREQ = LENWK
      RETURN
C----------------------- End of Subroutine DPREP -----------------------
      END

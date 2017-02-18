*DECK DPREPI
      SUBROUTINE DPREPI (NEQ, Y, S, YH, SAVR, EWT, RTEM, IA, JA, IC, JC,
     1                   WK, IWK, IPPER, RES, JAC, ADDA)
      EXTERNAL RES, JAC, ADDA
      INTEGER NEQ, IA, JA, IC, JC, IWK, IPPER
      DOUBLE PRECISION Y, S, YH, SAVR, EWT, RTEM, WK
      DIMENSION NEQ(*), Y(*), S(*), YH(*), SAVR(*), EWT(*), RTEM(*),
     1   IA(*), JA(*), IC(*), JC(*), WK(*), IWK(*)
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
      INTEGER I, IBR, IER, IPIL, IPIU, IPTT1, IPTT2, J, K, KNEW, KAMAX,
     1   KAMIN, KCMAX, KCMIN, LDIF, LENIGP, LENWK1, LIWK, LJFO, MAXG,
     2   NP1, NZSUT
      DOUBLE PRECISION ERWT, FAC, YJ
C-----------------------------------------------------------------------
C This routine performs preprocessing related to the sparse linear
C systems that must be solved.
C The operations that are performed here are:
C  * compute sparseness structure of the iteration matrix
C      P = A - con*J  according to MOSS,
C  * compute grouping of column indices (MITER = 2),
C  * compute a new ordering of rows and columns of the matrix,
C  * reorder JA corresponding to the new ordering,
C  * perform a symbolic LU factorization of the matrix, and
C  * set pointers for segments of the IWK/WK array.
C In addition to variables described previously, DPREPI uses the
C following for communication:
C YH     = the history array.  Only the first column, containing the
C          current Y vector, is used.  Used only if MOSS .ne. 0.
C S      = array of length NEQ, identical to YDOTI in the driver, used
C          only if MOSS .ne. 0.
C SAVR   = a work array of length NEQ, used only if MOSS .ne. 0.
C EWT    = array of length NEQ containing (inverted) error weights.
C          Used only if MOSS = 2 or 4 or if ISTATE = MOSS = 1.
C RTEM   = a work array of length NEQ, identical to ACOR in the driver,
C          used only if MOSS = 2 or 4.
C WK     = a real work array of length LENWK, identical to WM in
C          the driver.
C IWK    = integer work array, assumed to occupy the same space as WK.
C LENWK  = the length of the work arrays WK and IWK.
C ISTATC = a copy of the driver input argument ISTATE (= 1 on the
C          first call, = 3 on a continuation call).
C IYS    = flag value from ODRV or CDRV.
C IPPER  = output error flag , with the following values and meanings:
C        =   0  no error.
C        =  -1  insufficient storage for internal structure pointers.
C        =  -2  insufficient storage for JGROUP.
C        =  -3  insufficient storage for ODRV.
C        =  -4  other error flag from ODRV (should never occur).
C        =  -5  insufficient storage for CDRV.
C        =  -6  other error flag from CDRV.
C        =  -7  if the RES routine returned error flag IRES = IER = 2.
C        =  -8  if the RES routine returned error flag IRES = IER = 3.
C-----------------------------------------------------------------------
      IBIAN = LRAT*2
      IPIAN = IBIAN + 1
      NP1 = N + 1
      IPJAN = IPIAN + NP1
      IBJAN = IPJAN - 1
      LENWK1 = LENWK - N
      LIWK = LENWK*LRAT
      IF (MOSS .EQ. 0) LIWK = LIWK - N
      IF (MOSS .EQ. 1 .OR. MOSS .EQ. 2) LIWK = LENWK1*LRAT
      IF (IPJAN+N-1 .GT. LIWK) GO TO 310
      IF (MOSS .EQ. 0) GO TO 30
C
      IF (ISTATC .EQ. 3) GO TO 20
C ISTATE = 1 and MOSS .ne. 0.  Perturb Y for structure determination.
C Initialize S with random nonzero elements for structure determination.
      DO 10 I=1,N
        ERWT = 1.0D0/EWT(I)
        FAC = 1.0D0 + 1.0D0/(I + 1.0D0)
        Y(I) = Y(I) + FAC*SIGN(ERWT,Y(I))
        S(I) = 1.0D0 + FAC*ERWT
 10     CONTINUE
      GO TO (70, 100, 150, 200), MOSS
C
 20   CONTINUE
C ISTATE = 3 and MOSS .ne. 0. Load Y from YH(*,1) and S from YH(*,2). --
      DO 25 I = 1,N
         Y(I) = YH(I)
 25      S(I) = YH(N+I)
      GO TO (70, 100, 150, 200), MOSS
C
C MOSS = 0. Process user's IA,JA and IC,JC. ----------------------------
 30   KNEW = IPJAN
      KAMIN = IA(1)
      KCMIN = IC(1)
      IWK(IPIAN) = 1
      DO 60 J = 1,N
        DO 35 I = 1,N
 35       IWK(LIWK+I) = 0
        KAMAX = IA(J+1) - 1
        IF (KAMIN .GT. KAMAX) GO TO 45
        DO 40 K = KAMIN,KAMAX
          I = JA(K)
          IWK(LIWK+I) = 1
          IF (KNEW .GT. LIWK) GO TO 310
          IWK(KNEW) = I
          KNEW = KNEW + 1
 40       CONTINUE
 45     KAMIN = KAMAX + 1
        KCMAX = IC(J+1) - 1
        IF (KCMIN .GT. KCMAX) GO TO 55
        DO 50 K = KCMIN,KCMAX
          I = JC(K)
          IF (IWK(LIWK+I) .NE. 0) GO TO 50
          IF (KNEW .GT. LIWK) GO TO 310
          IWK(KNEW) = I
          KNEW = KNEW + 1
 50       CONTINUE
 55     IWK(IPIAN+J) = KNEW + 1 - IPJAN
        KCMIN = KCMAX + 1
 60     CONTINUE
      GO TO 240
C
C MOSS = 1. Compute structure from user-supplied Jacobian routine JAC. -
 70   CONTINUE
C A dummy call to RES allows user to create temporaries for use in JAC.
      IER = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IER)
      IF (IER .GT. 1) GO TO 370
      DO 75 I = 1,N
        SAVR(I) = 0.0D0
 75     WK(LENWK1+I) = 0.0D0
      K = IPJAN
      IWK(IPIAN) = 1
      DO 95 J = 1,N
        CALL ADDA (NEQ, TN, Y, J, IWK(IPIAN), IWK(IPJAN), WK(LENWK1+1))
        CALL JAC (NEQ, TN, Y, S, J, IWK(IPIAN), IWK(IPJAN), SAVR)
        DO 90 I = 1,N
          LJFO = LENWK1 + I
          IF (WK(LJFO) .EQ. 0.0D0) GO TO 80
          WK(LJFO) = 0.0D0
          SAVR(I) = 0.0D0
          GO TO 85
 80       IF (SAVR(I) .EQ. 0.0D0) GO TO 90
          SAVR(I) = 0.0D0
 85       IF (K .GT. LIWK) GO TO 310
          IWK(K) = I
          K = K+1
 90       CONTINUE
        IWK(IPIAN+J) = K + 1 - IPJAN
 95     CONTINUE
      GO TO 240
C
C MOSS = 2. Compute structure from results of N + 1 calls to RES. ------
 100  DO 105 I = 1,N
 105    WK(LENWK1+I) = 0.0D0
      K = IPJAN
      IWK(IPIAN) = 1
      IER = -1
      IF (MITER .EQ. 1) IER = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IER)
      IF (IER .GT. 1) GO TO 370
      DO 130 J = 1,N
        CALL ADDA (NEQ, TN, Y, J, IWK(IPIAN), IWK(IPJAN), WK(LENWK1+1))
        YJ = Y(J)
        ERWT = 1.0D0/EWT(J)
        Y(J) = YJ + SIGN(ERWT,YJ)
        CALL RES (NEQ, TN, Y, S, RTEM, IER)
        IF (IER .GT. 1) RETURN
        Y(J) = YJ
        DO 120 I = 1,N
          LJFO = LENWK1 + I
          IF (WK(LJFO) .EQ. 0.0D0) GO TO 110
          WK(LJFO) = 0.0D0
          GO TO 115
 110      IF (RTEM(I) .EQ. SAVR(I)) GO TO 120
 115      IF (K .GT. LIWK) GO TO 310
          IWK(K) = I
          K = K + 1
 120      CONTINUE
        IWK(IPIAN+J) = K + 1 - IPJAN
 130    CONTINUE
      GO TO 240
C
C MOSS = 3. Compute structure from the user's IA/JA and JAC routine. ---
 150  CONTINUE
C A dummy call to RES allows user to create temporaries for use in JAC.
      IER = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IER)
      IF (IER .GT. 1) GO TO 370
      DO 155 I = 1,N
 155    SAVR(I) = 0.0D0
      KNEW = IPJAN
      KAMIN = IA(1)
      IWK(IPIAN) = 1
      DO 190 J = 1,N
        CALL JAC (NEQ, TN, Y, S, J, IWK(IPIAN), IWK(IPJAN), SAVR)
        KAMAX = IA(J+1) - 1
        IF (KAMIN .GT. KAMAX) GO TO 170
        DO 160 K = KAMIN,KAMAX
          I = JA(K)
          SAVR(I) = 0.0D0
          IF (KNEW .GT. LIWK) GO TO 310
          IWK(KNEW) = I
          KNEW = KNEW + 1
 160      CONTINUE
 170    KAMIN = KAMAX + 1
        DO 180 I = 1,N
          IF (SAVR(I) .EQ. 0.0D0) GO TO 180
          SAVR(I) = 0.0D0
          IF (KNEW .GT. LIWK) GO TO 310
          IWK(KNEW) = I
          KNEW = KNEW + 1
 180      CONTINUE
        IWK(IPIAN+J) = KNEW + 1 - IPJAN
 190    CONTINUE
      GO TO 240
C
C MOSS = 4. Compute structure from user's IA/JA and N + 1 RES calls. ---
 200  KNEW = IPJAN
      KAMIN = IA(1)
      IWK(IPIAN) = 1
      IER = -1
      IF (MITER .EQ. 1) IER = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IER)
      IF (IER .GT. 1) GO TO 370
      DO 235 J = 1,N
        YJ = Y(J)
        ERWT = 1.0D0/EWT(J)
        Y(J) = YJ + SIGN(ERWT,YJ)
        CALL RES (NEQ, TN, Y, S, RTEM, IER)
        IF (IER .GT. 1) RETURN
        Y(J) = YJ
        KAMAX = IA(J+1) - 1
        IF (KAMIN .GT. KAMAX) GO TO 225
        DO 220 K = KAMIN,KAMAX
          I = JA(K)
          RTEM(I) = SAVR(I)
          IF (KNEW .GT. LIWK) GO TO 310
          IWK(KNEW) = I
          KNEW = KNEW + 1
 220      CONTINUE
 225    KAMIN = KAMAX + 1
        DO 230 I = 1,N
          IF (RTEM(I) .EQ. SAVR(I)) GO TO 230
          IF (KNEW .GT. LIWK) GO TO 310
          IWK(KNEW) = I
          KNEW = KNEW + 1
 230      CONTINUE
        IWK(IPIAN+J) = KNEW + 1 - IPJAN
 235    CONTINUE
C
 240  CONTINUE
      IF (MOSS .EQ. 0 .OR. ISTATC .EQ. 3) GO TO 250
C If ISTATE = 0 or 1 and MOSS .ne. 0, restore Y from YH. ---------------
      DO 245 I = 1,N
 245    Y(I) = YH(I)
 250  NNZ = IWK(IPIAN+N) - 1
      IPPER = 0
      NGP = 0
      LENIGP = 0
      IPIGP = IPJAN + NNZ
      IF (MITER .NE. 2) GO TO 260
C
C Compute grouping of column indices (MITER = 2). ----------------------
C
      MAXG = NP1
      IPJGP = IPJAN + NNZ
      IBJGP = IPJGP - 1
      IPIGP = IPJGP + N
      IPTT1 = IPIGP + NP1
      IPTT2 = IPTT1 + N
      LREQ = IPTT2 + N - 1
      IF (LREQ .GT. LIWK) GO TO 320
      CALL JGROUP (N, IWK(IPIAN), IWK(IPJAN), MAXG, NGP, IWK(IPIGP),
     1   IWK(IPJGP), IWK(IPTT1), IWK(IPTT2), IER)
      IF (IER .NE. 0) GO TO 320
      LENIGP = NGP + 1
C
C Compute new ordering of rows/columns of Jacobian. --------------------
 260  IPR = IPIGP + LENIGP
      IPC = IPR
      IPIC = IPC + N
      IPISP = IPIC + N
      IPRSP = (IPISP-2)/LRAT + 2
      IESP = LENWK + 1 - IPRSP
      IF (IESP .LT. 0) GO TO 330
      IBR = IPR - 1
      DO 270 I = 1,N
 270    IWK(IBR+I) = I
      NSP = LIWK + 1 - IPISP
      CALL ODRV(N, IWK(IPIAN), IWK(IPJAN), WK, IWK(IPR), IWK(IPIC), NSP,
     1   IWK(IPISP), 1, IYS)
      IF (IYS .EQ. 11*N+1) GO TO 340
      IF (IYS .NE. 0) GO TO 330
C
C Reorder JAN and do symbolic LU factorization of matrix. --------------
      IPA = LENWK + 1 - NNZ
      NSP = IPA - IPRSP
      LREQ = MAX(12*N/LRAT, 6*N/LRAT+2*N+NNZ) + 3
      LREQ = LREQ + IPRSP - 1 + NNZ
      IF (LREQ .GT. LENWK) GO TO 350
      IBA = IPA - 1
      DO 280 I = 1,NNZ
 280    WK(IBA+I) = 0.0D0
      IPISP = LRAT*(IPRSP - 1) + 1
      CALL CDRV(N,IWK(IPR),IWK(IPC),IWK(IPIC),IWK(IPIAN),IWK(IPJAN),
     1   WK(IPA),WK(IPA),WK(IPA),NSP,IWK(IPISP),WK(IPRSP),IESP,5,IYS)
      LREQ = LENWK - IESP
      IF (IYS .EQ. 10*N+1) GO TO 350
      IF (IYS .NE. 0) GO TO 360
      IPIL = IPISP
      IPIU = IPIL + 2*N + 1
      NZU = IWK(IPIL+N) - IWK(IPIL)
      NZL = IWK(IPIU+N) - IWK(IPIU)
      IF (LRAT .GT. 1) GO TO 290
      CALL ADJLR (N, IWK(IPISP), LDIF)
      LREQ = LREQ + LDIF
 290  CONTINUE
      IF (LRAT .EQ. 2 .AND. NNZ .EQ. N) LREQ = LREQ + 1
      NSP = NSP + LREQ - LENWK
      IPA = LREQ + 1 - NNZ
      IBA = IPA - 1
      IPPER = 0
      RETURN
C
 310  IPPER = -1
      LREQ = 2 + (2*N + 1)/LRAT
      LREQ = MAX(LENWK+1,LREQ)
      RETURN
C
 320  IPPER = -2
      LREQ = (LREQ - 1)/LRAT + 1
      RETURN
C
 330  IPPER = -3
      CALL CNTNZU (N, IWK(IPIAN), IWK(IPJAN), NZSUT)
      LREQ = LENWK - IESP + (3*N + 4*NZSUT - 1)/LRAT + 1
      RETURN
C
 340  IPPER = -4
      RETURN
C
 350  IPPER =  -5
      RETURN
C
 360  IPPER = -6
      LREQ = LENWK
      RETURN
C
 370  IPPER = -IER - 5
      LREQ = 2 + (2*N + 1)/LRAT
      RETURN
C----------------------- End of Subroutine DPREPI ----------------------
      END

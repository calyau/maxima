      SUBROUTINE CONTRL (XI, XIOLD, Z, DMZ, RHS, DELZ, DELDMZ,
     1           DQZ, DQDMZ, G, W, V, VALSTR, SLOPE, SCALE, DSCALE,
     2           ACCUM, IPVTG, INTEGS, IPVTW, NFXPNT, FIXPNT, IFLAG,
     3           FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C**********************************************************************
C
C   purpose
C     this subroutine is the actual driver.  the nonlinear iteration
C     strategy is controlled here ( see [4] ). upon convergence, errchk
C     is called to test for satisfaction of the requested tolerances.
C
C   variables
C
C     check  - maximum tolerance value, used as part of criteria for
C              checking for nonlinear iteration convergence
C     relax  - the relaxation factor for damped newton iteration
C     relmin - minimum allowable value for relax  (otherwise the
C              jacobian is considered singular).
C     rlxold - previous relax
C     rstart - initial value for relax when problem is sensitive
C     ifrz   - number of fixed jacobian iterations
C     lmtfrz - maximum value for ifrz before performing a reinversion
C     iter   - number of iterations (counted only when jacobian
C              reinversions are performed).
C     xi     - current mesh
C     xiold  - previous mesh
C     ipred  = 0  if relax is determined by a correction
C            = 1  if relax is determined by a prediction
C     ifreez = 0  if the jacobian is to be updated
C            = 1  if the jacobian is currently fixed (frozen)
C     iconv  = 0  if no previous convergence has been obtained
C            = 1  if convergence on a previous mesh has been obtained
C     icare  =-1  no convergence occurred (used for regular problems)
C            = 0  a regular problem
C            = 1  a sensitive problem
C            = 2  used for continuation (see description of ipar(10)
C                 in colnew).
C     rnorm  - norm of rhs (right hand side) for current iteration
C     rnold  - norm of rhs for previous iteration
C     anscl  - scaled norm of newton correction
C     anfix  - scaled norm of newton correction at next step
C     anorm  - scaled norm of a correction obtained with jacobian fixed
C     nz     - number of components of  z  (see subroutine approx)
C     ndmz   - number of components of  dmz  (see subroutine approx)
C     imesh  - a control variable for subroutines newmsh and errchk
C            = 1  the current mesh resulted from mesh selection
C                 or is the initial mesh.
C            = 2  the current mesh resulted from doubling the
C                 previous mesh
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION XI(1), XIOLD(1), Z(1), DMZ(1), RHS(1)
      DIMENSION G(1), W(1), V(1), VALSTR(1), SLOPE(1), ACCUM(1)
      DIMENSION DELZ(1), DELDMZ(1), DQZ(1), DQDMZ(1) , FIXPNT(1)
      DIMENSION DUMMY(1), SCALE(1), DSCALE(1)
      DIMENSION INTEGS(1), IPVTG(1), IPVTW(1)
C
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
      COMMON /COLORD/ K, NCOMP, MSTAR, KD, MMAX, M(20)
      COMMON /COLAPR/ N, NOLD, NMAX, NZ, NDMZ
      COMMON /COLMSH/ MSHFLG, MSHNUM, MSHLMT, MSHALT
      COMMON /COLSID/ ZETA(40), ALEFT, ARIGHT, IZETA, IDUM
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
      COMMON /COLEST/ TOL(40), WGTMSH(40), WGTERR(40), TOLIN(40),
     1                ROOT(40), JTOL(40), LTOL(40), NTOL
C
      EXTERNAL FSUB, DFSUB, GSUB, DGSUB, GUESS
C
C...  constants for control of nonlinear iteration
C
      RELMIN = 1.D-3
      RSTART = 1.D-2
      LMTFRZ = 4
C
C...  compute the maximum tolerance
C
      CHECK = 0.D0
      DO 10 I = 1, NTOL
   10   CHECK = DMAX1 ( TOLIN(I), CHECK )
      IMESH = 1
      ICONV = 0
      IF ( NONLIN .EQ. 0 ) ICONV = 1
      ICOR = 0
      NOCONV = 0
      MSING = 0
C
C...  the main iteration begins here .
C...  loop 20 is executed until error tolerances are satisfied or
C...  the code fails (due to a singular matrix or storage limitations)
C
   20      CONTINUE
C
C...       initialization for a new mesh
C
           ITER = 0
           IF ( NONLIN .GT. 0 )                     GO TO 50
C
C...       the linear case.
C...       set up and solve equations
C
           CALL LSYSLV (MSING, XI, XIOLD, DUMMY, DUMMY, Z, DMZ, G,
     1          W, V, RHS, DUMMY, INTEGS, IPVTG, IPVTW, RNORM, 0,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       check for a singular matrix
C
           IF ( MSING .EQ. 0 )                      GO TO 400
   30      IF ( MSING .LT. 0 )                      GO TO 40
           IF ( IPRINT .LT. 1 )  WRITE (IOUT,495)
           GO TO 460
   40      IF ( IPRINT .LT. 1 )  WRITE (IOUT,490)
           IFLAG = 0
           RETURN
C
C...       iteration loop for nonlinear case
C...       define the initial relaxation parameter (= relax)
C
   50      RELAX = 1.D0
C
C...       check for previous convergence and problem sensitivity
C
           IF ( ICARE .EQ. 1 .OR. ICARE .EQ. (-1) )  RELAX = RSTART
           IF ( ICONV .EQ. 0 )                      GO TO 160
C
C...       convergence on a previous mesh has been obtained.    thus
C...       we have a very good initial approximation for the newton
C...       process.    proceed with one full newton and then iterate
C...       with a fixed jacobian.
C
           IFREEZ = 0
C
C...       evaluate right hand side and its norm  and
C...       find the first newton correction
C
           CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DELZ, DELDMZ, G,
     1          W, V, RHS, DQDMZ, INTEGS, IPVTG, IPVTW, RNOLD, 1,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
           IF ( IPRINT .LT. 0 )  WRITE(IOUT,530)
           IF ( IPRINT .LT. 0 )  WRITE (IOUT,510) ITER, RNOLD
           GO TO 70
C
C...       solve for the next iterate .
C...       the value of ifreez determines whether this is a full
C...       newton step (=0) or a fixed jacobian iteration (=1).
C
   60      IF ( IPRINT .LT. 0 )  WRITE (IOUT,510) ITER, RNORM
           RNOLD = RNORM
           CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DELZ, DELDMZ, G,
     1          W, V, RHS, DUMMY, INTEGS, IPVTG, IPVTW, RNORM,
     2          3+IFREEZ, FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       check for a singular matrix
C
   70      IF ( MSING .NE. 0 )                      GO TO 30
           IF ( IFREEZ .EQ. 1 )                     GO TO 80
C
C...       a full newton step
C
           ITER = ITER + 1
           IFRZ = 0
   80      CONTINUE
C
C...       update   z and dmz , compute new  rhs  and its norm
C
           DO 90 I = 1, NZ
             Z(I) = Z(I) + DELZ(I)
   90      CONTINUE
           DO 100 I = 1, NDMZ
             DMZ(I) = DMZ(I) + DELDMZ(I)
  100      CONTINUE
           CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DELZ, DELDMZ, G,
     1          W, V, RHS, DUMMY, INTEGS, IPVTG, IPVTW, RNORM, 2,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       check monotonicity. if the norm of  rhs  gets smaller,
C...       proceed with a fixed jacobian; else proceed cautiously,
C...       as if convergence has not been obtained before (iconv=0).
C
           IF ( RNORM .LT. PRECIS )                 GO TO 390
           IF ( RNORM .GT. RNOLD )                  GO TO 130
           IF ( IFREEZ .EQ. 1 )                     GO TO 110
           IFREEZ = 1
           GO TO 60
C
C...       verify that the linear convergence with fixed jacobian
C...       is fast enough.
C
  110      IFRZ = IFRZ + 1
           IF ( IFRZ .GE. LMTFRZ )       IFREEZ = 0
           IF ( RNOLD .LT. 4.D0*RNORM )  IFREEZ = 0
C
C...       check convergence (iconv = 1).
C
           DO 120 IT = 1, NTOL
             INZ = LTOL(IT)
             DO 120 IZ = INZ, NZ, MSTAR
               IF ( DABS(DELZ(IZ)) .GT.
     1           TOLIN(IT) * (DABS(Z(IZ)) + 1.D0))  GO TO 60
  120      CONTINUE
C
C...       convergence obtained
C
           IF ( IPRINT .LT. 1 )  WRITE (IOUT,560) ITER
           GO TO 400
C
C...      convergence of fixed jacobian iteration failed.
C
  130      IF ( IPRINT .LT. 0 )  WRITE (IOUT,510) ITER, RNORM
           IF ( IPRINT .LT. 0 )  WRITE (IOUT,540)
           ICONV = 0
           RELAX = RSTART
           DO 140 I = 1, NZ
             Z(I) = Z(I) - DELZ(I)
  140      CONTINUE
           DO 150 I = 1, NDMZ
             DMZ(I) = DMZ(I) - DELDMZ(I)
  150      CONTINUE
C
C...       update old mesh
C
           NP1 = N + 1
           DO 155 I = 1, NP1
  155        XIOLD(I) = XI(I)
           NOLD = N
C
           ITER = 0
C
C...       no previous convergence has been obtained. proceed
C...       with the damped newton method.
C...       evaluate rhs and find the first newton correction.
C
  160      IF(IPRINT .LT. 0)  WRITE (IOUT,500)
           CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DELZ, DELDMZ, G,
     1          W, V, RHS, DQDMZ, INTEGS, IPVTG, IPVTW, RNOLD, 1,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       check for a singular matrix
C
           IF ( MSING .NE. 0 )                       GO TO 30
C
C...       bookkeeping for first mesh
C
           IF ( IGUESS .EQ. 1 )  IGUESS = 0
C
C...       find initial scaling
C
           CALL SKALE (N, MSTAR, KD, Z, XI, SCALE, DSCALE)
           GO TO 220
C
C...       main iteration loop
C
  170      RNOLD = RNORM
           IF ( ITER .GE. LIMIT )                   GO TO 430
C
C...       update scaling
C
           CALL SKALE (N, MSTAR, KD, Z, XI, SCALE, DSCALE)
C
C...       compute norm of newton correction with new scaling
C
           ANSCL = 0.D0
           DO 180 I = 1, NZ
             ANSCL = ANSCL + (DELZ(I) * SCALE(I))**2
  180      CONTINUE
           DO 190 I = 1, NDMZ
             ANSCL = ANSCL + (DELDMZ(I) * DSCALE(I))**2
  190      CONTINUE
           ANSCL = DSQRT(ANSCL / DFLOAT(NZ+NDMZ))
C
C...       find a newton direction
C
           CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DELZ, DELDMZ, G,
     1          W, V, RHS, DUMMY, INTEGS, IPVTG, IPVTW, RNORM, 3,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       check for a singular matrix
C
           IF ( MSING .NE. 0 )                      GO TO 30
C
C...       predict relaxation factor for newton step.
C
           ANDIF = 0.D0
           DO 200 I = 1, NZ
             ANDIF = ANDIF + ((DQZ(I) - DELZ(I)) * SCALE(I))**2
  200      CONTINUE
           DO 210 I = 1, NDMZ
             ANDIF = ANDIF + ((DQDMZ(I) - DELDMZ(I)) * DSCALE(I))**2
  210      CONTINUE
           ANDIF = DSQRT(ANDIF/DFLOAT(NZ+NDMZ) + PRECIS)
           RELAX = RELAX * ANSCL / ANDIF
           IF ( RELAX .GT. 1.D0 )  RELAX = 1.D0
  220      RLXOLD = RELAX
           IPRED = 1
           ITER = ITER + 1
C
C...       determine a new  z and dmz  and find new  rhs  and its norm
C
           DO 230 I = 1, NZ
             Z(I) = Z(I)  +  RELAX * DELZ(I)
  230      CONTINUE
           DO 240 I = 1, NDMZ
             DMZ(I) = DMZ(I)  +  RELAX * DELDMZ(I)
  240      CONTINUE
  250      CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DQZ, DQDMZ, G,
     1          W, V, RHS, DUMMY, INTEGS, IPVTG, IPVTW, RNORM, 2,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       compute a fixed jacobian iterate (used to control relax)
C
           CALL LSYSLV (MSING, XI, XIOLD, Z, DMZ, DQZ, DQDMZ, G,
     1          W, V, RHS, DUMMY, INTEGS, IPVTG, IPVTW, RNORM, 4,
     2          FSUB, DFSUB, GSUB, DGSUB, GUESS )
C
C...       find scaled norms of various terms used to correct relax
C
           ANORM = 0.D0
           ANFIX = 0.D0
           DO 260 I = 1, NZ
             ANORM = ANORM  +  (DELZ(I) * SCALE(I))**2
             ANFIX = ANFIX  +  (DQZ(I) * SCALE(I))**2
  260      CONTINUE
           DO 270 I = 1, NDMZ
             ANORM = ANORM  +  (DELDMZ(I) * DSCALE(I))**2
             ANFIX = ANFIX  +  (DQDMZ(I) * DSCALE(I))**2
  270      CONTINUE
           ANORM = DSQRT(ANORM / DFLOAT(NZ+NDMZ))
           ANFIX = DSQRT(ANFIX / DFLOAT(NZ+NDMZ))
           IF ( ICOR .EQ. 1 )                         GO TO 280
           IF (IPRINT .LT. 0)  WRITE (IOUT,520) ITER, RELAX, ANORM,
     1           ANFIX, RNOLD, RNORM
           GO TO 290
  280      IF (IPRINT .LT. 0) WRITE (IOUT,550) RELAX, ANORM, ANFIX,
     1           RNOLD, RNORM
  290      ICOR = 0
C
C...       check for monotonic decrease in  delz and deldmz.
C
           IF (ANFIX.LT.PRECIS .OR. RNORM.LT.PRECIS)  GO TO 390
           IF ( ANFIX .GT. ANORM )                    GO TO 300
C
C...       we have a decrease.
C...       if  dqz  and dqdmz  small, check for convergence
C
           IF ( ANFIX .LE. CHECK )                    GO TO 350
C
C...       correct the predicted  relax  unless the corrected
C...       value is within 10 percent of the predicted one.
C
           IF ( IPRED .NE. 1 )                        GO TO 170
  300      IF ( ITER .GE. LIMIT )                     GO TO 430
C
C...       correct the relaxation factor.
C
           IPRED = 0
           ARG = (ANFIX/ANORM - 1.D0) / RELAX + 1.D0
           IF ( ARG .LT. 0.D0 )                       GO TO 170
           IF (ARG .LE. .25D0*RELAX+.125D0*RELAX**2)  GO TO 310
           FACTOR = -1.D0 + DSQRT (1.D0+8.D0 * ARG)
           IF ( DABS(FACTOR-1.D0) .LT. .1D0*FACTOR )  GO TO 170
           IF ( FACTOR .LT. 0.5D0 )  FACTOR = 0.5D0
           RELAX = RELAX / FACTOR
           GO TO 320
  310      IF ( RELAX .GE. .9D0 )                     GO TO 170
           RELAX = 1.D0
  320      ICOR = 1
           IF ( RELAX .LT. RELMIN )                   GO TO 440
           FACT = RELAX - RLXOLD
           DO 330 I = 1, NZ
            Z(I) = Z(I)  +  FACT * DELZ(I)
  330      CONTINUE
           DO 340 I = 1, NDMZ
             DMZ(I) = DMZ(I)  +  FACT * DELDMZ(I)
  340      CONTINUE
           RLXOLD = RELAX
           GO TO 250
C
C...       check convergence (iconv = 0).
C
  350      CONTINUE
           DO 360 IT = 1, NTOL
             INZ = LTOL(IT)
             DO 360 IZ = INZ, NZ, MSTAR
               IF ( DABS(DQZ(IZ)) .GT.
     1         TOLIN(IT) * (DABS(Z(IZ)) + 1.D0) )   GO TO 170
  360      CONTINUE
C
C...       convergence obtained
C
           IF ( IPRINT .LT. 1 )  WRITE (IOUT,560) ITER
C
C...       since convergence obtained, update  z and dmz  with term
C...       from the fixed jacobian iteration.
C
           DO 370 I = 1, NZ
             Z(I) = Z(I)  +  DQZ(I)
  370      CONTINUE
           DO 380 I = 1, NDMZ
             DMZ(I) = DMZ(I)  +  DQDMZ(I)
  380      CONTINUE
  390      IF ( (ANFIX .LT. PRECIS .OR. RNORM .LT. PRECIS)
     1          .AND. IPRINT .LT. 1 )  WRITE (IOUT,560) ITER
           ICONV = 1
           IF ( ICARE .EQ. (-1) )  ICARE = 0
C
C...       if full output has been requested, print values of the
C...       solution components   z  at the meshpoints.
C
  400      IF ( IPRINT .GE. 0 )                     GO TO 420
           DO 410 J = 1, MSTAR
             WRITE(IOUT,610) J
  410      WRITE(IOUT,620) (Z(LJ), LJ = J, NZ, MSTAR)
C
C...       check for error tolerance satisfaction
C
  420      IFIN = 1
           IF (IMESH .EQ. 2) CALL ERRCHK (XI, Z, DMZ, VALSTR, IFIN)
           IF ( IMESH .EQ. 1 .OR.
     1          IFIN .EQ. 0 .AND. ICARE .NE. 2)     GO TO 460
           IFLAG = 1
           RETURN
C
C...       diagnostics for failure of nonlinear iteration.
C
  430      IF ( IPRINT .LT. 1 )  WRITE (IOUT,570) ITER
           GO TO 450
  440      IF( IPRINT .LT. 1 )  WRITE(IOUT,580) RELAX, RELMIN
  450      IFLAG = -2
           NOCONV = NOCONV + 1
           IF ( ICARE .EQ. 2 .AND. NOCONV .GT. 1 )  RETURN
           IF ( ICARE .EQ. 0 )  ICARE = -1
C
C...       update old mesh
C
  460      NP1 = N + 1
           DO 470 I = 1, NP1
  470        XIOLD(I) = XI(I)
           NOLD = N
C
C...       pick a new mesh
C...       check safeguards for mesh refinement
C
           IMESH = 1
           IF ( ICONV .EQ. 0 .OR. MSHNUM .GE. MSHLMT
     1                       .OR. MSHALT .GE. MSHLMT )  IMESH = 2
           IF ( MSHALT .GE. MSHLMT .AND.
     1          MSHNUM .LT. MSHLMT )  MSHALT = 1
           CALL NEWMSH (IMESH, XI, XIOLD, Z, DMZ, VALSTR,
     1                  SLOPE, ACCUM, NFXPNT, FIXPNT)
C
C...       exit if expected n is too large (but may try n=nmax once)
C
           IF ( N .LE. NMAX )                       GO TO 480
           N = N / 2
           IFLAG = -1
           IF ( ICONV .EQ. 0 .AND. IPRINT .LT. 1 )  WRITE (IOUT,590)
           IF ( ICONV .EQ. 1 .AND. IPRINT .LT. 1 )  WRITE (IOUT,600)
           RETURN
  480      IF ( ICONV .EQ. 0 )  IMESH = 1
           IF ( ICARE .EQ. 1 )  ICONV = 0
      GO TO 20
C     ---------------------------------------------------------------
  490 FORMAT(//35H THE GLOBAL BVP-MATRIX IS SINGULAR )
  495 FORMAT(//40H A LOCAL ELIMINATION MATRIX IS SINGULAR )
  500 FORMAT(/30H FULL DAMPED NEWTON ITERATION,)
  510 FORMAT(13H ITERATION = , I3, 15H  NORM (RHS) = , D10.2)
  520 FORMAT(13H ITERATION = ,I3,22H  RELAXATION FACTOR = ,D10.2
     1       /33H NORM OF SCALED RHS CHANGES FROM ,D10.2,3H TO,D10.2
     2       /33H NORM   OF   RHS  CHANGES  FROM  ,D10.2,3H TO,D10.2,
     2       D10.2)
  530 FORMAT(/27H FIXED JACOBIAN ITERATIONS,)
  540 FORMAT(/35H SWITCH TO DAMPED NEWTON ITERATION,)
  550 FORMAT(40H RELAXATION FACTOR CORRECTED TO RELAX = , D10.2
     1       /33H NORM OF SCALED RHS CHANGES FROM ,D10.2,3H TO,D10.2
     2       /33H NORM   OF   RHS  CHANGES  FROM  ,D10.2,3H TO,D10.2
     2       ,D10.2)
  560 FORMAT(/18H CONVERGENCE AFTER , I3,11H ITERATIONS /)
  570 FORMAT(/22H NO CONVERGENCE AFTER , I3, 11H ITERATIONS/)
  580 FORMAT(/37H NO CONVERGENCE.  RELAXATION FACTOR =,D10.3
     1       ,24H IS TOO SMALL (LESS THAN, D10.3, 1H)/)
  590 FORMAT(18H  (NO CONVERGENCE) )
  600 FORMAT(50H  (PROBABLY TOLERANCES TOO STRINGENT, OR NMAX TOO
     1       ,6HSMALL) )
  610 FORMAT( 19H MESH VALUES FOR Z(, I2, 2H), )
  620 FORMAT(1H , 8D15.7)
      END

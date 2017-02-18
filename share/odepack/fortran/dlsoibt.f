*DECK DLSOIBT
      SUBROUTINE DLSOIBT (RES, ADDA, JAC, NEQ, Y, YDOTI, T, TOUT, ITOL,
     1  RTOL, ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW, MF )
      EXTERNAL RES, ADDA, JAC
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW, MF
      DOUBLE PRECISION Y, YDOTI, T, TOUT, RTOL, ATOL, RWORK
      DIMENSION NEQ(*), Y(*), YDOTI(*), RTOL(*), ATOL(*), RWORK(LRW),
     1          IWORK(LIW)
C-----------------------------------------------------------------------
C This is the 18 November 2003 version of
C DLSOIBT: Livermore Solver for Ordinary differential equations given
C          in Implicit form, with Block-Tridiagonal Jacobian treatment.
C
C This version is in double precision.
C
C DLSOIBT solves the initial value problem for linearly implicit
C systems of first order ODEs,
C     A(t,y) * dy/dt = g(t,y) ,  where A(t,y) is a square matrix,
C or, in component form,
C     ( a   * ( dy / dt ))  + ... +  ( a     * ( dy   / dt ))  =
C        i,1      1                     i,NEQ      NEQ
C
C      =   g ( t, y , y ,..., y    )   ( i = 1,...,NEQ )
C           i      1   2       NEQ
C
C If A is singular, this is a differential-algebraic system.
C
C DLSOIBT is a variant version of the DLSODI package, for the case where
C the matrices A, dg/dy, and d(A*s)/dy are all block-tridiagonal.
C-----------------------------------------------------------------------
C Reference:
C     Alan C. Hindmarsh,  ODEPACK, A Systematized Collection of ODE
C     Solvers, in Scientific Computing,  R. S. Stepleman et al. (Eds.),
C     North-Holland, Amsterdam, 1983, pp. 55-64.
C-----------------------------------------------------------------------
C Authors:       Alan C. Hindmarsh and Jeffrey F. Painter
C                Center for Applied Scientific Computing, L-561
C                Lawrence Livermore National Laboratory
C                Livermore, CA 94551
C and
C                Charles S. Kenney
C formerly at:   Naval Weapons Center
C                China Lake, CA 93555
C-----------------------------------------------------------------------
C Summary of Usage.
C
C Communication between the user and the DLSOIBT package, for normal
C situations, is summarized here.  This summary describes only a subset
C of the full set of options available.  See the full description for
C details, including optional communication, nonstandard options,
C and instructions for special situations.  See also the example
C problem (with program and output) following this summary.
C
C A. First, provide a subroutine of the form:
C               SUBROUTINE RES (NEQ, T, Y, S, R, IRES)
C               DOUBLE PRECISION T, Y(*), S(*), R(*)
C which computes the residual function
C     r = g(t,y)  -  A(t,y) * s ,
C as a function of t and the vectors y and s.  (s is an internally
C generated approximation to dy/dt.)  The arrays Y and S are inputs
C to the RES routine and should not be altered.  The residual
C vector is to be stored in the array R.  The argument IRES should be
C ignored for casual use of DLSOIBT.  (For uses of IRES, see the
C paragraph on RES in the full description below.)
C
C B. Next, identify the block structure of the matrices A = A(t,y) and
C dr/dy.  DLSOIBT must deal internally with a linear combination, P, of
C these two matrices.  The matrix P (hence both A and dr/dy) must have
C a block-tridiagonal form with fixed structure parameters
C     MB = block size, MB .ge. 1, and
C     NB = number of blocks in each direction, NB .ge. 4,
C with MB*NB = NEQ.  In each of the NB block-rows of the matrix P
C (each consisting of MB consecutive rows), the nonzero elements are
C to lie in three consecutive MB by MB blocks.  In block-rows
C 2 through NB - 1, these are centered about the main diagonal.
C in block-rows 1 and NB, they are the diagonal blocks and the two
C blocks adjacent to the diagonal block.  (Thus block positions (1,3)
C and (NB,NB-2) can be nonzero.)
C Alternatively, P (hence A and dr/dy) may be only approximately
C equal to matrices with this form, and DLSOIBT should still succeed.
C The block-tridiagonal matrix P is described by three arrays,
C each of size MB by MB by NB:
C     PA = array of diagonal blocks,
C     PB = array of superdiagonal (and one subdiagonal) blocks, and
C     PC = array of subdiagonal (and one superdiagonal) blocks.
C Specifically, the three MB by MB blocks in the k-th block-row of P
C are stored in (reading across):
C     PC(*,*,k) = block to the left of the diagonal block,
C     PA(*,*,k) = diagonal block, and
C     PB(*,*,k) = block to the right of the diagonal block,
C except for k = 1, where the three blocks (reading across) are
C     PA(*,*,1) (= diagonal block), PB(*,*,1), and PC(*,*,1),
C and k = NB, where they are
C     PB(*,*,NB), PC(*,*,NB), and PA(*,*,NB) (= diagonal block).
C (Each asterisk * stands for an index that ranges from 1 to MB.)
C
C C. You must also provide a subroutine of the form:
C     SUBROUTINE ADDA (NEQ, T, Y, MB, NB, PA, PB, PC)
C     DOUBLE PRECISION T, Y(*), PA(MB,MB,NB), PB(MB,MB,NB), PC(MB,MB,NB)
C which adds the nonzero blocks of the matrix A = A(t,y) to the
C contents of the arrays PA, PB, and PC, following the structure
C description in Paragraph B above.
C T and the Y array are input and should not be altered.
C Thus the affect of ADDA should be the following:
C     DO 30 K = 1,NB
C       DO 20 J = 1,MB
C         DO 10 I = 1,MB
C           PA(I,J,K) = PA(I,J,K) +
C             ( (I,J) element of K-th diagonal block of A)
C           PB(I,J,K) = PB(I,J,K) +
C             ( (I,J) element of block in block position (K,K+1) of A,
C             or in block position (NB,NB-2) if K = NB)
C           PC(I,J,K) = PC(I,J,K) +
C             ( (I,J) element of block in block position (K,K-1) of A,
C             or in block position (1,3) if K = 1)
C 10        CONTINUE
C 20      CONTINUE
C 30    CONTINUE
C
C D. For the sake of efficiency, you are encouraged to supply the
C Jacobian matrix dr/dy in closed form, where r = g(t,y) - A(t,y)*s
C (s = a fixed vector) as above.  If dr/dy is being supplied,
C use MF = 21, and provide a subroutine of the form:
C     SUBROUTINE JAC (NEQ, T, Y, S, MB, NB, PA, PB, PC)
C     DOUBLE PRECISION T, Y(*), S(*), PA(MB,MB,NB), PB(MB,MB,NB),
C    1                 PC(MB,MB,NB)
C which computes dr/dy as a function of t, y, and s.  Here T, Y, and
C S are inputs, and the routine is to load dr/dy into PA, PB, PC,
C according to the structure description in Paragraph B above.
C That is, load the diagonal blocks into PA, the superdiagonal blocks
C (and block (NB,NB-2) ) into PB, and the subdiagonal blocks (and
C block (1,3) ) into PC.  The blocks in block-row k of dr/dy are to
C be loaded into PA(*,*,k), PB(*,*,k), and PC(*,*,k).
C     Only nonzero elements need be loaded, and the indexing
C of PA, PB, and PC is the same as in the ADDA routine.
C     Note that if A is independent of Y (or this dependence
C is weak enough to be ignored) then JAC is to compute dg/dy.
C     If it is not feasible to provide a JAC routine, use
C MF = 22, and DLSOIBT will compute an approximate Jacobian
C internally by difference quotients.
C
C E. Next decide whether or not to provide the initial value of the
C derivative vector dy/dt.  If the initial value of A(t,y) is
C nonsingular (and not too ill-conditioned), you may let DLSOIBT compute
C this vector (ISTATE = 0).  (DLSOIBT will solve the system A*s = g for
C s, with initial values of A and g.)  If A(t,y) is initially
C singular, then the system is a differential-algebraic system, and
C you must make use of the particular form of the system to compute the
C initial values of y and dy/dt.  In that case, use ISTATE = 1 and
C load the initial value of dy/dt into the array YDOTI.
C The input array YDOTI and the initial Y array must be consistent with
C the equations A*dy/dt = g.  This implies that the initial residual
C r = g(t,y) - A(t,y)*YDOTI  must be approximately zero.
C
C F. Write a main program which calls Subroutine DLSOIBT once for
C each point at which answers are desired.  This should also provide
C for possible use of logical unit 6 for output of error messages by
C DLSOIBT.  on the first call to DLSOIBT, supply arguments as follows:
C RES    = name of user subroutine for residual function r.
C ADDA   = name of user subroutine for computing and adding A(t,y).
C JAC    = name of user subroutine for Jacobian matrix dr/dy
C          (MF = 21).  If not used, pass a dummy name.
C Note: the names for the RES and ADDA routines and (if used) the
C        JAC routine must be declared External in the calling program.
C NEQ    = number of scalar equations in the system.
C Y      = array of initial values, of length NEQ.
C YDOTI  = array of length NEQ (containing initial dy/dt if ISTATE = 1).
C T      = the initial value of the independent variable.
C TOUT   = first point where output is desired (.ne. T).
C ITOL   = 1 or 2 according as ATOL (below) is a scalar or array.
C RTOL   = relative tolerance parameter (scalar).
C ATOL   = absolute tolerance parameter (scalar or array).
C          the estimated local error in y(i) will be controlled so as
C          to be roughly less (in magnitude) than
C             EWT(i) = RTOL*ABS(Y(i)) + ATOL     if ITOL = 1, or
C             EWT(i) = RTOL*ABS(Y(i)) + ATOL(i)  if ITOL = 2.
C          Thus the local error test passes if, in each component,
C          either the absolute error is less than ATOL (or ATOL(i)),
C          or the relative error is less than RTOL.
C          Use RTOL = 0.0 for pure absolute error control, and
C          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
C          control.  Caution: Actual (global) errors may exceed these
C          local tolerances, so choose them conservatively.
C ITASK  = 1 for normal computation of output values of y at t = TOUT.
C ISTATE = integer flag (input and output).  Set ISTATE = 1 if the
C          initial dy/dt is supplied, and 0 otherwise.
C IOPT   = 0 to indicate no optional inputs used.
C RWORK  = real work array of length at least:
C             22 + 9*NEQ + 3*MB*MB*NB        for MF = 21 or 22.
C LRW    = declared length of RWORK (in user's dimension).
C IWORK  = integer work array of length at least 20 + NEQ.
C          Input in IWORK(1) the block size MB and in IWORK(2) the
C          number NB of blocks in each direction along the matrix A.
C          These must satisfy  MB .ge. 1, NB .ge. 4, and MB*NB = NEQ.
C LIW    = declared length of IWORK (in user's dimension).
C MF     = method flag.  Standard values are:
C          21 for a user-supplied Jacobian.
C          22 for an internally generated Jacobian.
C          For other choices of MF, see the paragraph on MF in
C          the full description below.
C Note that the main program must declare arrays Y, YDOTI, RWORK, IWORK,
C and possibly ATOL.
C
C G. The output from the first call (or any call) is:
C      Y = array of computed values of y(t) vector.
C      T = corresponding value of independent variable (normally TOUT).
C ISTATE = 2  if DLSOIBT was successful, negative otherwise.
C          -1 means excess work done on this call (check all inputs).
C          -2 means excess accuracy requested (tolerances too small).
C          -3 means illegal input detected (see printed message).
C          -4 means repeated error test failures (check all inputs).
C          -5 means repeated convergence failures (perhaps bad Jacobian
C             supplied or wrong choice of tolerances).
C          -6 means error weight became zero during problem. (Solution
C             component i vanished, and ATOL or ATOL(i) = 0.)
C          -7 cannot occur in casual use.
C          -8 means DLSOIBT was unable to compute the initial dy/dt.
C             In casual use, this means A(t,y) is initially singular.
C             Supply YDOTI and use ISTATE = 1 on the first call.
C
C  If DLSOIBT returns ISTATE = -1, -4, or -5, then the output of
C  DLSOIBT also includes YDOTI = array containing residual vector
C  r = g - A * dy/dt  evaluated at the current t, y, and dy/dt.
C
C H. To continue the integration after a successful return, simply
C reset TOUT and call DLSOIBT again.  No other parameters need be reset.
C
C-----------------------------------------------------------------------
C Example Problem.
C
C The following is an example problem, with the coding needed
C for its solution by DLSOIBT.  The problem comes from the partial
C differential equation (the Burgers equation)
C   du/dt  =  - u * du/dx  +  eta * d**2 u/dx**2,   eta = .05,
C on -1 .le. x .le. 1.  The boundary conditions are
C   du/dx = 0  at x = -1 and at x = 1.
C The initial profile is a square wave,
C   u = 1 in ABS(x) .lt. .5,  u = .5 at ABS(x) = .5,  u = 0 elsewhere.
C The PDE is discretized in x by a simplified Galerkin method,
C using piecewise linear basis functions, on a grid of 40 intervals.
C The equations at x = -1 and 1 use a 3-point difference approximation
C for the right-hand side.  The result is a system A * dy/dt = g(y),
C of size NEQ = 41, where y(i) is the approximation to u at x = x(i),
C with x(i) = -1 + (i-1)*delx, delx = 2/(NEQ-1) = .05.  The individual
C equations in the system are
C   dy(1)/dt = ( y(3) - 2*y(2) + y(1) ) * eta / delx**2,
C   dy(NEQ)/dt = ( y(NEQ-2) - 2*y(NEQ-1) + y(NEQ) ) * eta / delx**2,
C and for i = 2, 3, ..., NEQ-1,
C   (1/6) dy(i-1)/dt + (4/6) dy(i)/dt + (1/6) dy(i+1)/dt
C       = ( y(i-1)**2 - y(i+1)**2 ) / (4*delx)
C         + ( y(i+1) - 2*y(i) + y(i-1) ) * eta / delx**2.
C The following coding solves the problem with MF = 21, with output
C of solution statistics at t = .1, .2, .3, and .4, and of the
C solution vector at t = .4.  Here the block size is just MB = 1.
C
C     EXTERNAL RESID, ADDABT, JACBT
C     DOUBLE PRECISION ATOL, RTOL, RWORK, T, TOUT, Y, YDOTI
C     DIMENSION Y(41), YDOTI(41), RWORK(514), IWORK(61)
C     NEQ = 41
C     DO 10 I = 1,NEQ
C  10   Y(I) = 0.0
C     Y(11) = 0.5
C     DO 20 I = 12,30
C  20   Y(I) = 1.0
C     Y(31) = 0.5
C     T = 0.0
C     TOUT = 0.1
C     ITOL = 1
C     RTOL = 1.0D-4
C     ATOL = 1.0D-5
C     ITASK = 1
C     ISTATE = 0
C     IOPT = 0
C     LRW = 514
C     LIW = 61
C     IWORK(1) = 1
C     IWORK(2) = NEQ
C     MF = 21
C     DO 40 IO = 1,4
C       CALL DLSOIBT (RESID, ADDABT, JACBT, NEQ, Y, YDOTI, T, TOUT,
C    1     ITOL,RTOL,ATOL, ITASK, ISTATE, IOPT, RWORK,LRW,IWORK,LIW, MF)
C       WRITE (6,30) T, IWORK(11), IWORK(12), IWORK(13)
C  30   FORMAT(' At t =',F5.2,'   No. steps =',I4,'  No. r-s =',I4,
C    1         '  No. J-s =',I3)
C       IF (ISTATE .NE. 2)  GO TO 90
C       TOUT = TOUT + 0.1
C  40   CONTINUE
C     WRITE(6,50) (Y(I),I=1,NEQ)
C  50 FORMAT(/' Final solution values..'/9(5D12.4/))
C     STOP
C  90 WRITE(6,95) ISTATE
C  95 FORMAT(///' Error halt.. ISTATE =',I3)
C     STOP
C     END
C
C     SUBROUTINE RESID (N, T, Y, S, R, IRES)
C     DOUBLE PRECISION T, Y, S, R, ETA, DELX, EODSQ
C     DIMENSION Y(N), S(N), R(N)
C     DATA ETA/0.05/, DELX/0.05/
C     EODSQ = ETA/DELX**2
C     R(1) = EODSQ*(Y(3) - 2.0*Y(2) + Y(1)) - S(1)
C     NM1 = N - 1
C     DO 10 I = 2,NM1
C       R(I) = (Y(I-1)**2 - Y(I+1)**2)/(4.0*DELX)
C    1        + EODSQ*(Y(I+1) - 2.0*Y(I) + Y(I-1))
C    2        - (S(I-1) + 4.0*S(I) + S(I+1))/6.0
C  10   CONTINUE
C     R(N) = EODSQ*(Y(N-2) - 2.0*Y(NM1) + Y(N)) - S(N)
C     RETURN
C     END
C
C     SUBROUTINE ADDABT (N, T, Y, MB, NB, PA, PB, PC)
C     DOUBLE PRECISION T, Y, PA, PB, PC
C     DIMENSION Y(N), PA(MB,MB,NB), PB(MB,MB,NB), PC(MB,MB,NB)
C     PA(1,1,1) = PA(1,1,1) + 1.0
C     NM1 = N - 1
C     DO 10 K = 2,NM1
C       PA(1,1,K) = PA(1,1,K) + (4.0/6.0)
C       PB(1,1,K) = PB(1,1,K) + (1.0/6.0)
C       PC(1,1,K) = PC(1,1,K) + (1.0/6.0)
C  10   CONTINUE
C     PA(1,1,N) = PA(1,1,N) + 1.0
C     RETURN
C     END
C
C     SUBROUTINE JACBT (N, T, Y, S, MB, NB, PA, PB, PC)
C     DOUBLE PRECISION T, Y, S, PA, PB, PC, ETA, DELX, EODSQ
C     DIMENSION Y(N), S(N), PA(MB,MB,NB),PB(MB,MB,NB),PC(MB,MB,NB)
C     DATA ETA/0.05/, DELX/0.05/
C     EODSQ = ETA/DELX**2
C     PA(1,1,1) = EODSQ
C     PB(1,1,1) = -2.0*EODSQ
C     PC(1,1,1) = EODSQ
C     DO 10 K = 2,N
C       PA(1,1,K) = -2.0*EODSQ
C       PB(1,1,K) = -Y(K+1)*(0.5/DELX) + EODSQ
C       PC(1,1,K) = Y(K-1)*(0.5/DELX) + EODSQ
C  10   CONTINUE
C     PB(1,1,N) = EODSQ
C     PC(1,1,N) = -2.0*EODSQ
C     PA(1,1,N) = EODSQ
C     RETURN
C     END
C
C The output of this program (on a CDC-7600 in single precision)
C is as follows:
C
C At t = 0.10   No. steps =  35  No. r-s =  45  No. J-s =  9
C At t = 0.20   No. steps =  43  No. r-s =  54  No. J-s = 10
C At t = 0.30   No. steps =  48  No. r-s =  60  No. J-s = 11
C At t = 0.40   No. steps =  51  No. r-s =  64  No. J-s = 12
C
C Final solution values..
C  1.2747e-02  1.1997e-02  1.5560e-02  2.3767e-02  3.7224e-02
C  5.6646e-02  8.2645e-02  1.1557e-01  1.5541e-01  2.0177e-01
C  2.5397e-01  3.1104e-01  3.7189e-01  4.3530e-01  5.0000e-01
C  5.6472e-01  6.2816e-01  6.8903e-01  7.4612e-01  7.9829e-01
C  8.4460e-01  8.8438e-01  9.1727e-01  9.4330e-01  9.6281e-01
C  9.7632e-01  9.8426e-01  9.8648e-01  9.8162e-01  9.6617e-01
C  9.3374e-01  8.7535e-01  7.8236e-01  6.5321e-01  5.0003e-01
C  3.4709e-01  2.1876e-01  1.2771e-01  7.3671e-02  5.0642e-02
C  5.4496e-02
C
C-----------------------------------------------------------------------
C Full Description of User Interface to DLSOIBT.
C
C The user interface to DLSOIBT consists of the following parts.
C
C 1.   The call sequence to Subroutine DLSOIBT, which is a driver
C      routine for the solver.  This includes descriptions of both
C      the call sequence arguments and of user-supplied routines.
C      Following these descriptions is a description of
C      optional inputs available through the call sequence, and then
C      a description of optional outputs (in the work arrays).
C
C 2.   Descriptions of other routines in the DLSOIBT package that may be
C      (optionally) called by the user.  These provide the ability to
C      alter error message handling, save and restore the internal
C      Common, and obtain specified derivatives of the solution y(t).
C
C 3.   Descriptions of Common blocks to be declared in overlay
C      or similar environments, or to be saved when doing an interrupt
C      of the problem and continued solution later.
C
C 4.   Description of two routines in the DLSOIBT package, either of
C      which the user may replace with his/her own version, if desired.
C      These relate to the measurement of errors.
C
C-----------------------------------------------------------------------
C Part 1.  Call Sequence.
C
C The call sequence parameters used for input only are
C     RES, ADDA, JAC, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK,
C     IOPT, LRW, LIW, MF,
C and those used for both input and output are
C     Y, T, ISTATE, YDOTI.
C The work arrays RWORK and IWORK are also used for additional and
C optional inputs and optional outputs.  (The term output here refers
C to the return from Subroutine DLSOIBT to the user's calling program.)
C
C The legality of input parameters will be thoroughly checked on the
C initial call for the problem, but not checked thereafter unless a
C change in input parameters is flagged by ISTATE = 3 on input.
C
C The descriptions of the call arguments are as follows.
C
C RES    = the name of the user-supplied subroutine which supplies
C          the residual vector for the ODE system, defined by
C            r = g(t,y) - A(t,y) * s
C          as a function of the scalar t and the vectors
C          s and y (s approximates dy/dt).  This subroutine
C          is to have the form
C              SUBROUTINE RES (NEQ, T, Y, S, R, IRES)
C              DOUBLE PRECISION T, Y(*), S(*), R(*)
C          where NEQ, T, Y, S, and IRES are input, and R and
C          IRES are output. Y, S, and R are arrays of length NEQ.
C             On input, IRES indicates how DLSOIBT will use the
C          returned array R, as follows:
C             IRES = 1  means that DLSOIBT needs the full residual,
C                       r = g - A*s, exactly.
C             IRES = -1 means that DLSOIBT is using R only to compute
C                       the Jacobian dr/dy by difference quotients.
C          The RES routine can ignore IRES, or it can omit some terms
C          if IRES = -1.  If A does not depend on y, then RES can
C          just return R = g when IRES = -1.  If g - A*s contains other
C          additive terms that are independent of y, these can also be
C          dropped, if done consistently, when IRES = -1.
C             The subroutine should set the flag IRES if it
C          encounters a halt condition or illegal input.
C          Otherwise, it should not reset IRES.  On output,
C             IRES = 1 or -1 represents a normal return, and
C          DLSOIBT continues integrating the ODE.  Leave IRES
C          unchanged from its input value.
C             IRES = 2 tells DLSOIBT to immediately return control
C          to the calling program, with ISTATE = 3.  This lets
C          the calling program change parameters of the problem
C          if necessary.
C             IRES = 3 represents an error condition (for example, an
C          illegal value of y).  DLSOIBT tries to integrate the system
C          without getting IRES = 3 from RES.  If it cannot, DLSOIBT
C          returns with ISTATE = -7 or -1.
C             On an DLSOIBT return with ISTATE = 3, -1, or -7, the
C          values of T and Y returned correspond to the last point
C          reached successfully without getting the flag IRES = 2 or 3.
C             The flag values IRES = 2 and 3 should not be used to
C          handle switches or root-stop conditions.  This is better
C          done by calling DLSOIBT in a one-step mode and checking the
C          stopping function for a sign change at each step.
C             If quantities computed in the RES routine are needed
C          externally to DLSOIBT, an extra call to RES should be made
C          for this purpose, for consistent and accurate results.
C          To get the current dy/dt for the S argument, use DINTDY.
C             RES must be declared External in the calling
C          program. See note below for more about RES.
C
C ADDA   = the name of the user-supplied subroutine which adds the
C          matrix A = A(t,y) to another matrix, P, stored in
C          block-tridiagonal form.  This routine is to have the form
C               SUBROUTINE ADDA (NEQ, T, Y, MB, NB, PA, PB, PC)
C               DOUBLE PRECISION T, Y(*), PA(MB,MB,NB), PB(MB,MB,NB),
C              1                 PC(MB,MB,NB)
C          where NEQ, T, Y, MB, NB, and the arrays PA, PB, and PC
C          are input, and the arrays PA, PB, and PC are output.
C          Y is an array of length NEQ, and the arrays PA, PB, PC
C          are all MB by MB by NB.
C             Here a block-tridiagonal structure is assumed for A(t,y),
C          and also for the matrix P to which A is added here,
C          as described in Paragraph B of the Summary of Usage above.
C          Thus the affect of ADDA should be the following:
C               DO 30 K = 1,NB
C                 DO 20 J = 1,MB
C                   DO 10 I = 1,MB
C                     PA(I,J,K) = PA(I,J,K) +
C                       ( (I,J) element of K-th diagonal block of A)
C                     PB(I,J,K) = PB(I,J,K) +
C                       ( (I,J) element of block (K,K+1) of A,
C                       or block (NB,NB-2) if K = NB)
C                     PC(I,J,K) = PC(I,J,K) +
C                       ( (I,J) element of block (K,K-1) of A,
C                       or block (1,3) if K = 1)
C           10        CONTINUE
C           20      CONTINUE
C           30    CONTINUE
C             ADDA must be declared External in the calling program.
C          See note below for more information about ADDA.
C
C JAC    = the name of the user-supplied subroutine which supplies
C          the Jacobian matrix, dr/dy, where r = g - A*s.  JAC is
C          required if MITER = 1.  Otherwise a dummy name can be
C          passed.  This subroutine is to have the form
C               SUBROUTINE JAC (NEQ, T, Y, S, MB, NB, PA, PB, PC)
C               DOUBLE PRECISION T, Y(*), S(*), PA(MB,MB,NB),
C              1                 PB(MB,MB,NB), PC(MB,MB,NB)
C          where NEQ, T, Y, S, MB, NB, and the arrays PA, PB, and PC
C          are input, and the arrays PA, PB, and PC are output.
C          Y and S are arrays of length NEQ, and the arrays PA, PB, PC
C          are all MB by MB by NB.
C          PA, PB, and PC are to be loaded with partial derivatives
C          (elements of the Jacobian matrix) on output, in terms of the
C          block-tridiagonal structure assumed, as described
C          in Paragraph B of the Summary of Usage above.
C          That is, load the diagonal blocks into PA, the
C          superdiagonal blocks (and block (NB,NB-2) ) into PB, and
C          the subdiagonal blocks (and block (1,3) ) into PC.
C          The blocks in block-row k of dr/dy are to be loaded into
C          PA(*,*,k), PB(*,*,k), and PC(*,*,k).
C          Thus the affect of JAC should be the following:
C               DO 30 K = 1,NB
C                 DO 20 J = 1,MB
C                   DO 10 I = 1,MB
C                     PA(I,J,K) = ( (I,J) element of
C                       K-th diagonal block of dr/dy)
C                     PB(I,J,K) = ( (I,J) element of block (K,K+1)
C                       of dr/dy, or block (NB,NB-2) if K = NB)
C                     PC(I,J,K) = ( (I,J) element of block (K,K-1)
C                       of dr/dy, or block (1,3) if K = 1)
C           10        CONTINUE
C           20      CONTINUE
C           30    CONTINUE
C               PA, PB, and PC are preset to zero by the solver,
C          so that only the nonzero elements need be loaded by JAC.
C          Each call to JAC is preceded by a call to RES with the same
C          arguments NEQ, T, Y, and S.  Thus to gain some efficiency,
C          intermediate quantities shared by both calculations may be
C          saved in a user Common block by RES and not recomputed by JAC
C          if desired.  Also, JAC may alter the Y array, if desired.
C               JAC need not provide dr/dy exactly.  A crude
C          approximation will do, so that DLSOIBT may be used when
C          A and dr/dy are not really block-tridiagonal, but are close
C          to matrices that are.
C               JAC must be declared External in the calling program.
C               See note below for more about JAC.
C
C    Note on RES, ADDA, and JAC:
C          These subroutines may access user-defined quantities in
C          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in the subroutines) and/or Y has length
C          exceeding NEQ(1).  However, these routines should not alter
C          NEQ(1), Y(1),...,Y(NEQ) or any other input variables.
C          See the descriptions of NEQ and Y below.
C
C NEQ    = the size of the system (number of first order ordinary
C          differential equations or scalar algebraic equations).
C          Used only for input.
C          NEQ may be decreased, but not increased, during the problem.
C          If NEQ is decreased (with ISTATE = 3 on input), the
C          remaining components of Y should be left undisturbed, if
C          these are to be accessed in RES, ADDA, or JAC.
C
C          Normally, NEQ is a scalar, and it is generally referred to
C          as a scalar in this user interface description.  However,
C          NEQ may be an array, with NEQ(1) set to the system size.
C          (The DLSOIBT package accesses only NEQ(1).)  In either case,
C          this parameter is passed as the NEQ argument in all calls
C          to RES, ADDA, and JAC.  Hence, if it is an array,
C          locations NEQ(2),... may be used to store other integer data
C          and pass it to RES, ADDA, or JAC.  Each such subroutine
C          must include NEQ in a Dimension statement in that case.
C
C Y      = a real array for the vector of dependent variables, of
C          length NEQ or more.  Used for both input and output on the
C          first call (ISTATE = 0 or 1), and only for output on other
C          calls.  On the first call, Y must contain the vector of
C          initial values.  On output, Y contains the computed solution
C          vector, evaluated at t.  If desired, the Y array may be used
C          for other purposes between calls to the solver.
C
C          This array is passed as the Y argument in all calls to RES,
C          ADDA, and JAC.  Hence its length may exceed NEQ,
C          and locations Y(NEQ+1),... may be used to store other real
C          data and pass it to RES, ADDA, or JAC.  (The DLSOIBT
C          package accesses only Y(1),...,Y(NEQ). )
C
C YDOTI  = a real array for the initial value of the vector
C          dy/dt and for work space, of dimension at least NEQ.
C
C          On input:
C            If ISTATE = 0 then DLSOIBT will compute the initial value
C          of dy/dt, if A is nonsingular.  Thus YDOTI will
C          serve only as work space and may have any value.
C            If ISTATE = 1 then YDOTI must contain the initial value
C          of dy/dt.
C            If ISTATE = 2 or 3 (continuation calls) then YDOTI
C          may have any value.
C            Note: If the initial value of A is singular, then
C          DLSOIBT cannot compute the initial value of dy/dt, so
C          it must be provided in YDOTI, with ISTATE = 1.
C
C          On output, when DLSOIBT terminates abnormally with ISTATE =
C          -1, -4, or -5, YDOTI will contain the residual
C          r = g(t,y) - A(t,y)*(dy/dt).  If r is large, t is near
C          its initial value, and YDOTI is supplied with ISTATE = 1,
C          there may have been an incorrect input value of
C          YDOTI = dy/dt, or the problem (as given to DLSOIBT)
C          may not have a solution.
C
C          If desired, the YDOTI array may be used for other
C          purposes between calls to the solver.
C
C T      = the independent variable.  On input, T is used only on the
C          first call, as the initial point of the integration.
C          On output, after each call, T is the value at which a
C          computed solution y is evaluated (usually the same as TOUT).
C          On an error return, T is the farthest point reached.
C
C TOUT   = the next value of t at which a computed solution is desired.
C          Used only for input.
C
C          When starting the problem (ISTATE = 0 or 1), TOUT may be
C          equal to T for one call, then should .ne. T for the next
C          call.  For the initial T, an input value of TOUT .ne. T is
C          used in order to determine the direction of the integration
C          (i.e. the algebraic sign of the step sizes) and the rough
C          scale of the problem.  Integration in either direction
C          (forward or backward in t) is permitted.
C
C          If ITASK = 2 or 5 (one-step modes), TOUT is ignored after
C          the first call (i.e. the first call with TOUT .ne. T).
C          Otherwise, TOUT is required on every call.
C
C          If ITASK = 1, 3, or 4, the values of TOUT need not be
C          monotone, but a value of TOUT which backs up is limited
C          to the current internal T interval, whose endpoints are
C          TCUR - HU and TCUR (see optional outputs, below, for
C          TCUR and HU).
C
C ITOL   = an indicator for the type of error control.  See
C          description below under ATOL.  Used only for input.
C
C RTOL   = a relative error tolerance parameter, either a scalar or
C          an array of length NEQ.  See description below under ATOL.
C          Input only.
C
C ATOL   = an absolute error tolerance parameter, either a scalar or
C          an array of length NEQ.  Input only.
C
C             The input parameters ITOL, RTOL, and ATOL determine
C          the error control performed by the solver.  The solver will
C          control the vector E = (E(i)) of estimated local errors
C          in y, according to an inequality of the form
C                      RMS-norm of ( E(i)/EWT(i) )   .le.   1,
C          where       EWT(i) = RTOL(i)*ABS(Y(i)) + ATOL(i),
C          and the RMS-norm (root-mean-square norm) here is
C          RMS-norm(v) = SQRT(sum v(i)**2 / NEQ).  Here EWT = (EWT(i))
C          is a vector of weights which must always be positive, and
C          the values of RTOL and ATOL should all be non-negative.
C          The following table gives the types (scalar/array) of
C          RTOL and ATOL, and the corresponding form of EWT(i).
C
C             ITOL    RTOL       ATOL          EWT(i)
C              1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
C              2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
C              3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
C              4     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL(i)
C
C          When either of these parameters is a scalar, it need not
C          be dimensioned in the user's calling program.
C
C          If none of the above choices (with ITOL, RTOL, and ATOL
C          fixed throughout the problem) is suitable, more general
C          error controls can be obtained by substituting
C          user-supplied routines for the setting of EWT and/or for
C          the norm calculation.  See Part 4 below.
C
C          If global errors are to be estimated by making a repeated
C          run on the same problem with smaller tolerances, then all
C          components of RTOL and ATOL (i.e. of EWT) should be scaled
C          down uniformly.
C
C ITASK  = an index specifying the task to be performed.
C          Input only.  ITASK has the following values and meanings.
C          1  means normal computation of output values of y(t) at
C             t = TOUT (by overshooting and interpolating).
C          2  means take one step only and return.
C          3  means stop at the first internal mesh point at or
C             beyond t = TOUT and return.
C          4  means normal computation of output values of y(t) at
C             t = TOUT but without overshooting t = TCRIT.
C             TCRIT must be input as RWORK(1).  TCRIT may be equal to
C             or beyond TOUT, but not behind it in the direction of
C             integration.  This option is useful if the problem
C             has a singularity at or beyond t = TCRIT.
C          5  means take one step, without passing TCRIT, and return.
C             TCRIT must be input as RWORK(1).
C
C          Note:  If ITASK = 4 or 5 and the solver reaches TCRIT
C          (within roundoff), it will return T = TCRIT (exactly) to
C          indicate this (unless ITASK = 4 and TOUT comes before TCRIT,
C          in which case answers at t = TOUT are returned first).
C
C ISTATE = an index used for input and output to specify the
C          state of the calculation.
C
C          On input, the values of ISTATE are as follows.
C          0  means this is the first call for the problem, and
C             DLSOIBT is to compute the initial value of dy/dt
C             (while doing other initializations).  See note below.
C          1  means this is the first call for the problem, and
C             the initial value of dy/dt has been supplied in
C             YDOTI (DLSOIBT will do other initializations).
C             See note below.
C          2  means this is not the first call, and the calculation
C             is to continue normally, with no change in any input
C             parameters except possibly TOUT and ITASK.
C             (If ITOL, RTOL, and/or ATOL are changed between calls
C             with ISTATE = 2, the new values will be used but not
C             tested for legality.)
C          3  means this is not the first call, and the
C             calculation is to continue normally, but with
C             a change in input parameters other than
C             TOUT and ITASK.  Changes are allowed in
C             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, MF, MB, NB,
C             and any of the optional inputs except H0.
C             (See IWORK description for MB and NB.)
C          Note:  A preliminary call with TOUT = T is not counted
C          as a first call here, as no initialization or checking of
C          input is done.  (Such a call is sometimes useful for the
C          purpose of outputting the initial conditions.)
C          Thus the first call for which TOUT .ne. T requires
C          ISTATE = 0 or 1 on input.
C
C          On output, ISTATE has the following values and meanings.
C           0 or 1  means nothing was done; TOUT = t and
C              ISTATE = 0 or 1 on input.
C           2  means that the integration was performed successfully.
C           3  means that the user-supplied Subroutine RES signalled
C              DLSOIBT to halt the integration and return (IRES = 2).
C              Integration as far as T was achieved with no occurrence
C              of IRES = 2, but this flag was set on attempting the
C              next step.
C          -1  means an excessive amount of work (more than MXSTEP
C              steps) was done on this call, before completing the
C              requested task, but the integration was otherwise
C              successful as far as T.  (MXSTEP is an optional input
C              and is normally 500.)  To continue, the user may
C              simply reset ISTATE to a value .gt. 1 and call again
C              (the excess work step counter will be reset to 0).
C              In addition, the user may increase MXSTEP to avoid
C              this error return (see below on optional inputs).
C          -2  means too much accuracy was requested for the precision
C              of the machine being used.  This was detected before
C              completing the requested task, but the integration
C              was successful as far as T.  To continue, the tolerance
C              parameters must be reset, and ISTATE must be set
C              to 3.  The optional output TOLSF may be used for this
C              purpose.  (Note: If this condition is detected before
C              taking any steps, then an illegal input return
C              (ISTATE = -3) occurs instead.)
C          -3  means illegal input was detected, before taking any
C              integration steps.  See written message for details.
C              Note:  If the solver detects an infinite loop of calls
C              to the solver with illegal input, it will cause
C              the run to stop.
C          -4  means there were repeated error test failures on
C              one attempted step, before completing the requested
C              task, but the integration was successful as far as T.
C              The problem may have a singularity, or the input
C              may be inappropriate.
C          -5  means there were repeated convergence test failures on
C              one attempted step, before completing the requested
C              task, but the integration was successful as far as T.
C              This may be caused by an inaccurate Jacobian matrix.
C          -6  means EWT(i) became zero for some i during the
C              integration.  Pure relative error control (ATOL(i) = 0.0)
C              was requested on a variable which has now vanished.
C              The integration was successful as far as T.
C          -7  means that the user-supplied Subroutine RES set
C              its error flag (IRES = 3) despite repeated tries by
C              DLSOIBT to avoid that condition.
C          -8  means that ISTATE was 0 on input but DLSOIBT was unable
C              to compute the initial value of dy/dt.  See the
C              printed message for details.
C
C          Note:  Since the normal output value of ISTATE is 2,
C          it does not need to be reset for normal continuation.
C          Similarly, ISTATE (= 3) need not be reset if RES told
C          DLSOIBT to return because the calling program must change
C          the parameters of the problem.
C          Also, since a negative input value of ISTATE will be
C          regarded as illegal, a negative output value requires the
C          user to change it, and possibly other inputs, before
C          calling the solver again.
C
C IOPT   = an integer flag to specify whether or not any optional
C          inputs are being used on this call.  Input only.
C          The optional inputs are listed separately below.
C          IOPT = 0 means no optional inputs are being used.
C                   Default values will be used in all cases.
C          IOPT = 1 means one or more optional inputs are being used.
C
C RWORK  = a real working array (double precision).
C          The length of RWORK must be at least
C             20 + NYH*(MAXORD + 1) + 3*NEQ + LENWM    where
C          NYH    = the initial value of NEQ,
C          MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a
C                   smaller value is given as an optional input),
C          LENWM  = 3*MB*MB*NB + 2.
C          (See MF description for the definition of METH.)
C          Thus if MAXORD has its default value and NEQ is constant,
C          this length is
C             22 + 16*NEQ + 3*MB*MB*NB     for MF = 11 or 12,
C             22 + 9*NEQ + 3*MB*MB*NB      for MF = 21 or 22.
C          The first 20 words of RWORK are reserved for conditional
C          and optional inputs and optional outputs.
C
C          The following word in RWORK is a conditional input:
C            RWORK(1) = TCRIT = critical value of t which the solver
C                       is not to overshoot.  Required if ITASK is
C                       4 or 5, and ignored otherwise.  (See ITASK.)
C
C LRW    = the length of the array RWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C IWORK  = an integer work array.  The length of IWORK must be at least
C          20 + NEQ .  The first few words of IWORK are used for
C          additional and optional inputs and optional outputs.
C
C          The following 2 words in IWORK are additional required
C          inputs to DLSOIBT:
C            IWORK(1) = MB = block size
C            IWORK(2) = NB = number of blocks in the main diagonal
C          These must satisfy  MB .ge. 1, NB .ge. 4, and MB*NB = NEQ.
C
C LIW    = the length of the array IWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C Note:  The work arrays must not be altered between calls to DLSOIBT
C for the same problem, except possibly for the additional and
C optional inputs, and except for the last 3*NEQ words of RWORK.
C The latter space is used for internal scratch space, and so is
C available for use by the user outside DLSOIBT between calls, if
C desired (but not for use by RES, ADDA, or JAC).
C
C MF     = the method flag.  used only for input.  The legal values of
C          MF are 11, 12, 21, and 22.
C          MF has decimal digits METH and MITER: MF = 10*METH + MITER.
C            METH indicates the basic linear multistep method:
C              METH = 1 means the implicit Adams method.
C              METH = 2 means the method based on Backward
C                       Differentiation Formulas (BDFS).
C                The BDF method is strongly preferred for stiff
C              problems, while the Adams method is preferred when the
C              problem is not stiff.  If the matrix A(t,y) is
C              nonsingular, stiffness here can be taken to mean that of
C              the explicit ODE system dy/dt = A-inverse * g.  If A is
C              singular, the concept of stiffness is not well defined.
C                If you do not know whether the problem is stiff, we
C              recommend using METH = 2.  If it is stiff, the advantage
C              of METH = 2 over METH = 1 will be great, while if it is
C              not stiff, the advantage of METH = 1 will be slight.
C              If maximum efficiency is important, some experimentation
C              with METH may be necessary.
C            MITER indicates the corrector iteration method:
C              MITER = 1 means chord iteration with a user-supplied
C                        block-tridiagonal Jacobian.
C              MITER = 2 means chord iteration with an internally
C                        generated (difference quotient) block-
C                        tridiagonal Jacobian approximation, using
C                        3*MB+1 extra calls to RES per dr/dy evaluation.
C              If MITER = 1, the user must supply a Subroutine JAC
C              (the name is arbitrary) as described above under JAC.
C              For MITER = 2, a dummy argument can be used.
C-----------------------------------------------------------------------
C Optional Inputs.
C
C The following is a list of the optional inputs provided for in the
C call sequence.  (See also Part 2.)  For each such input variable,
C this table lists its name as used in this documentation, its
C location in the call sequence, its meaning, and the default value.
C The use of any of these inputs requires IOPT = 1, and in that
C case all of these inputs are examined.  A value of zero for any
C of these optional inputs will cause the default value to be used.
C Thus to use a subset of the optional inputs, simply preload
C locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and
C then set those of interest to nonzero values.
C
C Name    Location      Meaning and Default Value
C
C H0      RWORK(5)  the step size to be attempted on the first step.
C                   The default value is determined by the solver.
C
C HMAX    RWORK(6)  the maximum absolute step size allowed.
C                   The default value is infinite.
C
C HMIN    RWORK(7)  the minimum absolute step size allowed.
C                   The default value is 0.  (This lower bound is not
C                   enforced on the final step before reaching TCRIT
C                   when ITASK = 4 or 5.)
C
C MAXORD  IWORK(5)  the maximum order to be allowed.  The default
C                   value is 12 if METH = 1, and 5 if METH = 2.
C                   If MAXORD exceeds the default value, it will
C                   be reduced to the default value.
C                   If MAXORD is changed during the problem, it may
C                   cause the current order to be reduced.
C
C MXSTEP  IWORK(6)  maximum number of (internally defined) steps
C                   allowed during one call to the solver.
C                   The default value is 500.
C
C MXHNIL  IWORK(7)  maximum number of messages printed (per problem)
C                   warning that T + H = T on a step (H = step size).
C                   This must be positive to result in a non-default
C                   value.  The default value is 10.
C-----------------------------------------------------------------------
C Optional Outputs.
C
C As optional additional output from DLSOIBT, the variables listed
C below are quantities related to the performance of DLSOIBT
C which are available to the user.  These are communicated by way of
C the work arrays, but also have internal mnemonic names as shown.
C Except where stated otherwise, all of these outputs are defined
C on any successful return from DLSOIBT, and on any return with
C ISTATE = -1, -2, -4, -5, -6, or -7.  On a return with -3 (illegal
C input) or -8, they will be unchanged from their existing values
C (if any), except possibly for TOLSF, LENRW, and LENIW.
C On any error return, outputs relevant to the error will be defined,
C as noted below.
C
C Name    Location      Meaning
C
C HU      RWORK(11) the step size in t last used (successfully).
C
C HCUR    RWORK(12) the step size to be attempted on the next step.
C
C TCUR    RWORK(13) the current value of the independent variable
C                   which the solver has actually reached, i.e. the
C                   current internal mesh point in t.  On output, TCUR
C                   will always be at least as far as the argument
C                   T, but may be farther (if interpolation was done).
C
C TOLSF   RWORK(14) a tolerance scale factor, greater than 1.0,
C                   computed when a request for too much accuracy was
C                   detected (ISTATE = -3 if detected at the start of
C                   the problem, ISTATE = -2 otherwise).  If ITOL is
C                   left unaltered but RTOL and ATOL are uniformly
C                   scaled up by a factor of TOLSF for the next call,
C                   then the solver is deemed likely to succeed.
C                   (The user may also ignore TOLSF and alter the
C                   tolerance parameters in any other way appropriate.)
C
C NST     IWORK(11) the number of steps taken for the problem so far.
C
C NRE     IWORK(12) the number of residual evaluations (RES calls)
C                   for the problem so far.
C
C NJE     IWORK(13) the number of Jacobian evaluations (each involving
C                   an evaluation of a and dr/dy) for the problem so
C                   far.  This equals the number of calls to ADDA and
C                   (if MITER = 1) to JAC, and the number of matrix
C                   LU decompositions.
C
C NQU     IWORK(14) the method order last used (successfully).
C
C NQCUR   IWORK(15) the order to be attempted on the next step.
C
C IMXER   IWORK(16) the index of the component of largest magnitude in
C                   the weighted local error vector ( E(i)/EWT(i) ),
C                   on an error return with ISTATE = -4 or -5.
C
C LENRW   IWORK(17) the length of RWORK actually required.
C                   This is defined on normal returns and on an illegal
C                   input return for insufficient storage.
C
C LENIW   IWORK(18) the length of IWORK actually required.
C                   This is defined on normal returns and on an illegal
C                   input return for insufficient storage.
C
C
C The following two arrays are segments of the RWORK array which
C may also be of interest to the user as optional outputs.
C For each array, the table below gives its internal name,
C its base address in RWORK, and its description.
C
C Name    Base Address      Description
C
C YH      21             the Nordsieck history array, of size NYH by
C                        (NQCUR + 1), where NYH is the initial value
C                        of NEQ.  For j = 0,1,...,NQCUR, column j+1
C                        of YH contains HCUR**j/factorial(j) times
C                        the j-th derivative of the interpolating
C                        polynomial currently representing the solution,
C                        evaluated at t = TCUR.
C
C ACOR     LENRW-NEQ+1   array of size NEQ used for the accumulated
C                        corrections on each step, scaled on output to
C                        represent the estimated local error in y on
C                        the last step.  This is the vector E in the
C                        description of the error control.  It is
C                        defined only on a return from DLSOIBT with
C                        ISTATE = 2.
C
C-----------------------------------------------------------------------
C Part 2.  Other Routines Callable.
C
C The following are optional calls which the user may make to
C gain additional capabilities in conjunction with DLSOIBT.
C (The routines XSETUN and XSETF are designed to conform to the
C SLATEC error handling package.)
C
C     Form of Call                  Function
C   CALL XSETUN(LUN)          Set the logical unit number, LUN, for
C                             output of messages from DLSOIBT, if
C                             the default is not desired.
C                             The default value of LUN is 6.
C
C   CALL XSETF(MFLAG)         Set a flag to control the printing of
C                             messages by DLSOIBT.
C                             MFLAG = 0 means do not print. (Danger:
C                             This risks losing valuable information.)
C                             MFLAG = 1 means print (the default).
C
C                             Either of the above calls may be made at
C                             any time and will take effect immediately.
C
C   CALL DSRCOM(RSAV,ISAV,JOB) saves and restores the contents of
C                             the internal Common blocks used by
C                             DLSOIBT (see Part 3 below).
C                             RSAV must be a real array of length 218
C                             or more, and ISAV must be an integer
C                             array of length 37 or more.
C                             JOB=1 means save Common into RSAV/ISAV.
C                             JOB=2 means restore Common from RSAV/ISAV.
C                                DSRCOM is useful if one is
C                             interrupting a run and restarting
C                             later, or alternating between two or
C                             more problems solved with DLSOIBT.
C
C   CALL DINTDY(,,,,,)        Provide derivatives of y, of various
C        (see below)          orders, at a specified point t, if
C                             desired.  It may be called only after
C                             a successful return from DLSOIBT.
C
C The detailed instructions for using DINTDY are as follows.
C The form of the call is:
C
C   CALL DINTDY (T, K, RWORK(21), NYH, DKY, IFLAG)
C
C The input parameters are:
C
C T         = value of independent variable where answers are desired
C             (normally the same as the t last returned by DLSOIBT).
C             For valid results, T must lie between TCUR - HU and TCUR.
C             (See optional outputs for TCUR and HU.)
C K         = integer order of the derivative desired.  K must satisfy
C             0 .le. K .le. NQCUR, where NQCUR is the current order
C             (see optional outputs).  The capability corresponding
C             to K = 0, i.e. computing y(t), is already provided
C             by DLSOIBT directly.  Since NQCUR .ge. 1, the first
C             derivative dy/dt is always available with DINTDY.
C RWORK(21) = the base address of the history array YH.
C NYH       = column length of YH, equal to the initial value of NEQ.
C
C The output parameters are:
C
C DKY       = a real array of length NEQ containing the computed value
C             of the K-th derivative of y(t).
C IFLAG     = integer flag, returned as 0 if K and T were legal,
C             -1 if K was illegal, and -2 if T was illegal.
C             On an error return, a message is also written.
C-----------------------------------------------------------------------
C Part 3.  Common Blocks.
C
C If DLSOIBT is to be used in an overlay situation, the user
C must declare, in the primary overlay, the variables in:
C   (1) the call sequence to DLSOIBT, and
C   (2) the internal Common block
C         /DLS001/  of length  255  (218 double precision words
C                      followed by 37 integer words),
C
C If DLSOIBT is used on a system in which the contents of internal
C Common blocks are not preserved between calls, the user should
C declare the above Common block in the calling program to insure
C that their contents are preserved.
C
C If the solution of a given problem by DLSOIBT is to be interrupted
C and then later continued, such as when restarting an interrupted run
C or alternating between two or more problems, the user should save,
C following the return from the last DLSOIBT call prior to the
C interruption, the contents of the call sequence variables and the
C internal Common blocks, and later restore these values before the
C next DLSOIBT call for that problem.  To save and restore the Common
C blocks, use Subroutine DSRCOM (see Part 2 above).
C
C-----------------------------------------------------------------------
C Part 4.  Optionally Replaceable Solver Routines.
C
C Below are descriptions of two routines in the DLSOIBT package which
C relate to the measurement of errors.  Either routine can be
C replaced by a user-supplied version, if desired.  However, since such
C a replacement may have a major impact on performance, it should be
C done only when absolutely necessary, and only with great caution.
C (Note: The means by which the package version of a routine is
C superseded by the user's version may be system-dependent.)
C
C (a) DEWSET.
C The following subroutine is called just before each internal
C integration step, and sets the array of error weights, EWT, as
C described under ITOL/RTOL/ATOL above:
C     SUBROUTINE DEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT)
C where NEQ, ITOL, RTOL, and ATOL are as in the DLSOIBT call sequence,
C YCUR contains the current dependent variable vector, and
C EWT is the array of weights set by DEWSET.
C
C If the user supplies this subroutine, it must return in EWT(i)
C (i = 1,...,NEQ) a positive quantity suitable for comparing errors
C in y(i) to.  The EWT array returned by DEWSET is passed to the DVNORM
C routine (see below), and also used by DLSOIBT in the computation
C of the optional output IMXER, the diagonal Jacobian approximation,
C and the increments for difference quotient Jacobians.
C
C In the user-supplied version of DEWSET, it may be desirable to use
C the current values of derivatives of y.  Derivatives up to order NQ
C are available from the history array YH, described above under
C optional outputs.  In DEWSET, YH is identical to the YCUR array,
C extended to NQ + 1 columns with a column length of NYH and scale
C factors of H**j/factorial(j).  On the first call for the problem,
C given by NST = 0, NQ is 1 and H is temporarily set to 1.0.
C NYH is the initial value of NEQ.  The quantities NQ, H, and NST
C can be obtained by including in DEWSET the statements:
C     DOUBLE PRECISION RLS
C     COMMON /DLS001/ RLS(218),ILS(37)
C     NQ = ILS(33)
C     NST = ILS(34)
C     H = RLS(212)
C Thus, for example, the current value of dy/dt can be obtained as
C YCUR(NYH+i)/H  (i=1,...,NEQ)  (and the division by H is
C unnecessary when NST = 0).
C
C (b) DVNORM.
C The following is a real function routine which computes the weighted
C root-mean-square norm of a vector v:
C     D = DVNORM (N, V, W)
C where:
C   N = the length of the vector,
C   V = real array of length N containing the vector,
C   W = real array of length N containing weights,
C   D = SQRT( (1/N) * sum(V(i)*W(i))**2 ).
C DVNORM is called with N = NEQ and with W(i) = 1.0/EWT(i), where
C EWT is as set by Subroutine DEWSET.
C
C If the user supplies this function, it should return a non-negative
C value of DVNORM suitable for use in the error control in DLSOIBT.
C None of the arguments should be altered by DVNORM.
C For example, a user-supplied DVNORM routine might:
C   -substitute a max-norm of (V(i)*W(i)) for the RMS-norm, or
C   -ignore some components of V in the norm, with the effect of
C    suppressing the error control on those components of y.
C-----------------------------------------------------------------------
C
C***REVISION HISTORY  (YYYYMMDD)
C 19840625  DATE WRITTEN
C 19870330  Major update: corrected comments throughout;
C           removed TRET from Common; rewrote EWSET with 4 loops;
C           fixed t test in INTDY; added Cray directives in STODI;
C           in STODI, fixed DELP init. and logic around PJAC call;
C           combined routines to save/restore Common;
C           passed LEVEL = 0 in error message calls (except run abort).
C 20010425  Major update: convert source lines to upper case;
C           added *DECK lines; changed from 1 to * in dummy dimensions;
C           changed names R1MACH/D1MACH to RUMACH/DUMACH;
C           renamed routines for uniqueness across single/double prec.;
C           converted intrinsic names to generic form;
C           removed ILLIN and NTREP (data loaded) from Common;
C           removed all 'own' variables from Common;
C           changed error messages to quoted strings;
C           replaced XERRWV/XERRWD with 1993 revised version;
C           converted prologues, comments, error messages to mixed case;
C           converted arithmetic IF statements to logical IF statements;
C           numerous corrections to prologues and internal comments.
C 20010507  Converted single precision source to double precision.
C 20020502  Corrected declarations in descriptions of user routines.
C 20031105  Restored 'own' variables to Common block, to enable
C           interrupt/restart feature.
C 20031112  Added SAVE statements for data-loaded constants.
C 20031117  Changed internal names NRE, LSAVR to NFE, LSAVF resp.
C
C-----------------------------------------------------------------------
C Other routines in the DLSOIBT package.
C
C In addition to Subroutine DLSOIBT, the DLSOIBT package includes the
C following subroutines and function routines:
C  DAIGBT   computes the initial value of the vector
C             dy/dt = A-inverse * g
C  DINTDY   computes an interpolated value of the y vector at t = TOUT.
C  DSTODI   is the core integrator, which does one step of the
C           integration and the associated error control.
C  DCFODE   sets all method coefficients and test constants.
C  DEWSET   sets the error weight vector EWT before each step.
C  DVNORM   computes the weighted RMS-norm of a vector.
C  DSRCOM   is a user-callable routine to save and restore
C           the contents of the internal Common blocks.
C  DPJIBT   computes and preprocesses the Jacobian matrix
C           and the Newton iteration matrix P.
C  DSLSBT   manages solution of linear system in chord iteration.
C  DDECBT and DSOLBT   are routines for solving block-tridiagonal
C           systems of linear algebraic equations.
C  DGEFA and DGESL   are routines from LINPACK for solving full
C           systems of linear algebraic equations.
C  DDOT     is one of the basic linear algebra modules (BLAS).
C  DUMACH   computes the unit roundoff in a machine-independent manner.
C  XERRWD, XSETUN, XSETF, IXSAV, and IUMACH  handle the printing of all
C           error messages and warnings.  XERRWD is machine-dependent.
C Note:  DVNORM, DDOT, DUMACH, IXSAV, and IUMACH are function routines.
C All the others are subroutines.
C
C-----------------------------------------------------------------------
      EXTERNAL DPJIBT, DSLSBT
      DOUBLE PRECISION DUMACH, DVNORM
      INTEGER INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER I, I1, I2, IER, IFLAG, IMXER, IRES, KGO,
     1   LENIW, LENRW, LENWM, LP, LYD0, MB, MORD, MXHNL0, MXSTP0, NB
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION ATOLI, AYI, BIG, EWTI, H0, HMAX, HMX, RH, RTOLI,
     1   TCRIT, TDIST, TNEXT, TOL, TOLSF, TP, SIZE, SUM, W0
      DIMENSION MORD(2)
      LOGICAL IHIT
      CHARACTER*60 MSG
      SAVE MORD, MXSTP0, MXHNL0
C-----------------------------------------------------------------------
C The following internal Common block contains
C (a) variables which are local to any subroutine but whose values must
C     be preserved between calls to the routine ("own" variables), and
C (b) variables which are communicated between subroutines.
C The block DLS001 is declared in subroutines DLSOIBT, DINTDY, DSTODI,
C DPJIBT, and DSLSBT.
C Groups of variables are replaced by dummy arrays in the Common
C declarations in routines where those variables are not used.
C-----------------------------------------------------------------------
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
C
      DATA MORD(1),MORD(2)/12,5/, MXSTP0/500/, MXHNL0/10/
C-----------------------------------------------------------------------
C Block A.
C This code block is executed on every call.
C It tests ISTATE and ITASK for legality and branches appropriately.
C If ISTATE .gt. 1 but the flag INIT shows that initialization has
C not yet been done, an error return occurs.
C If ISTATE = 0 or 1 and TOUT = T, return immediately.
C-----------------------------------------------------------------------
      IF (ISTATE .LT. 0 .OR. ISTATE .GT. 3) GO TO 601
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
      IF (ISTATE .LE. 1) GO TO 10
      IF (INIT .EQ. 0) GO TO 603
      IF (ISTATE .EQ. 2) GO TO 200
      GO TO 20
 10   INIT = 0
      IF (TOUT .EQ. T) RETURN
C-----------------------------------------------------------------------
C Block B.
C The next code block is executed for the initial call (ISTATE = 0 or 1)
C or for a continuation call with parameter changes (ISTATE = 3).
C It contains checking of all inputs and various initializations.
C
C First check legality of the non-optional inputs NEQ, ITOL, IOPT,
C MF, MB, and NB.
C-----------------------------------------------------------------------
 20   IF (NEQ(1) .LE. 0) GO TO 604
      IF (ISTATE .LE. 1) GO TO 25
      IF (NEQ(1) .GT. N) GO TO 605
 25   N = NEQ(1)
      IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606
      IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607
      METH = MF/10
      MITER = MF - 10*METH
      IF (METH .LT. 1 .OR. METH .GT. 2) GO TO 608
      IF (MITER .LT. 1 .OR. MITER .GT. 2) GO TO 608
      MB = IWORK(1)
      NB = IWORK(2)
      IF (MB .LT. 1 .OR. MB .GT. N) GO TO 609
      IF (NB .LT. 4) GO TO 610
      IF (MB*NB .NE. N) GO TO 609
C Next process and check the optional inputs. --------------------------
      IF (IOPT .EQ. 1) GO TO 40
      MAXORD = MORD(METH)
      MXSTEP = MXSTP0
      MXHNIL = MXHNL0
      IF (ISTATE .LE. 1) H0 = 0.0D0
      HMXI = 0.0D0
      HMIN = 0.0D0
      GO TO 60
 40   MAXORD = IWORK(5)
      IF (MAXORD .LT. 0) GO TO 611
      IF (MAXORD .EQ. 0) MAXORD = 100
      MAXORD = MIN(MAXORD,MORD(METH))
      MXSTEP = IWORK(6)
      IF (MXSTEP .LT. 0) GO TO 612
      IF (MXSTEP .EQ. 0) MXSTEP = MXSTP0
      MXHNIL = IWORK(7)
      IF (MXHNIL .LT. 0) GO TO 613
      IF (MXHNIL .EQ. 0) MXHNIL = MXHNL0
      IF (ISTATE .GT. 1) GO TO 50
      H0 = RWORK(5)
      IF ((TOUT - T)*H0 .LT. 0.0D0) GO TO 614
 50   HMAX = RWORK(6)
      IF (HMAX .LT. 0.0D0) GO TO 615
      HMXI = 0.0D0
      IF (HMAX .GT. 0.0D0) HMXI = 1.0D0/HMAX
      HMIN = RWORK(7)
      IF (HMIN .LT. 0.0D0) GO TO 616
C-----------------------------------------------------------------------
C Set work array pointers and check lengths LRW and LIW.
C Pointers to segments of RWORK and IWORK are named by prefixing L to
C the name of the segment.  E.g., the segment YH starts at RWORK(LYH).
C Segments of RWORK (in order) are denoted YH, WM, EWT, SAVR, ACOR.
C-----------------------------------------------------------------------
 60   LYH = 21
      IF (ISTATE .LE. 1) NYH = N
      LWM = LYH + (MAXORD + 1)*NYH
      LENWM = 3*MB*MB*NB + 2
      LEWT = LWM + LENWM
      LSAVF = LEWT + N
      LACOR = LSAVF + N
      LENRW = LACOR + N - 1
      IWORK(17) = LENRW
      LIWM = 1
      LENIW = 20 + N
      IWORK(18) = LENIW
      IF (LENRW .GT. LRW) GO TO 617
      IF (LENIW .GT. LIW) GO TO 618
C Check RTOL and ATOL for legality. ------------------------------------
      RTOLI = RTOL(1)
      ATOLI = ATOL(1)
      DO 70 I = 1,N
        IF (ITOL .GE. 3) RTOLI = RTOL(I)
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        IF (RTOLI .LT. 0.0D0) GO TO 619
        IF (ATOLI .LT. 0.0D0) GO TO 620
 70     CONTINUE
      IF (ISTATE .LE. 1) GO TO 100
C If ISTATE = 3, set flag to signal parameter changes to DSTODI. -------
      JSTART = -1
      IF (NQ .LE. MAXORD) GO TO 90
C MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into YDOTI.---------
      DO 80 I = 1,N
 80     YDOTI(I) = RWORK(I+LWM-1)
C Reload WM(1) = RWORK(lWM), since lWM may have changed. ---------------
 90   RWORK(LWM) = SQRT(UROUND)
      IF (N .EQ. NYH) GO TO 200
C NEQ was reduced.  Zero part of YH to avoid undefined references. -----
      I1 = LYH + L*NYH
      I2 = LYH + (MAXORD + 1)*NYH - 1
      IF (I1 .GT. I2) GO TO 200
      DO 95 I = I1,I2
 95     RWORK(I) = 0.0D0
      GO TO 200
C-----------------------------------------------------------------------
C Block C.
C The next block is for the initial call only (ISTATE = 0 or 1).
C It contains all remaining initializations, the call to DAIGBT
C (if ISTATE = 1), and the calculation of the initial step size.
C The error weights in EWT are inverted after being loaded.
C-----------------------------------------------------------------------
 100  UROUND = DUMACH()
      TN = T
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 105
      TCRIT = RWORK(1)
      IF ((TCRIT - TOUT)*(TOUT - T) .LT. 0.0D0) GO TO 625
      IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)
     1   H0 = TCRIT - T
 105  JSTART = 0
      RWORK(LWM) = SQRT(UROUND)
      NHNIL = 0
      NST = 0
      NFE = 0
      NJE = 0
      NSLAST = 0
      HU = 0.0D0
      NQU = 0
      CCMAX = 0.3D0
      MAXCOR = 3
      MSBP = 20
      MXNCF = 10
C Compute initial dy/dt, if necessary, and load it and initial Y into YH
      LYD0 = LYH + NYH
      LP = LWM + 1
      IF ( ISTATE .EQ. 1 )  GO TO 120
C DLSOIBT must compute initial dy/dt (LYD0 points to YH(*,2)). ---------
         CALL DAIGBT( RES, ADDA, NEQ, T, Y, RWORK(LYD0),
     1               MB, NB, RWORK(LP), IWORK(21), IER )
         NFE = NFE + 1
         IF (IER .LT. 0) GO TO 560
         IF (IER .GT. 0) GO TO 565
         DO 115  I = 1,N
  115       RWORK(I+LYH-1) = Y(I)
         GO TO 130
C Initial dy/dt was supplied.  Load into YH (LYD0 points to YH(*,2).). -
  120    DO 125  I = 1,N
            RWORK(I+LYH-1) = Y(I)
  125       RWORK(I+LYD0-1) = YDOTI(I)
C Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
  130 CONTINUE
      NQ = 1
      H = 1.0D0
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 135 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 621
 135    RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
C-----------------------------------------------------------------------
C The coding below computes the step size, H0, to be attempted on the
C first step, unless the user has supplied a value for this.
C First check that TOUT - T differs significantly from zero.
C A scalar tolerance quantity TOL is computed, as MAX(RTOL(i))
C if this is positive, or MAX(ATOL(i)/ABS(Y(i))) otherwise, adjusted
C so as to be between 100*UROUND and 1.0E-3.
C Then the computed value H0 is given by..
C                                      NEQ
C   H0**2 = TOL / ( w0**-2 + (1/NEQ) * Sum ( YDOT(i)/ywt(i) )**2  )
C                                       1
C where   w0      = MAX ( ABS(T), ABS(TOUT) ),
C         YDOT(i) = i-th component of initial value of dy/dt,
C         ywt(i)  = EWT(i)/TOL  (a weight for y(i)).
C The sign of H0 is inferred from the initial values of TOUT and T.
C-----------------------------------------------------------------------
      IF (H0 .NE. 0.0D0) GO TO 180
      TDIST = ABS(TOUT - T)
      W0 = MAX(ABS(T),ABS(TOUT))
      IF (TDIST .LT. 2.0D0*UROUND*W0) GO TO 622
      TOL = RTOL(1)
      IF (ITOL .LE. 2) GO TO 145
      DO 140 I = 1,N
 140    TOL = MAX(TOL,RTOL(I))
 145  IF (TOL .GT. 0.0D0) GO TO 160
      ATOLI = ATOL(1)
      DO 150 I = 1,N
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        AYI = ABS(Y(I))
        IF (AYI .NE. 0.0D0) TOL = MAX(TOL,ATOLI/AYI)
 150    CONTINUE
 160  TOL = MAX(TOL,100.0D0*UROUND)
      TOL = MIN(TOL,0.001D0)
      SUM = DVNORM (N, RWORK(LYD0), RWORK(LEWT))
      SUM = 1.0D0/(TOL*W0*W0) + TOL*SUM**2
      H0 = 1.0D0/SQRT(SUM)
      H0 = MIN(H0,TDIST)
      H0 = SIGN(H0,TOUT-T)
C Adjust H0 if necessary to meet HMAX bound. ---------------------------
 180  RH = ABS(H0)*HMXI
      IF (RH .GT. 1.0D0) H0 = H0/RH
C Load H with H0 and scale YH(*,2) by H0. ------------------------------
      H = H0
      DO 190 I = 1,N
 190    RWORK(I+LYD0-1) = H0*RWORK(I+LYD0-1)
      GO TO 270
C-----------------------------------------------------------------------
C Block D.
C The next code block is for continuation calls only (ISTATE = 2 or 3)
C and is to check stop conditions before taking a step.
C-----------------------------------------------------------------------
 200  NSLAST = NST
      GO TO (210, 250, 220, 230, 240), ITASK
 210  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 220  TP = TN - HU*(1.0D0 + 100.0D0*UROUND)
      IF ((TP - TOUT)*H .GT. 0.0D0) GO TO 623
      IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
      GO TO 400
 230  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624
      IF ((TCRIT - TOUT)*H .LT. 0.0D0) GO TO 625
      IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 245
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 240  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624
 245  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
      IF (ISTATE .EQ. 2) JSTART = -2
C-----------------------------------------------------------------------
C Block E.
C The next block is normally executed for all calls and contains
C the call to the one-step core integrator DSTODI.
C
C This is a looping point for the integration steps.
C
C First check for too many steps being taken, update EWT (if not at
C start of problem), check for too much accuracy being requested, and
C check for H below the roundoff level in T.
C-----------------------------------------------------------------------
 250  CONTINUE
      IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 260 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 510
 260    RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
 270  TOLSF = UROUND*DVNORM (N, RWORK(LYH), RWORK(LEWT))
      IF (TOLSF .LE. 1.0D0) GO TO 280
      TOLSF = TOLSF*2.0D0
      IF (NST .EQ. 0) GO TO 626
      GO TO 520
 280  IF ((TN + H) .NE. TN) GO TO 290
      NHNIL = NHNIL + 1
      IF (NHNIL .GT. MXHNIL) GO TO 290
      MSG = 'DLSOIBT- Warning..Internal T (=R1) and H (=R2) are'
      CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG='      such that in the machine, T + H = T on the next step  '
      CALL XERRWD (MSG, 60, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '     (H = step size). Solver will continue anyway.'
      CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 2, TN, H)
      IF (NHNIL .LT. MXHNIL) GO TO 290
      MSG = 'DLSOIBT- Above warning has been issued I1 times.  '
      CALL XERRWD (MSG, 50, 102, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '     It will not be issued again for this problem.'
      CALL XERRWD (MSG, 50, 102, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
 290  CONTINUE
C-----------------------------------------------------------------------
C     CALL DSTODI(NEQ,Y,YH,NYH,YH1,EWT,SAVF,SAVR,ACOR,WM,IWM,RES,
C                 ADDA,JAC,DPJIBT,DSLSBT)
C Note: SAVF in DSTODI occupies the same space as YDOTI in DLSOIBT.
C-----------------------------------------------------------------------
      CALL DSTODI (NEQ, Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),
     1   YDOTI, RWORK(LSAVF), RWORK(LACOR), RWORK(LWM),
     2   IWORK(LIWM), RES, ADDA, JAC, DPJIBT, DSLSBT )
      KGO = 1 - KFLAG
      GO TO (300, 530, 540, 400, 550), KGO
C
C KGO = 1:success; 2:error test failure; 3:convergence failure;
C       4:RES ordered return; 5:RES returned error.
C-----------------------------------------------------------------------
C Block F.
C The following block handles the case of a successful return from the
C core integrator (KFLAG = 0).  Test for stop conditions.
C-----------------------------------------------------------------------
 300  INIT = 1
      GO TO (310, 400, 330, 340, 350), ITASK
C ITASK = 1.  If TOUT has been reached, interpolate. -------------------
 310  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
C ITASK = 3.  Jump to exit if TOUT was reached. ------------------------
 330  IF ((TN - TOUT)*H .GE. 0.0D0) GO TO 400
      GO TO 250
C ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary.
 340  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 345
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
 345  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
      JSTART = -2
      GO TO 250
C ITASK = 5.  see if TCRIT was reached and jump to exit. ---------------
 350  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
C-----------------------------------------------------------------------
C Block G.
C The following block handles all successful returns from DLSOIBT.
C If ITASK .ne. 1, Y is loaded from YH and T is set accordingly.
C ISTATE is set to 2, and the optional outputs are loaded into the
C work arrays before returning.
C-----------------------------------------------------------------------
 400  DO 410 I = 1,N
 410    Y(I) = RWORK(I+LYH-1)
      T = TN
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420
      IF (IHIT) T = TCRIT
  420 ISTATE = 2
      IF ( KFLAG .EQ. -3 )  ISTATE = 3
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      RETURN
C-----------------------------------------------------------------------
C Block H.
C The following block handles all unsuccessful returns other than
C those for illegal input.  First the error message routine is called.
C If there was an error test or convergence test failure, IMXER is set.
C Then Y is loaded from YH and T is set to TN.
C The optional outputs are loaded into the work arrays before returning.
C-----------------------------------------------------------------------
C The maximum number of steps was taken before reaching TOUT. ----------
 500  MSG = 'DLSOIBT- At current T (=R1), MXSTEP (=I1) steps   '
      CALL XERRWD (MSG, 50, 201, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      taken on this call before reaching TOUT     '
      CALL XERRWD (MSG, 50, 201, 0, 1, MXSTEP, 0, 1, TN, 0.0D0)
      ISTATE = -1
      GO TO 580
C EWT(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  EWTI = RWORK(LEWT+I-1)
      MSG = 'DLSOIBT- At T (=R1), EWT(I1) has become R2 .le. 0.'
      CALL XERRWD (MSG, 50, 202, 0, 1, I, 0, 2, TN, EWTI)
      ISTATE = -6
      GO TO 590
C Too much accuracy requested for machine precision. -------------------
 520  MSG = 'DLSOIBT- At T (=R1), too much accuracy requested  '
      CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      for precision of machine..  See TOLSF (=R2) '
      CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 2, TN, TOLSF)
      RWORK(14) = TOLSF
      ISTATE = -2
      GO TO 590
C KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
 530  MSG = 'DLSOIBT- At T (=R1) and step size H (=R2), the    '
      CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = 'error test failed repeatedly or with ABS(H) = HMIN'
      CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 2, TN, H)
      ISTATE = -4
      GO TO 570
C KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ----
 540  MSG = 'DLSOIBT- At T (=R1) and step size H (=R2), the    '
      CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      corrector convergence failed repeatedly     '
      CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      or with ABS(H) = HMIN   '
      CALL XERRWD (MSG, 30, 205, 0, 0, 0, 0, 2, TN, H)
      ISTATE = -5
      GO TO 570
C IRES = 3 returned by RES, despite retries by DSTODI.------------------
 550  MSG = 'DLSOIBT- At T (=R1) residual routine returned     '
      CALL XERRWD (MSG, 50, 206, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      error IRES = 3 repeatedly.        '
      CALL XERRWD (MSG, 40, 206, 0, 0, 0, 0, 1, TN, 0.0D0)
      ISTATE = -7
      GO TO 590
C DAIGBT failed because a diagonal block of A matrix was singular. -----
 560  IER = -IER
      MSG='DLSOIBT- Attempt to initialize dy/dt failed:  Matrix A has a'
      CALL XERRWD (MSG, 60, 207, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      singular diagonal block, block no. = (I1)   '
      CALL XERRWD (MSG, 50, 207, 0, 1, IER, 0, 0, 0.0D0, 0.0D0)
      ISTATE = -8
      RETURN
C DAIGBT failed because RES set IRES to 2 or 3. ------------------------
 565  MSG = 'DLSOIBT- Attempt to initialize dy/dt failed       '
      CALL XERRWD (MSG, 50, 208, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      because residual routine set its error flag '
      CALL XERRWD (MSG, 50, 208, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      to IRES = (I1)'
      CALL XERRWD (MSG, 20, 208, 0, 1, IER, 0, 0, 0.0D0, 0.0D0)
      ISTATE = -8
      RETURN
C Compute IMXER if relevant. -------------------------------------------
 570  BIG = 0.0D0
      IMXER = 1
      DO 575 I = 1,N
        SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
        IF (BIG .GE. SIZE) GO TO 575
        BIG = SIZE
        IMXER = I
 575    CONTINUE
      IWORK(16) = IMXER
C Compute residual if relevant. ----------------------------------------
 580  LYD0 = LYH + NYH
      DO 585 I = 1,N
         RWORK(I+LSAVF-1) = RWORK(I+LYD0-1)/H
 585     Y(I) = RWORK(I+LYH-1)
      IRES = 1
      CALL RES (NEQ, TN, Y, RWORK(LSAVF), YDOTI, IRES)
      NFE = NFE + 1
      IF (IRES .LE. 1)  GO TO 595
      MSG = 'DLSOIBT- Residual routine set its flag IRES       '
      CALL XERRWD (MSG, 50, 210, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      to (I1) when called for final output.       '
      CALL XERRWD (MSG, 50, 210, 0, 1, IRES, 0, 0, 0.0D0, 0.0D0)
      GO TO 595
C Set Y vector, T, and optional outputs. -------------------------------
 590  DO 592 I = 1,N
 592    Y(I) = RWORK(I+LYH-1)
 595  T = TN
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      RETURN
C-----------------------------------------------------------------------
C Block I.
C The following block handles all error returns due to illegal input
C (ISTATE = -3), as detected before calling the core integrator.
C First the error message routine is called.  If the illegal input
C is a negative ISTATE, the run is aborted (apparent infinite loop).
C-----------------------------------------------------------------------
 601  MSG = 'DLSOIBT- ISTATE (=I1) illegal.'
      CALL XERRWD (MSG, 30, 1, 0, 1, ISTATE, 0, 0, 0.0D0, 0.0D0)
      IF (ISTATE .LT. 0) GO TO 800
      GO TO 700
 602  MSG = 'DLSOIBT- ITASK (=I1) illegal. '
      CALL XERRWD (MSG, 30, 2, 0, 1, ITASK, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 603  MSG = 'DLSOIBT- ISTATE.gt.1 but DLSOIBT not initialized. '
      CALL XERRWD (MSG, 50, 3, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 604  MSG = 'DLSOIBT- NEQ (=I1) .lt. 1     '
      CALL XERRWD (MSG, 30, 4, 0, 1, NEQ(1), 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 605  MSG = 'DLSOIBT- ISTATE = 3 and NEQ increased (I1 to I2). '
      CALL XERRWD (MSG, 50, 5, 0, 2, N, NEQ(1), 0, 0.0D0, 0.0D0)
      GO TO 700
 606  MSG = 'DLSOIBT- ITOL (=I1) illegal.  '
      CALL XERRWD (MSG, 30, 6, 0, 1, ITOL, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 607  MSG = 'DLSOIBT- IOPT (=I1) illegal.  '
      CALL XERRWD (MSG, 30, 7, 0, 1, IOPT, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 608  MSG = 'DLSOIBT- MF (=I1) illegal.    '
      CALL XERRWD (MSG, 30, 8, 0, 1, MF, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 609  MSG = 'DLSOIBT- MB (=I1) or NB (=I2) illegal.  '
      CALL XERRWD (MSG, 40, 9, 0, 2, MB, NB, 0, 0.0D0, 0.0D0)
      GO TO 700
 610  MSG = 'DLSOIBT- NB (=I1) .lt. 4 illegal.       '
      CALL XERRWD (MSG, 40, 10, 0, 1, NB, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 611  MSG = 'DLSOIBT- MAXORD (=I1) .lt. 0  '
      CALL XERRWD (MSG, 30, 11, 0, 1, MAXORD, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 612  MSG = 'DLSOIBT- MXSTEP (=I1) .lt. 0  '
      CALL XERRWD (MSG, 30, 12, 0, 1, MXSTEP, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 613  MSG = 'DLSOIBT- MXHNIL (=I1) .lt. 0  '
      CALL XERRWD (MSG, 30, 13, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 614  MSG = 'DLSOIBT- TOUT (=R1) behind T (=R2)      '
      CALL XERRWD (MSG, 40, 14, 0, 0, 0, 0, 2, TOUT, T)
      MSG = '      Integration direction is given by H0 (=R1)  '
      CALL XERRWD (MSG, 50, 14, 0, 0, 0, 0, 1, H0, 0.0D0)
      GO TO 700
 615  MSG = 'DLSOIBT- HMAX (=R1) .lt. 0.0  '
      CALL XERRWD (MSG, 30, 15, 0, 0, 0, 0, 1, HMAX, 0.0D0)
      GO TO 700
 616  MSG = 'DLSOIBT- HMIN (=R1) .lt. 0.0  '
      CALL XERRWD (MSG, 30, 16, 0, 0, 0, 0, 1, HMIN, 0.0D0)
      GO TO 700
 617  MSG='DLSOIBT- RWORK length needed, LENRW (=I1), exceeds LRW (=I2)'
      CALL XERRWD (MSG, 60, 17, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0)
      GO TO 700
 618  MSG='DLSOIBT- IWORK length needed, LENIW (=I1), exceeds LIW (=I2)'
      CALL XERRWD (MSG, 60, 18, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0)
      GO TO 700
 619  MSG = 'DLSOIBT- RTOL(=I1) is R1 .lt. 0.0       '
      CALL XERRWD (MSG, 40, 19, 0, 1, I, 0, 1, RTOLI, 0.0D0)
      GO TO 700
 620  MSG = 'DLSOIBT- ATOL(=I1) is R1 .lt. 0.0       '
      CALL XERRWD (MSG, 40, 20, 0, 1, I, 0, 1, ATOLI, 0.0D0)
      GO TO 700
 621  EWTI = RWORK(LEWT+I-1)
      MSG = 'DLSOIBT- EWT(I1) is R1 .le. 0.0         '
      CALL XERRWD (MSG, 40, 21, 0, 1, I, 0, 1, EWTI, 0.0D0)
      GO TO 700
 622  MSG='DLSOIBT- TOUT(=R1) too close to T(=R2) to start integration.'
      CALL XERRWD (MSG, 60, 22, 0, 0, 0, 0, 2, TOUT, T)
      GO TO 700
 623  MSG='DLSOIBT- ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  '
      CALL XERRWD (MSG, 60, 23, 0, 1, ITASK, 0, 2, TOUT, TP)
      GO TO 700
 624  MSG='DLSOIBT- ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   '
      CALL XERRWD (MSG, 60, 24, 0, 0, 0, 0, 2, TCRIT, TN)
      GO TO 700
 625  MSG='DLSOIBT- ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   '
      CALL XERRWD (MSG, 60, 25, 0, 0, 0, 0, 2, TCRIT, TOUT)
      GO TO 700
 626  MSG = 'DLSOIBT- At start of problem, too much accuracy   '
      CALL XERRWD (MSG, 50, 26, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG='      requested for precision of machine..  See TOLSF (=R1) '
      CALL XERRWD (MSG, 60, 26, 0, 0, 0, 0, 1, TOLSF, 0.0D0)
      RWORK(14) = TOLSF
      GO TO 700
 627  MSG = 'DLSOIBT- Trouble in DINTDY.  ITASK = I1, TOUT = R1'
      CALL XERRWD (MSG, 50, 27, 0, 1, ITASK, 0, 1, TOUT, 0.0D0)
C
 700  ISTATE = -3
      RETURN
C
 800  MSG = 'DLSOIBT- Run aborted.. apparent infinite loop.    '
      CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, 0.0D0, 0.0D0)
      RETURN
C----------------------- End of Subroutine DLSOIBT ---------------------
      END

*DECK DLSODKR
      SUBROUTINE DLSODKR (F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK,
     1            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, PSOL,
     2            MF, G, NG, JROOT)
      EXTERNAL F, JAC, PSOL, G
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW, MF,
     1        NG, JROOT
      DOUBLE PRECISION Y, T, TOUT, RTOL, ATOL, RWORK
      DIMENSION NEQ(*), Y(*), RTOL(*), ATOL(*), RWORK(LRW), IWORK(LIW),
     1          JROOT(*)
C-----------------------------------------------------------------------
C This is the 18 November 2003 version of
C DLSODKR: Livermore Solver for Ordinary Differential equations,
C          with preconditioned Krylov iteration methods for the
C          Newton correction linear systems, and with Rootfinding.
C
C This version is in double precision.
C
C DLSODKR solves the initial value problem for stiff or nonstiff
C systems of first order ODEs,
C     dy/dt = f(t,y) ,  or, in component form,
C     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ).
C At the same time, it locates the roots of any of a set of functions
C     g(i) = g(i,t,y(1),...,y(NEQ))  (i = 1,...,ng).
C
C-----------------------------------------------------------------------
C Introduction.
C
C This is a modification of the DLSODE package, and differs from it
C in five ways:
C (a) It uses various preconditioned Krylov subspace iteration methods
C for the linear algebraic systems that arise in the case of stiff
C systems.  See the introductory notes below.
C (b) It does automatic switching between functional (fixpoint)
C iteration and Newton iteration in the corrector iteration.
C (c) It finds the root of at least one of a set of constraint
C functions g(i) of the independent and dependent variables.
C It finds only those roots for which some g(i), as a function
C of t, changes sign in the interval of integration.
C It then returns the solution at the root, if that occurs
C sooner than the specified stop condition, and otherwise returns
C the solution according the specified stop condition.
C (d) It supplies to JAC an input flag, JOK, which indicates whether
C JAC may (optionally) bypass the evaluation of Jacobian matrix data
C and instead process saved data (with the current value of scalar hl0).
C (e) It contains a new subroutine that calculates the initial step
C size to be attempted.
C
C
C Introduction to the Krylov methods in DLSODKR:
C
C The linear systems that must be solved have the form
C   A * x  = b ,  where  A = identity - hl0 * (df/dy) .
C Here hl0 is a scalar, and df/dy is the Jacobian matrix of partial
C derivatives of f (NEQ by NEQ).
C
C The particular Krylov method is chosen by setting the second digit,
C MITER, in the method flag MF.
C Currently, the values of MITER have the following meanings:
C
C  MITER = 1 means the Scaled Preconditioned Incomplete
C            Orthogonalization Method (SPIOM).
C
C          2 means an incomplete version of the preconditioned scaled
C            Generalized Minimal Residual method (SPIGMR).
C            This is the best choice in general.
C
C          3 means the Preconditioned Conjugate Gradient method (PCG).
C            Recommended only when df/dy is symmetric or nearly so.
C
C          4 means the scaled Preconditioned Conjugate Gradient method
C            (PCGS).  Recommended only when D-inverse * df/dy * D is
C            symmetric or nearly so, where D is the diagonal scaling
C            matrix with elements 1/EWT(i) (see RTOL/ATOL description).
C
C          9 means that only a user-supplied matrix P (approximating A)
C            will be used, with no Krylov iteration done.  This option
C            allows the user to provide the complete linear system
C            solution algorithm, if desired.
C
C The user can apply preconditioning to the linear system A*x = b,
C by means of arbitrary matrices (the preconditioners).
C     In the case of SPIOM and SPIGMR, one can apply left and right
C preconditioners P1 and P2, and the basic iterative method is then
C applied to the matrix (P1-inverse)*A*(P2-inverse) instead of to the
C matrix A.  The product P1*P2 should be an approximation to matrix A
C such that linear systems with P1 or P2 are easier to solve than with
C A.  Preconditioning from the left only or right only means using
C P2 = identity or P1 = identity, respectively.
C     In the case of the PCG and PCGS methods, there is only one
C preconditioner matrix P (but it can be the product of more than one).
C It should approximate the matrix A but allow for relatively
C easy solution of linear systems with coefficient matrix P.
C For PCG, P should be positive definite symmetric, or nearly so,
C and for PCGS, the scaled preconditioner D-inverse * P * D
C should be symmetric or nearly so.
C     If the Jacobian J = df/dy splits in a natural way into a sum
C J = J1 + J2, then one possible choice of preconditioners is
C     P1 = identity - hl0 * J1  and  P2 = identity - hl0 * J2
C provided each of these is easy to solve (or approximately solve).
C
C-----------------------------------------------------------------------
C References:
C 1.  Peter N. Brown and Alan C. Hindmarsh, Reduced Storage Matrix
C     Methods in Stiff ODE Systems, J. Appl. Math. & Comp., 31 (1989),
C     pp. 40-91; also  L.L.N.L. Report UCRL-95088, Rev. 1, June 1987.
C 2.  Alan C. Hindmarsh,  ODEPACK, A Systematized Collection of ODE
C     Solvers, in Scientific Computing, R. S. Stepleman et al. (Eds.),
C     North-Holland, Amsterdam, 1983, pp. 55-64.
C-----------------------------------------------------------------------
C Authors:       Alan C. Hindmarsh and Peter N. Brown
C                Center for Applied Scientific Computing, L-561
C                Lawrence Livermore National Laboratory
C                Livermore, CA 94551
C-----------------------------------------------------------------------
C Summary of Usage.
C
C Communication between the user and the DLSODKR package, for normal
C situations, is summarized here.  This summary describes only a subset
C of the full set of options available.  See the full description for
C details, including optional communication, nonstandard options,
C and instructions for special situations.  See also the demonstration
C program distributed with this solver.
C
C A. First provide a subroutine of the form:
C               SUBROUTINE F (NEQ, T, Y, YDOT)
C               DOUBLE PRECISION T, Y(*), YDOT(*)
C which supplies the vector function f by loading YDOT(i) with f(i).
C
C B. Provide a subroutine of the form:
C               SUBROUTINE G (NEQ, T, Y, NG, GOUT)
C               DOUBLE PRECISION T, Y(*), GOUT(NG)
C which supplies the vector function g by loading GOUT(i) with
C g(i), the i-th constraint function whose root is sought.
C
C C. Next determine (or guess) whether or not the problem is stiff.
C Stiffness occurs when the Jacobian matrix df/dy has an eigenvalue
C whose real part is negative and large in magnitude, compared to the
C reciprocal of the t span of interest.  If the problem is nonstiff,
C use a method flag MF = 10.  If it is stiff, MF should be between 21
C and 24, or possibly 29.  MF = 22 is generally the best choice.
C Use 23 or 24 only if symmetry is present.  Use MF = 29 if the
C complete linear system solution is to be provided by the user.
C The following four parameters must also be set.
C  IWORK(1) = LWP  = length of real array WP for preconditioning.
C  IWORK(2) = LIWP = length of integer array IWP for preconditioning.
C  IWORK(3) = JPRE = preconditioner type flag:
C                  = 0 for no preconditioning (P1 = P2 = P = identity)
C                  = 1 for left-only preconditioning (P2 = identity)
C                  = 2 for right-only preconditioning (P1 = identity)
C                  = 3 for two-sided preconditioning (and PCG or PCGS)
C  IWORK(4) = JACFLG = flag for whether JAC is called.
C                    = 0 if JAC is not to be called,
C                    = 1 if JAC is to be called.
C  Use JACFLG = 1 if JAC computes any nonconstant data for use in
C  preconditioning, such as Jacobian elements.
C  The arrays WP and IWP are work arrays under the user's control,
C  for use in the routines that perform preconditioning operations.
C
C D. If the problem is stiff, you must supply two routines that deal
C with the preconditioning of the linear systems to be solved.
C These are as follows:
C
C     SUBROUTINE JAC (F, NEQ, T, Y, YSV, REWT, FTY,V,HL0,JOK,WP,IWP,IER)
C     DOUBLE PRECISION T, Y(*), YSV(*), REWT(*), FTY(*), V(*), HL0,WP(*)
C     INTEGER IWP(*)
C        This routine must evaluate and preprocess any parts of the
C     Jacobian matrix df/dy involved in the preconditioners P1, P2, P.
C     The Y and FTY arrays contain the current values of y and f(t,y),
C     respectively, and YSV also contains the current value of y.
C     The array V is work space of length NEQ.
C     JAC must multiply all computed Jacobian elements by the scalar
C     -HL0, add the identity matrix, and do any factorization
C     operations called for, in preparation for solving linear systems
C     with a coefficient matrix of P1, P2, or P.  The matrix P1*P2 or P
C     should be an approximation to  identity - hl0 * (df/dy).
C     JAC should return IER = 0 if successful, and IER .ne. 0 if not.
C     (If IER .ne. 0, a smaller time step will be tried.)
C     JAC may alter Y and V, but not YSV, REWT, FTY, or HL0.
C     The JOK argument can be ignored (or see full description below).
C
C     SUBROUTINE PSOL (NEQ, T, Y, FTY, WK, HL0, WP, IWP, B, LR, IER)
C     DOUBLE PRECISION T, Y(*), FTY(*), WK(*), HL0, WP(*), B(*)
C     INTEGER IWP(*)
C        This routine must solve a linear system with B as right-hand
C     side and one of the preconditioning matrices, P1, P2, or P, as
C     coefficient matrix, and return the solution vector in B.
C     LR is a flag concerning left vs right preconditioning, input
C     to PSOL.  PSOL is to use P1 if LR = 1 and P2 if LR = 2.
C     In the case of the PCG or PCGS method, LR will be 3, and PSOL
C     should solve the system P*x = B with the preconditioner matrix P.
C     In the case MF = 29 (no Krylov iteration), LR will be 0,
C     and PSOL is to return in B the desired approximate solution
C     to A * x = B, where A = identity - hl0 * (df/dy).
C     PSOL can use data generated in the JAC routine and stored in
C     WP and IWP.  WK is a work array of length NEQ.
C     The argument HL0 is the current value of the scalar appearing
C     in the linear system.  If the old value, at the time of the last
C     JAC call, is needed, it must have been saved by JAC in WP.
C     on return, PSOL should set the error flag IER as follows:
C       IER = 0 if PSOL was successful,
C       IER .gt. 0 if a recoverable error occurred, meaning that the
C              time step will be retried,
C       IER .lt. 0 if an unrecoverable error occurred, meaning that the
C              solver is to stop immediately.
C
C E. Write a main program which calls Subroutine DLSODKR once for
C each point at which answers are desired.  This should also provide
C for possible use of logical unit 6 for output of error messages
C by DLSODKR.  On the first call to DLSODKR, supply arguments as
C follows:
C F      = name of subroutine for right-hand side vector f.
C          This name must be declared External in calling program.
C NEQ    = number of first order ODEs.
C Y      = array of initial values, of length NEQ.
C T      = the initial value of the independent variable.
C TOUT   = first point where output is desired (.ne. T).
C ITOL   = 1 or 2 according as ATOL (below) is a scalar or array.
C RTOL   = relative tolerance parameter (scalar).
C ATOL   = absolute tolerance parameter (scalar or array).
C          The estimated local error in y(i) will be controlled so as
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
C ISTATE = integer flag (input and output).  Set ISTATE = 1.
C IOPT   = 0 to indicate no optional inputs used.
C RWORK  = real work array of length at least:
C             20 + 16*NEQ + 3*NG           for MF = 10,
C             45 + 17*NEQ + 3*NG + LWP     for MF = 21,
C             61 + 17*NEQ + 3*NG + LWP     for MF = 22,
C             20 + 15*NEQ + 3*NG + LWP     for MF = 23 or 24,
C             20 + 12*NEQ + 3*NG + LWP     for MF = 29.
C LRW    = declared length of RWORK (in user's dimension).
C IWORK  = integer work array of length at least:
C             30            for MF = 10,
C             35 + LIWP     for MF = 21,
C             30 + LIWP     for MF = 22, 23, 24, or 29.
C LIW    = declared length of IWORK (in user's dimension).
C JAC,PSOL = names of subroutines for preconditioning.
C          These names must be declared External in the calling program.
C MF     = method flag.  Standard values are:
C          10 for nonstiff (Adams) method.
C          21 for stiff (BDF) method, with preconditioned SIOM.
C          22 for stiff method, with preconditioned GMRES method.
C          23 for stiff method, with preconditioned CG method.
C          24 for stiff method, with scaled preconditioned CG method.
C          29 for stiff method, with user's PSOL routine only.
C G      = name of subroutine for constraint functions, whose
C          roots are desired during the integration.
C          This name must be declared External in calling program.
C NG     = number of constraint functions g(i).  If there are none,
C          set NG = 0, and pass a dummy name for G.
C JROOT  = integer array of length NG for output of root information.
C          See next paragraph.
C Note that the main program must declare arrays Y, RWORK, IWORK,
C JROOT, and possibly ATOL.
C
C F. The output from the first call (or any call) is:
C      Y = array of computed values of y(t) vector.
C      T = corresponding value of independent variable (normally TOUT).
C ISTATE = 2 or 3  if DLSODKR was successful, negative otherwise.
C           2 means no root was found, and TOUT was reached as desired.
C           3 means a root was found prior to reaching TOUT.
C          -1 means excess work done on this call (perhaps wrong MF).
C          -2 means excess accuracy requested (tolerances too small).
C          -3 means illegal input detected (see printed message).
C          -4 means repeated error test failures (check all inputs).
C          -5 means repeated convergence failures (perhaps bad JAC
C             or PSOL routine supplied or wrong choice of MF or
C             tolerances, or this solver is inappropriate).
C          -6 means error weight became zero during problem. (Solution
C             component i vanished, and ATOL or ATOL(i) = 0.)
C          -7 means an unrecoverable error occurred in PSOL.
C JROOT  = array showing roots found if ISTATE = 3 on return.
C          JROOT(i) = 1 if g(i) has a root at T, or 0 otherwise.
C
C G. To continue the integration after a successful return, proceed
C as follows:
C (a) If ISTATE = 2 on return, reset TOUT and call DLSODKR again.
C (b) If ISTATE = 3 on return, reset ISTATE to 2 and call DLSODKR again.
C In either case, no other parameters need be reset.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C Full Description of User Interface to DLSODKR.
C
C The user interface to DLSODKR consists of the following parts.
C
C 1.   The call sequence to Subroutine DLSODKR, which is a driver
C      routine for the solver.  This includes descriptions of both
C      the call sequence arguments and of user-supplied routines.
C      Following these descriptions is a description of
C      optional inputs available through the call sequence, and then
C      a description of optional outputs (in the work arrays).
C
C 2.   Descriptions of other routines in the DLSODKR package that may be
C      (optionally) called by the user.  These provide the ability to
C      alter error message handling, save and restore the internal
C      Common, and obtain specified derivatives of the solution y(t).
C
C 3.   Descriptions of Common blocks to be declared in overlay
C      or similar environments, or to be saved when doing an interrupt
C      of the problem and continued solution later.
C
C 4.   Description of two routines in the DLSODKR package, either of
C      which the user may replace with his/her own version, if desired.
C      These relate to the measurement of errors.
C
C-----------------------------------------------------------------------
C Part 1.  Call Sequence.
C
C The call sequence parameters used for input only are
C  F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, PSOL, MF,
C  G, and NG,
C that used only for output is  JROOT,
C and those used for both input and output are
C  Y, T, ISTATE.
C The work arrays RWORK and IWORK are also used for conditional and
C optional inputs and optional outputs.  (The term output here refers
C to the return from Subroutine DLSODKR to the user's calling program.)
C
C The legality of input parameters will be thoroughly checked on the
C initial call for the problem, but not checked thereafter unless a
C change in input parameters is flagged by ISTATE = 3 on input.
C
C The descriptions of the call arguments are as follows.
C
C F      = the name of the user-supplied subroutine defining the
C          ODE system.  The system must be put in the first-order
C          form dy/dt = f(t,y), where f is a vector-valued function
C          of the scalar t and the vector y.  Subroutine F is to
C          compute the function f.  It is to have the form
C               SUBROUTINE F (NEQ, T, Y, YDOT)
C               DOUBLE PRECISION T, Y(*), YDOT(*)
C          where NEQ, T, and Y are input, and the array YDOT = f(t,y)
C          is output.  Y and YDOT are arrays of length NEQ.
C          Subroutine F should not alter Y(1),...,Y(NEQ).
C          F must be declared External in the calling program.
C
C          Subroutine F may access user-defined quantities in
C          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in F) and/or Y has length exceeding NEQ(1).
C          See the descriptions of NEQ and Y below.
C
C          If quantities computed in the F routine are needed
C          externally to DLSODKR, an extra call to F should be made
C          for this purpose, for consistent and accurate results.
C          If only the derivative dy/dt is needed, use DINTDY instead.
C
C NEQ    = the size of the ODE system (number of first order
C          ordinary differential equations).  Used only for input.
C          NEQ may be decreased, but not increased, during the problem.
C          If NEQ is decreased (with ISTATE = 3 on input), the
C          remaining components of Y should be left undisturbed, if
C          these are to be accessed in the user-supplied routines.
C
C          Normally, NEQ is a scalar, and it is generally referred to
C          as a scalar in this user interface description.  However,
C          NEQ may be an array, with NEQ(1) set to the system size.
C          (The DLSODKR package accesses only NEQ(1).)  In either case,
C          this parameter is passed as the NEQ argument in all calls
C          to the user-supplied routines.  Hence, if it is an array,
C          locations NEQ(2),... may be used to store other integer data
C          and pass it to the user-supplied routines. Each such routine
C          must include NEQ in a Dimension statement in that case.
C
C Y      = a real array for the vector of dependent variables, of
C          length NEQ or more.  Used for both input and output on the
C          first call (ISTATE = 1), and only for output on other calls.
C          On the first call, Y must contain the vector of initial
C          values.  On output, Y contains the computed solution vector,
C          evaluated at T.  If desired, the Y array may be used
C          for other purposes between calls to the solver.
C
C          This array is passed as the Y argument in all calls to F, G,
C          JAC, and PSOL.  Hence its length may exceed NEQ, and
C          locations Y(NEQ+1),... may be used to store other real data
C          and pass it to the user-supplied routines.
C          (The DLSODKR package accesses only Y(1),...,Y(NEQ).)
C
C T      = the independent variable.  On input, T is used only on the
C          first call, as the initial point of the integration.
C          On output, after each call, T is the value at which a
C          computed solution y is evaluated (usually the same as TOUT).
C          If a root was found, T is the computed location of the
C          root reached first, on output.
C          On an error return, T is the farthest point reached.
C
C TOUT   = the next value of t at which a computed solution is desired.
C          Used only for input.
C
C          When starting the problem (ISTATE = 1), TOUT may be equal
C          to T for one call, then should .ne. T for the next call.
C          For the initial T, an input value of TOUT .ne. T is used
C          in order to determine the direction of the integration
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
C              4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
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
C          in which case answers at T = TOUT are returned first).
C
C ISTATE = an index used for input and output to specify the
C          the state of the calculation.
C
C          On input, the values of ISTATE are as follows.
C          1  means this is the first call for the problem
C             (initializations will be done).  See note below.
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
C             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, MF,
C             and any of the optional inputs except H0.
C             In addition, immediately following a return with
C             ISTATE = 3 (root found), NG and G may be changed.
C             (But changing NG from 0 to .gt. 0 is not allowed.)
C          Note:  A preliminary call with TOUT = T is not counted
C          as a first call here, as no initialization or checking of
C          input is done.  (Such a call is sometimes useful for the
C          purpose of outputting the initial conditions.)
C          Thus the first call for which TOUT .ne. T requires
C          ISTATE = 1 on input.
C
C          On output, ISTATE has the following values and meanings.
C           1  means nothing was done; TOUT = T and ISTATE = 1 on input.
C           2  means the integration was performed successfully.
C           3  means the integration was successful, and one or more
C              roots were found before satisfying the stop condition
C              specified by ITASK.  See JROOT.
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
C          -6  means EWT(i) became zero for some i during the
C              integration.  Pure relative error control (ATOL(i)=0.0)
C              was requested on a variable which has now vanished.
C              The integration was successful as far as T.
C          -7  means the PSOL routine returned an unrecoverable error
C              flag (IER .lt. 0).  The integration was successful as
C              far as T.
C
C          Note:  Since the normal output value of ISTATE is 2,
C          it does not need to be reset for normal continuation.
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
C             20 + NYH*(MAXORD+1) + 3*NEQ + 3*NG + LENLS + LWP    where
C          NYH    = the initial value of NEQ,
C          MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a
C                   smaller value is given as an optional input),
C          LENLS = length of work space for linear system (Krylov)
C                  method, excluding preconditioning:
C            LENLS = 0                               if MITER = 0,
C            LENLS = NEQ*(MAXL+3) + MAXL**2          if MITER = 1,
C            LENLS = NEQ*(MAXL+3+MIN(1,MAXL-KMP))
C                 + (MAXL+3)*MAXL + 1                if MITER = 2,
C            LENLS = 6*NEQ                           if MITER = 3 or 4,
C            LENLS = 3*NEQ                           if MITER = 9.
C          (See the MF description for METH and MITER, and the
C          list of optional inputs for MAXL and KMP.)
C          LWP = length of real user work space for preconditioning
C          (see JAC/PSOL).
C          Thus if default values are used and NEQ is constant,
C          this length is:
C             20 + 16*NEQ + 3*NG           for MF = 10,
C             45 + 24*NEQ + 3*NG + LWP     for MF = 11,
C             61 + 24*NEQ + 3*NG + LWP     for MF = 12,
C             20 + 22*NEQ + 3*NG + LWP     for MF = 13 or 14,
C             20 + 19*NEQ + 3*NG + LWP     for MF = 19,
C             20 + 9*NEQ + 3*NG            for MF = 20,
C             45 + 17*NEQ + 3*NG + LWP     for MF = 21,
C             61 + 17*NEQ + 3*NG + LWP     for MF = 22,
C             20 + 15*NEQ + 3*NG + LWP     for MF = 23 or 24,
C             20 + 12*NEQ + 3*NG + LWP     for MF = 29.
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
C             30                 if MITER = 0 (MF = 10 or 20),
C             30 + MAXL + LIWP   if MITER = 1 (MF = 11, 21),
C             30 + LIWP          if MITER = 2, 3, 4, or 9.
C          MAXL = 5 unless a different optional input value is given.
C          LIWP = length of integer user work space for preconditioning
C          (see conditional input list following).
C          The first few words of IWORK are used for conditional and
C          optional inputs and optional outputs.
C
C          The following 4 words in IWORK are conditional inputs,
C          required if MITER .ge. 1:
C          IWORK(1) = LWP  = length of real array WP for use in
C                     preconditioning (part of RWORK array).
C          IWORK(2) = LIWP = length of integer array IWP for use in
C                     preconditioning (part of IWORK array).
C                     The arrays WP and IWP are work arrays under the
C                     user's control, for use in the routines that
C                     perform preconditioning operations (JAC and PSOL).
C          IWORK(3) = JPRE = preconditioner type flag:
C                   = 0 for no preconditioning (P1 = P2 = P = identity)
C                   = 1 for left-only preconditioning (P2 = identity)
C                   = 2 for right-only preconditioning (P1 = identity)
C                   = 3 for two-sided preconditioning (and PCG or PCGS)
C          IWORK(4) = JACFLG = flag for whether JAC is called.
C                   = 0 if JAC is not to be called,
C                   = 1 if JAC is to be called.
C                     Use JACFLG = 1 if JAC computes any nonconstant
C                     data needed in preconditioning operations,
C                     such as some of the Jacobian elements.
C
C LIW    = the length of the array IWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C Note:  The work arrays must not be altered between calls to DLSODKR
C for the same problem, except possibly for the conditional and
C optional inputs, and except for the last 3*NEQ words of RWORK.
C The latter space is used for internal scratch space, and so is
C available for use by the user outside DLSODKR between calls, if
C desired (but not for use by any of the user-supplied routines).
C
C JAC    = the name of the user-supplied routine to compute any
C          Jacobian elements (or approximations) involved in the
C          matrix preconditioning operations (MITER .ge. 1).
C          It is to have the form
C            SUBROUTINE JAC (F, NEQ, T, Y, YSV, REWT, FTY, V,
C           1                HL0, JOK, WP, IWP, IER)
C            DOUBLE PRECISION T, Y(*), YSV(*), REWT(*), FTY(*), V(*),
C           1                 HL0, WP(*)
C            INTEGER IWP(*)
C          This routine must evaluate and preprocess any parts of the
C          Jacobian matrix df/dy used in the preconditioners P1, P2, P.
C          The Y and FTY arrays contain the current values of y and
C          f(t,y), respectively, and the YSV array also contains
C          the current y vector.  The array V is work space of length
C          NEQ for use by JAC.  REWT is the array of reciprocal error
C          weights (1/EWT).  JAC must multiply all computed Jacobian
C          elements by the scalar -HL0, add the identity matrix, and do
C          any factorization operations called for, in preparation
C          for solving linear systems with a coefficient matrix of
C          P1, P2, or P.  The matrix P1*P2 or P should be an
C          approximation to  identity - hl0 * (df/dy).  JAC should
C          return IER = 0 if successful, and IER .ne. 0 if not.
C          (If IER .ne. 0, a smaller time step will be tried.)
C          The arrays WP (of length LWP) and IWP (of length LIWP)
C          are for use by JAC and PSOL for work space and for storage
C          of data needed for the solution of the preconditioner
C          linear systems.  Their lengths and contents are under the
C          user's control.
C               The argument JOK is an input flag for optional use
C          by JAC in deciding whether to recompute Jacobian elements
C          or use saved values.  If JOK = -1, then JAC must compute
C          any relevant Jacobian elements (or approximations) used in
C          the preconditioners.  Optionally, JAC may also save these
C          elements for later reuse.  If JOK = 1, the integrator has
C          made a judgement (based on the convergence history and the
C          value of HL0) that JAC need not recompute Jacobian elements,
C          but instead use saved values, and the current value of HL0,
C          to reconstruct the preconditioner matrices, followed by
C          any required factorizations.  This may be cost-effective if
C          Jacobian elements are costly and storage is available.
C               JAC may alter Y and V, but not YSV, REWT, FTY, or HL0.
C          JAC must be declared External in the calling program.
C               Subroutine JAC may access user-defined quantities in
C          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in JAC) and/or Y has length exceeding NEQ(1).
C          See the descriptions of NEQ and Y above.
C
C PSOL   = the name of the user-supplied routine for the
C          solution of preconditioner linear systems.
C          It is to have the form
C            SUBROUTINE PSOL (NEQ, T, Y, FTY, WK,HL0, WP,IWP, B, LR,IER)
C            DOUBLE PRECISION T, Y(*), FTY(*), WK(*), HL0, WP(*), B(*)
C            INTEGER IWP(*)
C          This routine must solve a linear system with B as right-hand
C          side and one of the preconditioning matrices, P1, P2, or P,
C          as coefficient matrix, and return the solution vector in B.
C          LR is a flag concerning left vs right preconditioning, input
C          to PSOL.  PSOL is to use P1 if LR = 1 and P2 if LR = 2.
C          In the case of the PCG or PCGS method, LR will be 3, and PSOL
C          should solve the system P*x = B with the preconditioner P.
C          In the case MITER = 9 (no Krylov iteration), LR will be 0,
C          and PSOL is to return in B the desired approximate solution
C          to A * x = B, where A = identity - hl0 * (df/dy).
C          PSOL can use data generated in the JAC routine and stored in
C          WP and IWP.
C          The Y and FTY arrays contain the current values of y and
C          f(t,y), respectively.  The array WK is work space of length
C          NEQ for use by PSOL.
C          The argument HL0 is the current value of the scalar appearing
C          in the linear system.  If the old value, as of the last
C          JAC call, is needed, it must have been saved by JAC in WP.
C          On return, PSOL should set the error flag IER as follows:
C	     IER = 0 if PSOL was successful,
C            IER .gt. 0 on a recoverable error, meaning that the
C                   time step will be retried,
C            IER .lt. 0 on an unrecoverable error, meaning that the
C                   solver is to stop immediately.
C          PSOL may not alter Y, FTY, or HL0.
C          PSOL must be declared External in the calling program.
C               Subroutine PSOL may access user-defined quantities in
C          NEQ(2),... and Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in PSOL) and/or Y has length exceeding NEQ(1).
C          See the descriptions of NEQ and Y above.
C
C MF     = the method flag.  Used only for input.  The legal values of
C          MF are 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 24, and 29.
C          MF has decimal digits METH and MITER: MF = 10*METH + MITER.
C          METH indicates the basic linear multistep method:
C            METH = 1 means the implicit Adams method.
C            METH = 2 means the method based on Backward
C                     Differentiation Formulas (BDFs).
C          MITER indicates the corrector iteration method:
C            MITER = 0 means functional iteration (no linear system
C                      is involved).
C            MITER = 1 means Newton iteration with Scaled Preconditioned
C                      Incomplete Orthogonalization Method (SPIOM)
C                      for the linear systems.
C            MITER = 2 means Newton iteration with Scaled Preconditioned
C                      Incomplete Generalized Minimal Residual method
C                      (SPIGMR) for the linear systems.
C            MITER = 3 means Newton iteration with Preconditioned
C                      Conjugate Gradient method (PCG)
C                      for the linear systems.
C            MITER = 4 means Newton iteration with scaled preconditioned
C                      Conjugate Gradient method (PCGS)
C                      for the linear systems.
C            MITER = 9 means Newton iteration with only the
C                      user-supplied PSOL routine called (no Krylov
C                      iteration) for the linear systems.
C                      JPRE is ignored, and PSOL is called with LR = 0.
C          See comments in the introduction about the choice of MITER.
C          If MITER .ge. 1, the user must supply routines JAC and PSOL
C          (the names are arbitrary) as described above.
C          For MITER = 0, a dummy argument can be used.
C
C G      = the name of subroutine for constraint functions, whose
C          roots are desired during the integration.  It is to have
C          the form
C               SUBROUTINE G (NEQ, T, Y, NG, GOUT)
C               DOUBLE PRECISION T, Y(*), GOUT(NG)
C          where NEQ, T, Y, and NG are input, and the array GOUT
C          is output.  NEQ, T, and Y have the same meaning as in
C          the F routine, and GOUT is an array of length NG.
C          For i = 1,...,NG, this routine is to load into GOUT(i)
C          the value at (t,y) of the i-th constraint function g(i).
C          DLSODKR will find roots of the g(i) of odd multiplicity
C          (i.e. sign changes) as they occur during the integration.
C          G must be declared External in the calling program.
C
C          Caution: Because of numerical errors in the functions
C          g(i) due to roundoff and integration error, DLSODKR may
C          return false roots, or return the same root at two or more
C          nearly equal values of t.  If such false roots are
C          suspected, the user should consider smaller error tolerances
C          and/or higher precision in the evaluation of the g(i).
C
C          If a root of some g(i) defines the end of the problem,
C          the input to DLSODKR should nevertheless allow integration
C          to a point slightly past that root, so that DLSODKR can
C          locate the root by interpolation.
C
C          Subroutine G may access user-defined quantities in
C          NEQ(2),... and Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in G) and/or Y has length exceeding NEQ(1).
C          See the descriptions of NEQ and Y above.
C
C NG     = number of constraint functions g(i).  If there are none,
C          set NG = 0, and pass a dummy name for G.
C
C JROOT  = integer array of length NG.  Used only for output.
C          On a return with ISTATE = 3 (one or more roots found),
C          JROOT(i) = 1 if g(i) has a root at t, or JROOT(i) = 0 if not.
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
C DELT    RWORK(8)  convergence test constant in Krylov iteration
C                   algorithm.  The default is .05.
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
C
C MAXL    IWORK(8)  maximum number of iterations in the SPIOM, SPIGMR,
C                   PCG, or PCGS algorithm (.le. NEQ).
C                   The default is MAXL = MIN(5,NEQ).
C
C KMP     IWORK(9)  number of vectors on which orthogonalization
C                   is done in SPIOM or SPIGMR algorithm (.le. MAXL).
C                   The default is KMP = MAXL.
C                   Note:  When KMP .lt. MAXL and MF = 22, the length
C                          of RWORK must be defined accordingly.  See
C                          the definition of RWORK above.
C-----------------------------------------------------------------------
C Optional Outputs.
C
C As optional additional output from DLSODKR, the variables listed
C below are quantities related to the performance of DLSODKR
C which are available to the user.  These are communicated by way of
C the work arrays, but also have internal mnemonic names as shown.
C Except where stated otherwise, all of these outputs are defined
C on any successful return from DLSODKR, and on any return with
C ISTATE = -1, -2, -4, -5, -6, or -7.  On an illegal input return
C (ISTATE = -3), they will be unchanged from their existing values
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
C NGE     IWORK(10) the number of g evaluations for the problem so far.
C
C NST     IWORK(11) the number of steps taken for the problem so far.
C
C NFE     IWORK(12) the number of f evaluations for the problem so far.
C
C NPE     IWORK(13) the number of calls to JAC so far (for evaluation
C                   of preconditioners).
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
C NNI     IWORK(19) number of nonlinear iterations so far (each of
C                   which calls an iterative linear solver).
C
C NLI     IWORK(20) number of linear iterations so far.
C                   Note: A measure of the success of algorithm is
C                   the average number of linear iterations per
C                   nonlinear iteration, given by NLI/NNI.
C                   If this is close to MAXL, MAXL may be too small.
C
C NPS     IWORK(21) number of preconditioning solve operations
C                   (PSOL calls) so far.
C
C NCFN    IWORK(22) number of convergence failures of the nonlinear
C                   (Newton) iteration so far.
C                   Note: A measure of success is the overall
C                   rate of nonlinear convergence failures, NCFN/NST.
C
C NCFL    IWORK(23) number of convergence failures of the linear
C                   iteration so far.
C                   Note: A measure of success is the overall
C                   rate of linear convergence failures, NCFL/NNI.
C
C NSFI    IWORK(24) number of functional iteration steps so far.
C                   Note: A measure of the extent to which the
C                   problem is nonstiff is the ratio NSFI/NST.
C
C NJEV    IWORK(25) number of JAC calls with JOK = -1 so far
C                   (number of evaluations of Jacobian data).
C
C The following two arrays are segments of the RWORK array which
C may also be of interest to the user as optional outputs.
C For each array, the table below gives its internal name,
C its base address in RWORK, and its description.
C
C Name    Base Address      Description
C
C YH      21 + 3*NG      the Nordsieck history array, of size NYH by
C                        (NQCUR + 1), where NYH is the initial value
C                        of NEQ.  For j = 0,1,...,NQCUR, column j+1
C                        of YH contains HCUR**j/factorial(j) times
C                        the j-th derivative of the interpolating
C                        polynomial currently representing the solution,
C                        evaluated at t = TCUR.
C
C ACOR     LENRW-NEQ+1   array of size NEQ used for the accumulated
C                        corrections on each step, scaled on output
C                        to represent the estimated local error in y
C                        on the last step.  This is the vector E in
C                        the description of the error control.  It is
C                        defined only on a successful return from
C                        DLSODKR.
C
C-----------------------------------------------------------------------
C Part 2.  Other Routines Callable.
C
C The following are optional calls which the user may make to
C gain additional capabilities in conjunction with DLSODKR.
C (The routines XSETUN and XSETF are designed to conform to the
C SLATEC error handling package.)
C
C     Form of Call                  Function
C   CALL XSETUN(LUN)          Set the logical unit number, LUN, for
C                             output of messages from DLSODKR, if
C                             the default is not desired.
C                             The default value of LUN is 6.
C
C   CALL XSETF(MFLAG)         Set a flag to control the printing of
C                             messages by DLSODKR.
C                             MFLAG = 0 means do not print. (Danger:
C                             This risks losing valuable information.)
C                             MFLAG = 1 means print (the default).
C
C                             Either of the above calls may be made at
C                             any time and will take effect immediately.
C
C   CALL DSRCKR(RSAV,ISAV,JOB) saves and restores the contents of
C                             the internal Common blocks used by
C                             DLSODKR (see Part 3 below).
C                             RSAV must be a real array of length 228
C                             or more, and ISAV must be an integer
C                             array of length 63 or more.
C                             JOB=1 means save Common into RSAV/ISAV.
C                             JOB=2 means restore Common from RSAV/ISAV.
C                                DSRCKR is useful if one is
C                             interrupting a run and restarting
C                             later, or alternating between two or
C                             more problems solved with DLSODKR.
C
C   CALL DINTDY(,,,,,)        Provide derivatives of y, of various
C        (see below)          orders, at a specified point t, if
C                             desired.  It may be called only after
C                             a successful return from DLSODKR.
C
C The detailed instructions for using DINTDY are as follows.
C The form of the call is:
C
C   LYH = 21 + 3*NG
C   CALL DINTDY (T, K, RWORK(LYH), NYH, DKY, IFLAG)
C
C The input parameters are:
C
C T         = value of independent variable where answers are desired
C             (normally the same as the T last returned by DLSODKR).
C             For valid results, T must lie between TCUR - HU and TCUR.
C             (See optional outputs for TCUR and HU.)
C K         = integer order of the derivative desired.  K must satisfy
C             0 .le. K .le. NQCUR, where NQCUR is the current order
C             (see optional outputs).  The capability corresponding
C             to K = 0, i.e. computing y(T), is already provided
C             by DLSODKR directly.  Since NQCUR .ge. 1, the first
C             derivative dy/dt is always available with DINTDY.
C LYH       = 21 + 3*NG = base address in RWORK of the history array YH.
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
C If DLSODKR is to be used in an overlay situation, the user
C must declare, in the primary overlay, the variables in:
C   (1) the call sequence to DLSODKR, and
C   (2) the four internal Common blocks
C         /DLS001/  of length  255  (218 double precision words
C                      followed by 37 integer words),
C         /DLS002/  of length   5  (1 double precision word
C                      followed by  4 integer words),
C         /DLPK01/  of length  17  (4 double precision words
C                      followed by 13 integer words),
C         /DLSR01/  of length  14     (5 double precision words
C                      followed by  9 integer words).
C
C If DLSODKR is used on a system in which the contents of internal
C Common blocks are not preserved between calls, the user should
C declare the above Common blocks in the calling program to insure
C that their contents are preserved.
C
C If the solution of a given problem by DLSODKR is to be interrupted
C and then later continued, such as when restarting an interrupted run
C or alternating between two or more problems, the user should save,
C following the return from the last DLSODKR call prior to the
C interruption, the contents of the call sequence variables and the
C internal Common blocks, and later restore these values before the
C next DLSODKR call for that problem.  To save and restore the Common
C blocks, use Subroutine DSRCKR (see Part 2 above).
C
C-----------------------------------------------------------------------
C Part 4.  Optionally Replaceable Solver Routines.
C
C Below are descriptions of two routines in the DLSODKR package which
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
C where NEQ, ITOL, RTOL, and ATOL are as in the DLSODKR call sequence,
C YCUR contains the current dependent variable vector, and
C EWT is the array of weights set by DEWSET.
C
C If the user supplies this subroutine, it must return in EWT(i)
C (i = 1,...,NEQ) a positive quantity suitable for comparing errors
C in y(i) to.  The EWT array returned by DEWSET is passed to the DVNORM
C routine (see below), and also used by DLSODKR in the computation
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
C value of DVNORM suitable for use in the error control in DLSODKR.
C None of the arguments should be altered by DVNORM.
C For example, a user-supplied DVNORM routine might:
C   -substitute a max-norm of (V(i)*W(i)) for the RMS-norm, or
C   -ignore some components of V in the norm, with the effect of
C    suppressing the error control on those components of y.
C-----------------------------------------------------------------------
C
C***REVISION HISTORY  (YYYYMMDD)
C 19900117  DATE WRITTEN
C 19900503  Added iteration switching (functional/Newton).
C 19900802  Added flag for Jacobian-saving in user preconditioner.
C 19900910  Added new initial stepsize routine LHIN.
C 19901019  Corrected LHIN - y array restored.
C 19910909  Changed names STOPK to STOKA, PKSET to SETPK;
C           removed unused variables in driver declarations;
C           minor corrections to main prologue.
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
C           numerous corrections to prologues and internal comments.
C 20010507  Converted single precision source to double precision.
C 20020502  Corrected declarations in descriptions of user routines.
C 20030603  Corrected duplicate type declaration for DUMACH.
C 20031105  Restored 'own' variables to Common blocks, to enable
C           interrupt/restart feature.
C 20031112  Added SAVE statements for data-loaded constants.
C 20031117  Changed internal name NPE to NJE.
C
C-----------------------------------------------------------------------
C Other routines in the DLSODKR package.
C
C In addition to Subroutine DLSODKR, the DLSODKR package includes the
C following subroutines and function routines:
C  DLHIN    calculates a step size to be attempted initially.
C  DRCHEK   does preliminary checking for roots, and serves as an
C           interface between Subroutine DLSODKR and Subroutine DROOTS.
C  DROOTS   finds the leftmost root of a set of functions.
C  DINTDY   computes an interpolated value of the y vector at t = TOUT.
C  DEWSET   sets the error weight vector EWT before each step.
C  DVNORM   computes the weighted RMS-norm of a vector.
C  DSTOKA   is the core integrator, which does one step of the
C           integration and the associated error control.
C  DCFODE   sets all method coefficients and test constants.
C  DSETPK   interfaces between DSTOKA and the JAC routine.
C  DSOLPK   manages solution of linear system in Newton iteration.
C  DSPIOM   performs the SPIOM algorithm.
C  DATV     computes a scaled, preconditioned product (I-hl0*J)*v.
C  DORTHOG  orthogonalizes a vector against previous basis vectors.
C  DHEFA    generates an LU factorization of a Hessenberg matrix.
C  DHESL    solves a Hessenberg square linear system.
C  DSPIGMR  performs the SPIGMR algorithm.
C  DHEQR    generates a QR factorization of a Hessenberg matrix.
C  DHELS    finds the least squares solution of a Hessenberg system.
C  DPCG     performs preconditioned conjugate gradient algorithm (PCG).
C  DPCGS    performs the PCGS algorithm.
C  DATP     computes the product A*p, where A = I - hl0*df/dy.
C  DUSOL    interfaces to the user's PSOL routine (MITER = 9).
C  DSRCKR   is a user-callable routine to save and restore
C           the contents of the internal Common blocks.
C  DAXPY, DCOPY, DDOT, DNRM2, and DSCAL   are basic linear
C           algebra modules (from the BLAS collection).
C  DUMACH   computes the unit roundoff in a machine-independent manner.
C  XERRWD, XSETUN, XSETF, IXSAV, and IUMACH  handle the printing of all
C           error messages and warnings.  XERRWD is machine-dependent.
C Note:  DVNORM, DDOT, DNRM2, DUMACH, IXSAV, and IUMACH are function
C routines.  All the others are subroutines.
C
C-----------------------------------------------------------------------
      DOUBLE PRECISION DUMACH, DVNORM
      INTEGER INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER NEWT, NSFI, NSLJ, NJEV
      INTEGER LG0, LG1, LGX, IOWNR3, IRFND, ITASKC, NGC, NGE
      INTEGER JPRE, JACFLG, LOCWP, LOCIWP, LSAVX, KMP, MAXL, MNEWT,
     1   NNI, NLI, NPS, NCFN, NCFL
      INTEGER I, I1, I2, IER, IFLAG, IMXER, KGO, LF0,
     1   LENIW, LENIWK, LENRW, LENWM, LENWK, LIWP, LWP, MORD, MXHNL0,
     2   MXSTP0, NCFN0, NCFL0, NITER, NLI0, NNI0, NNID, NSTD, NWARN
      INTEGER IRFP, IRT, LENYH, LYHNEW
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION STIFR
      DOUBLE PRECISION ROWNR3, T0, TLAST, TOUTC
      DOUBLE PRECISION DELT, EPCON, SQRTN, RSQRTN
      DOUBLE PRECISION ATOLI, AVDIM, BIG, EWTI, H0, HMAX, HMX, RCFL,
     1   RCFN, RH, RTOLI, TCRIT, TNEXT, TOLSF, TP, SIZE
      DIMENSION MORD(2)
      LOGICAL IHIT, LAVD, LCFN, LCFL, LWARN
      CHARACTER*60 MSG
      SAVE MORD, MXSTP0, MXHNL0
C-----------------------------------------------------------------------
C The following four internal Common blocks contain
C (a) variables which are local to any subroutine but whose values must
C     be preserved between calls to the routine ("own" variables), and
C (b) variables which are communicated between subroutines.
C The block DLS001 is declared in subroutines DLSODKR, DINTDY,
C DSTOKA, DSOLPK, and DATV.
C The block DLS002 is declared in subroutines DLSODKR and DSTOKA.
C The block DLSR01 is declared in subroutines DLSODKR, DRCHEK, DROOTS.
C The block DLPK01 is declared in subroutines DLSODKR, DSTOKA, DSETPK,
C and DSOLPK.
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
      COMMON /DLS002/ STIFR, NEWT, NSFI, NSLJ, NJEV
C
      COMMON /DLSR01/ ROWNR3(2), T0, TLAST, TOUTC,
     1   LG0, LG1, LGX, IOWNR3(2), IRFND, ITASKC, NGC, NGE
C
      COMMON /DLPK01/ DELT, EPCON, SQRTN, RSQRTN,
     1   JPRE, JACFLG, LOCWP, LOCIWP, LSAVX, KMP, MAXL, MNEWT,
     2   NNI, NLI, NPS, NCFN, NCFL
C
      DATA MORD(1),MORD(2)/12,5/, MXSTP0/500/, MXHNL0/10/
C-----------------------------------------------------------------------
C Block A.
C This code block is executed on every call.
C It tests ISTATE and ITASK for legality and branches appropriately.
C If ISTATE .gt. 1 but the flag INIT shows that initialization has
C not yet been done, an error return occurs.
C If ISTATE = 1 and TOUT = T, return immediately.
C-----------------------------------------------------------------------
      IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
      ITASKC = ITASK
      IF (ISTATE .EQ. 1) GO TO 10
      IF (INIT .EQ. 0) GO TO 603
      IF (ISTATE .EQ. 2) GO TO 200
      GO TO 20
 10   INIT = 0
      IF (TOUT .EQ. T) RETURN
C-----------------------------------------------------------------------
C Block B.
C The next code block is executed for the initial call (ISTATE = 1),
C or for a continuation call with parameter changes (ISTATE = 3).
C It contains checking of all inputs and various initializations.
C
C First check legality of the non-optional inputs NEQ, ITOL, IOPT, MF,
C and NG.
C-----------------------------------------------------------------------
 20   IF (NEQ(1) .LE. 0) GO TO 604
      IF (ISTATE .EQ. 1) GO TO 25
      IF (NEQ(1) .GT. N) GO TO 605
 25   N = NEQ(1)
      IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606
      IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607
      METH = MF/10
      MITER = MF - 10*METH
      IF (METH .LT. 1 .OR. METH .GT. 2) GO TO 608
      IF (MITER .LT. 0) GO TO 608
      IF (MITER .GT. 4 .AND. MITER .LT. 9) GO TO 608
      IF (MITER .GE. 1) JPRE = IWORK(3)
      JACFLG = 0
      IF (MITER .GE. 1) JACFLG = IWORK(4)
      IF (NG .LT. 0) GO TO 630
      IF (ISTATE .EQ. 1) GO TO 35
      IF (IRFND .EQ. 0 .AND. NG .NE. NGC) GO TO 631
 35   NGC = NG
C Next process and check the optional inputs. --------------------------
      IF (IOPT .EQ. 1) GO TO 40
      MAXORD = MORD(METH)
      MXSTEP = MXSTP0
      MXHNIL = MXHNL0
      IF (ISTATE .EQ. 1) H0 = 0.0D0
      HMXI = 0.0D0
      HMIN = 0.0D0
      MAXL = MIN(5,N)
      KMP = MAXL
      DELT = 0.05D0
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
      IF (ISTATE .NE. 1) GO TO 50
      H0 = RWORK(5)
      IF ((TOUT - T)*H0 .LT. 0.0D0) GO TO 614
 50   HMAX = RWORK(6)
      IF (HMAX .LT. 0.0D0) GO TO 615
      HMXI = 0.0D0
      IF (HMAX .GT. 0.0D0) HMXI = 1.0D0/HMAX
      HMIN = RWORK(7)
      IF (HMIN .LT. 0.0D0) GO TO 616
      MAXL = IWORK(8)
      IF (MAXL .EQ. 0) MAXL = 5
      MAXL = MIN(MAXL,N)
      KMP = IWORK(9)
      IF (KMP .EQ. 0 .OR. KMP .GT. MAXL) KMP = MAXL
      DELT = RWORK(8)
      IF (DELT .EQ. 0.0D0) DELT = 0.05D0
C-----------------------------------------------------------------------
C Set work array pointers and check lengths LRW and LIW.
C Pointers to segments of RWORK and IWORK are named by prefixing L to
C the name of the segment.  E.g., the segment YH starts at RWORK(LYH).
C RWORK segments (in order) are denoted  G0, G1, GX, YH, WM,
C EWT, SAVF, SAVX, ACOR.
C-----------------------------------------------------------------------
 60   IF (ISTATE .EQ. 1) NYH = N
      LG0 = 21
      LG1 = LG0 + NG
      LGX = LG1 + NG
      LYHNEW = LGX + NG
      IF (ISTATE .EQ. 1) LYH = LYHNEW
      IF (LYHNEW .EQ. LYH) GO TO 62
C If ISTATE = 3 and NG was changed, shift YH to its new location. ------
      LENYH = L*NYH
      IF (LRW .LT. LYHNEW-1+LENYH) GO TO 62
      I1 = 1
      IF (LYHNEW .GT. LYH) I1 = -1
      CALL DCOPY (LENYH, RWORK(LYH), I1, RWORK(LYHNEW), I1)
      LYH = LYHNEW
 62   CONTINUE
      LWM = LYH + (MAXORD + 1)*NYH
      IF (MITER .EQ. 0) LENWK = 0
      IF (MITER .EQ. 1) LENWK = N*(MAXL+2) + MAXL*MAXL
      IF (MITER .EQ. 2)
     1   LENWK = N*(MAXL+2+MIN(1,MAXL-KMP)) + (MAXL+3)*MAXL + 1
      IF (MITER .EQ. 3 .OR. MITER .EQ. 4) LENWK = 5*N
      IF (MITER .EQ. 9) LENWK = 2*N
      LWP = 0
      IF (MITER .GE. 1) LWP = IWORK(1)
      LENWM = LENWK + LWP
      LOCWP = LENWK + 1
      LEWT = LWM + LENWM
      LSAVF = LEWT + N
      LSAVX = LSAVF + N
      LACOR = LSAVX + N
      IF (MITER .EQ. 0) LACOR = LSAVF + N
      LENRW = LACOR + N - 1
      IWORK(17) = LENRW
      LIWM = 31
      LENIWK = 0
      IF (MITER .EQ. 1) LENIWK = MAXL
      LIWP = 0
      IF (MITER .GE. 1) LIWP = IWORK(2)
      LENIW = 30 + LENIWK + LIWP
      LOCIWP = LENIWK + 1
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
C Load SQRT(N) and its reciprocal in Common. ---------------------------
      SQRTN = SQRT(REAL(N))
      RSQRTN = 1.0D0/SQRTN
      IF (ISTATE .EQ. 1) GO TO 100
C If ISTATE = 3, set flag to signal parameter changes to DSTOKA.--------
      JSTART = -1
      IF (NQ .LE. MAXORD) GO TO 90
C MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into SAVF. ---------
      DO 80 I = 1,N
 80     RWORK(I+LSAVF-1) = RWORK(I+LWM-1)
 90   CONTINUE
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
C The next block is for the initial call only (ISTATE = 1).
C It contains all remaining initializations, the initial call to F,
C and the calculation of the initial step size.
C The error weights in EWT are inverted after being loaded.
C-----------------------------------------------------------------------
 100  UROUND = DUMACH()
      TN = T
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 110
      TCRIT = RWORK(1)
      IF ((TCRIT - TOUT)*(TOUT - T) .LT. 0.0D0) GO TO 625
      IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)
     1   H0 = TCRIT - T
 110  JSTART = 0
      NHNIL = 0
      NST = 0
      NJE = 0
      NSLAST = 0
      NLI0 = 0
      NNI0 = 0
      NCFN0 = 0
      NCFL0 = 0
      NWARN = 0
      HU = 0.0D0
      NQU = 0
      CCMAX = 0.3D0
      MAXCOR = 3
      MSBP = 20
      MXNCF = 10
      NNI = 0
      NLI = 0
      NPS = 0
      NCFN = 0
      NCFL = 0
      NSFI = 0
      NJEV = 0
C Initial call to F.  (LF0 points to YH(*,2).) -------------------------
      LF0 = LYH + NYH
      CALL F (NEQ, T, Y, RWORK(LF0))
      NFE = 1
C Load the initial value vector in YH. ---------------------------------
      DO 115 I = 1,N
 115    RWORK(I+LYH-1) = Y(I)
C Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
      NQ = 1
      H = 1.0D0
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 120 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 621
 120    RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
      IF (H0 .NE. 0.0D0) GO TO 180
C Call DLHIN to set initial step size H0 to be attempted. --------------
      CALL DLHIN (NEQ, N, T, RWORK(LYH), RWORK(LF0), F, TOUT, UROUND,
     1   RWORK(LEWT), ITOL, ATOL, Y, RWORK(LACOR), H0, NITER, IER)
      NFE = NFE + NITER
      IF (IER .NE. 0) GO TO 622
C Adjust H0 if necessary to meet HMAX bound. ---------------------------
 180  RH = ABS(H0)*HMXI
      IF (RH .GT. 1.0D0) H0 = H0/RH
C Load H with H0 and scale YH(*,2) by H0. ------------------------------
      H = H0
      DO 190 I = 1,N
 190    RWORK(I+LF0-1) = H0*RWORK(I+LF0-1)
C Check for a zero of g at T. ------------------------------------------
      IRFND = 0
      TOUTC = TOUT
      IF (NGC .EQ. 0) GO TO 270
      CALL DRCHEK (1, G, NEQ, Y, RWORK(LYH), NYH,
     1   RWORK(LG0), RWORK(LG1), RWORK(LGX), JROOT, IRT)
      IF (IRT .EQ. 0) GO TO 270
      GO TO 632
C-----------------------------------------------------------------------
C Block D.
C The next code block is for continuation calls only (ISTATE = 2 or 3)
C and is to check stop conditions before taking a step.
C First, DRCHEK is called to check for a root within the last step
C taken, other than the last root found there, if any.
C If ITASK = 2 or 5, and y(TN) has not yet been returned to the user
C because of an intervening root, return through Block G.
C-----------------------------------------------------------------------
 200  NSLAST = NST
C
      IRFP = IRFND
      IF (NGC .EQ. 0) GO TO 205
      IF (ITASK .EQ. 1 .OR. ITASK .EQ. 4) TOUTC = TOUT
      CALL DRCHEK (2, G, NEQ, Y, RWORK(LYH), NYH,
     1   RWORK(LG0), RWORK(LG1), RWORK(LGX), JROOT, IRT)
      IF (IRT .NE. 1) GO TO 205
      IRFND = 1
      ISTATE = 3
      T = T0
      GO TO 425
 205  CONTINUE
      IRFND = 0
      IF (IRFP .EQ. 1 .AND. TLAST .NE. TN .AND. ITASK .EQ. 2) GO TO 400
C
      NLI0 = NLI
      NNI0 = NNI
      NCFN0 = NCFN
      NCFL0 = NCFL
      NWARN = 0
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
      IF (IHIT) T = TCRIT
      IF (IRFP .EQ. 1 .AND. TLAST .NE. TN .AND. ITASK .EQ. 5) GO TO 400
      IF (IHIT) GO TO 400
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
      IF (ISTATE .EQ. 2) JSTART = -2
C-----------------------------------------------------------------------
C Block E.
C The next block is normally executed for all calls and contains
C the call to the one-step core integrator DSTOKA.
C
C This is a looping point for the integration steps.
C
C First check for too many steps being taken,
C check for poor Newton/Krylov method performance, update EWT (if not
C at start of problem), check for too much accuracy being requested,
C and check for H below the roundoff level in T.
C-----------------------------------------------------------------------
 250  CONTINUE
      IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500
      NSTD = NST - NSLAST
      NNID = NNI - NNI0
      IF (NSTD .LT. 10 .OR. NNID .EQ. 0) GO TO 255
      AVDIM = REAL(NLI - NLI0)/REAL(NNID)
      RCFN = REAL(NCFN - NCFN0)/REAL(NSTD)
      RCFL = REAL(NCFL - NCFL0)/REAL(NNID)
      LAVD = AVDIM .GT. (MAXL - 0.05D0)
      LCFN = RCFN .GT. 0.9D0
      LCFL = RCFL .GT. 0.9D0
      LWARN = LAVD .OR. LCFN .OR. LCFL
      IF (.NOT.LWARN) GO TO 255
      NWARN = NWARN + 1
      IF (NWARN .GT. 10) GO TO 255
      IF (LAVD) THEN
      MSG='DLSODKR- Warning. Poor iterative algorithm performance seen '
      CALL XERRWD (MSG, 60, 111, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      ENDIF
      IF (LAVD) THEN
      MSG='      at T = R1 by average no. of linear iterations = R2    '
      CALL XERRWD (MSG, 60, 111, 0, 0, 0, 0, 2, TN, AVDIM)
      ENDIF
      IF (LCFN) THEN
      MSG='DLSODKR- Warning. Poor iterative algorithm performance seen '
      CALL XERRWD (MSG, 60, 112, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      ENDIF
      IF (LCFN) THEN
      MSG='      at T = R1 by nonlinear convergence failure rate = R2  '
      CALL XERRWD (MSG, 60, 112, 0, 0, 0, 0, 2, TN, RCFN)
      ENDIF
      IF (LCFL) THEN
      MSG='DLSODKR- Warning. Poor iterative algorithm performance seen '
      CALL XERRWD (MSG, 60, 113, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      ENDIF
      IF (LCFL) THEN
      MSG='      at T = R1 by linear convergence failure rate = R2     '
      CALL XERRWD (MSG, 60, 113, 0, 0, 0, 0, 2, TN, RCFL)
      ENDIF
 255  CONTINUE
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
      MSG = 'DLSODKR-  Warning.. Internal T(=R1) and H(=R2) are'
      CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG='      such that in the machine, T + H = T on the next step  '
      CALL XERRWD (MSG, 60, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '     (H = step size). Solver will continue anyway.'
      CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 2, TN, H)
      IF (NHNIL .LT. MXHNIL) GO TO 290
      MSG = 'DLSODKR-  Above warning has been issued I1 times. '
      CALL XERRWD (MSG, 50, 102, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '     It will not be issued again for this problem.'
      CALL XERRWD (MSG, 50, 102, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
 290  CONTINUE
C-----------------------------------------------------------------------
C     CALL DSTOKA(NEQ,Y,YH,NYH,YH,EWT,SAVF,SAVX,ACOR,WM,IWM,F,JAC,PSOL)
C-----------------------------------------------------------------------
      CALL DSTOKA (NEQ, Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),
     1   RWORK(LSAVF), RWORK(LSAVX), RWORK(LACOR), RWORK(LWM),
     2   IWORK(LIWM), F, JAC, PSOL)
      KGO = 1 - KFLAG
      GO TO (300, 530, 540, 550), KGO
C-----------------------------------------------------------------------
C Block F.
C The following block handles the case of a successful return from the
C core integrator (KFLAG = 0).
C Call DRCHEK to check for a root within the last step.
C Then, if no root was found, check for stop conditions.
C-----------------------------------------------------------------------
 300  INIT = 1
C
      IF (NGC .EQ. 0) GO TO 315
      CALL DRCHEK (3, G, NEQ, Y, RWORK(LYH), NYH,
     1   RWORK(LG0), RWORK(LG1), RWORK(LGX), JROOT, IRT)
      IF (IRT .NE. 1) GO TO 315
      IRFND = 1
      ISTATE = 3
      T = T0
      GO TO 425
 315  CONTINUE
C
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
C ITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
 350  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
C-----------------------------------------------------------------------
C Block G.
C The following block handles all successful returns from DLSODKR.
C If ITASK .ne. 1, Y is loaded from YH and T is set accordingly.
C ISTATE is set to 2, and the optional outputs are loaded into the
C work arrays before returning.
C-----------------------------------------------------------------------
 400  DO 410 I = 1,N
 410    Y(I) = RWORK(I+LYH-1)
      T = TN
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420
      IF (IHIT) T = TCRIT
 420  ISTATE = 2
 425  CONTINUE
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      IWORK(19) = NNI
      IWORK(20) = NLI
      IWORK(21) = NPS
      IWORK(22) = NCFN
      IWORK(23) = NCFL
      IWORK(24) = NSFI
      IWORK(25) = NJEV
      IWORK(10) = NGE
      TLAST = T
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
 500  MSG = 'DLSODKR-  At current T (=R1), MXSTEP (=I1) steps  '
      CALL XERRWD (MSG, 50, 201, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      taken on this call before reaching TOUT     '
      CALL XERRWD (MSG, 50, 201, 0, 1, MXSTEP, 0, 1, TN, 0.0D0)
      ISTATE = -1
      GO TO 580
C EWT(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  EWTI = RWORK(LEWT+I-1)
      MSG = 'DLSODKR-  At T(=R1), EWT(I1) has become R2 .le. 0.'
      CALL XERRWD (MSG, 50, 202, 0, 1, I, 0, 2, TN, EWTI)
      ISTATE = -6
      GO TO 580
C Too much accuracy requested for machine precision. -------------------
 520  MSG = 'DLSODKR-  At T (=R1), too much accuracy requested '
      CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      for precision of machine..  See TOLSF (=R2) '
      CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 2, TN, TOLSF)
      RWORK(14) = TOLSF
      ISTATE = -2
      GO TO 580
C KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
 530  MSG = 'DLSODKR- At T(=R1) and step size H(=R2), the error'
      CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      test failed repeatedly or with ABS(H) = HMIN'
      CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 2, TN, H)
      ISTATE = -4
      GO TO 560
C KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ----
 540  MSG = 'DLSODKR-  At T (=R1) and step size H (=R2), the   '
      CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      corrector convergence failed repeatedly     '
      CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      or with ABS(H) = HMIN   '
      CALL XERRWD (MSG, 30, 205, 0, 0, 0, 0, 2, TN, H)
      ISTATE = -5
      GO TO 580
C KFLAG = -3.  Unrecoverable error from PSOL. --------------------------
 550  MSG = 'DLSODKR-  At T (=R1) an unrecoverable error return'
      CALL XERRWD (MSG, 50, 206, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      was made from Subroutine PSOL     '
      CALL XERRWD (MSG, 40, 206, 0, 0, 0, 0, 1, TN, 0.0D0)
      ISTATE = -7
      GO TO 580
C Compute IMXER if relevant. -------------------------------------------
 560  BIG = 0.0D0
      IMXER = 1
      DO 570 I = 1,N
        SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
        IF (BIG .GE. SIZE) GO TO 570
        BIG = SIZE
        IMXER = I
 570    CONTINUE
      IWORK(16) = IMXER
C Set Y vector, T, and optional outputs. -------------------------------
 580  DO 590 I = 1,N
 590    Y(I) = RWORK(I+LYH-1)
      T = TN
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      IWORK(19) = NNI
      IWORK(20) = NLI
      IWORK(21) = NPS
      IWORK(22) = NCFN
      IWORK(23) = NCFL
      IWORK(24) = NSFI
      IWORK(25) = NJEV
      IWORK(10) = NGE
      TLAST = T
      RETURN
C-----------------------------------------------------------------------
C Block I.
C The following block handles all error returns due to illegal input
C (ISTATE = -3), as detected before calling the core integrator.
C First the error message routine is called.  If the illegal input
C is a negative ISTATE, the run is aborted (apparent infinite loop).
C-----------------------------------------------------------------------
 601  MSG = 'DLSODKR-  ISTATE(=I1) illegal.'
      CALL XERRWD (MSG, 30, 1, 0, 1, ISTATE, 0, 0, 0.0D0, 0.0D0)
      IF (ISTATE .LT. 0) GO TO 800
      GO TO 700
 602  MSG = 'DLSODKR-  ITASK (=I1) illegal.'
      CALL XERRWD (MSG, 30, 2, 0, 1, ITASK, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 603  MSG = 'DLSODKR- ISTATE.gt.1 but DLSODKR not initialized. '
      CALL XERRWD (MSG, 50, 3, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 604  MSG = 'DLSODKR-  NEQ (=I1) .lt. 1    '
      CALL XERRWD (MSG, 30, 4, 0, 1, NEQ(1), 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 605  MSG = 'DLSODKR-  ISTATE = 3 and NEQ increased (I1 to I2).'
      CALL XERRWD (MSG, 50, 5, 0, 2, N, NEQ(1), 0, 0.0D0, 0.0D0)
      GO TO 700
 606  MSG = 'DLSODKR-  ITOL (=I1) illegal. '
      CALL XERRWD (MSG, 30, 6, 0, 1, ITOL, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 607  MSG = 'DLSODKR-  IOPT (=I1) illegal. '
      CALL XERRWD (MSG, 30, 7, 0, 1, IOPT, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 608  MSG = 'DLSODKR-  MF (=I1) illegal.   '
      CALL XERRWD (MSG, 30, 8, 0, 1, MF, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 611  MSG = 'DLSODKR-  MAXORD (=I1) .lt. 0 '
      CALL XERRWD (MSG, 30, 11, 0, 1, MAXORD, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 612  MSG = 'DLSODKR-  MXSTEP (=I1) .lt. 0 '
      CALL XERRWD (MSG, 30, 12, 0, 1, MXSTEP, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 613  MSG = 'DLSODKR-  MXHNIL (=I1) .lt. 0 '
      CALL XERRWD (MSG, 30, 13, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 614  MSG = 'DLSODKR-  TOUT (=R1) behind T (=R2)     '
      CALL XERRWD (MSG, 40, 14, 0, 0, 0, 0, 2, TOUT, T)
      MSG = '      Integration direction is given by H0 (=R1)  '
      CALL XERRWD (MSG, 50, 14, 0, 0, 0, 0, 1, H0, 0.0D0)
      GO TO 700
 615  MSG = 'DLSODKR-  HMAX (=R1) .lt. 0.0 '
      CALL XERRWD (MSG, 30, 15, 0, 0, 0, 0, 1, HMAX, 0.0D0)
      GO TO 700
 616  MSG = 'DLSODKR-  HMIN (=R1) .lt. 0.0 '
      CALL XERRWD (MSG, 30, 16, 0, 0, 0, 0, 1, HMIN, 0.0D0)
      GO TO 700
 617  MSG='DLSODKR-  RWORK length needed, LENRW(=I1), exceeds LRW(=I2) '
      CALL XERRWD (MSG, 60, 17, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0)
      GO TO 700
 618  MSG='DLSODKR-  IWORK length needed, LENIW(=I1), exceeds LIW(=I2) '
      CALL XERRWD (MSG, 60, 18, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0)
      GO TO 700
 619  MSG = 'DLSODKR-  RTOL(I1) is R1 .lt. 0.0       '
      CALL XERRWD (MSG, 40, 19, 0, 1, I, 0, 1, RTOLI, 0.0D0)
      GO TO 700
 620  MSG = 'DLSODKR-  ATOL(I1) is R1 .lt. 0.0       '
      CALL XERRWD (MSG, 40, 20, 0, 1, I, 0, 1, ATOLI, 0.0D0)
      GO TO 700
 621  EWTI = RWORK(LEWT+I-1)
      MSG = 'DLSODKR-  EWT(I1) is R1 .le. 0.0        '
      CALL XERRWD (MSG, 40, 21, 0, 1, I, 0, 1, EWTI, 0.0D0)
      GO TO 700
 622  MSG='DLSODKR- TOUT(=R1) too close to T(=R2) to start integration.'
      CALL XERRWD (MSG, 60, 22, 0, 0, 0, 0, 2, TOUT, T)
      GO TO 700
 623  MSG='DLSODKR-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2) '
      CALL XERRWD (MSG, 60, 23, 0, 1, ITASK, 0, 2, TOUT, TP)
      GO TO 700
 624  MSG='DLSODKR-  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)  '
      CALL XERRWD (MSG, 60, 24, 0, 0, 0, 0, 2, TCRIT, TN)
      GO TO 700
 625  MSG='DLSODKR-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)  '
      CALL XERRWD (MSG, 60, 25, 0, 0, 0, 0, 2, TCRIT, TOUT)
      GO TO 700
 626  MSG = 'DLSODKR-  At start of problem, too much accuracy  '
      CALL XERRWD (MSG, 50, 26, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG='      requested for precision of machine..  See TOLSF (=R1) '
      CALL XERRWD (MSG, 60, 26, 0, 0, 0, 0, 1, TOLSF, 0.0D0)
      RWORK(14) = TOLSF
      GO TO 700
 627  MSG = 'DLSODKR-  Trouble in DINTDY. ITASK = I1, TOUT = R1'
      CALL XERRWD (MSG, 50, 27, 0, 1, ITASK, 0, 1, TOUT, 0.0D0)
      GO TO 700
 630  MSG = 'DLSODKR-  NG (=I1) .lt. 0     '
      CALL XERRWD (MSG, 30, 30, 0, 1, NG, 0, 0, 0.0D0, 0.0D0)
      GO TO 700
 631  MSG = 'DLSODKR-  NG changed (from I1 to I2) illegally,   '
      CALL XERRWD (MSG, 50, 31, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      i.e. not immediately after a root was found.'
      CALL XERRWD (MSG, 50, 31, 0, 2, NGC, NG, 0, 0.0D0, 0.0D0)
      GO TO 700
 632  MSG = 'DLSODKR-  One or more components of g has a root  '
      CALL XERRWD (MSG, 50, 32, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      MSG = '      too near to the initial point.    '
      CALL XERRWD (MSG, 40, 32, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
C
 700  ISTATE = -3
      RETURN
C
 800  MSG = 'DLSODKR-  Run aborted.. apparent infinite loop.   '
      CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, 0.0D0, 0.0D0)
      RETURN
C----------------------- End of Subroutine DLSODKR ---------------------
      END

      SUBROUTINE PCGDS(NN,AA,LENAA,MAXA,PP,START,WORK,IFLAG)
C
C     This subroutine solves a system of equations using the method
C        of Conjugate Gradients.
C     
C     The system to be solved is in the form Bx=b, where
C
C        +--          --+        +-   -+                     
C        |        |     |        |  0  |   T = START(k), where 
C        |   AA   | -PP |        | ... |     
C    B = |        |     | ,  b = |  0  |    |START(k)|=    max    |START(i)|
C        +--------+-----+        +-----+                1<=i<=NN+1
C        |    E(k)**t   |        |  T  |                
C        +--          --+        +-   -+
C
C        AA is an (NN x NN) symmetric matrix, PP is an (NN x 1) vector, 
C        b is of length NN+1 and E(k)**t is the ( 1 x (NN+1) ) vector 
C        consisting of all zeros, except for a '1' in its k-th position.
C        It is assumed that rank [AA,-PP]=NN and B is invertible.
C
C   The system is solved by splitting B into two matrices M and L, where
C
C       +-        -+                                +-     -+
C       |      |   |                                |       |
C       |  AA  | c |                                | -PP-c |
C   M = |      |   |  ,  L = u * [E(NN+1)**t],  u = |       | ,
C       +------+---+                                +-------+
C       |  c   | d |                                |   0   |
C       +-        -+                                +-     -+
C
C   E(NN+1) is the (NN+1) x 1 vector consisting of all zeros except for
C   a '1' in its last position, and x**t is the transpose of x.
C
C    The final solution vector, x, is given by
C
C            +-                                    -+
C            |           [sol(u)]*[E(NN+1)**t]      |
C       x =  | I  -  -----------------------------  | * sol(b)
C            |        {[(sol(u))**t]*E(NN+1)}+1.0   |
C            +-                                    -+
C
C     where sol(a)=[M**(-1)]*a.  The two systems (Mz=u, Mz=b) are solved
C     by a preconditioned conjugate gradient algorithm.
C
C                                                        
C                                                       
C     Input variables:                                  
C
C        NN -- dimension of the matrix packed in AA.
C
C        AA -- one dimensional real array containing the leading NN x NN
C              submatrix of B in packed skyline storage form.
C
C        LENAA -- number of elements in the packed array AA.
C
C        MAXA -- integer array used for specifying information about AA.
C                Using packed skyline storage, it has length NN+2, and
C                stores the indices of the diagonal elements within AA.
C                MAXA(NN+1) = LENAA + 1 and MAXA(NN+2) = LENAA + NN + 3 - k
C                (k as defined above) by convention.
C                (NOTE:  The value of MAXA(NN+2) is set by this 
C                subroutine when the preconditioning matrix Q is 
C                initialized.)
C
C                For example, using the packed storage scheme,
C                a symmetric 5 x 5 matrix of the form
C
C                +--             --+
C                |  1  3  0  0  0  |
C                |  3  2  0  7  0  |
C                |  0  0  4  6  0  |
C                |  0  7  6  5  9  |
C                |  0  0  0  9  8  |
C                +--             --+
C
C                would result in NN=5, LENAA=9, MAXA=(1,2,4,5,8,10,*),
C                and AA=(1,2,3,4,5,6,7,8,9).
C
C        PP -- vector of length NN, used for (NN+1)st column of 
C              augmented matrix B .
C
C        START -- vector of length NN+1, normally the solution to the
C                 previous linear system; used to determine the index k .
C
C     Output variables:
C
C        START -- solution vector x of  B x = b  (defined above).
C
C        IFLAG -- normally unchanged on output.  If the conjugate gradient
C                 iteration fails to converge in 10*(NN+1) iterations (most
C                 likely due to a singular Jacobian matrix), PCGDS returns
C                 with  IFLAG = 4 , and does not compute x.
C
C     Working storage:
C
C        WORK -- array of length 6*(NN+1) + LENAA :
C
C             WORK(1..NN+1) = temporary working storage;
C
C             WORK(NN+2..2NN+2) = intermediate solution vector z for Mz=u,
C                input value is used as initial estimate for z;
C
C             WORK(2NN+3..3NN+3) = intermediate solution vector z for Mz=b,
C                input value is used as initial estimate for z;
C
C             WORK(3NN+4..4NN+4) = storage for residual vectors;
C
C             WORK(4NN+5..5NN+5) = storage for direction vectors;
C
C             WORK(5NN+6..  *  ) = storage for the preconditioning matrix
C                Q, normally of length LENAA+NN+1. A storage scheme for Q
C                (and AA) other than the default packed skyline storage
C                scheme can be accomodated by simply extending the length
C                of WORK (and MAXA), and prodiving different versions of
C                the subroutines MULTDS, MFACDS, and QIMUDS.
C
C
C     Three user-defined subroutines are required:
C
C       MULTDS(y,AA,x,MAXA,NN,LENAA) -- computes y = AA x  .
C
C       MFACDS(NN,Q,LENAA,MAXA) -- computes the preconditioning matrix
C          Q based on M.  A copy of AA is placed in Q before the call; 
C          after the call, it is assumed that Q contains some factorization
C          for the preconditioning matrix Q.  If no preconditioning is 
C          required, MFACDS may be a dummy subroutine.
C
C       QIMUDS(Q,f,MAXA,NN,LENAA) -- computes f := [Q**(-1)]*f for any 
C          vector f, given the factorization of Q produced by subroutine
C          MFACDS.  Again, if no preconditioning is required, QIMUDS
C          may be a dummy subroutine.
C
C
C     Subroutines and functions called:
C
C        BLAS -- DAXPY, DCOPY, DDOT, DNRM2, DSCAL, IDAMAX
C        D1MACH,MULTDS,MFACDS,QIMUDS
C
C
      INTEGER IFLAG,IMAX,IND,J,K,LENAA,NN,MAXA(NN+2),NP1,NP2,N2P3,
     &   N3P4,N4P5,N5P6
      DOUBLE PRECISION AA(LENAA),AB,AU,BB,BU,DZNRM,PBNPRD,PP(NN),
     &   PUNPRD,RBNPRD,RBTOL,RNPRD,RUNPRD,RUTOL,START(NN+1),
     &   STARTK,TEMP,UNRM,WORK(5*(NN+1)+LENAA+NN+1),
     &   ZLEN,ZTOL
      LOGICAL STILLU,STILLB
C
      DOUBLE PRECISION D1MACH,DDOT,DNRM2
      INTEGER IDAMAX
C
C
C     SET UP BASES FOR VECTORS STORED IN WORK ARRAY.
C
      NP1=NN+1
      NP2=NN+2
      N2P3=(2*NN)+3
      N3P4=(3*NN)+4
      N4P5=(4*NN)+5
      N5P6=(5*NN)+6
C
C     FIND THE ELEMENT OF LARGEST MAGNITUDE IN THE INITIAL VECTOR, AND
C     RECORD ITS POSITION IN K.
C
      K=IDAMAX(NP1,START,1)
      STARTK=START(K)
C
C     INITIALIZE Q, SET VALUES OF MAXA(NN+1) AND MAXA(NN+2),
C     COMPUTE PRECONDITIONER.
C
      CALL DCOPY(LENAA,AA,1,WORK(N5P6),1)
      MAXA(NN+1)=LENAA+1
      MAXA(NN+2)=LENAA+NN+3-K
      CALL MFACDS(NN,WORK(N5P6),LENAA,MAXA)
C
C     COMPUTE ALL TOLERANCES NEEDED FOR EXIT CRITERIA.
C
      CALL DCOPY(NN,PP,1,WORK,1)
      IF (K .LT. NP1) WORK(K)=WORK(K)+1.0D0
      UNRM=DNRM2(NN,WORK,1)
C
      IMAX=10*NP1
      STILLU=.TRUE.
      STILLB=.TRUE.
      ZTOL=100.0*D1MACH(4)
      RBTOL=ZTOL*ABS(STARTK)
      RUTOL=ZTOL*UNRM
C
C     COMPUTE INITIAL RESIDUAL VECTOR FOR THE SYSTEM M z = u .
C
      CALL MULTDS(WORK(N3P4),AA,WORK(NP2),MAXA,NN,LENAA)
      WORK(N3P4+NN)=WORK(NP2+K-1)
      IND=N3P4+K-1
      IF (K .LT. NP1) WORK(IND)=WORK(IND)+WORK(NP2+NN)
      CALL DSCAL(NP1,-1.0D0,WORK(N3P4),1)
      CALL DAXPY(NN,-1.0D0,PP,1,WORK(N3P4),1)
      IF (K .LT. NP1) WORK(IND)=WORK(IND)-1.0D0
      CALL QIMUDS(WORK(N5P6),WORK(N3P4),MAXA,NN,LENAA)
C
C     COMPUTE INITIAL DIRECTION VECTOR, ALL INNER PRODUCTS FOR M z = u .
C
      CALL DCOPY(NP1,WORK(N3P4),1,WORK,1)
      CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
      CALL MULTDS(WORK(N4P5),AA,WORK,MAXA,NN,LENAA)
      WORK(N4P5+NN)=WORK(K)
      IF (K .LT. NP1) WORK(N4P5+K-1)=WORK(N4P5+K-1)+WORK(NP1)
C
      RUNPRD=DDOT(NP1,WORK(N3P4),1,WORK(N3P4),1)
      PUNPRD=DDOT(NP1,WORK(N4P5),1,WORK(N4P5),1)
C
      J=1
C
C     DO WHILE ((STILLU) .AND. (J .LE. IMAX))
100   IF (.NOT. ((STILLU) .AND. (J .LE. IMAX)) ) GO TO 200
C
C        IF ||RESIDUAL|| IS STILL NOT SMALL ENOUGH, CONTINUE.
         IF (SQRT(RUNPRD) .GT. RUTOL) THEN
            IF (PUNPRD .EQ. 0.0) THEN
               CALL MULTDS(WORK(N3P4),AA,WORK(NP2),MAXA,NN,LENAA)
               WORK(N3P4+NN)=WORK(NP2+K-1)
               IND=N3P4+K-1
               IF (K .LT. NP1) WORK(IND)=WORK(IND)+WORK(NP2+NN)
               CALL DSCAL(NP1,-1.0D0,WORK(N3P4),1)
               CALL DAXPY(NN,-1.0D0,PP,1,WORK(N3P4),1)
               IF (K .LT. NP1) WORK(N3P4+K-1)=WORK(N3P4+K-1)-1.0D0
               CALL QIMUDS(WORK(N5P6),WORK(N3P4),MAXA,NN,LENAA)
               CALL DCOPY(NP1,WORK(N3P4),1,WORK,1)
               CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
               CALL MULTDS(WORK(N4P5),AA,WORK,MAXA,NN,LENAA)
               WORK(N4P5+NN)=WORK(K)
               IND=N4P5+K-1
               IF (K .LT. NP1) WORK(IND)=WORK(IND)+WORK(NP1)
               RUNPRD=DDOT(NP1,WORK(N3P4),1,WORK(N3P4),1)
               PUNPRD=DDOT(NP1,WORK(N4P5),1,WORK(N4P5),1)
               IF (SQRT(RUNPRD) .LE. RUTOL) THEN
                  STILLU=.FALSE.
               ENDIF
            ENDIF
            IF (STILLU) THEN
C              UPDATE SOLUTION VECTOR; COMPUTE ||DELTA SOL||/||UPDATED||.
               AU=RUNPRD/PUNPRD
               CALL DCOPY(NP1,WORK(NP2),1,WORK,1)
               CALL DAXPY(NP1,AU,WORK(N4P5),1,WORK(NP2),1)
               CALL DAXPY(NP1,-1.0D0,WORK(NP2),1,WORK,1)
               ZLEN=DNRM2(NP1,WORK(NP2),1)
               DZNRM=DNRM2(NP1,WORK,1)
C              IF RELATIVE CHANGE IN SOLUTIONS IS SMALL ENOUGH, EXIT.
               IF ( (DZNRM/ZLEN) .LT. ZTOL) STILLU=.FALSE.
            ENDIF
         ELSE
            STILLU=.FALSE.
         ENDIF
C
C        IF NO EXIT CRITERIA FOR Mz=u HAVE BEEN MET, CONTINUE.
         IF (STILLU) THEN
C           UPDATE RESIDUAL VECTOR; COMPUTE (RNEW,RNEW), ||RNEW|| .
            CALL MULTDS(WORK,AA,WORK(N4P5),MAXA,NN,LENAA)
            WORK(NP1)=WORK(N4P5+K-1)
            IF (K .LT. NP1) WORK(K)=WORK(K)+WORK(N4P5+NN)
            CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
            CALL DAXPY(NP1,-AU,WORK,1,WORK(N3P4),1)
            RNPRD=DDOT(NP1,WORK(N3P4),1,WORK(N3P4),1)
C           UPDATE DIRECTION VECTOR; COMPUTE (PNEW,PNEW).
            BU=RNPRD/RUNPRD
            RUNPRD=RNPRD
            CALL DCOPY(NP1,WORK(N3P4),1,WORK,1)
            CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
            CALL MULTDS(START,AA,WORK,MAXA,NN,LENAA)
            START(NP1)=WORK(K)
            IF (K .LT. NP1) START(K)=START(K)+WORK(NP1)
            CALL DAXPY(NP1,BU,WORK(N4P5),1,START,1)
            CALL DCOPY(NP1,START,1,WORK(N4P5),1)
            PUNPRD=DDOT(NP1,WORK(N4P5),1,WORK(N4P5),1)
         ENDIF
C
         J=J+1
      GO TO 100
200   CONTINUE
C     END DO
C
C     SET ERROR FLAG IF THE CONJUGATE GRADIENT ITERATION DID NOT CONVERGE.
C
      IF (J .GT. IMAX) THEN
         IFLAG=4
         RETURN
      ENDIF
C
C     COMPUTE INITIAL RESIDUAL VECTOR FOR THE SYSTEM M z = b .
C
      CALL MULTDS(WORK(N3P4),AA,WORK(N2P3),MAXA,NN,LENAA)
      WORK(N3P4+NN)=WORK(N2P3+K-1)
      IND=N3P4+K-1
      IF (K .LT. NP1) WORK(IND)=WORK(IND)+WORK(N2P3+NN)
      CALL DSCAL(NP1,-1.0D0,WORK(N3P4),1)
      WORK(N3P4+NN)=STARTK+WORK(N3P4+NN)
      CALL QIMUDS(WORK(N5P6),WORK(N3P4),MAXA,NN,LENAA)
C
C     COMPUTE INITIAL DIRECTION VECTOR, ALL INNER PRODUCTS FOR M z = b .
C
      CALL DCOPY(NP1,WORK(N3P4),1,WORK,1)
      CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
      CALL MULTDS(WORK(N4P5),AA,WORK,MAXA,NN,LENAA)
      WORK(N4P5+NN)=WORK(K)
      IF (K .LT. NP1) WORK(N4P5+K-1)=WORK(N4P5+K-1)+WORK(NP1)
C
      RBNPRD=DDOT(NP1,WORK(N3P4),1,WORK(N3P4),1)
      PBNPRD=DDOT(NP1,WORK(N4P5),1,WORK(N4P5),1)
C
      J=1
C
C     DO WHILE ( STILLB  .AND. (J .LE. IMAX) )
300   IF (.NOT. ( STILLB  .AND. (J .LE. IMAX) ) ) GO TO 400
C
C        IF ||RESIDUAL|| IS STILL NOT SMALL ENOUGH, CONTINUE.
         IF (SQRT(RBNPRD) .GT. RBTOL) THEN
            IF (PBNPRD .EQ. 0.0) THEN
               CALL MULTDS(WORK(N3P4),AA,WORK(N2P3),MAXA,NN,LENAA)
               WORK(N3P4+NN)=WORK(N2P3+K-1)
               IND=N3P4+K-1
               IF (K .LT. NP1) WORK(IND)=WORK(IND)+WORK(N2P3+NN)
               CALL DSCAL(NP1,-1.0D0,WORK(N3P4),1)
               WORK(N3P4+NN)=STARTK+WORK(N3P4+NN)
               CALL QIMUDS(WORK(N5P6),WORK(N3P4),MAXA,NN,LENAA)
               CALL DCOPY(NP1,WORK(N3P4),1,WORK,1)
               CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
               CALL MULTDS(WORK(N4P5),AA,WORK,MAXA,NN,LENAA)
               WORK(N4P5+NN)=WORK(K)
               IND=N4P5+K-1
               IF (K .LT. NP1) WORK(IND)=WORK(IND)+WORK(NP1)
               RBNPRD=DDOT(NP1,WORK(N3P4),1,WORK(N3P4),1)
               PBNPRD=DDOT(NP1,WORK(N4P5),1,WORK(N4P5),1)
               IF (SQRT(RBNPRD) .LE. RBTOL) THEN
                  STILLB=.FALSE.
               ENDIF
            ENDIF
            IF (STILLB) THEN
C              UPDATE SOLUTION VECTOR; COMPUTE ||DELTA SOL||/||UPDATED|| .
               AB=RBNPRD/PBNPRD
               CALL DCOPY(NP1,WORK(N2P3),1,WORK,1)
               CALL DAXPY(NP1,AB,WORK(N4P5),1,WORK(N2P3),1)
               CALL DAXPY(NP1,-1.0D0,WORK(N2P3),1,WORK,1)
               ZLEN=DNRM2(NP1,WORK(N2P3),1)
               DZNRM=DNRM2(NP1,WORK,1)
C              IF RELATIVE CHANGE IN SOLUTIONS IS SMALL ENOUGH, EXIT.
               IF ( (DZNRM/ZLEN) .LT. ZTOL) STILLB=.FALSE.
            ENDIF
         ELSE
            STILLB=.FALSE.
         ENDIF
C
C        IF NO EXIT CRITERIA FOR Mz=b HAVE BEEN MET, CONTINUE.
         IF (STILLB) THEN
C           UPDATE RESIDUAL VECTOR; COMPUTE (RNEW,RNEW), ||RNEW|| .
            CALL MULTDS(WORK,AA,WORK(N4P5),MAXA,NN,LENAA)
            WORK(NP1)=WORK(N4P5+K-1)
            IF (K .LT. NP1) WORK(K)=WORK(K)+WORK(N4P5+NN)
            CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
            CALL DAXPY(NP1,-AB,WORK,1,WORK(N3P4),1)
            RNPRD=DDOT(NP1,WORK(N3P4),1,WORK(N3P4),1)
C        UPDATE DIRECTION VECTOR; COMPUTE (PNEW,PNEW).
            BB=RNPRD/RBNPRD
            RBNPRD=RNPRD
            CALL DCOPY(NP1,WORK(N3P4),1,WORK,1)
            CALL QIMUDS(WORK(N5P6),WORK,MAXA,NN,LENAA)
            CALL MULTDS(START,AA,WORK,MAXA,NN,LENAA)
            START(NP1)=WORK(K)
            IF (K .LT. NP1) START(K)=START(K)+WORK(NP1)
            CALL DAXPY(NP1,BB,WORK(N4P5),1,START,1)
            CALL DCOPY(NP1,START,1,WORK(N4P5),1)
            PBNPRD=DDOT(NP1,WORK(N4P5),1,WORK(N4P5),1)
         ENDIF
C         
         J=J+1
      GO TO 300
400   CONTINUE
C     END DO
C
C     SET ERROR FLAG IF THE CONJUGATE GRADIENT ITERATION DID NOT CONVERGE.
C
      IF (J .GT. IMAX) THEN
         IFLAG=4
         RETURN
      ENDIF
C
C     COMPUTE FINAL SOLUTION VECTOR X, RETURN IT IN START.
C
      TEMP=-WORK(N2P3+NN)/(1.0D0+WORK(NP2+NN))
      CALL DCOPY(NP1,WORK(N2P3),1,START,1)
      CALL DAXPY(NP1,TEMP,WORK(NP2),1,START,1)
C
      RETURN
      END

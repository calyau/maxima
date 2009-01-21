      SUBROUTINE CONSTS (K, RHO, COEF)
C
C**********************************************************************
C
C   purpose
C            assign (once) values to various array constants.
C
C   arrays assigned during compilation:
C     cnsts1 - weights for extrapolation error estimate
C     cnsts2 - weights for mesh selection
C              (the above weights come from the theoretical form for
C              the collocation error -- see [3])
C
C   arrays assigned during execution:
C     wgterr - the particular values of cnsts1 used for current run
C              (depending on k, m)
C     wgtmsh - gotten from the values of cnsts2 which in turn are
C              the constants in the theoretical expression for the
C              errors. the quantities in wgtmsh are 10x the values
C              in cnsts2 so that the mesh selection algorithm
C              is aiming for errors .1x as large as the user
C              requested tolerances.
C     jtol   - components of differential system to which tolerances
C              refer (viz, if ltol(i) refers to a derivative of u(j),
C              then jtol(i)=j)
C     root   - reciprocals of expected rates of convergence of compo-
C              nents of z(j) for which tolerances are specified
C     rho    - the k collocation points on (0,1)
C     coef   -
C     acol  -  the runge-kutta coefficients values at collocation
C              points
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION RHO(7), COEF(K,1), CNSTS1(28), CNSTS2(28), DUMMY(1)
C
      COMMON /COLORD/ KDUM, NCOMP, MSTAR, KD, MMAX, M(20)
      COMMON /COLBAS/ B(28), ACOL(28,7), ASAVE(28,4)
      COMMON /COLEST/ TOL(40), WGTMSH(40), WGTERR(40), TOLIN(40),
     1                ROOT(40), JTOL(40), LTOL(40), NTOL
C
      DATA CNSTS1 /    .25D0,     .625D-1,  7.2169D-2, 1.8342D-2,
     1     1.9065D-2, 5.8190D-2, 5.4658D-3, 5.3370D-3, 1.8890D-2,
     2     2.7792D-2, 1.6095D-3, 1.4964D-3, 7.5938D-3, 5.7573D-3,
     3     1.8342D-2, 4.673D-3,  4.150D-4,  1.919D-3,  1.468D-3,
     4     6.371D-3,  4.610D-3,  1.342D-4,  1.138D-4,  4.889D-4,
     5     4.177D-4,  1.374D-3,  1.654D-3,  2.863D-3  /
      DATA CNSTS2 /   1.25D-1,   2.604D-3,  8.019D-3,  2.170D-5,
     1     7.453D-5,  5.208D-4,  9.689D-8,  3.689D-7,  3.100D-6,
     2     2.451D-5,  2.691D-10, 1.120D-9,  1.076D-8,  9.405D-8,
     3     1.033D-6,  5.097D-13, 2.290D-12, 2.446D-11, 2.331D-10,
     4     2.936D-9,  3.593D-8,  7.001D-16, 3.363D-15, 3.921D-14,
     5     4.028D-13, 5.646D-12, 7.531D-11, 1.129D-9  /
C
C...  assign weights for error estimate
C
      KOFF = K * ( K + 1 ) / 2
      IZ = 1
      DO 10 J = 1, NCOMP
           MJ = M(J)
           DO 10 L = 1, MJ
             WGTERR(IZ) = CNSTS1(KOFF - MJ + L)
             IZ = IZ + 1
   10 CONTINUE
C
C...  assign array values for mesh selection: wgtmsh, jtol, and root
C
      JCOMP = 1
      MTOT = M(1)
      DO 40 I = 1, NTOL
           LTOLI = LTOL(I)
   20      CONTINUE
           IF ( LTOLI .LE. MTOT )                   GO TO 30
           JCOMP = JCOMP + 1
           MTOT = MTOT + M(JCOMP)
           GO TO 20
   30      CONTINUE
           JTOL(I) = JCOMP
           WGTMSH(I) = 1.D1 * CNSTS2(KOFF+LTOLI-MTOT) / TOLIN(I)
           ROOT(I) = 1.D0 / DFLOAT(K+MTOT-LTOLI+1)
   40 CONTINUE
C
C...  specify collocation points
C
      GO TO (50,60,70,80,90,100,110), K
   50 RHO(1) = 0.D0
      GO TO 120
   60 RHO(2) = .57735026918962576451D0
      RHO(1) = - RHO(2)
      GO TO 120
   70 RHO(3) = .77459666924148337704D0
      RHO(2) = .0D0
      RHO(1) = - RHO(3)
      GO TO 120
   80 RHO(4) = .86113631159405257523D0
      RHO(3) = .33998104358485626480D0
      RHO(2) = - RHO(3)
      RHO(1) = - RHO(4)
      GO TO 120
   90 RHO(5) = .90617984593866399280D0
      RHO(4) = .53846931010568309104D0
      RHO(3) = .0D0
      RHO(2) = - RHO(4)
      RHO(1) = - RHO(5)
      GO TO 120
  100 RHO(6) = .93246951420315202781D0
      RHO(5) = .66120938646626451366D0
      RHO(4) = .23861918608319690863D0
      RHO(3) = -RHO(4)
      RHO(2) = -RHO(5)
      RHO(1) = -RHO(6)
      GO TO 120
  110 RHO(7) = .949107991234275852452D0
      RHO(6) = .74153118559939443986D0
      RHO(5) = .40584515137739716690D0
      RHO(4) = 0.D0
      RHO(3) = -RHO(5)
      RHO(2) = -RHO(6)
      RHO(1) = -RHO(7)
  120 CONTINUE
C
C...  map (-1,1) to (0,1) by  t = .5 * (1. + x)
C
      DO 130 J = 1, K
         RHO(J) = .5D0 * (1.D0 + RHO(J))
  130 CONTINUE
C
C...  now find runge-kutta coeffitients b, acol and asave
C...  the values of asave are to be used in  newmsh  and errchk .
C
      DO 140 J = 1, K
         DO 135 I = 1, K
  135      COEF(I,J) = 0.D0
         COEF(J,J) = 1.D0
         CALL VMONDE (RHO, COEF(1,J), K)
  140 CONTINUE
      CALL RKBAS ( 1.D0, COEF, K, MMAX, B, DUMMY, 0)
      DO 150 I = 1, K
         CALL RKBAS ( RHO(I), COEF, K, MMAX, ACOL(1,I), DUMMY, 0)
  150 CONTINUE
      CALL RKBAS ( 1.D0/6.D0, COEF, K, MMAX, ASAVE(1,1), DUMMY, 0)
      CALL RKBAS ( 1.D0/3.D0, COEF, K, MMAX, ASAVE(1,2), DUMMY, 0)
      CALL RKBAS ( 2.D0/3.D0, COEF, K, MMAX, ASAVE(1,3), DUMMY, 0)
      CALL RKBAS ( 5.D0/6.D0, COEF, K, MMAX, ASAVE(1,4), DUMMY, 0)
      RETURN
      END

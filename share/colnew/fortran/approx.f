      SUBROUTINE APPROX (I, X, ZVAL, A, COEF, XI, N, Z, DMZ, K,
     1                   NCOMP, MMAX, M, MSTAR, MODE, DMVAL, MODM )
C
C**********************************************************************
C
C   purpose
C                                    (1)       (m1-1)     (mncomp-1)
C           evaluate z(u(x))=(u (x),u (x),...,u  (x),...,u  (x)      )
C                              1     1         1          mncomp
C           at one point x.
C
C   variables
C     a      - array of mesh independent rk-basis coefficients
C     basm   - array of mesh dependent monomial coefficients
C     xi     - the current mesh (having n subintervals)
C     z      - the current solution vector
C     dmz    - the array of mj-th derivatives of the current solution
C     mode   - determines the amount of initialization needed
C            = 4  forms z(u(x)) using z, dmz and ha
C            = 3  as in =4, but computes local rk-basis
C            = 2  as in =3, but determines i such that
C                       xi(i) .le. x .lt. xi(i+1) (unless x=xi(n+1))
C            = 1  retrieve  z=z(u(x(i)))  directly
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ZVAL(1), DMVAL(1), XI(1), M(1), A(7,1), DM(7)
      DIMENSION Z(1), DMZ(1), BM(4), COEF(1)
C
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
C
      GO TO (10, 30, 80, 90), MODE
C
C...  mode = 1 , retrieve  z( u(x) )  directly for x = xi(i).
C
   10 X  = XI(I)
      IZ = (I-1) * MSTAR
      DO 20 J = 1, MSTAR
        IZ = IZ + 1
        ZVAL(J) = Z(IZ)
   20 CONTINUE
      RETURN
C
C...  mode = 2 ,  locate i so  xi(i) .le. x .lt. xi(i+1)
C
   30 CONTINUE
      IF ( X .GE. XI(1)-PRECIS .AND. X .LE. XI(N+1)+PRECIS )
     1                                              GO TO 40
      IF (IPRINT .LT. 1)  WRITE(IOUT,900) X, XI(1), XI(N+1)
      IF ( X .LT. XI(1) )  X = XI(1)
      IF ( X .GT. XI(N+1) )  X = XI(N+1)
   40 IF ( I .GT. N .OR. I .LT. 1 )  I = (N+1) / 2
      ILEFT = I
      IF ( X .LT. XI(ILEFT) )                       GO TO 60
      DO 50 L = ILEFT, N
           I = L
           IF ( X .LT. XI(L+1) )                    GO TO 80
   50 CONTINUE
      GO TO 80
   60 IRIGHT = ILEFT - 1
      DO 70 L = 1, IRIGHT
           I = IRIGHT + 1 - L
           IF ( X .GE. XI(I) )                      GO TO 80
   70 CONTINUE
C
C...  mode = 2 or 3 , compute mesh independent rk-basis.
C
   80 CONTINUE
      S = (X - XI(I)) / (XI(I+1) - XI(I))
      CALL RKBAS ( S, COEF, K, MMAX, A, DM, MODM )
C
C...  mode = 2, 3, or 4 , compute mesh dependent rk-basis.
C
   90 CONTINUE
      BM(1) = X - XI(I)
      DO 95 L = 2, MMAX
         BM(L) = BM(1) / DFLOAT(L)
   95 CONTINUE
C
C...  evaluate  z( u(x) ).
C
  100 IR = 1
      IZ = (I-1) * MSTAR + 1
      IDMZ = (I-1) * K * NCOMP
      DO 140 JCOMP = 1, NCOMP
          MJ = M(JCOMP)
          IR = IR + MJ
          IZ = IZ + MJ
          DO 130 L = 1, MJ
             IND = IDMZ + JCOMP
             ZSUM = 0.D0
             DO 110 J = 1, K
               ZSUM = ZSUM  +  A(J,L) * DMZ(IND)
  110        IND = IND + NCOMP
             DO 120 LL = 1, L
               LB = L + 1 - LL
  120          ZSUM = ZSUM * BM(LB)  +  Z(IZ-LL)
  130     ZVAL(IR-L) = ZSUM
  140 CONTINUE
      IF ( MODM .EQ. 0 )                            RETURN
C
C...  for modm = 1 evaluate  dmval(j) = mj-th derivative of uj.
C
      DO 150 JCOMP = 1, NCOMP
  150 DMVAL(JCOMP) = 0.D0
      IDMZ = IDMZ + 1
      DO 170 J = 1, K
         FACT = DM(J)
         DO 160 JCOMP = 1, NCOMP
            DMVAL(JCOMP) = DMVAL(JCOMP)  +  FACT * DMZ(IDMZ)
            IDMZ = IDMZ + 1
  160    CONTINUE
  170 CONTINUE
      RETURN
C--------------------------------------------------------------------
  900 FORMAT(37H ****** DOMAIN ERROR IN APPROX ******
     1       /4H X =,D20.10, 10H   ALEFT =,D20.10,
     2       11H   ARIGHT =,D20.10)
      END

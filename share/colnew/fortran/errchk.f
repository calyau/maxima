      SUBROUTINE ERRCHK (XI, Z, DMZ, VALSTR, IFIN)
C
C**********************************************************************
C
C      purpose
C               determine the error estimates and test to see if the
C               error tolerances are satisfied.
C
C      variables
C        xi     - current mesh points
C        valstr - values of the previous solution which are needed
C                 for the extrapolation- like error estimate.
C        wgterr - weights used in the extrapolation-like error
C                 estimate. the array values are assigned in
C                 subroutine  consts.
C        errest - storage for error estimates
C        err    - temporary storage used for error estimates
C        z      - approximate solution on mesh xi
C        ifin   - a 0-1 variable. on return it indicates whether
C                 the error tolerances were satisfied
C        mshflg - is set by errchk to indicate to newmsh whether
C                 any values of the current solution are stored in
C                 the array valstr. (0 for no, 1 for yes)
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ERR(40), ERREST(40), DUMMY(1)
      DIMENSION XI(1), Z(1), DMZ(1), VALSTR(1)
C
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
      COMMON /COLORD/ K, NCOMP, MSTAR, KD, MMAX, M(20)
      COMMON /COLAPR/ N, NOLD, NMAX, NZ, NDMZ
      COMMON /COLMSH/ MSHFLG, MSHNUM, MSHLMT, MSHALT
      COMMON /COLBAS/ B(28), ACOL(28,7), ASAVE(28,4)
      COMMON /COLEST/ TOL(40), WGTMSH(40), WGTERR(40), TOLIN(40),
     1                ROOT(40), JTOL(40), LTOL(40), NTOL
C
C...  error estimates are to be generated and tested
C...  to see if the tolerance requirements are satisfied.
C
      IFIN = 1
      MSHFLG = 1
      DO 10 J = 1, MSTAR
   10   ERREST(J) = 0.D0
      DO 60 IBACK = 1, N
           I = N + 1 - IBACK
C
C...       the error estimates are obtained by combining values of
C...       the numerical solutions for two meshes.
C...       for each value of iback we will consider the two
C...       approximations at 2 points in each of
C...       the new subintervals.  we work backwards through
C...       the subinterval so that new values can be stored
C...       in valstr in case they prove to be needed later
C...       for an error estimate. the routine  newmsh
C...       filled in the needed values of the old solution
C...       in valstr.
C
           KNEW = ( 4 * (I-1) + 2 ) * MSTAR + 1
           KSTORE = ( 2 * (I-1) + 1 ) * MSTAR + 1
           X = XI(I) +  (XI(I+1)-XI(I)) * 2.D0 / 3.D0
           CALL APPROX (I, X, VALSTR(KNEW), ASAVE(1,3), DUMMY, XI,
     1            N, Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 4, DUMMY, 0)
           DO 20 L = 1,MSTAR
             ERR(L) = WGTERR(L) * DABS(VALSTR(KNEW) -
     1       VALSTR(KSTORE))
             KNEW = KNEW + 1
             KSTORE = KSTORE + 1
   20      CONTINUE
           KNEW = ( 4 * (I-1) + 1 ) * MSTAR + 1
           KSTORE = 2 * (I-1) * MSTAR + 1
           X = XI(I) +  (XI(I+1)-XI(I)) / 3.D0
           CALL APPROX (I, X, VALSTR(KNEW), ASAVE(1,2), DUMMY, XI,
     1            N, Z, DMZ, K, NCOMP, MMAX, M, MSTAR, 4, DUMMY, 0)
           DO 30 L = 1,MSTAR
             ERR(L) = ERR(L) + WGTERR(L) * DABS(VALSTR(KNEW) -
     1       VALSTR(KSTORE))
             KNEW = KNEW + 1
             KSTORE = KSTORE + 1
   30      CONTINUE
C
C...       find component-wise maximum error estimate
C
           DO 40 L = 1,MSTAR
             ERREST(L) = DMAX1(ERREST(L),ERR(L))
   40      CONTINUE
C


C...       test whether the tolerance requirements are satisfied
C...       in the i-th interval.
C
           IF ( IFIN .EQ. 0 )                       GO TO 60
           DO 50 J = 1, NTOL
             LTOLJ = LTOL(J)
             LTJZ = LTOLJ  +  (I-1) * MSTAR
           IF ( ERR(LTOLJ) .GT.
     1          TOLIN(J) * (DABS(Z(LTJZ))+1.D0) )  IFIN = 0
   50      CONTINUE
   60 CONTINUE
      IF ( IPRINT .GE. 0 )                          RETURN
      WRITE(IOUT,130)
      LJ = 1
      DO 70 J = 1,NCOMP
           MJ = LJ - 1 + M(J)
           WRITE(IOUT,120) J, (ERREST(L), L= LJ, MJ)
           LJ = MJ + 1
   70 CONTINUE
      RETURN
C--------------------------------------------------------------
  120 FORMAT (3H U(, I2, 3H) -,4D12.4)
  130 FORMAT (/26H THE ESTIMATED ERRORS ARE,)
      END

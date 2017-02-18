*DECK DORTHOG
      SUBROUTINE DORTHOG (VNEW, V, HES, N, LL, LDHES, KMP, SNORMW)
      INTEGER N, LL, LDHES, KMP
      DOUBLE PRECISION VNEW, V, HES, SNORMW
      DIMENSION VNEW(*), V(N,*), HES(LDHES,*)
C-----------------------------------------------------------------------
C This routine orthogonalizes the vector VNEW against the previous
C KMP vectors in the V array.  It uses a modified Gram-Schmidt
C orthogonalization procedure with conditional reorthogonalization.
C This is the version of 28 may 1986.
C-----------------------------------------------------------------------
C
C      On entry
C
C         VNEW = the vector of length N containing a scaled product
C                of the Jacobian and the vector V(*,LL).
C
C         V    = the N x l array containing the previous LL
C                orthogonal vectors v(*,1) to v(*,LL).
C
C         HES  = an LL x LL upper Hessenberg matrix containing,
C                in HES(i,k), k.lt.LL, scaled inner products of
C                A*V(*,k) and V(*,i).
C
C        LDHES = the leading dimension of the HES array.
C
C         N    = the order of the matrix A, and the length of VNEW.
C
C         LL   = the current order of the matrix HES.
C
C          KMP = the number of previous vectors the new vector VNEW
C                must be made orthogonal to (KMP .le. MAXL).
C
C
C      On return
C
C         VNEW = the new vector orthogonal to V(*,i0) to V(*,LL),
C                where i0 = MAX(1, LL-KMP+1).
C
C         HES  = upper Hessenberg matrix with column LL filled in with
C                scaled inner products of A*V(*,LL) and V(*,i).
C
C       SNORMW = L-2 norm of VNEW.
C
C-----------------------------------------------------------------------
      INTEGER I, I0
      DOUBLE PRECISION ARG, DDOT, DNRM2, SUMDSQ, TEM, VNRM
C
C Get norm of unaltered VNEW for later use. ----------------------------
      VNRM = DNRM2 (N, VNEW, 1)
C-----------------------------------------------------------------------
C Do modified Gram-Schmidt on VNEW = A*v(LL).
C Scaled inner products give new column of HES.
C Projections of earlier vectors are subtracted from VNEW.
C-----------------------------------------------------------------------
      I0 = MAX(1,LL-KMP+1)
      DO 10 I = I0,LL
        HES(I,LL) = DDOT (N, V(1,I), 1, VNEW, 1)
        TEM = -HES(I,LL)
        CALL DAXPY (N, TEM, V(1,I), 1, VNEW, 1)
 10     CONTINUE
C-----------------------------------------------------------------------
C Compute SNORMW = norm of VNEW.
C If VNEW is small compared to its input value (in norm), then
C reorthogonalize VNEW to V(*,1) through V(*,LL).
C Correct if relative correction exceeds 1000*(unit roundoff).
C finally, correct SNORMW using the dot products involved.
C-----------------------------------------------------------------------
      SNORMW = DNRM2 (N, VNEW, 1)
      IF (VNRM + 0.001D0*SNORMW .NE. VNRM) RETURN
      SUMDSQ = 0.0D0
      DO 30 I = I0,LL
        TEM = -DDOT (N, V(1,I), 1, VNEW, 1)
        IF (HES(I,LL) + 0.001D0*TEM .EQ. HES(I,LL)) GO TO 30
        HES(I,LL) = HES(I,LL) - TEM
        CALL DAXPY (N, TEM, V(1,I), 1, VNEW, 1)
        SUMDSQ = SUMDSQ + TEM**2
 30     CONTINUE
      IF (SUMDSQ .EQ. 0.0D0) RETURN
      ARG = MAX(0.0D0,SNORMW**2 - SUMDSQ)
      SNORMW = SQRT(ARG)
C
      RETURN
C----------------------- End of Subroutine DORTHOG ---------------------
      END

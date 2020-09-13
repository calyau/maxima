      SUBROUTINE GMFADS(NN,A,NWK,MAXA)
C
C     This subroutine computes the LDU decomposition of a symmetric positive
C     definite matrix B where only the upper triangular skyline structure
C     is stored.  The decomposition is done by the Gill-Murray
C     strategy from P.E. Gill and W. Murray, Newton type Methods
C     for Unconstrained and Linearly Constrained Optimization,
C     Mathematical Programming, 7, 311-350 (1974) and gives an
C     approximate decomposition in the case of a nonpositive
C     definite or ill-conditioned matrix.
C
C     Input variables:
C
C        NN -- dimension of B.
C
C        A -- one dimensional real array containing the upper 
C             triangular skyline portion of a symmetric matrix B in
C             packed skyline storage format.
C
C        NWK -- number of elements in A.
C
C        MAXA -- an integer array of dimension NN+1 containing the 
C                locations of the diagonal elements of B in A.  
C                By convention, MAXA(NN+1)=NWK+1.  
C
C     Output variables:
C
C        A -- the upper triangular skyline portion of the LDU 
C             decomposition of the symmetric matrix B (or B + E if B
C             was not sufficiently positive definite).
C
C
C     No working storage is required by this routine.
C
C     Subroutines called:  D1MACH
C
      INTEGER I,I0,I1,I2,I3,I4,J,J1,K,K1,K2,KH,KL,KN,KU,KZ,L,L1,
     &   L2,L3,M,M1,MAXA(NN+1),N1,NN,NNN,NWK
      DOUBLE PRECISION A(NWK),BET,DEL,DJ,D1MACH,G,GAM,GAM1,PHI,
     &   THE,THE1,XT1,XT2,ZET,ZET1
C     LOGICAL GMALT
C     GMALT=.FALSE.
      G=0.0
      GAM=0.0
      DO 1 I=1,NN
         K=MAXA(I)
         G=G+A(K)*A(K)
         GAM1=ABS(A(K))
         IF(GAM1.GT.GAM)GAM=GAM1
1     CONTINUE
      ZET=0.0
      DO 3 I=1,NN
         K=MAXA(I)
         K1=MAXA(I+1)-1
         K2=K1-K
         IF(K2.EQ.0)GO TO 3
         L=K+1
         DO 2 J=L,K1
            G=G+2.0*A(J)*A(J)
            ZET1=ABS(A(J))
            IF(ZET1.GT.ZET)ZET=ZET1
2        CONTINUE
3     CONTINUE
      ZET=ZET/NN
      DEL=D1MACH(4)
      BET=DEL
      IF(ZET.GT.BET)BET=ZET
      IF(GAM.GT.BET)BET=GAM
      G=SQRT(G)
      IF(G.GT.1.0)DEL=DEL*G
      DO 4 I=1,NN
         N1=I-1
         KN=MAXA(I)
         KL=KN+1
         KU=MAXA(I+1)-1
         KH=KU-KL
         PHI=A(KN)
         IF(KH.LT.0)GO TO 10
         K1=KN+1
         K2=I
         DO 5 J=K1,KU
            K2=K2-1
            KZ=MAXA(K2)
            PHI=PHI-A(J)*A(J)*A(KZ)
5        CONTINUE
C10      IF(PHI.LE.0.0)GMALT=.TRUE.
10       PHI=ABS(PHI)
         L=I+1
         THE=0.0
         NNN=NN+1
         IF(L.EQ.NNN)GO TO 11
         DO 6 J=L,NN
            L1=MAXA(J)
            L2=MAXA(J+1)
            L3=L2-L1-1
            M=J-I
            IF(L3.LT.M)GO TO 6
            M1=L1+M
            IF(N1.EQ.0)GO TO 7
            DO 8 J1=1,N1
               I0=MAXA(J1)
               I1=MAXA(L)
               I2=I-J1
               I3=I1-KN-1
               I4=J-J1
               IF(I3.LT.I2)GO TO 8
               IF(L3.LT.I4)GO TO 8
               XT1=A(KN+I2)
               XT2=A(L1+I4)
               A(M1)=A(M1)-XT1*XT2*A(I0)
8           CONTINUE
7           THE1=ABS(A(M1))
            IF(THE.LT.THE1)THE=THE1
6        CONTINUE
11       THE=THE*THE/BET
         DJ=DEL
         IF(PHI.GT.DJ)DJ=PHI
         IF(THE.GT.DJ)DJ=THE
C        IF(ABS(DJ).NE.PHI)GMALT=.TRUE.
         A(KN)=DJ
         IF(L.EQ.NNN)GO TO 4
         DO 9 J=L,NN
            L1=MAXA(J)
            L2=MAXA(J+1)
            L3=L2-L1-1
            M=J-I
            IF(L3.LT.M)GO TO 9
            M1=L1+M
            A(M1)=A(M1)/A(KN)
9        CONTINUE
4     CONTINUE
      RETURN
      END

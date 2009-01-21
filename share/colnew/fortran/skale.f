      SUBROUTINE SKALE (N, MSTAR, KD, Z, XI, SCALE, DSCALE)
C
C**********************************************************************
C
C   purpose
C            provide a proper scaling of the state variables, used
C            to control the damping factor for a newton iteration [2].
C
C   variables
C
C            n      = number of mesh subintervals
C            mstar  = number of unknomns in z(u(x))
C            kd     = number of unknowns in dmz
C            z      = the global unknown vector
C            xi     = the current mesh
C            scale  = scaling vector for z
C            dscale = scaling vector for dmz
C
C**********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Z(MSTAR,1), SCALE(MSTAR,1), DSCALE(KD,1)
      DIMENSION XI(1), BASM(5)
C
      COMMON /COLORD/ K, NCOMP, ID1, ID2, MMAX, M(20)
C
      BASM(1) = 1.D0
      DO 50 J=1,N
        IZ = 1
        H = XI(J+1) - XI(J)
        DO 10 L = 1, MMAX
          BASM(L+1) = BASM(L) * H / DFLOAT(L)
  10    CONTINUE
        DO 40 ICOMP = 1, NCOMP
          SCAL = (DABS(Z(IZ,J)) + DABS(Z(IZ,J+1))) * .5D0 + 1.D0
          MJ = M(ICOMP)
          DO 20 L = 1, MJ
            SCALE(IZ,J) = BASM(L) / SCAL
            IZ = IZ + 1
  20      CONTINUE
          SCAL = BASM(MJ+1) / SCAL
          DO 30 IDMZ = ICOMP, KD, NCOMP
            DSCALE(IDMZ,J) = SCAL
  30      CONTINUE
  40    CONTINUE
  50  CONTINUE
      NP1 = N + 1
      DO 60 IZ = 1, MSTAR
        SCALE(IZ,NP1) = SCALE(IZ,N)
  60  CONTINUE
      RETURN
      END

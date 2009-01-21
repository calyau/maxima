      SUBROUTINE VWBLOK (XCOL, HRHO, JJ, WI, VI, IPVTW, KD, ZVAL,
     1                   DF, ACOL, DMZO, NCOMP, DFSUB, MSING)
C
C**********************************************************************
C
C   purpose:
C
C      construct a group of  ncomp  rows of the matrices  wi  and  vi.
C      corresponding to an interior collocation point.
C
C
C   variables:
C
C      xcol   - the location of the collocation point.
C      jj     - xcol is the jj-th of k collocation points
C               in the i-th subinterval.
C      wi,vi  - the i-th block of the collocation matrix
C               before parameter condensation.
C      kd     - no. of rows in vi and wi .
C      zval   - z(xcol)
C      df     - the jacobian at xcol .
C      jcomp  - counter for the component being dealt with.
C
C**********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION WI(KD,1), VI(KD,1), ZVAL(1), DMZO(1), DF(NCOMP,1)
      DIMENSION IPVTW(1),  HA(7,4), ACOL(7,4), BASM(5)
C
      COMMON /COLORD/ K, NCDUM, MSTAR, KDUM, MMAX, M(20)
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
C
C...  if jj = 1 initialize  wi .
C
      IF ( JJ .GT. 1 )                              GO TO 30
      DO 10 ID = 1, KD
        WI(ID,ID) = 1.D0
   10 CONTINUE
C
C...  calculate local basis
C
   30        FACT = 1.D0
             DO 150 L=1,MMAX
                FACT = FACT * HRHO / DFLOAT(L)
                BASM(L) = FACT
                DO 150 J=1,K
                   HA(J,L) = FACT * ACOL(J,L)
  150        CONTINUE
C
C... zero jacobian
C
      DO 40 JCOL = 1, MSTAR
        DO 40 IR = 1, NCOMP
   40 DF(IR,JCOL) = 0.D0
C
C...  build ncomp rows for interior collocation point x.
C...  the linear expressions to be constructed are:
C...   (m(id))
C...  u     -  df(id,1)*z(1) - ... - df(id,mstar)*z(mstar)
C...   id
C...  for id = 1 to ncomp.
C
      CALL DFSUB (XCOL, ZVAL, DF)
      I0 = (JJ-1) * NCOMP
      I1 = I0 + 1
      I2 = I0 + NCOMP
C
C...  evaluate  dmzo = dmz - df * zval  once for a new mesh
C
      IF (NONLIN .EQ. 0 .OR. ITER .GT. 0)          GO TO 60
      DO 50 J = 1, MSTAR
        FACT = - ZVAL(J)
        DO 50 ID = 1, NCOMP
          DMZO(I0+ID) = DMZO(I0+ID)  +  FACT * DF(ID,J)
  50  CONTINUE
C
C...  loop over the  ncomp  expressions to be set up for the
C...  current collocation point.
C
   60 DO 70 J = 1, MSTAR
        DO 70 ID = 1, NCOMP
          VI(I0+ID,J) = DF(ID,J)
   70 CONTINUE
      JN = 1
      DO 140 JCOMP = 1, NCOMP
         MJ = M(JCOMP)
         JN = JN + MJ
         DO 130 L = 1, MJ
            JV = JN - L
            JW = JCOMP
            DO 90 J = 1, K
              AJL = - HA(J,L)
              DO 80 IW = I1, I2
                 WI(IW,JW) = WI(IW,JW)  +  AJL * VI(IW,JV)
   80         CONTINUE
   90       JW = JW + NCOMP
            LP1 = L + 1
            IF ( L .EQ. MJ )                        GO TO 130
            DO 110 LL = LP1, MJ
              JDF = JN - LL
              BL = BASM(LL-L)
              DO 100 IW = I1, I2
                VI(IW,JV) = VI(IW,JV)  +  BL * VI(IW,JDF)
  100         CONTINUE
  110       CONTINUE
  130    CONTINUE
  140 CONTINUE
      IF ( JJ .LT. K )                          RETURN
C
C   ...decompose the wi block and solve for the mstar columns of vi
C
C
C...  do parameter condensation
C
      MSING = 0
      CALL DGEFA  (WI, KD, KD, IPVTW, MSING)
C
C...   check for singularity
C
      IF ( MSING .NE. 0 )                         RETURN
      DO 250 J= 1,MSTAR
         CALL DGESL  (WI, KD, KD, IPVTW, VI(1,J), 0)
  250 CONTINUE
      RETURN
      END

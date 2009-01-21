      SUBROUTINE GBLOCK (H, GI, NROW, IROW, WI, VI, KD,
     1                   RHSZ, RHSDMZ, IPVTW, MODE)
C
C**********************************************************************
C
C   purpose:
C
C      construct collocation matrix rows according to mode:
C      mode = 1  -  a group of  mstar    rows corresponding
C                   an interior mesh interval.
C           = 2  -  corresponding right hand side
C
C   variables:
C
C      h      - the  local stepsize.
C      gi     - the sub-block of the collocation matrix in
C               which the equations are to be formed.
C      wi     - the sub-block of noncondensed collocation equations,
C               left-hand side part.
C      vi     - the sub-block of noncondensed collocation equations,
C               right-hand side part.
C      rhsdmz - the inhomogenous term of the uncondensed collocation
C               equations.
C      rhsz   - the inhomogenous term of the condensed collocation
C               equations.
C      nrow   - no. of rows in gi.
C      irow   - the first row in gi to be used for equations.
C
C**********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION HB(7,4), BASM(5)
      DIMENSION GI(NROW,1), WI(1), VI(KD,1)
      DIMENSION RHSZ(1), RHSDMZ(1), IPVTW(1)
C
      COMMON /COLORD/  K, NCOMP, MSTAR, KDUM, MMAX, M(20)
      COMMON /COLBAS/ B(7,4), ACOL(28,7), ASAVE(28,4)
C
C...  compute local basis
C
      FACT = 1.D0
      BASM(1) = 1.D0
      DO 30 L=1,MMAX
         FACT = FACT * H / DFLOAT(L)
         BASM(L+1) = FACT
         DO 20 J=1,K
   20       HB(J,L) = FACT * B(J,L)
   30 CONTINUE
C
C...  branch according to  m o d e
C
      GO TO (40, 110), MODE
C
C...  set right gi-block to identity
C
   40 CONTINUE
      DO 60 J = 1, MSTAR
        DO 50 IR = 1, MSTAR
          GI(IROW-1+IR,J) = 0.D0
   50   GI(IROW-1+IR,MSTAR+J) = 0.D0
   60 GI(IROW-1+J,MSTAR+J) = 1.D0
C
C...  compute the block gi
C
      IR = IROW
      DO 100 ICOMP = 1, NCOMP
         MJ = M(ICOMP)
         IR = IR + MJ
         DO 90 L = 1, MJ
            ID = IR - L
            DO 80 JCOL = 1, MSTAR
               IND = ICOMP
               RSUM = 0.D0
               DO 70 J = 1, K
                  RSUM = RSUM  -  HB(J,L) * VI(IND,JCOL)
   70          IND = IND + NCOMP
               GI(ID,JCOL) = RSUM
   80       CONTINUE
            JD = ID - IROW
            DO 85 LL = 1, L
               GI(ID,JD+LL) = GI(ID,JD+LL) - BASM(LL)
   85       CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
C
C...  compute the appropriate piece of  rhsz
C
  110 CONTINUE
      CALL DGESL  (WI, KD, KD, IPVTW, RHSDMZ, 0)
      IR = IROW
      DO 140 JCOMP = 1, NCOMP
         MJ = M(JCOMP)
         IR = IR + MJ
         DO 130 L = 1, MJ
            IND = JCOMP
            RSUM = 0.D0
            DO 120 J = 1, K
               RSUM = RSUM  +  HB(J,L) * RHSDMZ(IND)
  120       IND = IND + NCOMP
            RHSZ(IR-L) = RSUM
  130    CONTINUE
  140 CONTINUE
      RETURN
      END

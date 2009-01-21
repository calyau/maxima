c
c     The common blocks used in colnew are defined in separate files,
c     but the definitions overlap.  That is, we can't define the common
c     blocks in multiple files because there are common names between
c     the files.  Hence, we move them here so we can define the blocks
c     here all at once.
c     
      block data colnew
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /COLORD/ KDUM, NCOMP, MSTAR, KD, MMAX, M(20)
      COMMON /COLBAS/ B(28), ACOL(28,7), ASAVE(28,4)
      COMMON /COLEST/ TOL(40), WGTMSH(40), WGTERR(40), TOLIN(40),
     1                ROOT(40), JTOL(40), LTOL(40), NTOL
      COMMON /COLOUT/ PRECIS, IOUT, IPRINT
      COMMON /COLLOC/ RHO(7), COEF(49)
      COMMON /COLAPR/ N, NOLD, NMAX, NZ, NDMZ
      COMMON /COLMSH/ MSHFLG, MSHNUM, MSHLMT, MSHALT
      COMMON /COLSID/ TZETA(40), TLEFT, TRIGHT, IZETA, IDUM
      COMMON /COLNLN/ NONLIN, ITER, LIMIT, ICARE, IGUESS
      end

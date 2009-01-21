C----------------------------------------------------------------------
C                            p a r t  5
C          we list here a modified (column oriented, faster)
C          version of the package solveblok of de boor - weiss [5].
C          we also give a listing of the linpack
C          routines dgefa und dgesl used by colnew.
C----------------------------------------------------------------------
C
      SUBROUTINE FCBLOK (BLOKS, INTEGS, NBLOKS, IPIVOT, SCRTCH, INFO)
C
C
C     calls subroutines  factrb  and  shiftb .
C
C     fcblok  supervises the plu factorization with pivoting of
C     scaled rows of the almost block diagonal matrix stored in the
C     arrays  bloks  and  integs .
C
C     factrb = subprogram which carries out steps 1,...,last of gauss
C            elimination (with pivoting) for an individual block.
C     shiftb = subprogram which shifts the remaining rows to the top of
C            the next block
C
C     parameters
C      bloks   an array that initially contains the almost block diago-
C            nal matrix  a  to be factored, and on return contains the
C            computed factorization of  a .
C      integs  an integer array describing the block structure of  a .
C      nbloks  the number of blocks in  a .
C      ipivot  an integer array of dimension   sum (integs(3,n) ; n=1,
C            ...,nbloks) which, on return, contains the pivoting stra-
C            tegy used.
C      scrtch  work area required, of length  max (integs(1,n) ; n=1,
C            ...,nbloks).
C      info    output parameter;
C            = 0  in case matrix was found to be nonsingular.
C            otherwise,
C            = n if the pivot element in the nth gauss step is zero.
C
C**********************************************************************
C
      INTEGER INTEGS(3,NBLOKS),IPIVOT(1),INFO, I,INDEX,INDEXN,LAST,
     1        NCOL,NROW
      DOUBLE PRECISION BLOKS(1),SCRTCH(1)
      INFO = 0
      INDEXX = 1
      INDEXN = 1
      I = 1
C
C...  loop over the blocks.  i  is loop index
C
   10      INDEX = INDEXN
           NROW = INTEGS(1,I)
           NCOL = INTEGS(2,I)
           LAST = INTEGS(3,I)
C
C...       carry out elimination on the i-th block until next block
C...       enters, i.e., for columns 1,...,last  of i-th block.
C
           CALL FACTRB ( BLOKS(INDEX), IPIVOT(INDEXX), SCRTCH, NROW,
     1                   NCOL, LAST, INFO)
C
C...       check for having reached a singular block or the last block
C
           IF ( INFO .NE. 0 )                       GO TO 20
           IF ( I .EQ. NBLOKS )                     RETURN
           I = I+1
           INDEXN = NROW * NCOL + INDEX
           INDEXX = INDEXX + LAST
C
C...       put the rest of the i-th block onto the next block
C
           CALL SHIFTB ( BLOKS(INDEX), NROW, NCOL, LAST,
     1                   BLOKS(INDEXN), INTEGS(1,I), INTEGS(2,I) )
      GO TO 10
   20 INFO = INFO + INDEXX - 1
      RETURN
      END

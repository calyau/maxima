      SUBROUTINE SBBLOK ( BLOKS, INTEGS, NBLOKS, IPIVOT, X )
C
C**********************************************************************
C
C     calls subroutines  subfor  and  subbak .
C
C     supervises the solution (by forward and backward substitution) of
C     the linear system  a*x = b  for x, with the plu factorization of
C     a  already generated in  fcblok .  individual blocks of
C     equations are solved via  subfor  and  subbak .
C
C    parameters
C       bloks, integs, nbloks, ipivot    are as on return from fcblok.
C       x       on input: the right hand side, in dense storage
C               on output: the solution vector
C
C*********************************************************************
C
      INTEGER INTEGS(3,NBLOKS),IPIVOT(1), I,INDEX,INDEXX,J,LAST,
     1        NBP1,NCOL,NROW
      DOUBLE PRECISION BLOKS(1), X(1)
C
C...  forward substitution pass
C
      INDEX = 1
      INDEXX = 1
      DO 10 I = 1, NBLOKS
           NROW = INTEGS(1,I)
           LAST = INTEGS(3,I)
           CALL SUBFOR ( BLOKS(INDEX), IPIVOT(INDEXX), NROW, LAST,
     1                   X(INDEXX) )
           INDEX = NROW * INTEGS(2,I) + INDEX
   10 INDEXX = INDEXX + LAST
C
C...  back substitution pass
C
      NBP1 = NBLOKS + 1
      DO 20 J = 1, NBLOKS
           I = NBP1 - J
           NROW = INTEGS(1,I)
           NCOL = INTEGS(2,I)
           LAST = INTEGS(3,I)
           INDEX = INDEX - NROW * NCOL
           INDEXX = INDEXX - LAST
   20 CALL SUBBAK ( BLOKS(INDEX), NROW, NCOL, LAST, X(INDEXX) )
      RETURN
      END

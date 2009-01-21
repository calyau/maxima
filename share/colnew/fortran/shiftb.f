      SUBROUTINE SHIFTB (AI, NROWI, NCOLI, LAST, AI1, NROWI1, NCOLI1)
C
C*********************************************************************
C
C     shifts the rows in current block, ai, not used as pivot rows, if
C     any, i.e., rows  (last+1),..., (nrowi), onto the first mmax =
C      = nrow-last  rows of the next block, ai1, with column last+j of
C      ai  going to column j , j=1,...,jmax=ncoli-last. the remaining
C     columns of these rows of ai1 are zeroed out.
C
C                                picture
C
C          original situation after         results in a new block i+1
C          last = 2 columns have been       created and ready to be
C          done in factrb (assuming no      factored by next factrb
C          interchanges of rows)            call.
C                      1
C                 x  x 1x  x  x           x  x  x  x  x
C                      1
C                 0  x 1x  x  x           0  x  x  x  x
C     block i          1                       ---------------
C     nrowi = 4   0  0 1x  x  x           0  0 1x  x  x  0  01
C     ncoli = 5        1                       1             1
C     last = 2    0  0 1x  x  x           0  0 1x  x  x  0  01
C     -------------------------------          1             1   new
C                      1x  x  x  x  x          1x  x  x  x  x1  block
C                      1                       1             1   i+1
C     block i+1        1x  x  x  x  x          1x  x  x  x  x1
C     nrowi1= 5        1                       1             1
C     ncoli1= 5        1x  x  x  x  x          1x  x  x  x  x1
C     -------------------------------          1-------------1
C                      1
C
C*********************************************************************
C
      INTEGER LAST, J,JMAX,JMAXP1,M,MMAX
      DOUBLE PRECISION AI(NROWI,NCOLI),AI1(NROWI1,NCOLI1)
      MMAX = NROWI - LAST
      JMAX = NCOLI - LAST
      IF (MMAX .LT. 1 .OR. JMAX .LT. 1)             RETURN
C
C...  put the remainder of block i into ai1
C
      DO 10 J=1,JMAX
           DO 10 M=1,MMAX
   10 AI1(M,J) = AI(LAST+M,LAST+J)
      IF (JMAX .EQ. NCOLI1)                         RETURN
C
C...  zero out the upper right corner of ai1
C
      JMAXP1 = JMAX + 1
      DO 20 J=JMAXP1,NCOLI1
           DO 20 M=1,MMAX
   20 AI1(M,J) = 0.D0
      RETURN
      END

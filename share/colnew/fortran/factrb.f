      SUBROUTINE FACTRB ( W, IPIVOT, D, NROW, NCOL, LAST, INFO)
C
C********************************************************************
C
C     adapted from p.132 of  element.numer.analysis  by conte-de boor
C
C     constructs a partial plu factorization, corresponding to steps
C      1,..., last   in gauss elimination, for the matrix  w  of
C      order ( nrow ,  ncol ), using pivoting of scaled rows.
C
C     parameters
C       w       contains the (nrow,ncol) matrix to be partially factored
C               on input, and the partial factorization on output.
C       ipivot  an integer array of length last containing a record of
C               the pivoting strategy used; explicit interchanges
C               are used for pivoting.
C       d       a work array of length nrow used to store row sizes
C               temporarily.
C       nrow    number of rows of w.
C       ncol    number of columns of w.
C       last    number of elimination steps to be carried out.
C       info    on output, zero if the matrix is found to be non-
C               singular, in case a zero pivot was encountered in row
C               n,  info = n on output.
C
C**********************************************************************
C
      INTEGER IPIVOT(NROW),NCOL,LAST,INFO, I,J,K,L,KP1
      DOUBLE PRECISION W(NROW,NCOL),D(NROW), COLMAX,T,S
      DOUBLE PRECISION DABS,DMAX1
C
C...  initialize  d
C
      DO 10 I = 1, NROW
        D(I) = 0.D0
   10 CONTINUE
      DO 20 J = 1, NCOL
        DO 20 I = 1, NROW
          D(I) = DMAX1( D(I) , DABS(W(I,J)))
   20 CONTINUE
C
C...  gauss elimination with pivoting of scaled rows, loop over
C...  k=1,.,last
C
      K = 1
C
C...  as pivot row for k-th step, pick among the rows not yet used,
C...  i.e., from rows  k ,..., nrow , the one whose k-th entry
C...  (compared to the row size) is largest. then, if this row
C...  does not turn out to be row k, interchange row k with this
C...  particular row and redefine ipivot(k).
C
   30      CONTINUE
           IF ( D(K) .EQ. 0.D0 )                    GO TO 90
           IF (K .EQ. NROW)                         GO TO 80
           L = K
           KP1 = K+1
           COLMAX = DABS(W(K,K)) / D(K)
C
C...       find the (relatively) largest pivot
C
           DO 40 I = KP1, NROW
             IF ( DABS(W(I,K)) .LE. COLMAX * D(I) ) GO TO 40
             COLMAX = DABS(W(I,K)) / D(I)
             L = I
   40      CONTINUE
           IPIVOT(K) = L
           T = W(L,K)
           S = D(L)
           IF ( L .EQ. K )                          GO TO 50
             W(L,K) = W(K,K)
             W(K,K) = T
             D(L) = D(K)
             D(K) = S
   50      CONTINUE
C
C...       if pivot element is too small in absolute value, declare
C...       matrix to be noninvertible and quit.
C
           IF ( DABS(T)+D(K) .LE. D(K) )            GO TO 90
C
C...       otherwise, subtract the appropriate multiple of the pivot
C...       row from remaining rows, i.e., the rows (k+1),..., (nrow)
C...       to make k-th entry zero. save the multiplier in its place.
C...       for high performance do this operations column oriented.
C
           T = -1.0D0/T
           DO 60 I = KP1, NROW
   60        W(I,K) = W(I,K) * T
           DO 70 J=KP1,NCOL
             T = W(L,J)
             IF ( L .EQ. K )                        GO TO 62
               W(L,J) = W(K,J)
               W(K,J) = T
   62        IF ( T .EQ. 0.D0 )                     GO TO 70
             DO 64 I = KP1, NROW
   64           W(I,J) = W(I,J) + W(I,K) * T
   70      CONTINUE
           K = KP1
C
C...       check for having reached the next block.
C
           IF ( K .LE. LAST )                       GO TO 30
      RETURN
C
C...  if  last  .eq. nrow , check now that pivot element in last row
C...  is nonzero.
C
   80 IF( DABS(W(NROW,NROW))+D(NROW) .GT. D(NROW) ) RETURN
C
C...  singularity flag set
C
   90 INFO = K
      RETURN
      END

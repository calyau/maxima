      SUBROUTINE MULTDS(Y,AA,X,MAXA,NN,LENAA)
C
C     This subroutine accepts a matrix, AA, in packed skyline storage form and
C       a vector, x, and returns the product AA*x in y.
C
C     Input Variables:
C
C       AA -- one dimensional real array containing the NN x NN matrix in 
C             packed skyline storage form.
C
C       x -- real vector of length NN to be multiplied by AA.
C
C       MAXA -- integer array used for specifying information about AA.
C               MAXA has length NN+1, and stores the indices of the 
C               diagonal elements of the matrix packed in AA.  By 
C               convention, MAXA(NN+1) = LENAA + 1 .
C
C       NN -- dimension of the matrix packed in AA .
C
C       LENAA -- number of elements in AA.
C
C
C     Output Variables:
C
C       y -- real vector of length NN containing the product  AA*x .
C
C
C
      INTEGER I,II,KK,KL,KU,LENAA,NN,MAXA(NN+1)
      DOUBLE PRECISION AA(LENAA),B,CC,X(NN),Y(NN)
      IF(LENAA.GT.NN) GO TO 20
      DO 10 I=1,NN
   10 Y(I)=AA(I)*X(I)
      RETURN
   20 DO 40 I=1,NN
   40 Y(I)=0.00
      DO 100 I=1,NN
      KL=MAXA(I)
      KU=MAXA(I+1)-1
      II=I+1
      CC=X(I)
      DO 100 KK=KL,KU
      II=II-1
  100 Y(II)=Y(II)+AA(KK)*CC
      IF(NN.EQ.1) RETURN
      DO 200 I=2,NN
      KL=MAXA(I)+1
      KU=MAXA(I+1)-1
      IF(KU-KL) 200,210,210
  210 II=I
      B=0.00
      DO 220 KK=KL,KU
      II=II-1
  220 B=B+AA(KK)*X(II)
      Y(I)=Y(I)+B
  200 CONTINUE
      RETURN
      END

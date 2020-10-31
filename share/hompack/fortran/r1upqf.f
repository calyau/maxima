        SUBROUTINE R1UPQF(N,S,T,QT,R,W)
C ***** DECLARATIONS *****
C
C     FUNCTION DECLARATIONS
C
        DOUBLE PRECISION DDOT, DNRM2
C
C     LOCAL VARIABLES
C
        DOUBLE PRECISION C, DEN, ONE, SS, WW, YY, temp
        INTEGER I, INDEXR, INDXR2, J, K
        LOGICAL SKIPUP
C
C     SCALAR ARGUMENTS
C
        DOUBLE PRECISION ETA
        INTEGER N
C
C     ARRAY DECLARATIONS
C
        DOUBLE PRECISION  S(N), QT(N,N), R(N*(N+1)/2),
     $    W(N), T(N), TT(2)
C
C ***** END OF DECLARATIONS *****
C
C ***** COMPUTE THE QR FACTORIZATION Q- R- OF (R + T*S).  THEN,  *****
C       Q+ = Q*Q-,  AND  R+ = R-.
C
C FIND THE LARGEST  K  SUCH THAT  T(K) .NE. 0.
C
        K = N
  50    IF (T(K) .NE. 0.0 .OR. K .LE. 1) GOTO 60
          K=K-1
          GOTO 50
  60    CONTINUE
C
C COMPUTE THE INDEX OF R(K-1,K-1).
C
        INDEXR = (N + N - K + 3)*(K - 2) / 2 + 1
C
C ***** TRANSFORM R+T*ST INTO AN UPPER HESSENBERG MATRIX *****
C
C DETERMINE JACOBI ROTATIONS WHICH WILL ZERO OUT ROWS
C N, N-1,...,2  OF THE MATRIX  T*ST,  AND APPLY THESE
C ROTATIONS TO  R.  (THIS IS EQUIVALENT TO APPLYING THE
C SAME ROTATIONS TO  R+T*ST, EXCEPT FOR THE FIRST ROW.
C THUS, AFTER AN ADJUSTMENT FOR THE FIRST ROW, THE
C RESULT IS AN UPPER HESSENBERG MATRIX.  THE
C SUBDIAGONAL ELEMENTS OF WHICH WILL BE STORED IN  W.
C
C NOTE:  ROWS N,N-1,...,K+1 ARE ALREADY ALL ZERO.
C
        DO 90 I=K-1,1,-1
C
C         DETERMINE THE JACOBI ROTATION WHICH WILL ZERO OUT
C         ROW  I+1  OF THE  T*ST  MATRIX.
C
          IF (T(I) .EQ. 0.0) THEN
            C = 0.0
C         SS = SIGN(-T(I+1))= -T(I+1)/|T(I+1)|
            SS = -SIGN(ONE,T(I+1))
          ELSE
            DEN = DNRM2(2,T(I),1)
            C = T(I) / DEN
            SS = -T(I+1)/DEN
          END IF
C
C         PREMULTIPLY  R  BY THE JACOBI ROTATION.
C
          YY = R(INDEXR)
          WW = 0.0
          R(INDEXR) = C*YY - SS*WW
          W(I+1) = SS*YY + C*WW
          INDEXR = INDEXR + 1
          INDXR2 = INDEXR + N - I
          DO 70 J= I+1,N
C           YY = R(I,J)
C           WW = R(I+1,J)
              YY = R(INDEXR)
              WW = R(INDXR2)
C           R(I,J) = C*YY - SS*WW
C           R(I+1,J) = SS*YY + C*WW
              R(INDEXR) = C*YY - SS*WW
              R(INDXR2) = SS*YY + C*WW
            INDEXR = INDEXR + 1
            INDXR2 = INDXR2 + 1
  70      CONTINUE
C
C         PREMULTIPLY  QT  BY THE JACOBI ROTATION.
C
          DO 80 J=1,N
            YY = QT(I,J)
            WW = QT(I+1,J)
            QT(I,J) = C*YY - SS*WW
            QT(I+1,J) = SS*YY + C*WW
  80      CONTINUE
C
C         UPDATE  T(I)  SO THAT  T(I)*ST(J)  IS THE  (I,J)TH  COMPONENT
C         OF  T*ST, PREMULTIPLIED BY ALL OF THE JACOBI ROTATIONS SO
C         FAR.
C
          IF (T(I) .EQ. 0.0) THEN
            T(I) = ABS(T(I+1))
          ELSE
            T(I) = DNRM2(2,T(I),1)
          END IF
C
C         LET INDEXR = THE INDEX OF R(I-1,I-1).
C
          INDEXR = INDEXR - 2*(N - I) - 3
C
  90    CONTINUE
C
C     UPDATE THE FIRST ROW OF  R  SO THAT  R  HOLDS  (R+T*ST)
C     PREMULTIPLIED BY ALL OF THE ABOVE JACOBI ROTATIONS.
C
        temp = T(1)
        CALL DAXPY(N,temp,S,1,R,1)
C
C ***** END OF TRANSFORMATION TO UPPER HESSENBERG *****
C
C
C ***** TRANSFORM UPPER HESSENBERG MATRIX INTO UPPER *****
C       TRIANGULAR MATRIX.
C
C       INDEXR = INDEX OF R(1,1).
C
          INDEXR = 1
          DO 120 I=1,K-1
C
C           DETERMINE APPROPRIATE JACOBI ROTATION TO ZERO OUT
C           R(I+1,I).
C
            IF (R(INDEXR) .EQ. 0.0) THEN
              C = 0.0
              SS = -SIGN(ONE,W(I+1))
            ELSE
              TT(1) = R(INDEXR)
              TT(2) = W(I+1)
              DEN = DNRM2(2,TT,1)
              C = R(INDEXR) / DEN
              SS = -W(I+1)/DEN
            END IF
C
C           PREMULTIPLY  R  BY JACOBI ROTATION.
C
            YY = R(INDEXR)
            WW = W(I+1)
            R(INDEXR) = C*YY - SS*WW
            W(I+1) = 0.0
            INDEXR = INDEXR + 1
            INDXR2 = INDEXR + N - I
            DO 100 J= I+1,N
C             YY = R(I,J)
C             WW = R(I+1,J)
                YY = R(INDEXR)
                WW = R(INDXR2)
C             R(I,J) = C*YY -SS*WW
C             R(I+1,J) = SS*YY + C*WW
                R(INDEXR) = C*YY - SS*WW
                R(INDXR2) = SS*YY + C*WW
              INDEXR = INDEXR + 1
              INDXR2 = INDXR2 + 1
  100       CONTINUE
C
C           PREMULTIPLY  QT  BY JACOBI ROTATION.
C
            DO 110 J=1,N
              YY = QT(I,J)
              WW = QT(I+1,J)
              QT(I,J) = C*YY - SS*WW
              QT(I+1,J) = SS*YY + C*WW
  110       CONTINUE
  120     CONTINUE
C
C ***** END OF TRANSFORMATION TO UPPER TRIANGULAR *****
C
C
C ***** END OF UPDATE *****
C
C
        RETURN
C
C ***** END OF SUBROUTINE UPQRQF *****
        END

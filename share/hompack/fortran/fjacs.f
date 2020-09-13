      SUBROUTINE FJACS(X,QR,LENQR,PIVOT)
      INTEGER LENQR,N,PIVOT(N+2)
      DOUBLE PRECISION QR(LENQR),X(N)
C
C Evaluate the N x N symmetric Jacobian matrix of F(X) at X, and return
C the result in packed skyline storage format in QR.  LENQR is the length
C of QR, and PIVOT contains the indices of the diagonal elements of the
C Jacobian matrix within QR.  PIVOT(N+1) and PIVOT(N+2) are set by 
C subroutine FODE.
C
      RETURN
      END

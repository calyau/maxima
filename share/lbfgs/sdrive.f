C     Modification of sdrive.f as retrieved 1997/03/29 from 
C     ftp://ftp.netlib.org/opt/lbfgs_um.shar
C
C     This version copyright 2006 by Robert Dodier and released
C     under the terms of the GNU General Public License, version 2.
C
C     ---------------- Message from the author ----------------
C     From: Jorge Nocedal [mailto:nocedal@dario.ece.nwu.edu]
C     Sent: Friday, August 17, 2001 9:09 AM
C     To: Robert Dodier
C     Subject: Re: Commercial licensing terms for LBFGS?
C    
C     Robert:
C     The code L-BFGS (for unconstrained problems) is in the public domain.
C     It can be used in any commercial application.
C    
C     The code L-BFGS-B (for bound constrained problems) belongs to
C     ACM. You need to contact them for a commercial license. It is
C     algorithm 778.
C    
C     Jorge
C     --------------------- End of message --------------------

      SUBROUTINE FGCOMPUTE(F,G,X,N)
      INTEGER N,J
      DOUBLE PRECISION F,G(N),X(N),T1,T2
      F= 0.D0
      DO 30 J=1,N,2
        T1= 1.D0-X(J)
        T2= 1.D1*(X(J+1)-X(J)**2)
        G(J+1)= 2.D1*T2
        G(J)= -2.D0*(X(J)*G(J+1)+T1)
        F= F+T1**2+T2**2
 30   CONTINUE
      RETURN
      END

C
C     ***********************
C     SIMPLE DRIVER FOR LBFGS
C     ***********************
C
C     Example of driver for LBFGS routine, using a
C     simple test problem. The solution point is at 
C     X=(1,...,1) and the optimal function value of 0.
C
C                          JORGE NOCEDAL
C                        *** July 1990 ***
C
      PROGRAM SDRIVE
C     Change NFEVALMAX to some workable number like 100.
C     It is currently assigned a small value to ensure that we'll
C     terminate in the middle of a line search; that tests the
C     solution cache code.
      PARAMETER(NDIM=2000,MSAVE=7,NWORK=NDIM*(2*MSAVE +1)+2*MSAVE,
     &NFEVALMAX=42)
      DOUBLE PRECISION X(NDIM),G(NDIM),DIAG(NDIM),W(NWORK),SCACHE(NDIM)
      DOUBLE PRECISION F,EPS,XTOL,GTOL,T1,T2,STPMIN,STPMAX
      INTEGER IPRINT(2),IFLAG,ICALL,N,M,MP,LP,J
      LOGICAL DIAGCO
C
C     The driver for LBFGS must always declare LB2 as EXTERNAL
C
      EXTERNAL LB2
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
C
      N=100
      M=5
      IPRINT(1)= 1
      IPRINT(2)= 0
C
C     We do not wish to provide the diagonal matrices Hk0, and 
C     therefore set DIAGCO to FALSE.
C
      DIAGCO= .FALSE.
      EPS= 1.0D-5
      XTOL= 1.0D-16
      ICALL=0
      IFLAG=0
      DO 10 J=1,N,2
         X(J)=-1.2D0
         X(J+1)=1.D0
 10   CONTINUE
C
 20   CONTINUE
      CALL FGCOMPUTE(F,G,X,N)
      CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG,SCACHE)
      IF(IFLAG.LE.0) GO TO 50
      ICALL=ICALL + 1
C     We allow at most NFEVALMAX evaluations of F and G
      IF(ICALL.GE.NFEVALMAX) GO TO 50
      GO TO 20
  50  CONTINUE

      WRITE(6,60)ICALL,NFEVALMAX
      WRITE(6,70)(X(I),I=1,N)
      WRITE(6,80)
      WRITE(6,70)(SCACHE(I),I=1,N)

      CALL FGCOMPUTE(F,G,X,N)
      WRITE(6,90)F

      CALL FGCOMPUTE(F,G,SCACHE,N)
      WRITE(6,100)F

  60  FORMAT('SEARCH TERMINATED AFTER ',I4,' FUNCTION EVALUATIONS',
     &' (LIMIT: ',I4,')',/,'CURRENT SOLUTION VECTOR: ')
  70  FORMAT(4(2X,1PD22.15))
  80  FORMAT('SOLUTION CACHE: ')
  90  FORMAT('F(CURRENT SOLUTION VECTOR) = ',1PD22.15)
 100  FORMAT('F(SOLUTION CACHE) = ',1PD22.15)
      END
C
C     ** LAST LINE OF SIMPLE DRIVER (SDRIVE) **

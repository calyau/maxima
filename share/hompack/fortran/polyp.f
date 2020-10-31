      SUBROUTINE POLYP(N,NUMT,COEF,KDEG,IFLG1,IFLG2,EPSBIG,EPSSML,
     $ NUMRR,NN,MMAXT,TTOTDG,LAMBDA,ROOTS,ARCLEN,NFE,TOTDG,
     $ PDG,QDG,R,FACV,CL,Y,YP,YOLD,YPOLD,QR,ALPHA,TZ,W,
     $ WP,Z0,Z1,SSPAR,PAR,IDEG,ICOUNT,PIVOT,IPAR)
C
C THE PURPOSE OF POLYP IS TO ALIAS THE WORKSPACES "WK" AND
C "IWK" IN POLSYS TO THE VARIABLES "PDG" THROUGH "IPAR".
C POLYP GENERATES THE START POINTS FOR THE PATHS AND CALLS THE
C PATH TRACKER  POLYNF .
C
C SUBROUTINES CALLED: INITP, STRPTP, POLYNF, OTPUTP .
C
C ON INPUT:
C
C N,NUMT,COEF,KDEG,IFLG1,IFLG2,EPSBIG,EPSSML,NUMRR,NN,
C   MMAXT,TTOTDG  ARE AS DESCRIBED IN POLSYS.
C
C TOTDG  IS THE TOTAL DEGREE OF THE SYSTEM.
C
C PDG,QDG,R,FACV,CL,Y,YP,YOLD,YPOLD,QR,ALPHA,TZ,W,WP,Z0,
C   Z1,SSPAR,PAR,IDEG,ICOUNT,PIVOT,IPAR  ARE VARIABLES.
C
C ON OUTPUT:
C
C   LAMBDA,ROOTS,ARCLEN,NFE  ARE AS DESCRIBED IN POLSYS.
C ----------------------------------------------------------------------
C
C TYPE DECLARATIONS
      INTEGER N,NUMT,KDEG,IFLG1,IFLG2,NUMRR,NN,MMAXT,
     $  TTOTDG,NFE,TOTDG,IDEG,ICOUNT,PIVOT,IPAR
      INTEGER I,I1,I2,I3,IDUMMY,IFLAG,IJ,IJP1,INDEX,IPROFF,J,LIPAR,
     $ LPAR,N2,N2P1,NNFE,NP1,NUMPAT,PROFF,TRACE
      DOUBLE PRECISION COEF,EPSBIG,EPSSML,LAMBDA,ROOTS,
     $  ARCLEN,PDG,QDG,R,FACV,CL,Y,YP,YOLD,YPOLD,
     $  QR,ALPHA,TZ,W,WP,Z0,Z1,SSPAR,PAR
      DOUBLE PRECISION AARCLN,ANSAE,ANSRE,ARCAE,ARCRE,XNP1
C
C ARRAY DECLARATIONS
      DIMENSION NUMT(NN),KDEG(NN,NN+1,MMAXT),IFLG2(TTOTDG),
     $  NFE(TTOTDG),IDEG(N),ICOUNT(N),PIVOT(2*N+1),
     $  IPAR(42 + 2*N + N*(N+1)*MMAXT)
      DIMENSION IPROFF(15),LIPAR(15),LPAR(25),PROFF(25)
      DIMENSION COEF(NN,MMAXT),LAMBDA(TTOTDG),
     $  ROOTS(2,NN+1,TTOTDG), ARCLEN(TTOTDG),
     $  PDG(2,N),QDG(2,N),R(2,N),FACV(N),CL(2,N+1),
     $  Y(2*N+1),YP(2*N+1),YOLD(2*N+1),YPOLD(2*N+1),
     $  QR(2*N,2*N+2),ALPHA(2*N+1),TZ(2*N+1),W(2*N+1),
     $  WP(2*N+1),Z0(2*N+1),Z1(2*N+1),SSPAR(8),
     $  PAR(2 + 28*N + 6*N**2 + 7*N*MMAXT + 4*N**2*MMAXT)
      DIMENSION XNP1(2)
C
      N2=2*N
      NP1=N+1
      N2P1=N2+1
      IF (NUMRR .LE. 0) NUMRR=1
C
C INITIALIZATION
C
      CALL INITP(IFLG1,N,NUMT,KDEG,COEF,NN,MMAXT,PAR,IPAR,
     $                              IDEG,FACV,CL,PDG,QDG,R)
C
C INTEGER VARIABLES AND ARRAYS TO BE PASSED IN IPAR:
C
C    IPAR INDEX     VARIABLE NAME       LENGTH
C    ----------     -------------    -----------------
C          1                N               1
C          2             MMAXT              1
C          3            PROFF               25
C          4           IPROFF               15
C          5             IDEG               N
C          6             NUMT               N
C          7             KDEG               N*(N+1)*MMAXT
C
C
C DOUBLE PRECISION VARIABLES AND ARRAYS TO BE PASSED IN PAR:
C
C     PAR INDEX     VARIABLE NAME       LENGTH
C    ----------     -------------    -----------------
C          1              PDG               2*N
C          2               CL               2*(N+1)
C          3             COEF               N*MMAXT
C          4                H               N2
C          5              DHX               N2*N2
C          6              DHT               N2
C          7            XDGM1               2*N
C          8              XDG               2*N
C          9              G                 2*N
C         10             DG                 2*N
C         11           PXDGM1               2*N
C         12             PXDG               2*N
C         13               F                2*N
C         14              DF                2*N*(N+1)
C         15               XX               2*N*(N+1)*MMAXT
C         16              TRM               2*N*MMAXT
C         17             DTRM               2*N*(N+1)*MMAXT
C         18              CLX               2*N
C         19            DXNP1               2*N
C
C SET LENGTHS OF VARIABLES
      LIPAR(1)=1
      LIPAR(2)=1
      LIPAR(3)=25
      LIPAR(4)=15
      LIPAR(5)=N
      LIPAR(6)=N
      LIPAR(7)=N*(N+1)*MMAXT
      LPAR( 1)=2*N
      LPAR( 2)=2*NP1
      LPAR( 3)=N*MMAXT
      LPAR( 4)=N2
      LPAR( 5)=N2*N2
      LPAR( 6)=N2
      LPAR( 7)=2*N
      LPAR( 8)=2*N
      LPAR( 9)=2*N
      LPAR(10)=2*N
      LPAR(11)=2*N
      LPAR(12)=2*N
      LPAR(13)=2*N
      LPAR(14)=2*N*NP1
      LPAR(15)=2*N*NP1*MMAXT
      LPAR(16)=2*N*MMAXT
      LPAR(17)=2*N*NP1*MMAXT
      LPAR(18)=2*N
      LPAR(19)=2*N
C
C PROFF AND IPROFF ARE OFFSETS THAT DEFINE THE VARIABLES LISTED ABOVE
      PROFF(1)=1
      DO 10 I=2,19
          PROFF(I)=PROFF(I-1)+LPAR(I-1)
  10  CONTINUE
      IPROFF(1)=1
      DO 11 I=2,7
          IPROFF(I)=IPROFF(I-1)+LIPAR(I-1)
  11  CONTINUE
C
C DEFINE VARIABLES
      IPAR(1)=N
      IPAR(2)=MMAXT
      DO 16 I=1,19
        IPAR(IPROFF(3) + (I-1))= PROFF(I)
  16  CONTINUE
      DO 18 I=1,7
        IPAR(IPROFF(4) + (I-1))= IPROFF(I)
  18  CONTINUE
      DO 20 I=1,N
        IPAR(IPROFF(5) + (I-1))= IDEG(I)
        IPAR(IPROFF(6) + (I-1))= NUMT(I)
  20  CONTINUE
      DO 22 I1=1, N
      DO 22 I2=1, NP1
      DO 22 I3=1, NUMT(I1)
        INDEX=IPROFF(7) + (I1-1) + N*(I2-1) + N*NP1*(I3-1)
        IPAR(INDEX) = KDEG(I1,I2,I3)
  22  CONTINUE
      DO 36 I1=1,2
      DO 36 I2=1,N
        PAR(PROFF( 1) +(I1-1) + 2*(I2-1))= PDG(I1,I2)
  36  CONTINUE
      DO 37 I1=1,2
      DO 37 I2=1,NP1
        PAR(PROFF( 2) +(I1-1) +2*(I2-1))= CL(I1,I2)
  37  CONTINUE
      DO 38 I1=1,N
      DO 38 I2=1, NUMT(I1)
        PAR(PROFF( 3) +(I1-1) + N*(I2-1))=COEF(I1,I2)
  38  CONTINUE
C
C ICOUNT IS A COUNTER USED BY "STRPTP"
      ICOUNT(1)=0
      DO 50 J=2,N
          ICOUNT(J)=1
  50  CONTINUE
C
C PATHS LOOP -- ITERATE THROUGH PATHS
C
      DO 1000 NUMPAT = 1,TOTDG
C         GET A START POINT, Y, FOR THE PATH.
          Y(1) = 0.0
          CALL STRPTP(N,ICOUNT,IDEG,R ,Y(2))
C         CHECK WHETHER PATH IS TO BE FOLLOWED.
          IFLAG = IFLG2(NUMPAT)
          IF (IFLAG .NE. -2) GO TO 1000
          ARCRE = EPSBIG
          ARCAE = ARCRE
          ANSRE = EPSSML
          ANSAE = ANSRE
          TRACE = 0
C         TRACK A HOMOTOPY PATH.
          DO 65 IDUMMY=1,NUMRR
              CALL POLYNF(N2,Y,IFLAG,ARCRE,ARCAE,ANSRE,ANSAE,TRACE,
     $        QDG,NNFE,AARCLN,YP,YOLD,YPOLD,QR,ALPHA,TZ,PIVOT,W,WP,
     $        Z0,Z1,SSPAR,PAR,IPAR)
              IF (IFLAG .NE. 2 .AND. IFLAG .NE. 3) GOTO 66
  65      CONTINUE
  66      CONTINUE
C         UNSCALE AND UNTRANSFORM COMPUTED SOLUTION.
          CALL OTPUTP(N,NUMPAT,CL,FACV,PAR(PROFF(18)),Y(2),XNP1)
          LAMBDA(NUMPAT) = Y(1)
          DO 70 J=1,N
          DO 70 I=1,2
            IJ=2*J+I-2
            IJP1=IJ+1
            ROOTS(I,J,NUMPAT) = Y(IJP1)
  70      CONTINUE
          DO 72 I=1,2
            ROOTS(I,NP1,NUMPAT) = XNP1(I)
  72      CONTINUE
          ARCLEN(NUMPAT)= AARCLN
          NFE(NUMPAT)   = NNFE
          IFLG2(NUMPAT) = IFLAG
 1000 CONTINUE
C
      RETURN
      END

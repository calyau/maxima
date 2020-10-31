      SUBROUTINE HFUNP(QDG,LAMBDA,X,PAR,IPAR)
C
C HFUNP ALLOCATES STORAGE FOR SUBROUTINE HFUN1P FROM THE WORK ARRAYS
C PAR AND IPAR, AS FOLLOWS:
C
C DOUBLE PRECISION VARIABLES AND ARRAYS PASSED IN PAR
C
C     PAR INDEX     VARIABLE NAME       LENGTH
C    ----------     -------------    -----------------
C          1              PDG               2*N
C          2               CL               2*(N+1)
C          3             COEF               N*MMAXT
C          4              RHO               N2
C          5              DRHOX             N2*N2
C          6              DRHOL             N2
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
C INTEGER VARIABLES AND ARRAYS PASSED IN IPAR
C
C    IPAR INDEX     VARIABLE NAME       LENGTH            OFFSET
C    ----------     -------------    -----------------
C          1                N               1               1
C          2             MMAXT              1               2
C          3            PROFF               25              3
C          4           IPROFF               15              28
C          5             IDEG               N               43
C          6             NUMT               N               43+N
C          7             KDEG               N*(N+1)*MMAXT   43+N2+1
C
C ON INPUT:
C
C QDG  IS THE "RANDOM" VECTOR DENOTED  "A"  IN HOMPACK DOCUMENTATION.
C
C LAMBDA  IS THE CONTINUATION PARAMETER.
C
C X  IS THE INDEPENDENT VARIABLE.
C
C PAR  IS THE REAL PARAMETER ARRAY.
C
C IPAR  IS THE INTEGER PARAMETER ARRAY.
C
C ON OUTPUT:
C
C THE WORK ARRAYS PAR AND IPAR HAVE BEEN UPDATED.
C
C SUBROUTINES:  HFUN1P.
C
      INTEGER IPAR
      DOUBLE PRECISION QDG,LAMBDA,X,PAR
      DIMENSION QDG(2,1),X(2,1),PAR(*),IPAR(*)
C
      CALL HFUN1P(QDG,LAMBDA,X,
     $ PAR( IPAR(3 + ( 1-1))), PAR( IPAR(3 + ( 2-1))),
     $ PAR( IPAR(3 + ( 3-1))), PAR( IPAR(3 + ( 4-1))),
     $ PAR( IPAR(3 + ( 5-1))), PAR( IPAR(3 + ( 6-1))),
     $ PAR( IPAR(3 + ( 7-1))), PAR( IPAR(3 + ( 8-1))),
     $ PAR( IPAR(3 + ( 9-1))), PAR( IPAR(3 + (10-1))),
     $ PAR( IPAR(3 + (11-1))), PAR( IPAR(3 + (12-1))),
     $ PAR( IPAR(3 + (13-1))), PAR( IPAR(3 + (14-1))),
     $ PAR( IPAR(3 + (15-1))), PAR( IPAR(3 + (16-1))),
     $ PAR( IPAR(3 + (17-1))), PAR( IPAR(3 + (18-1))),
     $ PAR( IPAR(3 + (19-1))),
     $IPAR( IPAR(28+ ( 1-1))),IPAR( IPAR(28+ ( 2-1))),
     $IPAR( IPAR(28+ ( 5-1))),IPAR( IPAR(28+ ( 6-1))),
     $IPAR( IPAR(28+ ( 7-1))) )
C
      RETURN
      END

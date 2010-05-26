C                                                                       MAN   10
C----------------------------------------------------------------       MAN   20
C                                                                       MAN   30
C  PROBLEM 2 - SEE COMPANION PAPER                                      MAN   40
C                                                                       MAN   50
      implicit real*8 (a-h, o-z)
      REAL*8 ZETA(4), FSPACE(40000), TOL(4), Z(4)                         MAN   60
      INTEGER M(2), IPAR(11), ISPACE(2500), LTOL(4)                     MAN   70
      COMMON EPS, DMU, EPS4MU, GAMMA, XT                                MAN   80
      EXTERNAL SOLUTN, FSUB, DFSUB, GSUB, DGSUB                         MAN   90
C     DEFINE CONSTANTS, PRINT A HEADING.                                MAN  100
      GAMMA = 1.1d0                                                       MAN  110
      EPS = .01d0                                                         MAN  120
      DMU = EPS                                                         MAN  130
      EPS4MU = EPS**4/DMU                                               MAN  140
      XT = SQRT(2.*(GAMMA-1.)/GAMMA)                                    MAN  150
      WRITE (6,99999) GAMMA, XT, EPS, DMU, EPS4MU                       MAN  160
C     DEFINE NO. OF DIFFERENTIAL EQUATIONS.                             MAN  170
      NCOMP = 2                                                         MAN  180
C     ORDERS                                                            MAN  190
      M(1) = 2                                                          MAN  200
      M(2) = 2                                                          MAN  210
C     INTERVAL ENDS                                                     MAN  220
      ALEFT = 0.                                                        MAN  230
      ARIGHT = 1.                                                       MAN  240
C     LOCATIONS OF SIDE CONDITIONS                                      MAN  250
      ZETA(1) = 0.                                                      MAN  260
      ZETA(2) = 0.                                                      MAN  270
      ZETA(3) = 1.                                                      MAN  280
      ZETA(4) = 1.                                                      MAN  290
C     IPAR  VALUES                                                      MAN  300
C     A NONLINEAR PROBLEM                                               MAN  310
      IPAR(1) = 1                                                       MAN  320
C     4 COLLOCATION POINTS PER SUBINTERVAL                              MAN  330
      IPAR(2) = 4                                                       MAN  340
C     INITIAL UNIFORM MESH OF 10 SUBINTERVALS                           MAN  350
      IPAR(3) = 10                                                      MAN  360
      IPAR(8) = 0                                                       MAN  370
C     DIMENSION OF REAL WORK ARRAY  FSPACE  IS 40000                    MAN  380
      IPAR(5) = 40000                                                   MAN  390
C     DIMENSION OF INTEGER WORK ARRAY  ISPACE  IS 2500                  MAN  400
      IPAR(6) = 2500                                                    MAN  410
C     (THESE DIMENSIONS OF  FSPACE  AND  ISPACE                         MAN  420
C      ENABLE  COLSYS  TO USE MESHES OF UP TO 192 INTERVALS.)           MAN  430
C     PRINT FULL OUTPUT.                                                MAN  440
      IPAR(7) = -1                                                      MAN  450
C     INITIAL APPROXIMATION FOR NONLINEAR ITERATION IS PROVIDED         MAN  460
C     IN  SOLUTN                                                        MAN  470
      IPAR(9) = 1                                                       MAN  480
C     A REGULAR PROBLEM                                                 MAN  490
      IPAR(10) = 0                                                      MAN  500
C     NO FIXED POINTS IN THE MESH                                       MAN  510
      IPAR(11) = 0                                                      MAN  520
C     TOLERANCES ON  ALL COMPONENTS                                     MAN  530
      IPAR(4) = 4                                                       MAN  540
      DO 10 I=1,4                                                       MAN  550
        LTOL(I) = I                                                     MAN  560
        TOL(I) = 1.d-5                                                  MAN  570
   10 CONTINUE                                                          MAN  580
C     CALL  COLSYS                                                      MAN  590
      CALL COLSYS(NCOMP, M, ALEFT, ARIGHT, ZETA, IPAR, LTOL, TOL,       MAN  600
     * FIXPNT, ISPACE, FSPACE, IFLAG, FSUB, DFSUB, GSUB, DGSUB, SOLUTN) MAN  610
C     PRINT VALUES OF THE OBTAINED APPROXIMATE SOLUTION AT POINTS       MAN  620
C     X = 0,.05, ..., 1.                                                MAN  630
      X = 0.                                                            MAN  640
      WRITE (6,99998)                                                   MAN  650
      NP1 = 21                                                          MAN  660
      DO 20 III=1,NP1                                                   MAN  670
        CALL APPSLN(X, Z, FSPACE, ISPACE)                               MAN  680
        WRITE (6,99997) X, Z                                            MAN  690
        X = X + .05d0                                                     MAN  700
   20 CONTINUE                                                          MAN  710
      STOP                                                              MAN  720
99999 FORMAT (1H1, 27HDIMPLING OF SPHERICAL CAPS./8H GAMMA =,           MAN  730
     * F7.2/6H  XT =, E12.5/6H EPS =, E12.5/6H  MU =, E12.5/9H EPS**4/M,MAN  740
     * 3HU =, E12.5)                                                    MAN  750
99998 FORMAT (1H1, 44H       X             PHI           DPHI     ,     MAN  760
     * 23H      PSI          DPSI/)                                     MAN  770
99997 FORMAT (6X, F5.2, 4X, 6E15.5)                                     MAN  780
      END                                                               MAN  790

C                                                                       MAN   10
C----------------------------------------------------------------       MAN   20
C                                                                       MAN   30
C  PROBLEM 3 - SEE COMPANION PAPER                                      MAN   40
C                                                                       MAN   50
      implicit real*8 (a-h, o-z)
      REAL*8 ZETA(5), FSPACE(40000), TOL(2), SVAL(3), ELVAL(3)            MAN   60
      INTEGER M(2), ISPACE(2500), LTOL(2), IPAR(11)                     MAN   70
      REAL*8 Z(5)                                                         MAN   80
      real*8 a(28)
      COMMON EN, S, EL, CONS                                            MAN   90
      EXTERNAL FSUB, DFSUB, GSUB, DGSUB, SOLUTN                         MAN  100
      DATA SVAL /.2d0,.1d0,.05d0/, ELVAL /60d0,120d0,200d0/                      MAN  110
C                                                                       MAN  120
      EN = .2                                                           MAN  130
      CONS = .5*(3.-EN)                                                 MAN  140
      NCOMP = 2                                                         MAN  150
      M(1) = 2                                                          MAN  160
      M(2) = 3                                                          MAN  170
      ALEFT = 0.                                                        MAN  180
      ARIGHT = 1.                                                       MAN  190
C                                                                       MAN  200
      ZETA(1) = 0.                                                      MAN  210
      ZETA(2) = 0.                                                      MAN  220
      ZETA(3) = 0.                                                      MAN  230
      ZETA(4) = 1.                                                      MAN  240
      ZETA(5) = 1.                                                      MAN  250
C                                                                       MAN  260
      IPAR(1) = 1                                                       MAN  270
      IPAR(2) = 4                                                       MAN  280
      IPAR(3) = 10                                                      MAN  290
      IPAR(4) = 2                                                       MAN  300
      IPAR(5) = 40000                                                   MAN  310
      IPAR(6) = 2500                                                    MAN  320
      IPAR(7) = 0                                                       MAN  330
      IPAR(8) = 0                                                       MAN  340
      IPAR(9) = 1                                                       MAN  350
      IPAR(10) = 0                                                      MAN  360
      IPAR(11) = 0                                                      MAN  370
C                                                                       MAN  380
      LTOL(1) = 1                                                       MAN  390
      LTOL(2) = 3                                                       MAN  400
      TOL(1) = 1.E-5                                                    MAN  410
      TOL(2) = 1.E-5                                                    MAN  420
C                                                                       MAN  430
C     SOLVE A CHAIN OF 3 PROBLEMS                                       MAN  440
      DO 30 IJK=1,3                                                     MAN  450
        S = SVAL(IJK)                                                   MAN  460
        EL = ELVAL(IJK)                                                 MAN  470
        IF (IJK.EQ.1) GO TO 10                                          MAN  480
C        SET CONTINUATION PARAMETERS                                    MAN  490
        IPAR(9) = 3                                                     MAN  500
        IPAR(3) = ISPACE(1)                                             MAN  510
   10   CONTINUE                                                        MAN  520
        WRITE (6,99999) EN, S, EL                                       MAN  530
C                                                                       MAN  540
        CALL COLSYS(NCOMP, M, ALEFT, ARIGHT, ZETA, IPAR, LTOL, TOL,     MAN  550
     *   FIXPNT, ISPACE, FSPACE, IFLAG, FSUB, DFSUB, GSUB, DGSUB,       MAN  560
     *   SOLUTN)                                                        MAN  570
C                                                                       MAN  580
        IF (IFLAG.NE.1) STOP                                            MAN  590
C        PRINT VALUES OF THE OBTAINED APPROXIMATE SOLUTION AT POINTS    MAN  600
C        X = 0,1,2, ..., L.                                             MAN  610
        IS6 = ISPACE(6)                                                 MAN  620
        IS5 = ISPACE(1) + 2                                             MAN  630
        is4 = is5 + ispace(4)*(ispace(1)+1)
        X = 0.                                                          MAN  640
        WRITE (6,99998)                                                 MAN  650
        NP1 = EL + 1.5                                                  MAN  660
        DO 20 III=1,NP1                                                 MAN  670
c          CALL APPROX(III, X, Z, FSPACE(IS6), FSPACE(1), ISPACE(1),      MAN  680
c     *     FSPACE(IS5), ISPACE(2), NCOMP, M, ISPACE(4), 1, DM, 0)       MAN  690

c Not sure if this is the correct replacement for the above call.           
           call approx(iii, x, z, a, fspace(is6), fspace(1), ispace(1),
     $          fspace(is5), fspace(is4), ispace(2), ispace(3),
     $          ispace(5), ispace(8), ispace(4), 1, dm, 0)
          XL = X*EL                                                     MAN  700
          Z(2) = Z(2)/EL                                                MAN  710
          Z(4) = Z(4)/EL                                                MAN  720
          Z(5) = Z(5)/EL/EL                                             MAN  730
          WRITE (6,99997) XL, Z                                         MAN  740
          X = X + 1./EL                                                 MAN  750
   20   CONTINUE                                                        MAN  760
   30 CONTINUE                                                          MAN  770
      STOP                                                              MAN  780
99999 FORMAT (1H1, 38H ROTATING FLOW OVER A STATIONARY DISK./8H  PARAME,MAN  790
     * 11HTERS -  N =, F5.2, 6H   S =, F5.2, 6H   L =, F6.1/)           MAN  800
99998 FORMAT (1H1, 44H       X              G              DG     ,     MAN  810
     * 38H       H             DH            D2H/)                      MAN  820
99997 FORMAT (6E15.5)                                                   MAN  830
      END                                                               MAN  840

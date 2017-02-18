*DECK DAINVG
      SUBROUTINE DAINVG (RES, ADDA, NEQ, T, Y, YDOT, MITER,
     1                   ML, MU, PW, IPVT, IER )
      EXTERNAL RES, ADDA
      INTEGER NEQ, MITER, ML, MU, IPVT, IER
      INTEGER I, LENPW, MLP1, NROWPW
      DOUBLE PRECISION T, Y, YDOT, PW
      DIMENSION Y(*), YDOT(*), PW(*), IPVT(*)
      dimension neq(1)
C-----------------------------------------------------------------------
C This subroutine computes the initial value
C of the vector YDOT satisfying
C     A * YDOT = g(t,y)
C when A is nonsingular.  It is called by DLSODI for
C initialization only, when ISTATE = 0 .
C DAINVG returns an error flag IER:
C   IER  =  0  means DAINVG was successful.
C   IER .ge. 2 means RES returned an error flag IRES = IER.
C   IER .lt. 0 means the a-matrix was found to be singular.
C-----------------------------------------------------------------------
C
      IF (MITER .GE. 4)  GO TO 100
C
C Full matrix case -----------------------------------------------------
C
      LENPW = NEQ(1)*NEQ(1)
      DO 10  I = 1, LENPW
   10    PW(I) = 0.0D0
C
      IER = 1
      CALL RES ( NEQ, T, Y, PW, YDOT, IER )
      IF (IER .GT. 1) RETURN
C
      itemp = neq(1)
      CALL ADDA ( NEQ, T, Y, 0, 0, PW, itemp )
      CALL DGEFA ( PW, itemp, itemp, IPVT, IER )
      IF (IER .EQ. 0) GO TO 20
         IER = -IER
         RETURN
   20 CALL DGESL ( PW, itemp, itemp, IPVT, YDOT, 0 )
      RETURN
C
C Band matrix case -----------------------------------------------------
C
  100 CONTINUE
      NROWPW = 2*ML + MU + 1
      LENPW = NEQ(1) * NROWPW
      DO 110  I = 1, LENPW
  110    PW(I) = 0.0D0
C
      IER = 1
      CALL RES ( NEQ, T, Y, PW, YDOT, IER )
      IF (IER .GT. 1) RETURN
C
      MLP1 = ML + 1
      CALL ADDA ( NEQ, T, Y, ML, MU, PW(MLP1), NROWPW )
      CALL DGBFA ( PW, NROWPW, NEQ, ML, MU, IPVT, IER )
      IF (IER .EQ. 0) GO TO 120
         IER = -IER
         RETURN
  120 CALL DGBSL ( PW, NROWPW, NEQ, ML, MU, IPVT, YDOT, 0 )
      RETURN
C----------------------- End of Subroutine DAINVG ----------------------
      END

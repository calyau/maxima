*DECK DAIGBT
      SUBROUTINE DAIGBT (RES, ADDA, NEQ, T, Y, YDOT,
     1                   MB, NB, PW, IPVT, IER )
      EXTERNAL RES, ADDA
      INTEGER NEQ, MB, NB, IPVT, IER
      INTEGER I, LENPW, LBLOX, LPB, LPC
      DOUBLE PRECISION T, Y, YDOT, PW
      DIMENSION Y(*), YDOT(*), PW(*), IPVT(*), NEQ(*)
C-----------------------------------------------------------------------
C This subroutine computes the initial value
C of the vector YDOT satisfying
C     A * YDOT = g(t,y)
C when A is nonsingular.  It is called by DLSOIBT for
C initialization only, when ISTATE = 0 .
C DAIGBT returns an error flag IER:
C   IER  =  0  means DAIGBT was successful.
C   IER .ge. 2 means RES returned an error flag IRES = IER.
C   IER .lt. 0 means the A matrix was found to have a singular
C              diagonal block (hence YDOT could not be solved for).
C-----------------------------------------------------------------------
      LBLOX = MB*MB*NB
      LPB = 1 + LBLOX
      LPC = LPB + LBLOX
      LENPW = 3*LBLOX
      DO 10 I = 1,LENPW
 10     PW(I) = 0.0D0
      IER = 1
      CALL RES (NEQ, T, Y, PW, YDOT, IER)
      IF (IER .GT. 1) RETURN
      CALL ADDA (NEQ, T, Y, MB, NB, PW(1), PW(LPB), PW(LPC) )
      CALL DDECBT (MB, NB, PW, PW(LPB), PW(LPC), IPVT, IER)
      IF (IER .EQ. 0) GO TO 20
      IER = -IER
      RETURN
 20   CALL DSOLBT (MB, NB, PW, PW(LPB), PW(LPC), YDOT, IPVT)
      RETURN
C----------------------- End of Subroutine DAIGBT ----------------------
      END

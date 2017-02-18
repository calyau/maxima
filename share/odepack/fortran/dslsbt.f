*DECK DSLSBT
      SUBROUTINE DSLSBT (WM, IWM, X, TEM)
      INTEGER IWM
      INTEGER LBLOX, LPB, LPC, MB, NB
      DOUBLE PRECISION WM, X, TEM
      DIMENSION WM(*), IWM(*), X(*), TEM(*)
C-----------------------------------------------------------------------
C This routine acts as an interface between the core integrator
C routine and the DSOLBT routine for the solution of the linear system
C arising from chord iteration.
C Communication with DSLSBT uses the following variables:
C WM    = real work space containing the LU decomposition,
C         starting at WM(3).
C IWM   = integer work space containing pivot information, starting at
C         IWM(21).  IWM also contains block structure parameters
C         MB = IWM(1) and NB = IWM(2).
C X     = the right-hand side vector on input, and the solution vector
C         on output, of length N.
C TEM   = vector of work space of length N, not used in this version.
C-----------------------------------------------------------------------
      MB = IWM(1)
      NB = IWM(2)
      LBLOX = MB*MB*NB
      LPB = 3 + LBLOX
      LPC = LPB + LBLOX
      CALL DSOLBT (MB, NB, WM(3), WM(LPB), WM(LPC), X, IWM(21))
      RETURN
C----------------------- End of Subroutine DSLSBT ----------------------
      END

*DECK ADJLR
      SUBROUTINE ADJLR (N, ISP, LDIF)
      INTEGER N, ISP, LDIF
      DIMENSION ISP(*)
C-----------------------------------------------------------------------
C This routine computes an adjustment, LDIF, to the required
C integer storage space in IWK (sparse matrix work space).
C It is called only if the word length ratio is LRAT = 1.
C This is to account for the possibility that the symbolic LU phase
C may require more storage than the numerical LU and solution phases.
C-----------------------------------------------------------------------
      INTEGER IP, JLMAX, JUMAX, LNFC, LSFC, NZLU
C
      IP = 2*N + 1
C Get JLMAX = IJL(N) and JUMAX = IJU(N) (sizes of JL and JU). ----------
      JLMAX = ISP(IP)
      JUMAX = ISP(IP+IP)
C NZLU = (size of L) + (size of U) = (IL(N+1)-IL(1)) + (IU(N+1)-IU(1)).
      NZLU = ISP(N+1) - ISP(1) + ISP(IP+N+1) - ISP(IP+1)
      LSFC = 12*N + 3 + 2*MAX(JLMAX,JUMAX)
      LNFC = 9*N + 2 + JLMAX + JUMAX + NZLU
      LDIF = MAX(0, LSFC - LNFC)
      RETURN
C----------------------- End of Subroutine ADJLR -----------------------
      END

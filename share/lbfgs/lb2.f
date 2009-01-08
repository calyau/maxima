C     ******
C
C
C   ----------------------------------------------------------
C     DATA 
C   ----------------------------------------------------------
C
      BLOCK DATA LB2
      INTEGER LP,MP
      DOUBLE PRECISION GTOL,STPMIN,STPMAX
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
      DATA MP,LP,GTOL,STPMIN,STPMAX/6,6,9.0D-01,1.0D-20,1.0D+20/
      END

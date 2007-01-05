      double precision function dcabs1(z)
C ORIGINAL:
c      double complex z,zz
c      double precision t(2)
c      equivalence (zz,t(1))
c      zz = z
c     dcabs1 = dabs(t(1)) + dabs(t(2))
c NEW      
      double complex z
      dcabs1 = dabs(dble(z)) + dabs(dimag(z))
      return
      end

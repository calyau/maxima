c     This example was written by Michel Talon and Raymond Toy to solve
c     an example problem by Michel Talon.  This is used to test and
c     verify that continuation mode of colnew works in both Fortran and
c     maxima.
      subroutine fsub(x, z, f)
      common /cprob3/v, y
      real*8 v, y
      real*8 z(4), f(3), x
c      print *, 'fsub'
c      print *, '  x = ', x
c      print *, '  z = ', z
      if (x .eq. 0 .or. x .eq. 1) then
         print *, 'fsub x = ', x
      endif
      
      f(1) = - (1/2.0d0)*(1/x + 1/(x-1.0d0) + 1/(x-y))*z(2) +
     $     ((v - z(3) - z(4))/x + z(3)/(x-1.0d0) + z(4)/(x-y))*z(1)
      f(2) = 0
      f(3) = 0
c      print *, '  f = ', f
      return
      end
      subroutine dfsub(x, z, df)
      common /cprob3/v, y
      real*8 v, y
      real*8 z(4), f(3), x, df(3, 4)
c      print *, 'dfsub'
c      print *, '  x = ', x
c      print *, '  z = ', z
      if (x .eq. 0 .or. x .eq. 1) then
         print *, 'fsub x = ', x
      endif
      df(1,1) = z(4)/(x-y)+(-z(4)-z(3)+v)/x+z(3)/(x-1.0d0)
      df(1,2) =-0.5d0*(1/x+1/(x-1.0d0)
     $     +1/(x-1.9d0))
      df(1,3) = (1/(x-1.0d0)-1/x)*z(1)
      df(1,4) =(1/(x-y)-1/x)*z(1)
      df(2,1) =0
      df(2,2) =0
      df(2,3) =0
      df(2,4) =0
      df(3,1) =0
      df(3,2) =0
      df(3,3) =0
      df(3,4) =0
c      print *, '  df =', df
      return
      end
      subroutine gsub(i, z, g)
      common /cprob3/v, y
      real*8 v, y
      real*8 z(4), g
c      print *, 'gsub '
c      print *, '  i, z = ', i, z
      go to (10, 20, 30, 40), i
   10 g = z(2) - 2.0d0*z(1)*(v-z(3)-z(4))
      go to 99
   20 g = z(2) - 2.0d0*z(1)*z(3)
      go to 99
   30 g = z(1) - 1.0d0
      go to 99
   40 g = z(2) - 2.0d0*z(1)*z(4)
   99 continue
c      print *, '  g = ', g
      return
      end
      subroutine dgsub(i, z, dg)
      common /cprob3/v, y
      real*8 v, y
      real*8 z(4), dg(4)
c      print *, 'dgsub i,z =', i, z
      go to (10, 20, 30, 40), i
   10 dg(1) = -2.0d0*(-z(4)-z(3)+v)
      dg(2) = 1.0d0
      dg(3) = 2.0d0*z(1)
      dg(4) = 2.0d0*z(1)
      go to 99
   20 dg(1) = -2.0d0*z(3)
      dg(2) = 1.0d0
      dg(3) = -2.0d0*z(1)
      dg(4) = 0
      go to 99
   30 dg(1) = 1.0d0
      dg(2) = 0
      dg(3) = 0
      dg(4) = 0
      go to 99
   40 dg(1) = -2.0d0*z(4)
      dg(2) = 1.0d0
      dg(3) = 0
      dg(4) = -2.0d0*z(1)

   99 continue
c      print *, '  dg =', i, dg
      return
      end

      subroutine guess(x, z, dmval)
      common /cprob3/v, y
      real*8 v, y
      real*8 x, z(4), dmval(3)
      z(1) = 2.0d0*x-1.0d0
      z(2) = 2.0d0
      z(3) = 1.0d0
      z(4) = 1/(2.0d0*y-1.0d0)
      dmval(1) = 0
      dmval(2) = 0
      dmval(3) = 0
      return
      end
      
      program prob3
      implicit real*8 (a-h, o-z)
      REAL*8 FSPACE(100000), ZETA(4), TOL(4), Z(4), U(4), ERR(4)
      INTEGER ISPACE(5000), M(3), IPAR(11), LTOL(4)            
      EXTERNAL FSUB, DFSUB, GSUB, DGSUB, guess                       

      real*8 fixpnt(5)
      common /cprob3/v, y
      real*8 v, y

      parameter (nsteps=100)
      real*8 eig(nsteps+1, 4)
      
      y = 1.9d0
      v = 0.00001d0
      m(1) = 2
      m(2) = 1
      m(3) = 1

      zeta(1) = 0.0d0
      zeta(2) = 1.0d0
      zeta(3) = 1.0d0
      zeta(4) = y

      do 10 i = 1, 11
         ipar(i) = 0
   10 continue

c     Non-linear prob
      ipar(1) = 1
c     Number of collocation points
      ipar(2) = 3
c     Initial uniform mesh of 4 subintervals
      ipar(3) = 4
      ipar(8) = 0
c     Size of fspace, ispace
      ipar(5) = 100000
      ipar(6) = 5000
c     medium output
      ipar(7) = 0
c     Initial approx provided
      ipar(9) = 1
c     fixpnt is an array containing all fixed points in the mesh
      ipar(11) = 1
c     tolerances on all components
      ipar(4) = 4
c     tolerance on f and diff(f,x) and on c2 and c3
      do 20 i = 1, 4
         ltol(i) = i
         tol(i) = 1d-3
   20 continue
c     only one interior boundary
      fixpnt(1) = 1.0d0
      print *, 'v= ',v
      call colnew(3, m, 0d0, y, zeta, ipar, ltol, tol, fixpnt, ispace,
     $     fspace, iflag, fsub, dfsub, gsub, dgsub, guess)

      print *, 'iflag = ', iflag
      len = int(1+y/.1d0)
      do 30 i = 0, len
         call appsln(.1d0*i, z, fspace, ispace)
   30 continue
      write(9999, *) z(3), z(4), z(3)+y*z(4)
      
 9999 format(' c_2 = f5.2  c_3 = f5.2  E/2 = f5.2')

c     do continuation
      ipar(9) = 3
      ipar(8) = 1
      print *, 'ispace= ', ispace(1)
      do 100 i = 1, nsteps
         ipar(3) = ispace(1)
         v = v + 0.2d0
         print *, 'v = ', v
         call colnew(3, m, 0d0, y, zeta, ipar, ltol, tol, fixpnt,
     $        ispace,
     $        fspace, iflag, fsub, dfsub, gsub, dgsub, guess)
         call appsln(.1d0, z, fspace, ispace)
         eig(i,1) = z(3)
         eig(i,2) = z(4)
         eig(i,3) = z(3)+y*z(4)
         eig(i,4) = v
  100 continue
      print *, 'Eigenvalues'
      do 110 i = 1, nsteps
         print *, eig(i, 4), eig(i, 1), eig(i, 2), eig(i, 3)
  110 continue
      
      end

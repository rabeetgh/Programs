program linear
     implicit none
      integer :: i, n
      real :: x, y, a0, a1, sx, sxsq, sxy, sy
      n = 7

      open(unit=1, file='xy.txt')

      sx = 0.0; sy=0.0; sxsq=0.0; sxy=0.0

      do i=1, n
         read(1, *)x, y
         sx = sx + x
         sxsq = sxsq + x**2
         sy = sy + y
         sxy = sxy + x*y
      end do

      !! the fitting curve is y = a0 + a1*x !!

      a0 = (sy*sxsq - sx*sxy)/(n*sxsq - sx*sx)
      a1 = (n*sxy - sx*sy)/(n*sxsq - sx*sx)

      write(*,*)a0, a1 !! You can verify these values with those calculated in the book
      end program linear

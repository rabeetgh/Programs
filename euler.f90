program eulerm
      implicit none
      real :: x, h, y, a, b, yp
      integer :: i, n

      n=20; a=0.0; b=1.0
      h = 0.05

      y = 1.0
      do i=0, n
         x = a + i*h
         yp = -x*y !! yp is dy/dx which is equal to -x*y !! 

         y = y + h*yp

         write(*,*)x, y 
      end do 
      end program eulerm

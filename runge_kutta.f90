program eulerm
      implicit none
      real :: x1, x2, h, y1, y2, s1, s2
      integer :: i, n

      n=20
      h = 0.05

      x1 = 0.0
      y1 = 1.0

      do i=0, n
         s1 = -x1*y1 !! here dy/dx is equal to -x*y !! 
          
         x2= x1 + h
         y2 = y1 + h*s1

         s2 = -x2*y2

         y2 = y1 + 0.5*h*(s1+s2)

         x1 = x2
         y1 = y2

         write(*,*)x2, y2
      end do 
      end program eulerm

program trap
      implicit none
      real :: sm, x1, h, func, a, b, ans
      integer :: i, n

      !! Integration limits are  a and  b
      !! Number of points chosen are 100
      !! h is the step size and 
      !! the answer is stored in the variable ans
      !! func is the function to be integrated

      a = -1.0; b = 1.0; n=100

      h = (b-a)/n

      sm = 0.5*(func(a) + func(b))

      do i=1, n-1
         x1 = a + i*h
         sm = sm + func(x1)   
      end do

      ans=sm*h

      write(*,*)ans

      end program trap


      function func(x) result(y)

              implicit none

              real, intent(in) :: x
              real :: y

              y = 5.0*x**3-3.0*x**2 + 2.0*x + 1.0

              end function func

program lagrange
      implicit none
      integer :: i, j
      integer, parameter :: n=4 !! number of given points 
      real, dimension(n) :: xi, fi
      real :: x, sm, prodfunc

      !! here are the data points  
      xi(1) = 0.1; xi(2) = 0.2; xi(3) = 0.3; xi(4) = 0.4
      fi(1) = 1.005; fi(2) = 1.020; fi(3) = 1.045; fi(4) = 1.081 

      x = 0.16
      sm = 0.0

      do i=1, n
         prodfunc = 1
         do j=1, n
             if (j /= i) then
                     prodfunc = prodfunc*(x - xi(j))/(xi(i) - xi(j))
             end if
         end do 
                     sm = sm + fi(i) * prodfunc
      end do
       
      write(*,*)'[x, f(x)] = ', x, sm

      end program lagrange

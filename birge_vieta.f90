! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program birge
      implicit none
      integer, parameter::dp=selected_real_kind(15)
      real(dp) :: a0, b0, x, x1
      real(dp) :: a(0:4), b(0:3), p, g, f
      integer :: i, j, n

      n = 4
      a(0) = -10; a(1) = 8; a(2)=3.0; a(3) =-4.0; a(4) = 1.0


      x = -1.3_dp; x1=0.0_dp

  
      do while ((abs((x-x1)/x1))>1.0e-8)
          call poly(x, a, b, f, g, n)
          p = a(0) + b(0)*x
          x1 = x 
          x = x - p/g
          write(*,'(5F10.5, 5x, 4F10.5, 5x, 4F10.5)')a, b, x, x1, p, g
      end do
    
      end program birge

      subroutine poly(x, a, b, f, g, n)
              implicit none
              integer :: i
              integer, intent(in) :: n
              integer, parameter::dp=selected_real_kind(15)
              real(dp), intent(in) :: x, a(0:n)
              real(dp), intent(out):: b(0:n-1), f, g

              real(dp) :: s
              f = 0.0d0
              do i=0, n
                 f = f + a(i)*x**i
                 end do
              
              b(n-1) = a(n)
              s = b(n-1)    
              do i=1, n-1
                 b(n-(i+1)) = a(n-i) + x*b(n-i)
                 s = b(n-(i+1)) + x*s
              end do 

              g = s
              end subroutine poly

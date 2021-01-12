! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program succ_approx
      implicit none
      integer, parameter:: dp=selected_real_kind(15)
      real(dp) :: x0, x1, e
      real(dp) :: f0
      integer  :: i
      
        x0=-2.0; e=1.0e-10


     !! calling subroutine outside to make sure that f and df are not equal to zero
     call fdf(x0, f0) 
     write(*,'(3f15.10)')x0, f0
     x1 = f0
     
     do while (abs((x1-x0)/x1) > e)
       x0 = x1
       call fdf(x0, f0)
       x1 = f0
       i = i + 1
       write(*,'(i5, 4f15.5)')i, x0, f0
     end do 

      end program succ_approx


   ! this subroutine calculates the function value and its derivative at point x
   subroutine fdf(x, f) 
          integer, parameter:: dp=selected_real_kind(15)
          real(dp), intent(in) :: x ! input
          real(dp), intent(out) :: f ! outputs: function value and its derivative at x
  
          f = (1.0_dp + exp(-x))/2.0_dp

    end subroutine fdf

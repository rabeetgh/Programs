! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program newton_raph
      implicit none
      integer, parameter:: dp=selected_real_kind(15)
      real(dp) :: x, e
      real(dp) :: f, df
      integer  :: i
      
        x=-2.0; e=1.0e-10


     !! calling subroutine outside to make sure that f and df are not equal to zero
     call fdf(x, f, df) 
     write(*,'(3f15.10)')x, f, df
     
     do while (abs(f) > e)
       call fdf(x, f, df) 
       x = x - f/df
       i = i + 1
       write(*,'(i5, 3f15.10)')i, x, f, df
     end do 


      end program newton_raph


   ! this subroutine calculates the function value and its derivative at point x
   subroutine fdf(x, f, df) 
          integer, parameter:: dp=selected_real_kind(15)
          real(dp), intent(in) :: x ! input
          real(dp), intent(out) :: f, df ! outputs: function value and its derivative at x
  
          f = x**3 - 2.5_dp*x**2 - 2.46_dp*x + 3.96_dp
          df= 3.0*x**2 - 2.0*2.5*x - 2.46

    end subroutine fdf

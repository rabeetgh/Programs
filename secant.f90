! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program newton_raph
      implicit none
      integer, parameter:: dp=selected_real_kind(15)
      real(dp) :: x0, x1, x2, e
      real(dp) :: f0, f1
      integer  :: i
      
        x0=-2.0; x1=-2.001; e=1.0e-10


     !! calling subroutine outside to make sure that f and df are not equal to zero
     call fdf(x0, f0) 
     write(*,'(3f15.10)')x0, f0

     call fdf(x1, f1) 
     write(*,'(3f15.10)')x1, f1



     
     do while (abs(f0) > e)

       x2 = (x0*f1 - x1*f0)/(f1 - f0)

       f0 = f1
       x0 = x1
       x1 = x2

       call fdf(x2, f1)

        
       i = i + 1
       write(*,'(i5, 4f15.5)')i, x0, x1, f0, f1
     end do 


      end program newton_raph


   ! this subroutine calculates the function value and its derivative at point x
   subroutine fdf(x, f) 
          integer, parameter:: dp=selected_real_kind(15)
          real(dp), intent(in) :: x ! input
          real(dp), intent(out) :: f ! outputs: function value and its derivative at x
  
          f = x**3 - 2.5_dp*x**2 - 2.46_dp*x + 3.96_dp

    end subroutine fdf

! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program newton_raph_2d
      implicit none
      integer, parameter:: dp=selected_real_kind(15)
      real(dp) :: x, y, e
      real(dp) :: f, fx, fy, g, gx, gy, d
      integer  :: i
      
        x=4.0; y=5.0; e=1.0e-10; i=0


     call fg(x, y, f, fx, fy, g, gx, gy, d)
     write(*,'(i5, 5f15.10)')i, x, y, f, g
     
     do while (abs(f) > e .and. abs(g) > e)
       call fg(x, y, f, fx, fy, g, gx, gy, d)
       x = x + (-f*gy + g*fy)/d
       y = y + (-g*fx + f*gx)/d
       i = i + 1
       write(*,'(i5, 5f15.10)')i, x, y, f, g
     end do 


      end program newton_raph_2d


   subroutine fg(x, y, f, fx, fy, g, gx, gy, d) 
          integer, parameter:: dp=selected_real_kind(15)
          real(dp), intent(in) :: x, y 
          real(dp), intent(out) :: f, fx, fy, g, gx, gy, d 
  
          f = x**2 - y**2 + 7.0_dp
          fx= 2.0*x; fy=-2.0*y

          g = x - x*y + 9.0_dp
          gx= 1.0_dp - y ; gy = -x

          d = fx*gy - gx*fy
    end subroutine fg

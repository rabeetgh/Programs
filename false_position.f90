! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program falseposition
      implicit none
      integer, parameter:: dp=selected_real_kind(15)
      real(dp) :: x, x0, x1, x2, e
      real(dp) :: y0, y1, y2
      integer  :: i
      real(dp), external :: func

      x=-5.0; e=1.0d-10; i=0

! tablulate the function values to guess the initial points 
! for the bisection method (we print from -4.0 to 4.0 with dx=1.0)
      do while (x < 4.0_dp)
                x = x + 1.0_dp
                write(*,'(2F15.5)')x, func(x)
      end do
      
      
!x0 and x1 are the intial points at which function has different sign     

        x0=-2.0; x1=-1.0
        y0 = func(x0); y1 = func(x1)

!making sure that the function has different sign at intial points

        if(sign(1.0_dp, y0)==sign(1.0_dp, y1)) then
              write(*,*)"Unsuitable inital value"
              stop
        end if

! calculating the root using false_position method

      do i=1, 50
                x2 = (x0*y1 - x1*y0)/(y1 - y0)
                y2 = func(x2)

                if (abs(y2) < e) exit 

                if(sign(1.0_dp, y0)==sign(1.0_dp, y2)) then
                        x0 = x2
                        y0 = y2
                else
                        x1 = x2
                        y1 = y2
                end if
                write(*,'(I5, 3F15.10)')i, x2, y2
      end do 


      end program falseposition



      

      ! here we define the funcion f(x) 

        function func(x) result(y)
          integer, parameter:: dp=selected_real_kind(15)
          real(dp), intent(in) :: x ! input
          real(dp) :: y ! output
  
          y = x**3 - 2.5_dp*x**2 - 2.46_dp*x + 3.96_dp

        end function

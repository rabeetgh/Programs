! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program bisection
      implicit none
      integer, parameter:: dp=selected_real_kind(15)
      real(dp) :: x, x0, x1, x2, p
      real(dp) :: y0, y1, y2
      integer  :: i
      real(dp) :: func
      
      x=-5
      p=1.0d-10
      i=0

! tablulate the function values to guess the initial points 
! for the bisection method (we print from -4.0 to 4.0 with dx=1.0)
     ! do while (x < 5.0_dp)
     !           x = x + 0.5_dp
     !           write(*,*)x, func(x)
     ! end do
      
      
!x0 and x1 are the intial points at which function has different sign     

        x0=-1.5; x1=0.0
        y0 = func(x0); y1 = func(x1)

!making sure that the function has different sign at intial points

      !  if(sign(1.0_dp, y0)==sign(1.0_dp, y1)) then
      !        write(*,*)"Unsuitable inital value"
      !        stop
      !  end if

! calculating the root using bisection method

     do while (abs((x1-x0)/x1) > p)
               x2 = (x0 + x1)/2.0_dp
               y2 = func(x2)
               i = i + 1
               if(sign(1.0_dp, y0)==sign(1.0_dp, y2)) then
                      x0 = x2
               else
                      x1 = x2
               end if
               write(*,'(I3, 5X, 3F10.5)')i, x2, y2
     end do 


      end program bisection



      

      !here we define the funcion f(x) 

        function func(x) result(y)
          integer, parameter:: dp=selected_real_kind(15)
          real(dp), intent(in) :: x ! input
          real(dp) :: y ! output
           y=x**2-2.0
          !y = x**3 - 2.5_dp*x**2 - 2.46_dp*x + 3.96_dp
          !y = x*sin(x) + cos(x)
        end function

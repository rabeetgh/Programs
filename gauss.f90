program gaussqd
          implicit none
          real ::a,b, ans, func
          integer, parameter :: n=2
          real, dimension(n):: xi, w
          real::x1,x2,s
          integer::i

          a = -1.0; b=1.0

          x1=-1.0
          x2=1.0
          call gauleg(x1,x2,xi,w,n)
          s=0.0
          
          write(*,*) 'Printing here the weights and points'
          do i=1,n
             s=s+0.5*(b-a)*w(i)*func(0.5*(xi(i)*(b-a)+b+a))

             write(*,'(2F10.5)')w(i), xi(i)
          end do
          
          write(*,*) 
          write(*,*)'Answer is here' 
          write(*,'(F10.5)')s
end program gaussqd



  function func(x) result(y)

          implicit none

          real, intent(in) :: x
          real :: y

          y = 5.0*x**3-3.0*x**2 + 2.0*x + 1.0

   end function func


   subroutine gauleg(x1,x2,x,w,n)
     real,intent(in):: x1,x2
     integer,intent(in)::n
     real,intent(out)::x(n),w(n)
     real,parameter::eps=3.d-14
     integer:: i,j,m
     real::p1,p2,p3,pp,xm,xl,z,z1
     m=(n+1)/2
     xm=0.5d0*(x2+x1)
     xl=0.5d0*(x2-x1)
     do i=1,m
           z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
     1     continue
           p1=1.d0
           p2=0.d0
           do j=1,n
                 p3=p2
                 p2=p1
                                                                                                                           
                  p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
           end do
           pp=n*(z*p1-p2)/(z*z-1.d0)
           z1=z
           z=z1-p1/pp
           if(abs(z-z1).gt.eps) go to 1
           x(i)=xm-xl*z
           x(n+1-i)=xm+xl*z
           w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
           w(n+1-i)=w(i)
     end do
     end subroutine gauleg

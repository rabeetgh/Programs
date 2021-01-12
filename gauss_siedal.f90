! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program gausss
     implicit none
        integer :: iter, i, j, k
        integer, parameter:: dp=selected_real_kind(15)
        integer, parameter:: n=3, mx=50
        real(dp) :: a(n,n+1), x(n), big, s, temp
        real(dp) :: relerror
        real(dp), parameter :: e=1.0e-8

        !! You can either assign each value of coffiecients a manually !!
        !! I am solving the equations given in example 4.1

        !a(1,1) = 1.0; a(1,2) = 1.0; a(1,3) = 2.0
        !a(2,1) = 3.0; a(2,2) = -10.0; a(2,3) = 3.0

        !! you may also read cofficients a from the file
        open (unit=1, file='aij.dat') 

        do i=1, n
              read(1,*)(a(i, j), j=1, n+1)
        end do

        x = 0.0d0

        do iter=1, mx
           big = 0.0_dp
           do i=1, n
              s=0.0
              do j=1, n
                 if (j /= i) then
                         s = s + a(i,j) * x(j)
                 end if
               end do
               temp = (a(i, n+1) - s)/a(i,i)
               relerror = abs((x(i) - temp)/temp)
               if (relerror > big) then
                       big = relerror
               end if
               x(i) = temp
              end do      
            if (big <= e) then
             write(*,*)'Converges to a solution'
             write(*, '(5F10.5)')x
             stop
            end if
        end do

        write(*,*)'Does not converges in mx iterations'
        write(*, *)x

     end program gausss

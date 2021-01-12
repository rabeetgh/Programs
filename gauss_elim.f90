! Acknowledgement:: Computer oriented numerical methods by Rajaraman

program gauss
     implicit none
        integer :: i, j, k, p, q, m
        integer, parameter:: dp=selected_real_kind(15)
        integer, parameter:: n=3
        real(dp) :: a(n,n+1), x(n), u, s, mx, temp
        real(dp), parameter :: e=1.0e-6

        !! You can either assign each value of coffiecients a manually !!
        !! I am solving the equations given in example 4.1

        !2.0 x1 + 3 x2 + 5 x3 =23           (1)
        !3.0 x1 + 4.0 x2 + x3 = 14          (2)
        !6.0 x1 + 7.0 x2 + 2 x3 = 26        (3)

        a(1,1) = 2.0; a(1,2) = 6.0; a(1,3) = -1.0; a(1,4) = -14.0
        a(2,1) = 5.0; a(2,2) = -1.0; a(2,3) = 2.0; a(2,4) = 29.0
        a(3,1) = -3.0; a(3,2) = -4.0; a(3,3) = 1.0; a(3,4) = 4.0

        !! you may also read cofficients a from the file
        !open (unit=1, file='aij.dat') 

        !do i=1, n
        !      read(1,*)(a(i, j), j=1, n+1)
        !end do

        do k=1, n-1

           !! Pivotal condensation !!
            mx = abs(a(k,k))
            p = k 
            do m=(k+1), n
               if (abs(a(m,k)) > mx) then
                       mx = abs(a(m,k))
                       p = m
               end if
            end do

            if (mx <= e) then
                    write(*,*)'Ill-conditioned equations'
                    stop
            end if

            do q=k, n+1
               temp = a(k,q)
               a(k,q) = a(p,q)
               a(p,q) = temp
            end do
           !!==========================!!

           do i=(k+1), n
              u = a(i, k)/a(k,k)
              do j=k, n+1
                 a(i,j) = a(i,j) - u*a(k,j)
              end do
           end do
         end do

         x(n) = a(n, n+1)/a(n,n)

         do i=n-1, 1, -1
            s=0.0
            do j=i+1, n
               s = s + a(i,j)*x(j)
            end do
            x(i) = (a(i, n+1) - s)/a(i,i)
          end do

          write(*, '(5F10.5)')x
     end program gauss

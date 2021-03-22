program polyreg
     implicit none
      integer :: i
      integer, parameter:: dp=selected_real_kind(15)
      integer, parameter:: n=9
      real(dp):: a(3, 4)
      real(dp):: x, y, a0, a1, sx, sxx, sxxx, sxxxx, sxy, sy, sxxy

      open(unit=1, file='xy_pol_reg.dat')

      sx = 0.0; sy=0.0; sxx=0.0; sxy=0.0
      sxxx=0.0; sxxxx=0.0; sxxy=0.0

      do i=1, n
         read(1, *)x, y
         sx = sx + x
         sxx = sxx + x**2
         sxxx = sxxx + x**3
         sxxxx = sxxxx + x**4
         sy = sy + y
         sxy = sxy + y*x
         sxxy = sxxy + x*x*y
      end do

      !! the fitting curve is y = a0 + a1*x + a2*x*x!!

      write(*,*)'sx=', sx, 'sxx=', sxx, 'sxxx=', sxxx, 'sxxxx=', sxxxx
      write(*,*)'sy=', sy, 'sxy=', sxy, 'sxxy=', sxxy

       a(1,1) = n; a(1,2) = sx; a(1,3) = sxx; a(1,4) = sy
       a(2,1) = sx; a(2,2) = sxx; a(2,3) = sxxx; a(2,4) = sxy
       a(3,1) = sxx; a(3,2) = sxxx; a(3,3) = sxxxx; a(3,4) = sxxy

      ! "System of equations to be solved"
      !write(*,*)a0, a1, a2

      call gauss(3, a)

      end program polyreg

   subroutine gauss(n, a)
     implicit none
        integer :: i, j, k, p, q, m
        integer, parameter:: dp=selected_real_kind(15)
        integer, intent(in):: n
        real(dp), intent(in) :: a(n, n+1)
        real(dp) :: b(n, n+1)
        real(dp) :: x(n), u, s, mx, temp
        real(dp), parameter :: e=1.0e-6

        b = a
        do k=1, n-1

           !! Pivotal condensation !!
            mx = abs(b(k,k))
            p = k 
            do m=(k+1), n
               if (abs(b(m,k)) > mx) then
                       mx = abs(b(m,k))
                       p = m
               end if
            end do

            if (mx <= e) then
                    write(*,*)'Ill-conditioned equations'
                    stop
            end if

            do q=k, n+1
               temp = b(k,q)
               b(k,q) = b(p,q)
               b(p,q) = temp
            end do
           !!==========================!!

           do i=(k+1), n
              u = b(i, k)/b(k,k)
              do j=k, n+1
                 b(i,j) = b(i,j) - u*b(k,j)
              end do
           end do
         end do

         x(n) = b(n, n+1)/b(n,n)

         do i=n-1, 1, -1
            s=0.0
            do j=i+1, n
               s = s + b(i,j)*x(j)
            end do
            x(i) = (b(i, n+1) - s)/b(i,i)
          end do
          write(*,*)
          write(*, 100)x
 
          100 format(3x, 'y(x)=',F8.4,'+',F8.4,'x',F8.4,'x^2')
     end subroutine gauss

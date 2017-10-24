function test_integrand(x, n)
   implicit none
   integer, parameter :: dp = kind(1.d0)
   real(dp) :: test_integrand
   integer, intent(in) :: n
   real(dp), dimension(n), intent(in) :: x

   integer :: i

   test_integrand = 1d0

   do i = 1, n
      test_integrand = test_integrand*x(i)
   enddo

end function test_integrand

function strange_test(x, n)
   implicit none
   integer, parameter :: dp = kind(1d0)
   real(dp) :: strange_test
   integer, intent(in) :: n
   real(dp), dimension(n), intent(in) :: x

   ! Result between 0-1 : 0.632121

   strange_test = x(1)*Exp(-x(2))*(1d0 - log(x(3)))

end function strange_test

function lepage_test(x, n)
   implicit none
   integer, parameter :: dp = kind(1d0)
   real(dp), parameter :: pi = 3.141592653589793238d0
   real(dp) :: lepage_test
   integer, intent(in) :: n
   real(dp), dimension(n), intent(in) :: x

   real(dp) :: a, pref, coef
   integer :: i

   a = 0.1d0
   pref = (1d0/a/dsqrt(pi))**n
   coef = 0d0
   do i = 1, 100*n
      coef = coef + float(i)
   enddo
   do i = 1, n
      coef = coef + (x(i) - 1d0/2d0)**2/a**2
   enddo
   coef = coef - float(100*n)*float(100*n+1)/2d0

   lepage_test = pref*exp(-coef)

end function lepage_test


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

program vegas_example
   use Vegas_mod, only : vegas
   implicit none
   integer, parameter :: dp = kind(1.d0)
   real(dp), external :: test_integrand, strange_test, lepage_test
   integer :: n_dim, n_iter, n_events
   real(dp) :: res, sigma, chi2
   double precision, external :: square

   print *, "Start of the program"

   print *, "Computing integral..."
   n_dim = 9
   n_iter = 10
   n_events = 1d6

   call vegas(lepage_test, n_dim, n_iter, n_events, res, sigma, chi2)

   print *, "integral of (x*y dxdy) from 0 to 1 is: ", res, "+/-", sigma

end program

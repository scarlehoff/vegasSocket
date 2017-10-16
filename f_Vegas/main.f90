program vegas_example
   use vegas_mod, only : vegas
   implicit none
   integer, parameter :: dp = kind(1.d0)
   real(dp), external :: test_integrand, strange_test
   integer :: n_dim, n_iter, n_events
   real(dp) :: res, sigma
   double precision, external :: square

   print *, "Start of the program"

   print *, "Computing integral..."
   n_dim = 3
   n_iter = 10
   n_events = 1d5

   call vegas(strange_test, n_dim, n_iter, n_events, res, sigma)

   print *, "integral of (x*y dxdy) from 0 to 1 is: ", res, "+/-", sigma

end program

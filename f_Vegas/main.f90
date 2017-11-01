program vegas_example
   use Vegas_mod, only : vegas, activate_parallel_sockets
   implicit none
   integer, parameter :: dp = kind(1.d0)
   real(dp), external :: test_integrand, strange_test, lepage_test
   integer :: n_dim, n_iter, n_events
   real(dp) :: res, sigma, chi2
   double precision, external :: square
#ifdef USE_SOCKETS
   integer :: n_sockets, socket_number
   character(len=32) :: arg
   if (iargc() < 2) then
      print *, "Needs number of sockets and socket number of this instance"
      print *, "./vegas_example [n_sockets] [socket_number_this_instance]"
      stop
   endif
   call getarg(1, arg)
   read(arg, '(I3)') n_sockets
   call getarg(2, arg)
   read(arg, '(I3)') socket_number
   call activate_parallel_sockets(n_sockets, socket_number)
#endif

   n_dim = 9
   n_iter = 10
   n_events = 1d7

   call vegas(lepage_test, n_dim, n_iter, n_events, res, sigma, chi2)

   print *, "integral total is: ", res, "+/-", sigma

end program

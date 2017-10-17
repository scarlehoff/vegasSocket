program example
   implicit none
   integer, parameter :: dp = selected_real_kind(14)
   real(dp) :: a_double
   character(len=9) :: hostname = "localhost"
   integer :: port = 8888
   integer :: ifail

   a_double = 45.9d0
   print *, "Sending a nunber to the python socket: ", a_double

   call socket_exchange(a_double, dp, hostname, port, ifail)

   print *, "And the socket returned: ", a_double

   if (ifail == 0) then
      print *, "Success!"
   else
      print *, "Failure!"
   endif

end program example

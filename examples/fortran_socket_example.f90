program example
   implicit none
   integer, parameter :: dp = selected_real_kind(14)
   character(len=9) :: hostname = "localhost"
   integer :: port = 8888
   integer :: ifail, i, j
   real(dp), dimension(2,3) :: a_double
   real(dp) :: v_double

   do i = 1, 3
      do j = 1, 2
         call random_number(v_double)
         a_double(j,i) = v_double
      enddo
   enddo

   print *, "Sending an array to the socket: "
   print *, "Size of the array: ", size(a_double)
   print *, "Contents:"

   do i = 1, 3
      print *, a_double(:,i)
   enddo

   call socket_exchange(a_double, size(a_double)*dp, hostname, port, ifail)

   print *, "And the socket returned: "
   print *, a_double(:,1)
   print *, a_double(:,2)
   print *, a_double(:,3)

   if (ifail == 0) then
      print *, "Success!"
   else
      print *, "Failure!"
   endif

end program example

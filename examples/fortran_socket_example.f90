program example
   implicit none
   integer, parameter :: dp = selected_real_kind(14)
   character(len=32) :: hostname = "localhost" // char(0)
   integer :: port = 8888
   integer :: seed  = 1
   character(len=32) :: arg
   integer :: ifail, i, j, n_args
   real(dp), dimension(2,3) :: a_double, a_double_sanity

   n_args = iargc()
   if (n_args > 0) then
      call getarg(1, arg)
      read(arg, '(I3)') seed
   endif
   if (n_args > 1) then
      call getarg(2, hostname)
   endif
   if (n_args > 2) then
      call getarg(3, arg)
      read(arg, '(I4)') port
   endif

   call srand(seed)

   do i = 1, 3
      do j = 1, 2
         a_double(j,i) = rand()
      enddo
   enddo

   print *, "Sending an array to the socket: "
   print *, "Size of the array: ", size(a_double)
   print *, "Contents:"

   do i = 1, 3
      print *, a_double(:,i)
   enddo

   a_double_sanity = a_double

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

   print *, "If we were to do the sum here in fortran we would obtain (assume other seed = seed+1):"
   call srand(seed+1)
   do i = 1, 3
      do j = 1, 2
         a_double_sanity(j,i) = rand() + a_double_sanity(j,i)
      enddo
   enddo
   print *, a_double_sanity(:,1)
   print *, a_double_sanity(:,2)
   print *, a_double_sanity(:,3)

   print *, "The difference python - fortran is: "
   print *, a_double(:,1) - a_double_sanity(:,1)
   print *, a_double(:,2) - a_double_sanity(:,2)
   print *, a_double(:,3) - a_double_sanity(:,3)

end program example

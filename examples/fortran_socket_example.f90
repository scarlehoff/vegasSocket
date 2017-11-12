program example
   implicit none
   integer, parameter :: dp = selected_real_kind(14)
   logical :: sanity_check = .false.
   character(len=32) :: hostname = "localhost" // char(0)
   integer :: port = 8888
   integer :: seed  = 1
   character(len=32) :: arg
   integer :: ifail, i, j, n_args
   real(dp), dimension(2,3) :: a_double, a_double_sanity

   !>
   !> Argument parser 
   !> 
   n_args = iargc()
   if (n_args > 0) then
      call getarg(1, arg)
      read(arg, '(I3)') seed
      if (seed == 1) sanity_check = .true.
   else
      print *, "You need to at least specify the random number"
      stop
   endif
   if (n_args > 1) then
      call getarg(2, arg)
      hostname = trim(arg) // char(0)
   endif
   if (n_args > 2) then
      call getarg(3, arg)
      read(arg, '(I4)') port
   endif

   !>
   !> Create an array of 6 members
   !> generate 6 random numbers
   !> 
   call srand(seed)
   do i = 1, 3
      do j = 1, 2
         a_double(j,i) = rand()
      enddo
   enddo

   print *, "Sending an array to the socket: "
   print *, "Size of the array: ", size(a_double)
   print *, "Contents:"
   do i = 1, 2
      write(*, '(3f12.5)') (a_double(i,j), j = 1,3)
   enddo

   !> Save the result so we can compare afterwards
   a_double_sanity = a_double

   !> args: (array to sync, size of the array, hostname, port, error flag)
   call socket_exchange(a_double, size(a_double)*dp, hostname, port, ifail)
   if (ifail == 0) then
      print *, "Success!"
   else
      print *, "Failure!"
   endif

   print *, "Content the socket returned: "
   do i = 1, 2
      write(*, '(3f12.5)') (a_double(i,j), j = 1,3)
   enddo

   !>
   !> As a sanity check, let's do the same sum in Fortran
   !> 
   if (sanity_check) then
      print *, "If we were to do the sum here in fortran we would obtain (assume other seed = seed+1):"
      call srand(seed+1)
      do i = 1, 3
         do j = 1, 2
            a_double_sanity(j,i) = rand() + a_double_sanity(j,i)
         enddo
      enddo

      do i = 1, 2
         write(*, '(3f12.5)') (a_double_sanity(i,j), j = 1,3)
      enddo

      print *, "The difference python - fortran is: "
      do i = 1, 2
         write(*, '(3f12.5)') (a_double_sanity(i,j) - a_double(i,j), j = 1,3)
      enddo
   endif

end program example

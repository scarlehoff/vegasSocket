module vegas_mod
!$ use omp_lib
   implicit none
   private

   public :: vegas, vegasnr_new, activate_parallel_sockets, recover_run

   ! Parameters
   integer, parameter :: dp = kind(1.d0)
   logical, parameter :: verbose_progress = .true.
   integer, parameter :: debug_level = 0
   
   ! Subdivisions of the Vegas grid per dimension
   integer, parameter :: NDMX = 100
   ! Damping parameter, alpha = 0d0 -> no adaptation
   real(dp), parameter :: ALPHA = 1.5d0
   ! Kahan summation for improved numerical precision
   logical, parameter :: kahan = .true. 
   ! Write & read the grid using hexadecimal 
   character(len=9), parameter :: grid_fmt = "(/(5z16))"
   ! Legacy vegas compatibility
   integer, parameter :: EXTERNAL_FUNCTIONS = 6
   integer, parameter :: MXDIM = 26

   type resultado
      real(dp) :: sigma = 0d0
      real(dp) :: weight = 0d0
      real(dp) :: integral = 0d0
      real(dp) :: chi2 = 0d0
   end type resultado

   ! Socket data
   character(len=:), allocatable :: hostname
   integer :: port, ifail
   logical :: socketed_warmup = .false.

   ! General attributes
   logical :: warmup_flag = .true.
   logical :: writegrid = .true.
   logical :: recover_grid = .false.
   integer :: n_sockets, socket_number
   integer :: n_events_initial, n_events_final
   integer :: ev_counter
   type(resultado), allocatable, dimension(:) :: resultados

   ! Vegas output files
   character(len=128) :: log_filename = "vegas_output.log"
   integer, parameter :: log_unit = 506
   integer, dimension(2) :: units
   character(len=128) :: grid_filename = "vegas_grid.grid"

   contains
      subroutine activate_parallel_sockets(n_sockets_in, socket_number_in, hostname_in, port_in)
         !>
         !> This subroutine must be called by the program willing to use vegas prior
         !> to the actual call to vegas in order to define the number of sockets that are going to be used
         !> the socket number this instance of vegas will have (ex: 3 of 6 would be n_sockets = 6, socket_number = 3
         !> 
         integer, intent(in) :: n_sockets_in, socket_number_in
         integer, intent(in), optional :: port_in
         character(len=*), intent(in), optional :: hostname_in
         n_sockets = n_sockets_in
         socket_number = socket_number_in
         if (n_sockets > 1) then
            socketed_warmup = .true.
         else
            socketed_warmup = .false.
         endif
        
         if (present(hostname_in)) then
            hostname = trim(hostname_in) // char(0)
         else
            hostname = "localhost" // char(0)
         endif

         if (present(port_in)) then
            port = port_in
         else
            port = 8888
         endif
      end subroutine

      subroutine recover_run(old_grid_file)
         character(len=128), optional, intent(in) :: old_grid_file
         recover_grid = .true.
         if (present(old_grid_file)) then
            grid_filename = old_grid_file
         endif
      end subroutine 

      subroutine vegas(f_integrand, n_dim, n_iter, n_events, final_result, sigma, chi2, &
           sigR, sigS, sigV, sigT, sigVV, sigU )
         
         real(dp), external :: f_integrand
         integer, intent(in) :: n_dim, n_events, n_iter
         real(dp), intent(out) :: final_result, sigma, chi2
         real(dp), external, optional :: sigR, sigS, sigV, sigT, sigVV, sigU

         integer :: i, j, k, ind, n_events_per_instance, n_check
         real(dp) :: tmp, tmp2, err_tmp, err_tmp2, xjac, wgt, xwgt
         ! Random variables vector
         real(dp), dimension(n_dim) :: x
         ! Stores in which vegas subdivision each random number falls
         integer, dimension(n_dim) :: div_index
         ! Auxiliary array to be use during grid adaptation (this one only used upon first integration)
         real(dp), dimension(NDMX) :: rweight
         ! Store the Vegas grid
         real(dp), dimension(NDMX, n_dim) :: divisions
         ! All data necessary for integration or grid adaptation will be stored in grid_data
         real(dp), dimension(5, NDMX, n_dim), target :: grid_data
         ! Pointers to different slices of the array for ease of use
         ! res and res2 are the current sum of f and f^2 (f=integral) 
         ! and err_r, err_r2 their correspondent precision error from kahan sum
         real(dp), pointer :: res, res2, err_r, err_r2
         ! ar_[] stores the result of [] for each of the NDMX, n_dim vegas subdivisions
         real(dp), dimension(:,:), pointer :: ar_res, ar_res2
         real(dp), dimension(:,:), pointer :: ar_err, ar_err2



#ifndef USE_SOCKETS
         socketed_warmup = .false.
#endif

         print *, "Entering New Vegas"

         !> Initialise pointers (we assume NDMX is always >= 4)
         !> but we make no assumption on n_dim
         res  => grid_data(1,1,1)
         res2 => grid_data(1,2,1)
         ar_res  => grid_data(2,:,:)
         ar_res2 => grid_data(3,:,:)
         if (kahan) then
            err_r  => grid_data(1,3,1)
            err_r2 => grid_data(1,4,1)
            ar_err  => grid_data(4,:,:)
            ar_err2 => grid_data(5,:,:)
         endif

         !> Initialise variables
         grid_data(:,:,:) = 0d0
         divisions(1,:) = 1d0
         divisions(2:,:) = 0d0
         rweight(:) = 1d0
         xjac = 1d0/n_events
         call seed_rand(1)

         allocate(resultados(n_iter))

         !> Open up the log file and start writing to it
         open(unit = log_unit, file=trim(log_filename), position="Append", action="write")
         units = (/6, log_unit/)
         write(log_unit,*) "Welcome to New Vegas"

         if (socketed_warmup) then
            !> if sockets are active, follow necessary initialisation
            n_events_per_instance = n_events/n_sockets
            n_events_initial = 1 + n_events_per_instance*(socket_number-1)
            n_events_final = socket_number*n_events_per_instance
            if (socket_number == n_sockets) then
               n_events_final = n_events_final + mod(n_events, n_sockets)
            endif
            do i = 1, 2
               write(units(i),'(A,I0,A,I0)') " > > > Vegas instance No: ", socket_number, " of ", n_sockets
               write(units(i),'(A,I0,A)') " > > > Running for ", n_events_final + 1 - n_events_initial, " events for this instance"
               write(units(i),'(A,I0)') " > > > From: ", n_events_initial
               write(units(i),'(A,I0)') " > > > To: ", n_events_final
            enddo
         else
            n_events_initial = 1
            n_events_final = n_events
            n_events_per_instance = n_events
         endif
         n_check = ceiling(n_events_per_instance/10d0)

         !$ print *, " $ OMP active"
         !$ print *, " $ Maximum number of threads: ", OMP_get_num_procs()
         !$ print *, " $ Number of threads selected: ", OMP_get_max_threads()

         !> Debug Level = 1
         !> Don't write warmup grid file
         if (debug_level == 1) writegrid = .false.

         !> Initial rebining
         !> we can either create a new grid or read an old one
         if ((warmup_flag).and.(.not.recover_grid)) then
            do j = 1, n_dim
               call rebin(1d0/NDMX, NDMX, rweight, divisions(:, j))
            enddo
         else
            call read_grid_up(n_dim, divisions, grid_filename)
         endif

         !>
         !> Start integration loop
         !>
         do k = 1, n_iter
            ev_counter = 0
            close(log_unit) 
            open(unit = log_unit, file=trim(log_filename), position="Append", action="write")
            do i = 1, 2
               write(units(i),'(A,I0)') "Commencing iteration n ", k
               write(units(i),'(A,I0)') "Number of events: ", n_events
            enddo

            if(socketed_warmup) then
               print *, "Sockets are active"
               !> Rewind the random number sequence
               call roll_random(1, n_events_initial - 1, n_dim)
               if (k > 1) then
                  call roll_random(n_events_final + 1, n_events, n_dim)
               endif
            endif

            !$omp parallel private(tmp,tmp2,xwgt,wgt,x,div_index) 

            !$omp do schedule(dynamic)
            do i = n_events_initial, n_events_final

               !>
               !> Generate a random vector of n_dim
               !> For reproducibility of results, this has to be omp critical
               !> 
               !$omp critical
               call generate_random_array(n_dim, NDMX, divisions, div_index, x, wgt)
               !$omp end critical
               xwgt = xjac * wgt

               !>
               !> Call integrand
               !> 
               if (present(sigR)) then
                  tmp = xwgt*f_integrand(x, 0d0, xwgt, sigR, sigS, sigV, sigT, sigVV, sigU)
               else
                  tmp = xwgt*f_integrand(x, n_dim)
               endif
               !> 
               !> For each event we need to store both f and f^2
               !> since the variance of a MC integation is S = (\int f^2) - (\int f)^2
               !>
               tmp2 = tmp**2

               !$omp critical 
               !>
               !> We do omp critical instead of "reduction(+:grid_data)" because
               !> 1) Kahan summation needs information on previous call
               !> 2) we want to use the pointers for clarity
               !> Todo: benchmark how much faster would reduction be
               !> 
               if (kahan) then
                  err_r = err_r + error_sum(res, tmp)
                  err_r2 = err_r2 + error_sum(res2, tmp2)
               endif
               res = res + tmp
               res2 = res2 + tmp2
               !>
               !> We also need to store the value of the integral for each subdivision of the 
               !> integration region
               !> (non-squared quantities not needed for importance sampling)
               !>
               if (warmup_flag) then
                  do j = 1, n_dim
                     ind = div_index(j) 
                     if (kahan) then
!                          ar_err(ind, j) = ar_err(ind, j) + error_sum(ar_res(ind,j), tmp)
                        ar_err2(ind, j) = ar_err2(ind, j) + error_sum(ar_res2(ind,j), tmp2)
                     endif
!                       ar_res(ind, j) = ar_res(ind, j) + tmp 
                     ar_res2(ind, j) = ar_res2(ind, j) + tmp2
                  enddo
               endif
               ev_counter = ev_counter + 1
               if ((mod(ev_counter, n_check) == 0).and.(verbose_progress)) then
                  write(6,'(A,I0,A)') "  > > Current progress: ", floor((1.0*ev_counter)/n_check*10), "%"
                  flush(6)
               endif
               !$omp end critical
            enddo
            !$omp enddo

            if (kahan) then
               !$omp single
               res = res + err_r
               res2 = res2 + err_r2
               !$omp end single
               !$omp do 
               do j = 1, n_dim
!                    ar_res(:,j) = ar_res(:,j) + ar_err(:,j)
                  ar_res2(:,j) = ar_res2(:,j) + ar_err2(:,j)
               enddo
               !$omp enddo
            endif

#ifdef USE_SOCKETS
            !>
            !> If we are using sockets, send the value of the integral
            !> for each subdivision to the server and wait for a response
            !> todo: let another thread roll the random numbers while we wait
            !>
            if (socketed_warmup) then
               !$omp single
               write(*,'(A,A,A,I0)')"Communicating with server at ", hostname, ":", port
               call socket_exchange(grid_data, size(grid_data)*dp, hostname, port, ifail)
               if (ifail == 0) then
                  print *, "Success communicating with server"
               else
                  print *, "Server communication failed"
               endif
               !$omp end single
               !$omp barrier
            endif
#endif

            !> Refine the grid for the next iteration
            if(warmup_flag) then
               !$omp do
               do j = 1, n_dim
                  call refine_grid(NDMX, ar_res2(:,j), divisions(:,j))
               enddo
               !$omp end do
            endif
  

            !$omp end parallel
            !>
            !> Treat the final results
            !> Compute the error
            !> S^2 =  (<f^2/p> - <f>^2)/N (with <g> = \int g (pdp))
            !> 
            tmp = dsqrt(n_events*res2) 
            err_tmp2 = (tmp-res)*(tmp+res)/(n_events-1d0)
            if (err_tmp2 < 0d0) then
               err_tmp = 1d-30
            else
               err_tmp = dsqrt(err_tmp2)
            endif

            !>
            !> Save the results (globally accessible for this module)
            !> print them to the log file and std output
            !> And retrieve the current I and error
            !> 
            resultados(k)%sigma = err_tmp
            resultados(k)%weight = 1d0/err_tmp2
            resultados(k)%integral = res
            call print_final_results(k, final_result, sigma, chi2, log_unit)

            if ((warmup_flag).and.(writegrid)) then
               call write_grid_down(n_dim, divisions, grid_filename)
               write(log_unit, *) "Writing grid to " // grid_filename
            endif


            !> Resets grid_data after every iteration
            grid_data(:,:,:) = 0d0

            call flush(6)

         enddo

         ! Clean before exit
         deallocate(resultados)
         close(log_unit)

      end subroutine vegas

      subroutine vegasnr_new(region, ndim, fxn, init, ncall, itmx, nprn, tgral, sd, &
            chi2a, sigR, sigS, sigV, sigT, sigVV, sigU)
         real(dp), dimension(2*MXDIM), intent(in) :: region
         integer, intent(in) :: ndim, init, ncall, itmx, nprn
         real(dp), intent(out) :: tgral, sd, chi2a
         real(dp), external :: fxn
         real(dp), external :: sigR, sigS, sigV, sigT, sigVV, sigU
         call vegas(fxn, ndim, itmx, ncall, tgral, sd, chi2a, sigR, sigS, sigV, sigT, sigVV, sigU)

      end subroutine vegasnr_new

      subroutine print_final_results(k_iter, final_result, sigma, chi2, log_unit)
         integer, intent(in) :: k_iter
         real(dp), intent(out) :: final_result, sigma, chi2
         integer, intent(in) :: log_unit
         real(dp) :: weight_sum, aux_result, chi2_sum
         integer :: i
         integer, dimension(2) :: units
         units = (/6, log_unit/)

         !>
         !> Weighted average of final results
         !> weight_i = w_i = 1/sigma^2
         !> final_result = (\sum res_i * w_i) / (\sum w_i)
         !> Returns final_result, std error and chi2 just in case
         !> 

         weight_sum   = sum(resultados(:)%weight)
         aux_result   = sum(resultados(:)%integral*resultados(:)%weight)
         final_result = aux_result/weight_sum
         sigma        = dsqrt(1d0/weight_sum)
         chi2_sum     = sum(resultados(:)%integral**2*resultados(:)%weight)
         chi2         = max(0d0, (chi2_sum - final_result*aux_result)/(k_iter - 0.99d0))

         !> 
         !> Prints to stdout and a .log file
         !>
         do i = 1, 2
            write(units(i),201) k_iter, resultados(k_iter)%integral, resultados(k_iter)%sigma, final_result, sigma, chi2
      201 format(/&
         & 'Result for iteration number ',I0,':',/, &
         & ' > > > I = ', g14.8,' +/- ', g14.8,/, &
         & ' > Total result: ', g14.8,' +/- ', g14.8,/, &
         & ' > chi2/n-1: ', g10.4/)
         enddo

      end subroutine print_final_results

      subroutine generate_random_array(n_dim, n_divisions, divisions, div_index, x, wgt)
         !>
         !> Input: n of dimensions and n of subdivisions we have
         !>        and the limits of the subdivisions of the vegas grid in each dimension (divisions)
         !>
         !> Output: x(n_dim) = array of random numbers
         !>         div_index(n_dim) = a way of labeling in which subdivisions/division each number 
         !>         wgt associated to this random vector
         !>
         integer, intent(in) :: n_dim, n_divisions
         real(dp), intent(in), dimension(n_divisions, n_dim) :: divisions
         integer, intent(out), dimension(n_dim) :: div_index
         real(dp), intent(out), dimension(n_dim) :: x
         real(dp), intent(out) :: wgt
         integer, parameter :: kg = 1 ! Using this a parameter at the moment to make the notation compatible with vegas
         integer :: j, int_xn
         real(dp) :: rn, aux_rand, x_n, rand_x, xdelta, x_ini

         !>
         !> NOTE: we assume here the region of integration to be 0 to 1 in all dimensions
         !> 
         real(dp), parameter :: reg_i = 0d0, reg_f = 1d0

         wgt = 1d0

         do j = 1, n_dim
            !>
            !> Get a random number randomly asigned to one
            !> of the subdivisions
            !> 
            rn = internal_rand()
            x_n = 1d0 + n_divisions*(dble(kg) - rn)
            int_xn = max(1, min(int(x_n), n_divisions)) ! In practice int_xn = int(x_n) unless x_n < 1
            aux_rand = x_n - int_xn ! which is not the same as fraction(x_n), aux_rand can go negative if int_xn = 1
            if (int_xn == 1) then
               x_ini = 0d0
            else
               x_ini = divisions(int_xn - 1, j)
            endif
            xdelta = divisions(int_xn, j) - x_ini
            !>
            !> Get the random number from within the subdivision
            !> 
            rand_x = x_ini + xdelta*aux_rand
            x(j) = reg_i + rand_x*(reg_f - reg_i) ! x(i) = rand_x
            wgt = wgt * xdelta * n_divisions
            div_index(j) = int_xn
         enddo

      end subroutine generate_random_array

      subroutine refine_grid(n_divisions, div_sq, divisions)
         integer, intent(in) :: n_divisions
         real(dp), dimension(n_divisions), intent(in) :: div_sq
         real(dp), dimension(n_divisions), intent(inout) :: divisions
         real(dp), dimension(n_divisions) :: aux, rw
         integer :: i
         real(dp) :: rc, aux_sum
         rc = 0d0
         !>
         !> First we smear out the array div_sq, where we have store
         !> the value of f^2 for each sub_division for each dimension
         !>
         aux(1) = (div_sq(1) + div_sq(2))/2d0
         aux_sum = aux(1)
         do i = 2, n_divisions - 1
            aux(i) = (div_sq(i-1) + div_sq(i) + div_sq(i+1))/3d0
            aux_sum = aux_sum + aux(i)
         enddo
         aux(n_divisions) = (div_sq(n_divisions-1) + div_sq(n_divisions))/2d0
         aux_sum = aux_sum + aux(n_divisions)

         !>
         !> Now we refine the grid according to 
         !> journal of comp phys, 27, 192-203 (1978) G.P. Lepage
         !>
         do i = 1, n_divisions
            if (aux(i) < 1d-30) then
               aux(i) = 1d-30
            endif
            rw(i) = ( (1d0 - aux(i)/aux_sum)/(dlog(aux_sum) - dlog(aux(i))) )**ALPHA
            rc = rc + rw(i)
         enddo
         call rebin(rc/n_divisions, n_divisions, rw, divisions)

      end subroutine refine_grid

      subroutine rebin(rc, n_divisions, rw, subdivisions)
         real(dp), intent(in) :: rc
         integer, intent(in) :: n_divisions
         real(dp), intent(in), dimension(n_divisions) :: rw
         real(dp), intent(inout), dimension(n_divisions) :: subdivisions

         integer :: i, k
         real(dp) :: dr, old_xf, old_xi
         real(dp), dimension(n_divisions) :: aux
         !>
         !> Reweight the integration subdivisions according to
         !> the vector rw.
         !> This functon should be called by every dimension at the end
         !> of each warmup iteraton
         !> 
         k = 0
         dr = 0
         do i = 1, n_divisions-1
            do while (rc > dr) 
               k = k + 1
               dr = dr + rw(k)
            enddo

            if (k > 1) then
               old_xi = subdivisions(k-1)
            else
               old_xi = 0d0
            endif

            old_xf = subdivisions(k)
            dr = dr - rc
            aux(i) = old_xf - (old_xf - old_xi)*(dr / rw(k))
         enddo

         subdivisions(1:n_divisions-1) = aux(1:n_divisions-1)
         subdivisions(n_divisions) = 1d0
      end subroutine rebin

      subroutine write_grid_down(n_dim, divisions, gridfile)
         integer, intent(in) :: n_dim
         real(dp), dimension(NDMX, n_dim), intent(in) :: divisions
         character(len=128), intent(in) :: gridfile
         integer :: i, j
         !> 
         !> Writes down the Vegas adapted grid as hexadecimal number to a
         !> file. This is compatible with other version of Vegas used in
         !> HEP applications
         !> 
         write(*,*) "Writing grid to ", trim(gridfile)
         open(unit = 11, file = trim(gridfile), status = 'unknown')
         do j = 1, n_dim
            write(11,grid_fmt) j, (divisions(i,j), i = 1, NDMX)
         enddo
         close(11)
      end subroutine write_grid_down

      subroutine read_grid_up(n_dim, divisions, gridfile)
         integer, intent(in) :: n_dim
         real(dp), dimension(NDMX, n_dim), intent(out) :: divisions
         character(len=128), intent(in) :: gridfile
         integer :: i, j, old_j
         !>
         !> Read the Vegas adapted grid in hexadecimal from a file
         !> This is compatible with other version of Vegas used in
         !> HEP applications
         !>
         write(*,*) "Reading grid from ", trim(gridfile)
         open(unit = 11, file = trim(gridfile), status = 'unknown')
         do j = 1, n_dim
            read(11,grid_fmt) old_j, (divisions(i,j), i = 1, NDMX)
         enddo
         close(11)
      end subroutine read_grid_up
      
      pure real(dp) function error_sum(a, b)
         real(dp), intent(in) :: a,b
         real(dp) :: s, ap, bp, da, db
         ! lo digits can be lost
         s = a + b
         ! a' and b' are the actual value we are summing when
         ! rounding to real(dp)
         ap = s - b
         bp = (s - ap)
         ! da db are the difference between the actual a,b
         ! and the a and b which are actually being used
         da = a - ap
         db = b - bp
         error_sum = da + db
      end function error_sum

      subroutine seed_rand(seed)
         integer, intent(in) :: seed
         !>
         !> Wrapper for srand
         !>
      end subroutine seed_rand

      real(dp) function internal_rand()
         !>
         !> Wrapper for the generation of random variables
         !>
         internal_rand = rand()
      end function internal_rand

      subroutine roll_random(ini, fin, n_dim)
         integer, intent(in) :: ini, fin, n_dim
         integer :: i, j
         real(dp) :: tmp
         !>
         !> Wrapper to roll the random number generator when using sockets
         !> in order to ensure all sockets are synchronised 
         !> 
         do j = 1, n_dim
            do i = ini, fin
               tmp = internal_rand()
            enddo
         enddo
      end subroutine roll_random

end module vegas_mod

module vegas_mod
#ifdef USE_NNLOJET
   use pmap, only: clear_pstore
#endif
!$ use omp_lib
   implicit none
   private

   public :: vegas, vegasnr_new, activate_parallel_sockets

   ! Parameters
   integer, parameter :: dp = kind(1.d0)
   integer, parameter :: NDMX = 100
   integer, parameter :: EXTERNAL_FUNCTIONS = 6
   integer, parameter :: MXDIM = 26
   real(dp), parameter :: ALPHA = 1.5d0
   real(dp), parameter :: TINY = 1d-10
   ! Write.read the grid using hexadecimal because that's what the old version uses
   character(len=9), parameter :: grid_fmt = "(/(5z16))"
!     logical :: stratified_sampling = .false., &
!             importance_sampling = .true.

   type resultado
      real(dp) :: sigma
      real(dp) :: weight
      real(dp) :: integral
      real(dp) :: chi2
   end type resultado
   type(resultado), allocatable, dimension(:) :: resultados

   ! Socket data
   character(len=:), allocatable :: hostname
   integer :: port, ifail

   logical :: parallel_warmup = .false.
   logical :: warmup_flag = .true. 
   integer :: n_events_initial, n_events_final
   integer :: n_sockets, socket_number

   contains
      subroutine activate_parallel_sockets(n_sockets_in, socket_number_in, hostname_in, port_in)
         !>
         !> Store the total number of sockets
         !> And the socket number of that corresponds to this instance of Vegas
         !> 
         integer, intent(in) :: n_sockets_in, socket_number_in
         integer, intent(in), optional :: port_in
         character(len=*), intent(in), optional :: hostname_in
         n_sockets = n_sockets_in
         socket_number = socket_number_in
         if (n_sockets > 1) then
            parallel_warmup = .true.
         else
            parallel_warmup = .false.
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

      subroutine vegas(f_integrand, n_dim, n_iter, n_events, final_result, sigma, chi2, &
           sigR, sigS, sigV, sigT, sigVV, sigU )
         
         real(dp), external :: f_integrand
         integer, intent(in) :: n_dim, n_events, n_iter
         real(dp), intent(out) :: final_result, sigma, chi2
         real(dp), external, optional :: sigR, sigS, sigV, sigT, sigVV, sigU

         integer :: i, j, k, n_events_per_instance
         real(dp) :: tmp, error_tmp, xjac, wgt, xwgt
         integer, dimension(n_dim) :: div_index
         real(dp), dimension(n_dim) :: x
         real(dp), dimension(NDMX) :: rweight
         real(dp), dimension(NDMX, n_dim) :: divisions
         real(dp), dimension(3, NDMX, n_dim), target :: grid_data
         real(dp), pointer :: res, res_sq
         real(dp), dimension(:,:), pointer :: div_res, div_res_sq
         character(len=128) :: grid_filename = "grid_test.grid"

#ifdef USE_NNLOJET
         real(dp) :: amz, zewfac, zewnull
         real(dp) :: amzW, stw, ewfac
         real(dp) :: rma2, rmb2, rm2, shat
         integer :: veg_iave, veg_it
         real(dp) :: veg_wgt, veg_swgt
         integer :: iproc
         character(len=128) gridfile
         logical :: bin
         real(dp) :: dv2g
         integer :: npg
         common /vegasiterweight/veg_wgt,veg_iave,veg_it,veg_swgt
         common /bin/bin
         common /vegasnumcalls/dv2g,npg
         common /eweakZ/amz,zewfac(4),zewnull(4)
         !$omp threadprivate(/eweakZ/)
         common /eweakW/amzW,stw,ewfac
         !$omp threadprivate(/eweakW/)
         common /pmasses/rma2,rmb2,rm2(1:7),shat
         !$omp threadprivate(/pmasses/)
         common /currentprocess/iproc
         !$omp threadprivate(/currentprocess/)
         common /gridfilename/gridfile
         grid_filename = gridfile
         bin = .false.
#endif 

         grid_data(:,:,:) = 0d0
         div_res => grid_data(1,:,:)
         div_res_sq => grid_data(2,:,:)
         res => grid_data(3,1,1)
         res_sq => grid_data(3,2,2)

         print *, "Entering New Vegas"

         allocate(resultados(n_iter))

         ! Initialise variables
         divisions(:,:) = 0d0
         divisions(1,:) = 1d0
         rweight(:) = 1d0
         call srand(1)

         if (parallel_warmup) then
            n_events_per_instance = n_events/n_sockets
            n_events_initial = 1 + n_events_per_instance*(socket_number-1)
            n_events_final = socket_number*n_events_per_instance
            write(*,'(A,I0,A,I0)') " > > > Vegas instance No: ", socket_number, " of ", n_sockets
            write(*,'(A,I0,A)') " > > > Running for ", n_events_per_instance, "events"
            write(*,'(A,I0)') " > > > From: ", n_events_initial
            write(*,'(A,I0)') " > > > To: ", n_events_final
         else
            n_events_initial = 1
            n_events_final = n_events
         endif

         !$ print *, " $ OMP active"
         !$ print *, " $ Maximum number of threads: ", OMP_get_num_procs()
         !$ print *, " $ Number of threads selected: ", OMP_get_max_threads()

         !>
         !> Initial rebining
         !> either all subdivisions equal or we read an old grid
         !>
         if(warmup_flag) then
            do j = 1, n_dim
               call rebin(1d0/NDMX, NDMX, rweight, divisions(:, j))
            enddo
         else
            call read_grid_up(n_dim, divisions, grid_filename)
         endif

         xjac = 1d0/n_events
         do k = 1, n_iter
            write(*,'(A,I0)') "Commencing iteration n ", k
            grid_data(:,:,:) = 0d0

#ifdef USE_SOCKETS
            if(parallel_warmup) then
               print *, "Sockets are active"
               !>
               !> Rewind the random number sequence
               !> 
               call roll_random(1, n_events_initial - 1)
               if (k > 1) then
                  call roll_random(n_events_final + 1, n_events)
               endif
            endif
#endif

#ifdef USE_NNLOJET
            !$omp parallel default(private) shared(divisions, grid_data) &
            !$omp& shared(n_dim, n_events_initial, n_events_final, xjac, warmup_flag) &
            !$omp& shared(parallel_warmup, n_sockets) &
            !$omp& shared(res, res_sq, div_res, div_res_sq) &
            !$omp& copyin(/eweakZ/,/eweakW/,/pmasses/,/currentprocess/)
            call init_parallel()
            !$omp single
            print *, "NNLOJET initilisation done"
            !$omp end single
#else
            !$omp parallel private(xwgt,wgt,x,div_index,tmp) shared(divisions, grid_data)
#endif

            !$omp do schedule(dynamic) reduction(+:grid_data)
            do i = n_events_initial, n_events_final
               !>
               !> Generate a random vector of n_dim
               !> TODO: It needs to be omp critical otherwise the program slows down in kernel calls at this point
               !> maybe rand() is at fault? investigate
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
               res = res + tmp
               res_sq = res_sq + tmp**2
               !>
               !> We also need to store the value of the integral for each subdivision of the 
               !> integration region
               !>
               if (warmup_flag) then
                  do j = 1, n_dim
!                       div_res(div_index(j), j) = div_res(div_index(j),j) + tmp
                     div_res_sq(div_index(j), j) = div_res_sq(div_index(j), j) + tmp**2
                  enddo
               endif
            enddo
            !$omp end do

            !>
            !> In principle, "end do" implies synchronisation, however 
            !> during debugging it was not clear that all threads were 
            !> finished with the loop at this point
            !>
            !$omp barrier
#ifdef USE_SOCKETS
            !$omp master
            !>
            !> If we are using sockets, send the value of the integral
            !> for each subdivision to the server and wait for a response
            !>
            if (parallel_warmup) then
               write(*,'(A,A,A,I0)')"Communicating with server at ", hostname, ":", port
               call socket_exchange(grid_data, size(grid_data)*dp, hostname, port, ifail)
               if (ifail == 0) then
                  print *, "Success communicating with server"
               else
                  print *, "Server communication failed"
               endif
            endif
            !$omp end master
            !$omp barrier
#endif

            !>
            !> And refine the grid for the next iteration
            !> 
            if(warmup_flag) then
               !$omp do
               do j = 1, n_dim
                  call refine_grid(NDMX, div_res_sq(:,j), divisions(:,j))
               enddo
               !$omp end do
            endif
  
#ifdef USE_NNLOJET
            call destroy_parallel()
            call clear_pstore()
            !$omp single
            print *, "NNLOJET finalisation done"
            !$omp end single
#endif

            !$omp end parallel
            !>
            !> Treat the final results
            !> Compute the error
            !> S^2 =  (<f^2/p> - <f>^2)/N (with <g> = \int g (pdp))
            !> 
            error_tmp = (n_events*res_sq - res**2)/n_events
            if (error_tmp < 0d0) then
               error_tmp = 1d-10
            else
               error_tmp = dsqrt(error_tmp)
            endif
            !>
            !> Save the results to the resultados(:) array
            !> 
            resultados(k)%sigma = error_tmp
            resultados(k)%weight = 1d0/error_tmp**2
            resultados(k)%integral = res
            write(*,200) k, res, error_tmp

            call get_final_results(k, final_result, sigma, chi2)
            write(*,201) final_result, sigma, chi2

            if (warmup_flag) then
               call write_grid_down(n_dim, divisions, grid_filename)
            endif

#ifdef USE_NNLOJET
            if(.not.warmup_flag) then
               veg_it = k
               veg_wgt = res
               veg_swgt = final_result
               npg = n_events ! Since we are not doing stratified sampling npg=n_events
               call bino(2,0d0,0)
            endif
#endif 


         enddo

         ! Clean before exit
         deallocate(resultados)

         ! Formateo del output
      200   FORMAT('Iteration no: ', I5, ' Integral = ', g14.7, ' +/- ', g9.2)
      201   FORMAT('     > > > > Total result: ', g14.7, ' +/- ', g9.2, ' chi2: ', g9.2)

      end subroutine vegas

      subroutine vegasnr_new(region, ndim, fxn, init, ncall, itmx, nprn, tgral, sd, &
            chi2a, sigR, sigS, sigV, sigT, sigVV, sigU)
         real(dp), dimension(2*MXDIM), intent(in) :: region
         integer, intent(in) :: ndim, init, ncall, itmx, nprn
         real(dp), intent(out) :: tgral, sd, chi2a
         real(dp), external :: fxn
         real(dp), external :: sigR, sigS, sigV, sigT, sigVV, sigU
         !>
         !> Wrapper for programs that call the old version of vegas
         !> It uses the same argument names as the old version
         !>
         print *, "Entering legacy wrapper for New Vegas!"
         print *, "init, nprn, region(1): ", nprn, region(1)

         select case(init)
         case(0)
            warmup_flag = .true.
         case(1)
            warmup_flag = .false.
         end select
            
         call vegas(fxn, ndim, itmx, ncall, tgral, sd, chi2a, sigR, sigS, sigV, sigT, sigVV, sigU)

      end subroutine vegasnr_new

      subroutine get_final_results(k_iter, final_result, sigma, chi2)
         integer, intent(in) :: k_iter
         real(dp), intent(out) :: final_result, sigma, chi2
         real(dp) :: weight_sum, aux_result, chi2_sum

         !>
         !> Weighted average of final results
         !> weight_i = w_i = 1/sigma^2
         !> final_result = (\sum res_i * w_i) / (\sum w_i)
         !> 

         weight_sum   = sum(resultados(:)%weight)
         aux_result   = sum(resultados(:)%integral*resultados(:)%weight)
         final_result = aux_result/weight_sum
         sigma        = dsqrt(1d0/weight_sum)
         chi2_sum     = sum(resultados(:)%integral**2*resultados(:)%weight)
         chi2         = max(0d0, (chi2_sum - final_result*aux_result)/(k_iter - 0.99d0))

      end subroutine get_final_results

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
            if (aux(i) < TINY) then
               aux(i) = TINY
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
         subdivisions(n_divisions) = 1d0-1d-10
      end subroutine rebin

      subroutine write_grid_down(n_dim, divisions, gridfile)
         integer, intent(in) :: n_dim
         real(dp), dimension(NDMX, n_dim), intent(in) :: divisions
         character(len=128), intent(in) :: gridfile
         integer :: i, j
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
         write(*,*) "Reading grid from ", trim(gridfile)
         open(unit = 11, file = trim(gridfile), status = 'unknown')
         do j = 1, n_dim
            read(11,grid_fmt) old_j, (divisions(i,j), i = 1, NDMX)
         enddo
         close(11)
      end subroutine read_grid_up


      real(dp) function internal_rand()
         internal_rand = rand()
      end function

#ifdef USE_SOCKETS
      subroutine roll_random(ini, fin)
         integer, intent(in) :: ini, fin
         integer :: i
         real(dp) :: tmp
         do i = ini, fin
            tmp = internal_rand()
         enddo
      end subroutine roll_random
#endif

end module vegas_mod

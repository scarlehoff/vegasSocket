module vegas_mod
#ifdef USE_NNLOJET
   use pmap, only: clear_pstore
#endif
!$ use omp_lib
   implicit none
   private

   public :: vegas, vegasnr, activate_parallel_sockets

   integer, parameter :: dp = kind(1.d0)
#ifdef USE_SOCKETS
   character(len=9) :: hostname = "localhost"
   integer :: port = 8888
   integer :: ifail = 0
#endif

   ! Parameters
   integer, parameter :: NDMX = 10
   integer, parameter :: EXTERNAL_FUNCTIONS = 6
   integer, parameter :: MXDIM = 26
   real(dp), parameter :: ALPHA = 1.5d0
   real(dp), parameter :: TINY = 1d-10
!     logical :: stratified_sampling = .false., &
!              importance_sampling = .true.

   type resultado
      real(dp) :: sigma
      real(dp) :: weight
      real(dp) :: integral
      real(dp) :: chi2
   end type resultado
   type(resultado), allocatable, dimension(:) :: resultados

   logical :: parallel_warmup = .false.
   integer :: n_events_initial, n_events_final
   integer :: n_sockets, socket_number
   
   contains
      subroutine activate_parallel_sockets(n_sockets_in, socket_number_in)
         !>
         !> Store the total number of sockets
         !> And the socket number of that corresponds to this instance of Vegas
         !> 
         integer, intent(in) :: n_sockets_in, socket_number_in
         n_sockets = n_sockets_in
         socket_number = socket_number_in
         if (n_sockets > 1) then
            parallel_warmup = .true.
         else
            parallel_warmup = .false.
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
            print *, " > > > Vegas instance No: ", socket_number, " of ", n_sockets
            print *, " > > > Running for ", n_events_per_instance, "events"
            print *, " > > > From: ", n_events_initial
            print *, " > > > To: ", n_events_final
         else
            n_events_initial = 1
            n_events_final = n_events
         endif

         !$ print *, " $ OMP active"
         !$ print *, " $ Maximum number of threads: ", OMP_get_num_procs()
         !$ print *, " $ Number of threads selected: ", OMP_get_max_threads()

         !>
         !> Initial rebining (ie, all subdivisions are equal)
         !>
         do j = 1, n_dim
            call rebin(1d0/NDMX, NDMX, rweight, divisions(:, j))
         enddo

         do k = 1, n_iter
            write(*,'(A,I0)') "Commencing iteration n ", k
            grid_data(:,:,:) = 0d0
            xjac = 1d0/n_events

            !$omp parallel private(xwgt,wgt,x,div_index,tmp) shared(divisions, grid_data)
#ifdef USE_NNLOJET
            call init_parallel()
            print *, "NNLOJET initilisation done"
#endif
            !$omp do
            do i = 1, n_events
               !>
               !> Generate a random vector of n_dim
               !> TODO: It needs to be omp critical otherwise the program slows down in kernel calls at this point
               !> maybe rand() is at fault? investigate
               !>
               !$omp critical
               call generate_random_array(n_dim, NDMX, divisions, div_index, x, wgt)
               !$omp end critical
               xwgt = xjac * wgt
               if ((i < n_events_initial).or.(i > n_events_final)) then
                  cycle
               endif

               !>
               !> Call integrand
               !> 
               if (present(sigR)) then
                  tmp = xwgt*f_integrand(x, 0d0, n_dim, sigR, sigS, sigV, sigT, sigVV, sigU)
               else
                  tmp = xwgt*f_integrand(x, n_dim)
               endif

               !> 
               !> For each event we need to store both f and f^2
               !> since the variance of a MC integation is S = (\int f^2) - (\int f)^2
               !>
               !$omp critical
               res = res + tmp
               res_sq = res_sq + tmp**2
               !>
               !> We also need to store the value of the integral for each subdivision of the 
               !> integration region
               !>
               do j = 1, n_dim
!                    div_res(div_index(j), j) = div_res(div_index(j),j) + tmp
                  div_res_sq(div_index(j), j) = div_res_sq(div_index(j), j) + tmp**2
               enddo
               !$omp end critical
            enddo
            !$omp end do

#ifdef USE_SOCKETS
            !$omp master
            !>
            !> If we are using sockets, send the value of the integral
            !> for each subdivision to the server and wait for a response
            !>
            if (parallel_warmup) then
               print *, "Communicating with server"
               print *, "Partial total result: ", res*n_sockets
               call socket_exchange(grid_data, size(grid_data)*dp, hostname, port, ifail)
               if (ifail == 0) then
                  print *, "Success communicating with server"
               else
                  print *, "Server communication failed"
               endif
            endif
            !$omp end master
#endif


            !>
            !> And refine the grid for the next iteration
            !> 
            !$omp do
            do j = 1, n_dim
               call refine_grid(NDMX, div_res_sq(:,j), divisions(:,j))
            enddo
            !$omp end do

            !$omp end parallel
            !>
            !> Treat the final results
            !> Compute the error
            !> S =  (\int f^2/p) - (\int f)^2, where p, in first approximation = 1/n_events
            !> 
            error_tmp = n_events*res_sq - res**2
            if (error_tmp < 0d0) then
               error_tmp = 1d-10
            else
               error_tmp = error_tmp/dsqrt(n_events-1d0)
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

#ifdef USE_NNLOJET
            call destroy_parallel()
            call clear_pstore()
            print *, "NNLOJET finalisation done"
#endif
         enddo

         ! Clean before exit
         deallocate(resultados)

         ! Formateo del output
      200   FORMAT('Iteration no: ', I5, ' Integral = ', g14.7, ' +/- ', g9.2)
      201   FORMAT('     > > > > Total result: ', g14.7, ' +/- ', g9.2, ' chi2: ', g9.2)

      end subroutine vegas

      subroutine vegasnr(region, ndim, fxn, init, ncall, itmx, nprn, tgral, sd, &
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
         print *, "init, nprn, region(1): ", init, nprn, region(1)

         call vegas(fxn, ndim, itmx, ncall, tgral, sd, chi2a, sigR, sigS, sigV, sigT, sigVV, sigU)

      end subroutine vegasnr

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
         integer :: i, int_xn
         real(dp) :: rn, aux_rand, x_n, rand_x, xdelta, x_ini

         !>
         !> NOTE: we assume here the region of integration to be 0 to 1 in all dimensions
         !> 
         real(dp), parameter :: reg_i = 0d0, reg_f = 1d0

         wgt = 1d0

         do i = 1, n_dim
            !>
            !> Get a random number randomly asigned to one
            !> of the subdivisions
            !> 
            rn = rand()
            x_n = 1d0 + n_divisions*(dble(kg) - rn)
            int_xn = max(1, min(int(x_n), n_divisions)) ! In practice int_xn = int(x_n) unless x_n < 1
            aux_rand = x_n - int_xn ! which is not the same as fraction(x_n), aux_rand can go negative if int_xn = 1
            if (int_xn == 1) then
               x_ini = 0d0
            else
               x_ini = divisions(int_xn - 1, i)
            endif
            xdelta = divisions(int_xn, i) - x_ini
            !>
            !> Get the random number from within the subdivision
            !> 
            rand_x = x_ini + xdelta*aux_rand
            x(i) = reg_i + rand_x*(reg_f - reg_i) ! x(i) = rand_x
            wgt = wgt * xdelta * n_divisions
            div_index(i) = int_xn
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
         subdivisions(n_divisions) = 1d0

      end subroutine rebin

end module vegas_mod

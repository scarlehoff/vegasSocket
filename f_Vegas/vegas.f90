module vegas_mod
   implicit none
   private

   public :: vegas

   integer, parameter :: dp = kind(1.d0)

   ! Parameters
   integer, parameter :: NDMX = 10
   real(dp), parameter :: ALPHA = 1.5d0
   real(dp), parameter :: TINY = 1d-10
!     logical :: stratified_sampling = .false., &
!              importance_sampling = .true.

   type resultado
      real(dp) :: sigma
      real(dp) :: weight
      real(dp) :: integral
   end type resultado

   type(resultado), allocatable, dimension(:) :: resultados

   contains

      subroutine vegas(f_integrand, n_dim, n_iter, n_events, final_result, sigma)
         real(dp), external :: f_integrand
         integer, intent(in) :: n_dim, n_events, n_iter
         real(dp), intent(out) :: final_result, sigma

         integer :: i, j, k
         real(dp) :: tmp, error_tmp, res, res_sq, xjac, wgt, xwgt
         integer, dimension(n_dim) :: div_index
         real(dp), dimension(n_dim) :: x
         real(dp), dimension(NDMX) :: rweight
         real(dp), dimension(NDMX, n_dim) :: divisions, div_res, div_res_sq

         allocate(resultados(n_iter))

         ! Init < 1

         ! Init < 2
         divisions(:,:) = 0d0
         divisions(1,:) = 1d0
         rweight(:) = 1d0

         ! Initialise variables
         
         ! Init < 3

         !>
         !> Initial rebining (ie, all subdivisions are equal)
         !>
         do j = 1, n_dim
            call rebin(1d0/NDMX, NDMX, rweight, divisions(:, j))
         enddo


         do k = 1, n_iter
            xjac = 1d0/n_events
            res = 0d0
            res_sq = 0d0
            div_res(:,:) = 0d0
            div_res_sq(:,:) = 0d0

            do i = 1, n_events
               !>
               !> Generate a random vector of n_dim
               !> 
               call generate_random_array(n_dim, NDMX, divisions, div_index, x, wgt)
               xwgt = xjac * wgt

               !>
               !> Call integrand
               !> 
               tmp = xwgt*f_integrand(x, n_dim)
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
               do j = 1, n_dim
!                    div_res(div_index(j), j) = div_res(div_index(j),j) + tmp
                  div_res_sq(div_index(j), j) = div_res_sq(div_index(j), j) + tmp**2
               enddo
            enddo
            !>
            !> And refine the grid for the next iteration
            !> 
            call refine_grid(n_dim, NDMX, div_res_sq, divisions)

            !>
            !> Treat the final results
            !> Compute the error
            !> S =  (\int f^2/p) - (\int f)^2, where p, in first approximation = 1/n_events
            !> 
            error_tmp = n_events*res_sq - res**2
            if (error_tmp < 0d0) then
               error_tmp = 1d-10
            else
               error_tmp = error_tmp/(n_events-1d0)
            endif
            !>
            !> Save the results to the resultados(:) array
            !> 
            resultados(k)%sigma = error_tmp
            resultados(k)%weight = 1d0/error_tmp**2
            resultados(k)%integral = res
            write(*,200) k, res, error_tmp
            call get_final_results(final_result, sigma)
            write(*,201) final_result, sigma
         enddo

         ! Formateo del output
      200   FORMAT('Iteration no: ', I5, ' Integral = ', g14.7, ' +/- ', g9.2)
      201   FORMAT('     > > > > Total result: ', g14.7, ' +/- ', g9.2)

      end subroutine vegas

      subroutine get_final_results(final_result, sigma)
         real(dp), intent(out) :: final_result, sigma
         real(dp) :: weight_sum

         !>
         !> Weighted average of final results
         !> weight_i = w_i = 1/sigma^2
         !> final_result = (\sum res_i * w_i) / (\sum w_i)
         !> 

         weight_sum   = sum(resultados(:)%weight)
         final_result = sum(resultados(:)%integral*resultados(:)%weight)/weight_sum
         sigma        = dsqrt(1d0/weight_sum)

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
            call random_number(rn)
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

      subroutine refine_grid(n_dim, n_divisions, div_sq, divisions)
         integer, intent(in) :: n_dim, n_divisions
         real(dp), dimension(n_divisions, n_dim), intent(in) :: div_sq
         real(dp), dimension(n_divisions, n_dim), intent(inout) :: divisions
         real(dp), dimension(n_divisions, n_dim) :: aux
         real(dp), dimension(n_dim) :: aux_sum
         real(dp), dimension(n_divisions) :: rw
         integer :: i, j
         real(dp) :: rc

         !>
         !> First we smear out the array div_sq, where we have store
         !> the value of f^2 for each sub_division for each dimension
         !>
         do j = 1, n_dim
            aux(1,j) = (div_sq(1,j) + div_sq(2,j))/2d0
            aux_sum(j) = aux(1,j)
            do i = 2, n_divisions - 1
               aux(i,j) = (div_sq(i-1,j) + div_sq(i,j) + div_sq(i+1,j))/3d0
               aux_sum(j) = aux_sum(j) + aux(i,j)
            enddo
            aux(n_divisions,j) = (div_sq(n_divisions-1,j) + div_sq(n_divisions,j))/2d0
            aux_sum(j) = aux_sum(j) + aux(n_divisions,j)
         enddo

         !>
         !> Now we refine the grid according to 
         !> journal of comp phys, 27, 192-203 (1978) G.P. Lepage
         !>
         do j = 1, n_dim
            rc = 0d0
            do i = 1, n_divisions
               if (aux(i,j) < TINY) then
                  aux(i,j) = TINY
               endif
               rw(i) = ( (1d0 - aux(i,j)/aux_sum(j))/(dlog(aux_sum(j)) - dlog(aux(i,j))) )**ALPHA
               rc = rc + rw(i)
            enddo
            call rebin(rc/n_divisions, n_divisions, rw, divisions(:,j))
         enddo

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
            if (rc > dr) then
               k = k+1
               dr = dr + rw(k)
            endif

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

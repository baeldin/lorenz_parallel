! integrate over dtt using time step dt, then return the result
module mod_interval
 interface
   subroutine lorenz_interval(x,dt,dtt)
     real, dimension(:,:), intent(inout) :: x
     real,                 intent(in)    :: dt, dtt
   end subroutine
 end interface
end module
subroutine lorenz_interval(x,dt,dtt)
use mod_step
  implicit none
  ! parameters
  real, dimension(:,:), intent(inout) :: x
  real, intent(in)                    :: dt, dtt
  ! local variables
  integer :: nsteps, ii
  ! calculate the number of time steps in the interval
  nsteps = int(dtt/dt)
  ! call the forward step repeatedly
  do ii=1,nsteps
    call lorenz_step(x,dt)
  end do
end subroutine

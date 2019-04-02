! integrate one time step of duration dt using a simple Euler forward step
module mod_step
  interface
    subroutine lorenz_step(x,dt)
      real, dimension(:,:), intent(inout) :: x
      real,                 intent(in)    :: dt
    end subroutine
  end interface
end module
subroutine lorenz_step(x,dt)
  implicit none
  ! parameters
  real, dimension(:,:), intent(inout) :: x
  real, intent(in)                    :: dt
  ! local variables
  real                                :: a,b,c
  integer                             :: ii
  ! a, b, c for the classic lorenz attractor
  a=10.
  b=28.
  c=8./3.
  ! loop over the array and move each point one step forward
  do ii=1,size(x,2)
    x(:,ii)=x(:,ii)+dt*(/a*(x(2,ii)-x(1,ii)), &
                         x(1,ii)*(b-x(3,ii)), &
                         x(1,ii)*x(2,ii)-c*x(3,ii)/)
  end do
end subroutine


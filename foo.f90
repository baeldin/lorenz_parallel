program lorenz
  ! use MPI
  use mpi
  implicit none
  ! variables
  integer :: nread
  ! used for MPI
  integer ( kind = 4 ) error
  integer ( kind = 4 ) id
  integer ( kind = 4 ) p
  open(20,file='in.txt',action='read')
  read(20,*) nread
  close(20)
  write(*,*) 'nread=',nread
  ! initialize MPI
  call MPI_Init ( error )
  call MPI_Comm_size ( MPI_COMM_WORLD, p, error )
  call MPI_Comm_rank ( MPI_COMM_WORLD, id, error )
  write(*,*) 'Hello world from process ',id, ', nread=',nread
!  write(*,*) 'Hello world, nread=',nread
  ! finalize MPI
  call MPI_Finalize ( error )
end program

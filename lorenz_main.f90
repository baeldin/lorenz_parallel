! simple parallelized lorenz attractor program using forward integration
!
! written by Phillip Scheffknecht
!
! The program calls lorenz_interval, which moves one point through the attractor
! for time dtt, then returns one value.
!
! lorenz_interval() is called to obtain one output value, it calls lorenz_step()
! repeatedly until the time interval is complete.
!
! The program is parallelized and runs in MPI mode
!
! 2017-02-20
!
! Fixed parallel writing, Phillip Scheffknecht, 2017-03-13
!
program lorenz
  ! use the module for the integration over time intervall dtt
  use mod_interval
  ! use MPI
  use mpi
  implicit none
  ! variables
  real, allocatable    :: x(:,:)        ! array containing the initial values
  real, allocatable    :: x_full(:,:,:) ! the  array for all points at all output times
  real                 :: dt, dtt, dttt ! time step, output interval, integration duration
  integer              :: nints, ii, jj, kk, nmax, ksta, kend ! counters, limits and indices 
  character(len=64)    :: frmt, outfile, filenum ! for creating the output file names
  character(len=32)    :: outfile_prefix
  ! used for MPI
  integer ( kind = 4 ) error
  integer ( kind = 4 ) id
  integer ( kind = 4 ) p
  ! use integer with 4 digits and leading zeros for file names, adjust if more than 9999 points
  ! are desired
  frmt='(i4.4)'
  ! CONFIGURE THE PROGRAM BY READING conf
  ! open config file
  open (19,action='read',file='conf')
  ! how many points should be treated?
  read (19,*) nmax
  if(nmax.gt.9999) then
    write(*,*) 'Current limit is 9999, please adjust format and nlimit if you want more points'
  end if
  !n=150
  allocate(x(3,nmax))
  ! time step, output interval and integration time
  read(19,*) dt, dtt, dttt
  read(19,*) outfile_prefix
  ! get the initial conditions for each point
  do ii=1,nmax
    read(19,*) x(1,ii), x(2,ii), x(3,ii)
    ! open the file for the first time, replace if it already exists
    ! and write the initial conditions into the first line, then close
    write(filenum,frmt) ii
    outfile=trim(outfile_prefix)//trim(filenum)//'.txt'
    open (unit=20,file=outfile,action="write",status="replace")
    write(20,*) x(:,ii)
    close(20)
  end do
  !write(*,*) x
  close(19)
  ! determine the number of output steps
  nints=int(dttt/dtt)
  ! allocate the array and fill the first time step
  allocate(x_full(3,nmax,nints+1))
  x_full(:,:,1)=x(:,:)
  ! 
  ! PARALLEL PART STARTS HERE
  !
  ! initialize MPI
  call MPI_Init ( error )
  call MPI_Comm_size ( MPI_COMM_WORLD, p, error )
  call MPI_Comm_rank ( MPI_COMM_WORLD, id, error )
  ! determine indices based on thread ID, each thread will treat a part of the array
  ksta=id*(nmax/p)+1
  kend=(id+1)*(nmax/p)
  ! start the loop and write the results into files
  write(*,*) 'Process ',id,' calculating indices ',ksta,' to ',kend
  do ii=1,nints
    ! call the actual calculation subroutine
    call lorenz_interval(x(:,ksta:kend),dt,dtt)
    x_full(:,ksta:kend,ii+1)=x(:,ksta:kend)
  end do
  ! loop through the threads, such that each thread can write its part of the array
  do kk=0,p-1
    if(id.eq.kk) then
      do jj=ksta,kend
        write(filenum,frmt) jj
        outfile=trim(outfile_prefix)//trim(filenum)//'.txt'
        open (unit=20,file=outfile,action="write",status="old",position="append")
        write(*,*) 'Writing file ', jj, ' to disk...'
        do ii=1,nints
          write(20,*) x_full(:,jj,ii)
        end do
        close(20)
      end do
      write(*,*) 'Done'
    end if
    ! force all threads to wait here, such that they write their files one by one
    ! instead of writing them at the same time
    call mpi_barrier(MPI_COMM_WORLD,error)
  end do
  ! finalize MPI
  call MPI_Finalize ( error )
  !
  ! PARALLEL PART ENDS HERE
  !
end program

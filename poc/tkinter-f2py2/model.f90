module global_variables
  real, allocatable, dimension(:) :: xvals
  real, allocatable, dimension(:) :: yvals
  public :: xvals, yvals
end module global_variables

module runtime_parameters
  integer :: nx
  integer :: ny
  integer :: koverall
  public :: nx, ny, koverall
end module runtime_parameters

subroutine initialise(filename)

  USE global_variables, ONLY: xvals, yvals
  USE runtime_parameters, ONLY: nx, ny, koverall

 implicit none

  ! dummy vars                                                                          
  character(len=*),intent(in) :: filename
  ! local vars
  integer :: errstat
  integer, parameter :: unit_number = 56

  namelist /runtime_params/ nx, ny, koverall

  write (*,*) 'FORTRAN initialising..'

  ! open the input file containing the data
  open(unit=unit_number,file=filename,status='old',iostat=errstat)
  if (errstat /= 0) then
     print*,'ERROR: could not open namelist file:', filename
     stop
  end if

  ! read data into the declared namelist
  read(UNIT=unit_number,NML=runtime_params,IOSTAT=errstat)
  if (errstat /= 0) then
     print*,'ERROR: could not read namelist'
     stop
  else
     close(unit_number)
  end if

  ! allocate arrays
  allocate(xvals(nx), stat=errstat)
  if (errstat /= 0) then
     print*,'ERROR: could not allocate xvals array'
     stop
  end if

  allocate(yvals(ny), stat=errstat)
  if (errstat /= 0) then
     print*,'ERROR: could not allocate yvals array'
     stop
  end if

  write (*,*) 'done'

end subroutine initialise


subroutine timestep(offset)

  ! Just a simple routine that updates data in the allocated
  ! arrays.  In this case, y=sin(offset + x), where we can vary
  ! the offset so that we can draw a different curve each timestep.

  USE global_variables, ONLY: xvals, yvals
  USE runtime_parameters, ONLY: nx, ny

  ! dummy variables
  real, intent(in) :: offset  ! in radians
  real, parameter :: PI=4.D0*DATAN(1.D0)

  ! local variables
  integer :: ii
  real :: step_size

  write (*,*) 'performing a FORTRAN timestep..'

  step_size = (2 * PI) / real(nx)
  ! set the vals in x and y
  do ii=1,nx
     xvals(ii) = offset + (real(ii) * step_size)
     yvals(ii) = sin(xvals(ii))
  end do

  write (*,*) 'done'

end subroutine timestep


subroutine finalise

  USE global_variables, ONLY: xvals, yvals

  implicit none

  integer :: errstat

  write (*,*) 'FORTRAN finalising..'

  ! Free up the allocated space
  if(allocated(xvals)) then
     deallocate(xvals,stat=errstat)
  end if
  if (errstat /= 0) then
     write (*,*) 'ERROR: could not dellocate xvals array'
     stop
  endif 

  if(allocated(yvals)) then
     deallocate(yvals,stat=errstat)
  end if
  if (errstat /= 0) then
     write (*,*) 'ERROR: could not dellocate yvals array'
     stop
  endif 

  write (*,*) 'done'

end subroutine finalise



subroutine hello_world(mystring)
  implicit none
  !f2py character(len=256),intent(out) :: mystring
  character(len=*),intent(out) :: mystring
  mystring = "hello, from fortran"
end subroutine hello_world

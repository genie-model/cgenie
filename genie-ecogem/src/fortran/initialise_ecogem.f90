
! ******************************************************************************************************************************** !
! SETUP ECOGEM
SUBROUTINE initialise_ecogem( &
     & )
  USE ecogem_lib
  USE ecogem_box
  USE ecogem_data
  USE genie_util, ONLY: check_iostat
  ! dummy arguments
!!!
  ! local variables
!!!

  print*,'======================================================='
  print*,' >>> Initialising ECOGEM ocean biogeochem. module ...'

  ! *** load goin information ***
  call sub_load_goin_ecogem()

  ! *** dimension tracer arrays ***
  ! NOTE: check for problems allocating array space
  ALLOCATE(plankton(n_p,n_i,n_j),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)

  ! *** initialise plankton array
  call sub_init_plankton()

  print*,' <<< Initialisation complete'
  print*,'======================================================='

  return

END SUBROUTINE initialise_ecogem
! ******************************************************************************************************************************** !


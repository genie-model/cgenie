
! ******************************************************************************************************************************** !
! SETUP AtChem
SUBROUTINE initialise_atchem( &
     & dum_sfxsumatm,         &
     & dum_sfcatm             &
     & )
  USE atchem_lib
  USE atchem_data
  use atchem_data_netCDF
  ! dummy arguments
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfcatm    ! atmosphere-surface tracer composition; atm grid

  print*,'======================================================='
  print*,' >>> Initialising ATCHEM atmospheric chem. module ...'
  
  ! *** load goin information ***
  call sub_load_goin_atchem()

  ! *** initialize AtChem ***
  CALL sub_init_phys_atm()
  CALL sub_init_tracer_atm_comp()

  ! *** load restart information ***
  IF (ctrl_continuing) then
     call sub_data_load_rst()
  end if

  ! *** initialize external interface arrays ***
  dum_sfxsumatm(:,:,:) = 0.0
  dum_sfcatm(:,:,:)    = atm(:,:,:)

  ! *** initialize MISC ***
  call sub_init_slabbiosphere()

  print*,' <<< Initialisation complete'
  print*,'======================================================='

end SUBROUTINE initialise_atchem
! ******************************************************************************************************************************** !

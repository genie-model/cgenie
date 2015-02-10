! ******************************************************************************************************************************** !
! ecogem_data.f90
! 
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE ecogem_data

  
  USE ecogem_lib
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD ECOGEM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_ecogem()
    USE genie_util, ONLY: check_unit,check_iostat
!    ! local variables
!    integer::ios                                                        !
!    ! read data_ECOGEM file
!    call check_unit(in,__LINE__,__FILE__)
!    open(unit=in,file='data_ECOGEM',status='old',action='read',iostat=ios)
!    if (ios /= 0) then
!       print*,'ERROR: could not open ECOGEM initialisation namelist file'
!       stop
!    end if
!    ! read in namelist and close data_ECOGEM file
!    read(UNIT=in,NML=ini_ecogem_nml,IOSTAT=ios)
!    if (ios /= 0) then
!       print*,'ERROR: could not read OCNLTIE namelist'
!       stop
!    else
!       close(unit=in)
!    end if
if (ctrl_debug_init > 0) then
    ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
    ! ############################################################################################################################ !
end if
  END SUBROUTINE sub_load_goin_ecogem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALISE PLANKTON
  SUBROUTINE sub_init_plankton()
    ! local variables


    plankton(:,:,:) = 0.0

    plankton(1,8,8) = 1.0



  END SUBROUTINE sub_init_plankton
  ! ****************************************************************************************************************************** !


END MODULE ecogem_data


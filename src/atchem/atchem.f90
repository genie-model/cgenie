MODULE atchem

  USE atchem_lib
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_atchem
  PUBLIC :: step_atchem
  PUBLIC :: end_atchem
  PUBLIC :: atchem_save_rst

CONTAINS

  SUBROUTINE initialise_atchem(dum_sfxsumatm, dum_sfcatm)
    USE atchem_data
    USE atchem_data_netCDF
    IMPLICIT NONE
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcatm    ! atmosphere-surface tracer composition; atm grid

    print*,'======================================================='
    print*,' >>> Initialising ATCHEM atmospheric chem. module ...'

    CALL sub_load_goin_atchem()

    CALL sub_init_phys_atm()
    CALL sub_init_tracer_atm_comp()

    IF (ctrl_continuing) CALL sub_data_load_rst()

    dum_sfxsumatm = 0.0
    dum_sfcatm = atm

    CALL sub_init_slabbiosphere()

    PRINT *, ' <<< Initialisation complete'
    PRINT *, '======================================================='
  END SUBROUTINE initialise_atchem


  SUBROUTINE step_atchem(dum_dts, dum_sfxsumatm, dum_sfcatm)
    USE atchem_box
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_dts
    real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm
    real,dimension(n_atm,n_i,n_j),intent(out)::dum_sfcatm

    integer::ia,l,i,j
    real::loc_dtyr
    REAL::loc_atm_tot_V
    REAL,DIMENSION(n_atm)::loc_atm_tot
    REAL,DIMENSION(n_i,n_j)::loc_conv_atm_mol,loc_conv_mol_atm
    REAL,DIMENSION(n_atm,n_i,n_j)::locij_fatm                  ! local flux to atmosphere (mol)
    REAL,DIMENSION(n_atm)::loc_fracdecay_atm                   ! local reduction factor for decaying atmospheric tracers

    ! *** INITIALIZE LOCAL VARIABLES ***
    locij_fatm = 0.0

    ! *** CALCULATE LOCAL CONSTANTS ***
    ! local constants for converting between partial pressure and molar quantity
    loc_conv_atm_mol = phys_atm(ipa_V,:,:) / (conv_Pa_atm * const_R_SI * atm(ia_T,:,:))
    loc_conv_mol_atm = 1.0 / loc_conv_atm_mol
    ! time
    loc_dtyr = dum_dts / conv_yr_s
    ! fractional reduction factors for decaying isotopes
    loc_fracdecay_atm = EXP(-loc_dtyr * const_lambda_atm)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
    ! *** (i,j) GRID PT LOOP START *** !
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !

    block_iloop: DO i=1,n_i
       block_jloop: DO j=1,n_j

          ! *** DECAY RADIOACTIVE TRACERS ***
          DO l=3,n_l_atm
             ia = conv_iselected_ia(l)
             ! radioactive decay of isotopes
             IF (ABS(const_lambda_atm(ia)) > const_real_nullsmall) THEN
                atm(ia,i,j) = loc_fracdecay_atm(ia) * atm(ia,i,j)
             END if
          end do

          ! *** OXIDIZE CH4 ***
          IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
             CALL sub_calc_oxidize_CH4(i,j,loc_dtyr)
          END IF

          ! *** ADD CH4 ***
          IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
             CALL sub_calc_wetlands_CH4(loc_dtyr,locij_fatm(:,i,j))
          END IF

          ! *** PRODUCE RADIOACTIVE TRACERS ***
          IF (atm_select(ia_pCO2_14C)) THEN
             CALL sub_calc_generate_14C(loc_dtyr,locij_fatm(:,i,j))
          END IF

          ! *** EXCHANGE CO2 WITH A VIRTUAL TERRESTRIAL RESERVOIR ***
          IF ((par_atm_FterrCO2exchange > const_real_nullsmall) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pCO2_13C)) THEN
             CALL sub_calc_terrCO2exchange(i,j,loc_dtyr,locij_fatm(:,i,j))
          END IF

       END DO block_jloop
    END DO block_iloop

    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !
    ! *** (i,j) GRID PT LOOP END *** !
    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

    ! *** UPDATE ATMOSPHERIC COMPOSITION ***
    ! set internal atmospheric flux
    fatm = dum_sfxsumatm
    ! NOTE: flux <fatm> in (mol m-2 per timestep)
    ! update atmospheric composition
    ! NOTE: units of partial pressure (atm)
    ! NOTE: carry out at every (i.e, wet + dry) grid point
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       ! update atmospheric tracers
       atm(ia,:,:) = atm(ia,:,:) + loc_conv_mol_atm(:,:)*phys_atm(ipa_A,:,:)*fatm(ia,:,:) + loc_conv_mol_atm(:,:)*locij_fatm(ia,:,:)
       ! <HACK TO HOMOGENIZE ATMOSPHERIC COMPOSITION>
       ! homogenize the partial pressure of tracers in the atmopshere across (all grid points)
       loc_atm_tot(ia) = SUM(loc_conv_atm_mol(:,:)*atm(ia,:,:))
       loc_atm_tot_V = SUM(phys_atm(ipa_V,:,:))
       atm(ia,:,:) = (loc_atm_tot(ia)/loc_atm_tot_V)*conv_Pa_atm*const_R_SI*atm(ia_T,:,:)
    end do

    ! *** UPDATE INTERFACE ARRAYS ***
    ! return new <atm>
    dum_sfcatm = atm
    ! reset integrated flux array
    dum_sfxsumatm = 0.0

  END SUBROUTINE step_atchem


  ! RESTART AtChem (save data)
  SUBROUTINE atchem_save_rst(dum_genie_clock)
    USE atchem_lib
    USE atchem_data_netCDF
    IMPLICIT NONE
    INTEGER(KIND=8), INTENT(IN) :: dum_genie_clock  ! genie clock (milliseconds since start)

    INTEGER :: l, loc_iou
    REAL :: loc_yr
    CHARACTER(LEN=255) :: loc_filename
    ! ---------------------------------------------------------- ! calculate local time (years)
    loc_yr = REAL(dum_genie_clock) / (1000.0 * conv_yr_s)
    ! ---------------------------------------------------------- ! test for restart format
    IF (ctrl_ncrst) THEN
       ! ------------------------------------------------------- !
       ! SAVE RESTART DATA: NETCDF FORMAT
       ! ------------------------------------------------------- !
       string_ncrst = TRIM(par_outdir_name) // TRIM(par_ncrst_name)
       ncrst_ntrec = 0
       call sub_data_netCDF_ncrstsave(TRIM(string_ncrst),loc_yr,loc_iou)
    ELSE
       ! ------------------------------------------------------- !
       ! SAVE RESTART DATA: BINARY DUMP FORMAT
       ! ------------------------------------------------------- !
       ! NOTE: data is saved unformatted for minimal file size
       !       also means that arrays can be written directly to file without needing to loop thought data
       loc_filename = TRIM(par_outdir_name) // TRIM(par_outfile_name)
       OPEN(UNIT=out,STATUS='replace',FILE=loc_filename,FORM='unformatted',ACTION='write')
       WRITE(unit=out)                                    &
            & n_l_atm,                                    &
            & (conv_iselected_ia(l),l=1,n_l_atm),         &
            & (atm(conv_iselected_ia(l),:,:),l=1,n_l_atm)
       CLOSE(UNIT=out)
    END IF
  END SUBROUTINE atchem_save_rst


  SUBROUTINE end_atchem()
    USE atchem_lib

    print*,'======================================================='
    print*,' >>> Initialising ATCHEM module shutdown ...'

    print*,' <<< Shutdown complete'
    print*,'======================================================='
  END SUBROUTINE end_atchem

END MODULE atchem

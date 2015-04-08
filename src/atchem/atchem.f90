MODULE atchem

  USE genie_control
  USE atchem_lib
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_atchem
  PUBLIC :: step_atchem
  PUBLIC :: end_atchem
  PUBLIC :: atchem_save_rst
  PUBLIC :: cpl_comp_atmgem
  PUBLIC :: cpl_comp_gematm
  PUBLIC :: cpl_comp_atmocn
  PUBLIC :: cpl_comp_EMBM
  PUBLIC :: cpl_comp_atmlnd
  PUBLIC :: cpl_flux_ocnatm
  PUBLIC :: cpl_flux_lndatm
CONTAINS

  SUBROUTINE initialise_atchem(dum_sfxsumatm, dum_sfcatm)
    USE atchem_data
    USE atchem_data_netCDF
    USE genie_util, ONLY: check_iostat
    IMPLICIT NONE
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcatm    ! atmosphere-surface tracer composition; atm grid

    print*,'======================================================='
    print*,' >>> Initialising ATCHEM atmospheric chem. module ...'

    n_i = dim_GENIENX
    n_j = dim_GENIENY
    n_phys_atm = 15

    ALLOCATE(atm(n_atm,n_i,n_j),STAT=alloc_error)               ; atm = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fatm(n_atm,n_i,n_j),STAT=alloc_error)              ; fatm = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(phys_atm(n_phys_atm,n_i,n_j),STAT=alloc_error)     ; phys_atm = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(atm_slabbiosphere(n_atm,n_i,n_j),STAT=alloc_error) ; atm_slabbiosphere = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

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
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumatm
    REAL, DIMENSION(:,:,:), INTENT(OUT) :: dum_sfcatm

    INTEGER :: ia, i, j
    REAL :: loc_dtyr
    REAL :: loc_atm_tot_V
    REAL, DIMENSION(n_atm)         :: loc_atm_tot
    REAL, DIMENSION(n_i,n_j)       :: loc_conv_atm_mol, loc_conv_mol_atm
    REAL, DIMENSION(n_atm,n_i,n_j) :: locij_fatm           ! local flux to atmosphere (mol)
    REAL, DIMENSION(n_atm)         :: loc_fracdecay_atm    ! local reduction factor for decaying atmospheric tracers

    ! *** INITIALIZE LOCAL VARIABLES ***
    loc_atm_tot = 0.0 ; loc_conv_atm_mol = 0.0 ; loc_conv_mol_atm = 0.0
    locij_fatm = 0.0 ; loc_fracdecay_atm = 0.0

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
          DO ia = 3, n_atm
             ! radioactive decay of isotopes
             IF (ABS(const_lambda_atm(ia)) > const_real_nullsmall) THEN
                atm(ia,i,j) = loc_fracdecay_atm(ia) * atm(ia,i,j)
             END if
          end do

          ! *** OXIDIZE CH4 ***
          IF (atm_select(ias_pCH4) .AND. atm_select(ias_pCO2) .AND. atm_select(ias_pO2)) THEN
             CALL sub_calc_oxidize_CH4(i,j,loc_dtyr)
          END IF

          ! *** ADD CH4 ***
          IF (atm_select(ias_pCH4) .AND. atm_select(ias_pCO2) .AND. atm_select(ias_pO2)) THEN
             CALL sub_calc_wetlands_CH4(loc_dtyr,locij_fatm(:,i,j))
          END IF

          ! *** PRODUCE RADIOACTIVE TRACERS ***
          IF (atm_select(ias_pCO2_14C)) THEN
             CALL sub_calc_generate_14C(loc_dtyr,locij_fatm(:,i,j))
          END IF

          ! *** EXCHANGE CO2 WITH A VIRTUAL TERRESTRIAL RESERVOIR ***
          IF ((par_atm_FterrCO2exchange > const_real_nullsmall) .AND. atm_select(ias_pCO2) .AND. atm_select(ias_pCO2_13C)) THEN
             CALL sub_calc_terrCO2exchange(i,j,loc_dtyr,locij_fatm(:,i,j))
          END IF

       END DO block_jloop
    END DO block_iloop

    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !
    ! *** (i,j) GRID PT LOOP END *** !
    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

    ! *** UPDATE ATMOSPHERIC COMPOSITION ***
    ! set internal atmospheric flux
    fatm = 0.0
    DO ia = 1, n_atm
       fatm(ia,:,:) = dum_sfxsumatm(ia,:,:)
    END DO

    ! NOTE: flux <fatm> in (mol m-2 per timestep)
    ! update atmospheric composition
    ! NOTE: units of partial pressure (atm)
    ! NOTE: carry out at every (i.e, wet + dry) grid point
    DO ia = 3, n_atm
       ! update atmospheric tracers
       atm(ia,:,:) = atm(ia,:,:) + loc_conv_mol_atm(:,:) * &
            & (phys_atm(ipa_A,:,:) * fatm(ia,:,:) + locij_fatm(ia,:,:))
       ! <HACK TO HOMOGENIZE ATMOSPHERIC COMPOSITION>
       ! homogenize the partial pressure of tracers in the atmopshere across (all grid points)
       loc_atm_tot(ia) = SUM(loc_conv_atm_mol(:,:) * atm(ia,:,:))
       loc_atm_tot_V = SUM(phys_atm(ipa_V,:,:))
       atm(ia,:,:) = (loc_atm_tot(ia) / loc_atm_tot_V) * conv_Pa_atm * const_R_SI * atm(ia_T,:,:)
    END DO

    ! *** UPDATE INTERFACE ARRAYS ***
    ! return new <atm>
    dum_sfcatm = atm
    ! reset integrated flux array
    dum_sfxsumatm = 0.0
  END SUBROUTINE step_atchem


  ! RESTART AtChem (save data)
  SUBROUTINE atchem_save_rst(dum_genie_clock)
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
       WRITE(unit=out) n_atm, (ia_ias(l),l=1,n_atm), (atm(l,:,:),l=1,n_atm)
       CLOSE(UNIT=out)
    END IF
  END SUBROUTINE atchem_save_rst


  SUBROUTINE end_atchem()
    print*,'======================================================='
    print*,' >>> Initialising ATCHEM module shutdown ...'

    print*,' <<< Shutdown complete'
    print*,'======================================================='
  END SUBROUTINE end_atchem

  ! ******************************************************************************************************************************** !
  ! COUPLE TRACER FIELDS: ATM->GEM(GENIE)
  SUBROUTINE cpl_comp_atmgem(dum_dts, dum_genie_atm1)
    USE genie_global
    USE atchem_lib
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dts
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_genie_atm1
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! create time-averaged tracer array
    ! NOTE: currently, the GENIE arrays are defiend with the max number of atm tracers (not selected number)
    INTEGER :: ia, ias
    DO ia = 1, n_atm
       ias = ia_ias(ia)
       dum_genie_atm1(ias,:,:) = dum_genie_atm1(ias,:,:) + dum_dts * atm(ia,:,:) / conv_yr_s
    END DO
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  end SUBROUTINE cpl_comp_atmgem
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! COUPLE TRACER FIELDS: GEM(GENIE)->ATM
  SUBROUTINE cpl_comp_gematm(dum_genie_datm1)
    USE genie_global
    USE atchem_lib
    IMPLICIT NONE
    ! dummy arguments
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_genie_datm1
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! update tracer array
    ! NOTE: currently, the GENIE arrays are defiend with the max number of atm tracers (not selected number)
    ! NTOE: <dum_genie_atm1> is passed as an ANOMALY
    INTEGER :: ia, ias
    DO ia = 1, n_atm
       ias = ia_ias(ia)
       atm(ia,:,:) = atm(ia,:,:) + dum_genie_datm1(ias,:,:)
    END DO
    ! reset anomaly array
    dum_genie_datm1 = 0.0
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  end SUBROUTINE cpl_comp_gematm
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! COUPLE AtChem atmospheric composition
  SUBROUTINE cpl_comp_atmocn(dum_sfcatm, dum_sfcatm1)
    IMPLICIT NONE
    ! dummy arguments
    REAL, DIMENSION(:,:,:), INTENT(IN) :: dum_sfcatm     ! atmosphere-surface tracer composition; atm grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! NOTE: currently no summation done!
    ! NOTE: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
    dum_sfcatm1(3:,:,:) = dum_sfcatm(3:,:,:)
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  end SUBROUTINE cpl_comp_atmocn
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! COUPLE EMBM TRACERS
  SUBROUTINE cpl_comp_EMBM(dum_t, dum_q, dum_sfcatm1)
    IMPLICIT NONE
    ! dummy arguments
    REAL, DIMENSION(:,:), INTENT(IN) :: dum_t
    REAL, DIMENSION(:,:), INTENT(IN) :: dum_q
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! NOTE: currently no summation done!
    dum_sfcatm1(1,:,:) = dum_t
    dum_sfcatm1(2,:,:) = dum_q
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  end SUBROUTINE cpl_comp_EMBM
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! COUPLE AtChem atmospheric composition
  SUBROUTINE cpl_comp_atmlnd(dum_sfcatm, dum_sfcatm_lnd)
    IMPLICIT NONE
    ! dummy arguments
    REAL, DIMENSION(:,:,:), INTENT(IN) :: dum_sfcatm        ! atmosphere-surface tracer composition; atm grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcatm_lnd ! atmosphere-surface tracer composition; lnd grid
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! NOTE: currently no summation done!
    ! NOTE: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
    INTEGER :: ia, ias
    DO ia = 3, n_atm
       ias = ia_ias(ia)
       dum_sfcatm_lnd(ias,:,:) = dum_sfcatm(ia,:,:)
    END DO
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  end SUBROUTINE cpl_comp_atmlnd
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! COUPLE AtChem fluxes
  SUBROUTINE cpl_flux_ocnatm(dum_dts, dum_sfxatm1, dum_sfxsumatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dts
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxatm1   ! atmosphere-surface fluxes; ocn grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! integrate flux to atmosphere <dum_sfxatm1> (mol m-2 s-1)
    ! running total <dum_sfxsumatm> is in units of (mol m-2)
    dum_sfxsumatm = dum_sfxsumatm + dum_dts * dum_sfxatm1
    ! zero flux
    dum_sfxatm1 = 0.0
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  END SUBROUTINE cpl_flux_ocnatm
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! COUPLE AtChem fluxes
  SUBROUTINE cpl_flux_lndatm(dum_dts, dum_sfxatm_lnd, dum_sfxsumatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dts
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxatm_lnd   ! atmosphere-surface fluxes; lnd grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
    ! \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ !
    ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
    ! integrate flux to atmosphere <dum_sfxatm_lnd> (mol m-2 s-1)
    ! running total <dum_sfxsumatm> is in units of (mol m-2)
    INTEGER :: ia, ias
    DO ia = 1, n_atm
       ias = ia_ias(ia)
       dum_sfxsumatm(ia,:,:) = dum_sfxsumatm(ia,:,:) + dum_dts * dum_sfxatm_lnd(ias,:,:)
    END DO
    ! zero flux
    dum_sfxatm_lnd = 0.0
    ! /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ !
  END SUBROUTINE cpl_flux_lndatm
  ! ******************************************************************************************************************************** !

END MODULE atchem

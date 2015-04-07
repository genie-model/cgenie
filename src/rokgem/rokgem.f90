MODULE rokgem

  USE rokgem_lib
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_rokgem
  PUBLIC :: step_rokgem
  PUBLIC :: end_rokgem
  PUBLIC :: rest_rokgem
  PUBLIC :: reinit_flux_rokocn
  PUBLIC :: reinit_flux_rokatm

CONTAINS

  ! Subroutine: initialise_rokgem
  !
  ! Initialisation subroutine for RokGeM, called in <initialise_genie.F> through <genie_ini_wrappers.f90>
  !
  ! Uses:
  !
  !  - <rokgem_lib.f90>
  !  - <rokgem_data.f90>
  !  - <rokgem_box.f90>
  !  - <rokgem_data_netCDF.f90>
  !
  ! Calls:
  !
  ! - <sub_load_goin_rokgem>
  ! - <sub_init_phys_rok>
  ! - <sub_init_netcdf_rg>
  ! - <sub_load_rokgem_restart>
  ! - <sub_init_netcdf_rg>
  ! - <sub_data_output_years>
  ! - <sub_ini_output>
  ! - <sub_load_data_ij>
  ! - <sub_land>
  ! - <sub_antarctica>
  ! - <define_river_array>
  ! - <sub_drainage>
  ! - <define_2D_arrays>
  ! - <sub_load_weath>
  !
  ! Input/Output:
  !
  ! dum_genie_timestep - number of seconds in a genie timestep
  ! dum_sfxrok - rocks-surface ocean tracer composition; rok grid
  ! dum_sfxsumrok1 - rocks-surface ocean fluxes; integrated, ocn grid

  SUBROUTINE initialise_rokgem(dum_genie_timestep, dum_sfxrok, dum_sfxsumrok1)
    USE genie_control, ONLY: dim_ROKGEMNLONS, dim_ROKGEMNLATS
    USE genie_util, ONLY: die
    USE rokgem_data
    USE rokgem_box
    USE rokgem_data_netCDF
    REAL, INTENT(INOUT) :: dum_genie_timestep
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxrok     ! rocks-surface tracer composition; rok grid
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumrok1 ! rocks-surface fluxes; integrated, ocn grid

    INTEGER :: loc_iou, i, j, status

    print*,'======================================================='
    print*,' >>> Initialising rokgem weathering module ...'

    n_i = dim_ROKGEMNLONS
    n_j = dim_ROKGEMNLATS
    n_phys_rok = 8
    n_phys_ocnrok = 6

    CALL sub_load_goin_rokgem()

    ALLOCATE(phys_rok(n_phys_rok,n_i,n_j),STAT=status)         ; phys_rok = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(phys_ocnrok(n_phys_ocnrok,n_i,n_j),STAT=status)   ; phys_ocnrok = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(goldstein_k1(ilon1_ocn,ilat1_ocn),STAT=status)    ; goldstein_k1 = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(landmask(n_i,n_j),STAT=status)                    ; landmask = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(runoff_drainage(n_i+2,n_j+2),STAT=status)         ; runoff_drainage = 0.0  !'+2' comes from fact that *.k1 file is 38x38
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(runoff_drainto(n_i,n_j,2),STAT=status)            ; runoff_drainto = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(runoff_coast(n_i,n_j),STAT=status)                ; runoff_coast = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(total_calcium_flux(n_i,n_j),STAT=status)          ; total_calcium_flux = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(total_calcium_flux_Ca(n_i,n_j),STAT=status)       ; total_calcium_flux_Ca = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(total_calcium_flux_Si(n_i,n_j),STAT=status)       ; total_calcium_flux_Si = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(weather_fCaCO3_2D(n_i,n_j),STAT=status)           ; weather_fCaCO3_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(weather_fCaSiO3_2D(n_i,n_j),STAT=status)          ; weather_fCaSiO3_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(orogeny(n_i,n_j),STAT=status)                     ; orogeny = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(regimes_calib(n_i,n_j),STAT=status)               ; regimes_calib = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ref_T0_2D(n_i,n_j),STAT=status)                   ; ref_T0_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ref_R0_2D(n_i,n_j),STAT=status)                   ; ref_R0_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ref_P0_2D(n_i,n_j),STAT=status)                   ; ref_P0_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(data_T_2D(n_i,n_j),STAT=status)                   ; data_T_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(data_R_2D(n_i,n_j),STAT=status)                   ; data_R_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(data_P_2D(n_i,n_j),STAT=status)                   ; data_P_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(calibrate_T_2D(n_i,n_j),STAT=status)              ; calibrate_T_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(calibrate_R_2D(n_i,n_j),STAT=status)              ; calibrate_R_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(calibrate_P_2D(n_i,n_j),STAT=status)              ; calibrate_P_2D = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    CALL sub_init_phys_rok()

    ! *** setup for netcdf output  ***
    if (debug_init > 1) print*, 'initialize netCDF'
    string_ncout2d_rg  = TRIM(par_outdir_name)//'fields_rokgem_2d.nc' !note: this needs to be less than 100 characters
    if (debug_init > 1) print*, 'netcdf ouput file: ',TRIM(string_ncout2d_rg)
    ! initialise 2d netcdf files
    IF (ctrl_continuing.AND.opt_append_data) THEN
       call sub_load_rokgem_restart()
    ELSE
       ncout2d_ntrec_rg = 0
       call sub_init_netcdf_rg(trim(string_ncout2d_rg),loc_iou)
    ENDIF
    if (debug_init > 1) print*, 'netcdf record number: ',ncout2d_ntrec_rg
    if (debug_init > 1) print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name

    ! *** setup for netcdf output  ***
    if (debug_init > 1) print*, 'initialize netCDF'
    string_ncout2d_rg  = TRIM(par_outdir_name)//'fields_rokgem_2d.nc' !note: this needs to be less than 100 characters
    if (debug_init > 1) print*, 'netcdf ouput file: ',TRIM(string_ncout2d_rg)
    ! initialise 2d netcdf files
    IF (ctrl_continuing.AND.opt_append_data) THEN
       call sub_load_rokgem_restart()
    ELSE
       ncout2d_ntrec_rg = 0
       call sub_init_netcdf_rg(trim(string_ncout2d_rg),loc_iou)
    ENDIF
    if (debug_init > 1) print*, 'netcdf record number: ',ncout2d_ntrec_rg
    if (debug_init > 1) print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name

    ! *** initialize external interface arrays ***
    dum_sfxsumrok1 = 0.0
    dum_sfxrok     = 0.0

    ! *** initialize timestep counter ***
    tstep_count = 0
    tsteps_per_year = conv_yr_s/(dum_genie_timestep*kocn_loop*conv_kocn_krokgem)
    if (debug_init > 1) PRINT*,'timesteps per year                                  :',tsteps_per_year
    ! *** load in years for output generation and initialise output ***
    CALL sub_data_output_years()
    year = min(output_years_0d(output_counter_0d),output_years_2d(output_counter_2d))
    CALL sub_ini_output()
    if (debug_init > 1) print*,'======================================================='

    ! -------------------------------------------------------- ! set legacy weathering options
    IF (opt_weather_T_Ca) opt_weather_CaCO3="BLAG"
    if (opt_weather_C_Ca) opt_weather_CaCO3="WalkerKasting"
    IF (opt_weather_T_Si) opt_weather_CaSiO3="Brady_approx"
    IF (opt_weather_Talt_Si) opt_weather_CaSiO3="linear"
    IF (opt_weather_C_Si) opt_weather_CaSiO3="WalkerKasting"

    ! ======= k_T constant ==================================================================!
    if (debug_init > 1) print*,'---  k_T constant ---'
    k_T=1000*par_E_a/(8.314472*((par_ref_T0+273.15)**2))
    if (debug_init > 1) print*,'k_T = ',k_T

    ! ======= RIVER ROUTING =================================================================!

    if (debug_init > 1) print*,'--- RIVER ROUTING ---'

    ! Read basic land run-off routing file into array runoff_drainage (k1 file)

    CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(topo),n_i+2,n_j+2,runoff_drainage)

    ! Get landmask and number of landcells

    if (debug_init > 1) print*,'getting land mask from k1 file'
    CALL sub_land(runoff_drainage,landmask)
    if (debug_init > 1) print*,'number of land cells = ',nlandcells

    ! work out number of cells and rows in antarctica

    CALL sub_antarctica(landmask,ncells_antarctica,nrows_antarctica)

    ! Read detailed land run-off routing file (routing_new.dat) into array runoff_detail
    runoff_detail_i = 3*max_drain_cells
    runoff_detail_j = n_i*n_j
    CALL define_river_array()
    CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(routing)//'_'// &
         & fun_conv_num_char_n(3,n_i)//'_'//fun_conv_num_char_n(3,n_j)//'.dat', &
         & runoff_detail_i,runoff_detail_j,runoff_detail)

    ! Work out where on the coast to dump riverine solutes from each grid point

    CALL sub_drainage(runoff_drainage,runoff_drainto,runoff_detail,runoff_coast)

    ! Note: sub_coastal_output is used to do the actual routing in the main loop

    ! ======= Calibration to data ==============================================================!


    if (debug_init > 1) print*,'--- CALIBRATION TO DATA ---'

    ! 0D calibration
    IF (opt_calibrate_T_0D) THEN
       calibrate_T_0D = ( par_data_T_0D + 273.15 ) / ( par_ref_T0 + 273.15 )
    ELSE
       calibrate_T_0D = 1.0
    ENDIF
    if (debug_init > 1) print*,'calibrate_T_0D = ',calibrate_T_0D

    IF (opt_calibrate_R_0D) THEN
       conv_GKWM_runoff = conv_GKWM_runoff * ( par_data_R_0D / par_ref_R0 )
       conv_GEM_CO2 = conv_GEM_CO2 * ( par_data_R_0D / par_ref_R0 )
       calibrate_R_0D = par_data_R_0D / par_ref_R0
    ELSE
       calibrate_R_0D=1.0
    ENDIF
    if (debug_init > 1) print*,'calibrate_R_0D = ',calibrate_R_0D

    if (debug_init > 1) print*,'conv_GKWM_runoff = ',conv_GKWM_runoff
    if (debug_init > 1) print*,'conv_GEM_CO2 = ',conv_GEM_CO2

    IF (opt_calibrate_P_0D) THEN
       calibrate_P_0D = par_data_P_0D / par_ref_P0
    ELSE
       calibrate_P_0D = 1.0
    ENDIF
    if (debug_init > 1) print*,'calibrate_P_0D = ',calibrate_P_0D

    ! 2D calibration
    IF (opt_calibrate_T_2D) THEN
       CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_ref_T0_2D),n_i,n_j,ref_T0_2D)
       CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_data_T_2D),n_i,n_j,data_T_2D)
       DO i=1,n_i
          DO j=1,n_j
             calibrate_T_2D(i,j) = ( data_T_2D(i,j) + 273.15 ) / ( ref_T0_2D(i,j) + 273.15 )
          END DO
       END DO
    ENDIF

    if (debug_init > 2) then
       print*, data_T_2D(:,1)
       print*, ref_T0_2D(:,1)
       print*, calibrate_T_2D(:,1)
    end if

    IF (opt_calibrate_R_2D) THEN
       CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_ref_R0_2D),n_i,n_j,ref_R0_2D)
       CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_data_R_2D),n_i,n_j,data_R_2D)
       DO i=1,n_i
          DO j=1,n_j
             IF (ref_R0_2D(i,j).eq.0.0) THEN
                calibrate_R_2D(i,j) = 1.0
             ELSE
                calibrate_R_2D(i,j) = data_R_2D(i,j) / ref_R0_2D(i,j)
             ENDIF
          END DO
       END DO
    ENDIF

    IF (opt_calibrate_P_2D) THEN
       CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_ref_P0_2D),n_i,n_j,ref_P0_2D)
       CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(par_data_P_2D),n_i,n_j,data_P_2D)
       DO i=1,n_i
          DO j=1,n_j
             IF (ref_P0_2D(i,j).eq.0.0) THEN
                calibrate_P_2D(i,j) = 1.0
             ELSE
                calibrate_P_2D(i,j) = data_P_2D(i,j) / ref_P0_2D(i,j)
             ENDIF
          END DO
       END DO
    ENDIF


    ! ======= 2D WEATHERING =================================================================!

    ! *** load in data for 2D weathering scheme if selected
    IF (par_weathopt.ne.'Global_avg') THEN
       if (debug_init > 1) print*,'--- 2D WEATHERING ---'
       gridcell_area = phys_rok(ipr_A,1,1)/1.0E6         ! factor of 1E6 to convert from m^2 to km^2
       if (debug_init > 1) print*,'gridcell area = ',gridcell_area
       conv_GKWM = 0.5*gridcell_area                     ! Formula is for Flux of bicarbonate produced, and we want riverine flux of Ca2+
       ! (in line with global average formulation inputs), these are in ratio of 2:1 so factor of 0.5.
       ! And fluxes are calculated per km^2 so multiply by gridcell_area
       conv_GKWM_runoff = 0.1 * conv_yr_s                ! Have separate constant for runoff as it is raised to a power in the formula;
       ! runoff units are mm/s in EMBM but cm/yr in Gibbs' formula.
       ! normalise to annual average runoff used by Gibbs (41.8 cm/yr = 1.32E-05 mm/s)
       !  - number is divided by annual average runoff during calculation in sub_GKWM

       conv_GEM_CO2 = 1.0E3 * conv_yr_s * gridcell_area  ! Runoff units are mm/s in EMBM but l*km^-2*s-1 in the GEM-CO2 formula. (factor 1E6).
       ! Fluxes are calculated per km^2 so multiply by gridcell_area.
       ! Formula is for Flux of CO2 consumed, and we want riverine flux of Ca2+
       ! (in line with global average formulation inputs), these are in ratio of 1:1 so OK.
       ! Factor of 10^-3 * conv_yr_s as formula calculates quantites in 10^-3 mol/s and we want mol/yr.

       SELECT case (par_weathopt)
       case ('GKWM')
          par_nliths = 6
       case ('GEM_CO2')
          par_nliths = 7
       end SELECT
       if (debug_init > 1) print*,'number of rock types (no. of files listed in x_lithologies.txt) = ',par_nliths
       CALL define_2D_arrays()
       CALL sub_load_weath(lithology_names,lithology)

       IF (opt_weath_regimes) THEN
          if (debug_init > 1) print*,'Erosion/transport limited weathering on; reading in orogeny landmask'
          ! Read basic landmask file with locations of different weathering regimes
          CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(weath_regimes),n_i,n_j,orogeny)
       ENDIF

    ENDIF

    print*,' <<< Initialisation complete'
    print*,'======================================================='
  END SUBROUTINE initialise_rokgem


  ! ******************************************************************************************************************************** !

  ! Subroutine: step_rokgem
  !
  ! Main time-stepping subroutine for RokGeM
  !
  ! Uses:
  !
  ! - <rokgem_lib.f90>
  ! - <rokgem_data.f90>
  ! - <rokgem_box.f90>
  !
  ! Calls:
  !
  ! - <sub_glob_avg_weath>
  ! - <sub_GKWM>
  ! - <sub_GEM_CO2>
  ! - <sum_calcium_flux_CaSi>
  !
  ! Input:
  !
  ! dum_sfcatm1 - atmosphere composition interface array
  ! dum_runoff - run-off to be read in from exernal module (EMBM, or ENTS)
  ! dum_photo(n_i,n_j) - photosynthesis array from land veg module (ENTS)
  ! dum_respveg(n_i,n_j) - vegetation respiration array from land veg module (ENTS).
  ! NOTE - run_off, photo and respveg only work for same grid as RokGeM at the moment
  !
  ! Output:
  !
  ! dum_sfxrok - ocean flux interface array (same no of tracers as used in biogem ocean)
  ! dum_sfxatm1 - atmosphere flux interface array (same no of tracers as used in atchem atmosphere)

  SUBROUTINE step_rokgem(dum_dts, dum_sfcatm1, dum_runoff, dum_photo, dum_respveg, dum_sfxrok, dum_sfxatm1)
    USE rokgem_data
    USE rokgem_box
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_dts                      ! time-step
    REAL, INTENT(IN) :: dum_sfcatm1(n_atm,n_i,n_j)   ! atmosphere composition interface array
    REAL, INTENT(IN) :: dum_runoff(n_i,n_j)          ! run-off to be read in from exernal module (EMBM, or ENTS)
    REAL, INTENT(IN) :: dum_photo(n_i,n_j)           ! photosythesis from land veg module (ENTS)
    REAL, INTENT(IN) :: dum_respveg(n_i,n_j)         ! vegetation respiration from land veg module (ENTS)
    ! -> NOTE - run_off, photo and respveg only work
    ! for same grid as RokGeM at the moment
    REAL, INTENT(INOUT) :: dum_sfxrok(n_ocn,n_i,n_j)   ! ocean flux interface array
    !  (same no of tracers as used in biogem ocean)
    REAL, INTENT(INOUT) :: dum_sfxatm1(n_atm,n_i,n_j ) ! atmosphere flux interface array

    ! increment timestep counter
    tstep_count = tstep_count + 1

    ! if output due then change year
    CALL sub_output_year()

    ! reset rokgem flux array
    dum_sfxrok = 0.0

    ! calculate weathering fluxes dependent on chosen scheme, and then dump them into the relevant points on the coast
    SELECT case (par_weathopt)
    case ('Global_avg')
       ! global average weathering
       CALL sub_glob_avg_weath(dum_dts,dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)
       ! Gibbs et al (1999) 2D lithology-dependent weathering
    case ('GKWM')
       CALL sub_GKWM(dum_runoff,lithology,calcium_flux)
       CALL sum_calcium_flux_CaSi(calcium_flux,total_calcium_flux_Ca,total_calcium_flux_Si)
       CALL sub_2D_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)
       ! Amiotte-Suchet et al (2003) 2D lithology-dependent weathering
    case ('GEM_CO2')
       CALL sub_GEM_CO2(dum_runoff,lithology,calcium_flux)
       CALL sum_calcium_flux_CaSi(calcium_flux,total_calcium_flux_Ca,total_calcium_flux_Si)
       CALL sub_2D_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)
    end SELECT

    ! if output then increment output counter
    CALL sub_output_counters()
  END SUBROUTINE step_rokgem

  ! Subroutine: rest_rokgem
  !
  ! Restart RokGeM - saves netcdf record number for use in appending data to netcdfs when using restarts
  !
  ! Uses:
  !
  ! - <rokgem_lib.f90>

  SUBROUTINE rest_rokgem()
    USE rokgem_lib
    IMPLICIT NONE

    integer::ios
    CHARACTER(len=255)::loc_filename

    if (debug_init > 1) PRINT*,'saving netcdf record number',ncout2d_ntrec_rg

    ! dump restart data
    loc_filename = TRIM(par_outdir_name)//trim(par_outfile_name)
    OPEN(20,status='replace',file=loc_filename,form='formatted',action='write',iostat=ios)
    WRITE(20,fmt='(i6)') ncout2d_ntrec_rg
    close(20)


    !        Conditionals commented out because no calibration is done for stage 1 spin-up
    ! and files are needed to be written for stage 2.
    if (debug_init > 1) PRINT*,'saving 2D temperature reference field for calibration: ',TRIM(par_ref_T0_2D)
    OPEN(20,status='replace',file=TRIM(par_outdir_name)//'rg_par_ref_T0_2D',form='formatted',action='write',iostat=ios)
    WRITE(20,fmt='(A100)') par_ref_T0_2D
    CLOSE(20)
    CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(par_ref_T0_2D),n_i,n_j,ref_T0_2D(:,:))
    if (debug_init > 1) PRINT*,'saving 2D runoff reference field for calibration: ',TRIM(par_ref_R0_2D)
    OPEN(20,status='replace',file=TRIM(par_outdir_name)//'rg_par_ref_R0_2D',form='formatted',action='write',iostat=ios)
    WRITE(20,fmt='(A100)') par_ref_R0_2D
    CLOSE(20)
    CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(par_ref_R0_2D),n_i,n_j,ref_R0_2D(:,:))
    if (debug_init > 1) PRINT*,'saving 2D productivity reference field for calibration: ',TRIM(par_ref_P0_2D)
    OPEN(20,status='replace',file=TRIM(par_outdir_name)//'rg_par_ref_P0_2D',form='formatted',action='write',iostat=ios)
    WRITE(20,fmt='(A100)') par_ref_P0_2D
    CLOSE(20)
    CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(par_ref_P0_2D),n_i,n_j,ref_P0_2D(:,:))
  END SUBROUTINE rest_rokgem


  ! ******************************************************************************************************************************** !
  ! REINITIALIZE ROKGEM OCN INTERFACE FLUX ARRAY
  SUBROUTINE reinit_flux_rokatm(dum_sfxsumatm1)
    USE rokgem_lib
    IMPLICIT NONE
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumatm1

    ! *** RE-INITIALIZE VARIABLES ***
    ! reset cumulative weathering array
    dum_sfxsumatm1 = 0.0
  END SUBROUTINE reinit_flux_rokatm
  ! ******************************************************************************************************************************** !


  ! ******************************************************************************************************************************** !
  ! REINITIALIZE ROKGEM OCN INTERFACE FLUX ARRAY
  SUBROUTINE reinit_flux_rokocn(dum_sfxsumrok1)
    USE rokgem_lib
    IMPLICIT NONE
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumrok1

    ! *** RE-INITIALIZE VARIABLES ***
    ! reset cumulative weathering array
    dum_sfxsumrok1 = 0.0
  END SUBROUTINE reinit_flux_rokocn
  ! ******************************************************************************************************************************** !


  ! Subroutine: end_rokgem
  !
  ! shuts down RokGeM.
  !
  ! Calls:
  !
  ! - <rest_rokgem>
  ! - <deallocate_arrays>

  SUBROUTINE end_rokgem()
    USE rokgem_lib, ONLY: deallocate_arrays
    IMPLICIT NONE

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising rokgem module shutdown ...'

    CALL rest_rokgem()
    CALL deallocate_arrays()

    PRINT *, ' <<< Shutdown complete'
    PRINT *, '======================================================='
  END SUBROUTINE end_rokgem

END MODULE rokgem

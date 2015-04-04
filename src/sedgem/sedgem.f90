MODULE sedgem

  USE sedgem_lib
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_sedgem
  PUBLIC :: step_sedgem
  PUBLIC :: end_sedgem
  PUBLIC :: sedgem_save_rst
  PUBLIC :: sedgem_dsedage

CONTAINS

  SUBROUTINE initialise_sedgem(dum_genie_timestep, dum_sfxsumsed, &
       & dum_sfcsumocn, dum_sfcsed, dum_sfxocn)
    USE sedgem_data
    USE genie_util, ONLY: check_iostat
    IMPLICIT NONE
    REAL, INTENT(in) :: dum_genie_timestep                       ! genie time-step (in seconds)
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumsed
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcsumocn
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfcsed
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxocn

    integer::i,j                                                 ! local counting variables
    integer::loc_n_sed_stack_top                                 !
    real::loc_sed_stack_top_th                                   !
    real::loc_vol_top,loc_poros_top
    real::loc_vol,loc_poros

    print*,'======================================================='
    print*,' >>> Initialising SEDGEM sediment geochem. module ...'

    ! ---------------------------------------------------------- ! load goin information
    IF (ctrl_misc_debug2) print*, 'load goin information'
    CALL sub_load_goin_sedgem()
    ! ---------------------------------------------------------- ! initialize interface arrays
    IF (ctrl_misc_debug2) print*, 'initialize interface arrays'
    dum_sfxsumsed(:,:,:) = 0.0   !
    dum_sfcsumocn(:,:,:) = 0.0   !
    dum_sfcsed(:,:,:)    = 0.0   !
    dum_sfxocn(:,:,:)    = 0.0   !
    ! ---------------------------------------------------------- ! allocate arrays
    IF (ctrl_misc_debug2) print*, 'allocate arrays'
    ! *** dimension the size of the 2-D sediment grid arrays ***
    ! NOTE: check for problems allocating array space
    ALLOCATE(phys_sed(n_phys_sed,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_mask(n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_mask_reef(n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_mask_muds(n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed(n_sed,n_i,n_j,n_sed_tot),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_top(n_sed,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_top_h(n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_top_INTdth(n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_fsed(n_sed,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_fdis(n_sed,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sedocn_fnet(n_ocn,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_carb(n_carb,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_carbconst(n_carbconst,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_carbalk(n_carbalk,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_carbisor(n_carbisor,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_save_mask(n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_fsed_OLD(n_sed,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sed_fdis_OLD(n_sed,n_i,n_j),STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ! ---------------------------------------------------------- ! initialize allocated arrays
    IF (ctrl_misc_debug2) print*, 'initialize allocated arrays'
    ! initialize dynamically-allocated arrays (those not done elsewhere)
    sed_fsed(:,:,:)      = 0.0   !
    sed_fdis(:,:,:)      = 0.0   !
    sedocn_fnet(:,:,:)   = 0.0   !
    sed_carb(:,:,:)      = 0.0   !
    sed_carbconst(:,:,:) = 0.0   !
    ! ---------------------------------------------------------- ! main initialization
    IF (ctrl_misc_debug2) print*, 'main initialization'
    ! setup SedGeM grid
    CALL sub_init_phys_sed()
    ! set meta-options and verify self-consistency of chosen parameters
    CALL sub_check_par_sedgem()
    ! initialize sediment sub-system
    CALL sub_init_sed()
    CALL sub_init_sed_layers_default()
    ! initialize sediment core location data save mask
    CALL sub_init_sedgem_save_sed_data()
    ! initialize sediment core environmental conditions saving
    if (ctrl_data_save_sedcorenv) CALL sub_sedgem_init_sedcoresenv()
    ! seed the aqueous carbonate system with an initial value of [H+]
    sed_carb(ic_H,:,:) = 1.0E-8
    ! initialize bioturbation profile
    IF (ctrl_sed_bioturb) THEN
       if (ctrl_sed_bioturb_Archer) then
          ALLOCATE(par_sed_mix_k(0:par_n_sed_mix),STAT=error)
       else
          ! load bioturbation profile data
          CALL sub_load_sed_mix_k()
       end if
    end IF
    ! set initial sediment age
    sed_age = par_misc_t_runtime - 0.5*conv_s_yr*(dum_genie_timestep*kocn_loop*conv_kocn_ksedgem)
    ! set ash event (to mark run start)
    par_sed_ashevent = .true.
    ! initialize sedcorenv time counters
    sed_time      = 0.0
    sed_time_save = 0.0
    ! ---------------------------------------------------------- ! INITIALIZE netCDF OUTPUT
    IF (ctrl_misc_debug2) print*, 'INITIALIZE netCDF OUTPUT'
    string_ncout2d   = TRIM(par_outdir_name)//'fields_sedgem_2d.nc'
    IF (ctrl_timeseries_output) THEN
       IF (ctrl_continuing.AND.ctrl_append_data) THEN
          OPEN(unit=in,status='old',file=TRIM(par_rstdir_name)//'netcdf_record_numbers',form='formatted',action='read')
          READ(unit=in,fmt='(i6)') ntrec_sout
          close(unit=in)
       ELSE
          ntrec_sout = 0
       ENDIF
       print*, 'netcdf record number: ',ntrec_sout
       print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name
    ELSE
       ntrec_sout = 0
    ENDIF
    ! ---------------------------------------------------------- ! INITIALIZE DATA SAVING
    IF (ctrl_misc_debug2) print*, 'INITIALIZE DATA SAVING'
    IF (ctrl_timeseries_output) THEN
       ! initialize timestep counter
       tstep_count = 0
       tsteps_per_year = conv_yr_s/(dum_genie_timestep*kocn_loop*conv_kocn_ksedgem)
       PRINT*,'timesteps per year                                  :',tsteps_per_year
       ! load in years for output generation
       CALL sub_data_output_years()
       year = min(output_years_0d(output_counter_0d),output_years_2d(output_counter_2d))
    ENDIF
    ! ---------------------------------------------------------- ! LOAD SEDIMENT RE-START
    IF (ctrl_misc_debug2) print*, 'LOAD SEDIMENT RE-START'
    IF (ctrl_continuing) then
       CALL sub_data_load_rst(dum_sfxsumsed,dum_sfxocn)
       ! modify sediment ages
       if (sed_select(is_CaCO3_age)) then
          sed(is_CaCO3_age,:,:,:)    = sed(is_CaCO3_age,:,:,:)    + par_misc_t_runtime*sed(is_CaCO3,:,:,:)
          sed_top(is_CaCO3_age,:,:)  = sed_top(is_CaCO3_age,:,:)  + par_misc_t_runtime*sed_top(is_CaCO3,:,:)
       end if
       if (sed_select(is_det_age)) then
          sed(is_det_age,:,:,:)    = sed(is_det_age,:,:,:)    + par_misc_t_runtime*sed(is_det,:,:,:)
          sed_top(is_det_age,:,:)  = sed_top(is_det_age,:,:)  + par_misc_t_runtime*sed_top(is_det,:,:)
       end if
       ! ------------------------------------------------------- ! modify numerical CaCO3 tracers
       if (ctrl_sed_dyerestart .AND. sed_select(is_CaCO3_red)) then
          DO i=1,n_i
             DO j=1,n_j
                IF (sed_mask(i,j)) THEN
                   ! set local (loop) cariables
                   loc_n_sed_stack_top  = INT(sed_top_h(i,j)) + 1
                   loc_sed_stack_top_th = sed_top_h(i,j) - REAL(loc_n_sed_stack_top - 1)
                   loc_vol_top = fun_calc_sed_vol(sed_top(:,i,j))
                   loc_poros_top = fun_calc_sed_poros_nsur(sed_top(is_CaCO3,i,j)/loc_vol_top,par_sed_top_th)
                   loc_vol = fun_calc_sed_vol(sed(:,i,j,loc_n_sed_stack_top))
                   loc_poros = fun_calc_sed_poros(sed(is_CaCO3,i,j,loc_n_sed_stack_top)/loc_vol)
                   ! tag core-top (normalize for porosity difference)
                   sed_top(is_CaCO3_red,i,j) = 1.0*sed_top(is_CaCO3,i,j)*(1.0 - loc_poros)/(1.0 - loc_poros_top)
                   sed_top(is_CaCO3_blue,i,j) = 0.0
                   ! tag red core segment
                   sed(is_CaCO3_red,i,j,(loc_n_sed_stack_top - par_sed_dyerestart_n + 2):loc_n_sed_stack_top) = &
                        & 1.0*sed(is_CaCO3,i,j,(loc_n_sed_stack_top - par_sed_dyerestart_n + 2):loc_n_sed_stack_top)
                   sed(is_CaCO3_blue,i,j,(loc_n_sed_stack_top - par_sed_dyerestart_n + 2):loc_n_sed_stack_top) = 0.0
                   ! tag base of red core sedgment (adjust to take into account incomplete core stack layer)
                   sed(is_CaCO3_red,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1) = &
                        & (1.0 - loc_sed_stack_top_th)*sed(is_CaCO3,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1)
                   sed(is_CaCO3_blue,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1) = &
                        & loc_sed_stack_top_th*sed(is_CaCO3,i,j,loc_n_sed_stack_top - par_sed_dyerestart_n + 1)
                   ! tag blue core segment
                   sed(is_CaCO3_red,i,j,1:(loc_n_sed_stack_top - par_sed_dyerestart_n)) = 0.0
                   sed(is_CaCO3_blue,i,j,1:(loc_n_sed_stack_top - par_sed_dyerestart_n)) = &
                        & 1.0*sed(is_CaCO3,i,j,1:(loc_n_sed_stack_top - par_sed_dyerestart_n))
                end if
             end DO
          end do
       end if
       ! update sediment interface composition data arrays with restart
       dum_sfcsed(:,:,:) = fun_sed_coretop()
    end if
    ! ---------------------------------------------------------- ! INITIALIZE SEDCORES
    IF (ctrl_misc_debug2) print*, 'INITIALIZE SEDCORES'
    ! set number of sedcore array tracers
    n_sedcore_tracer = n_l_sed
    ! set number of sedcore array tracers
    ! NOTE: try and adapt the number of sedcore store layers allocated depending on runtime, e.g. 10 x kyr
    n_sedcore_tot = par_n_sedcore_tot_min + par_n_sedcore_tot_perky*int(par_misc_t_runtime/1000.0)
    ! initialize sedcores
    CALL sub_data_sedcore_init()

    PRINT *, ' <<< Initialisation complete'
    PRINT *, '======================================================='
  END SUBROUTINE initialise_sedgem


  SUBROUTINE step_sedgem(dum_dts, dum_sfxsumsed, dum_sfcsumocn, dum_sfcsed, &
       & dum_sfxocn, dum_reinit_sfxsumsed)
    USE sedgem_box
    USE sedgem_data
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_dts                                  ! time-step
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumsed       ! sediment rain flux interface array
    REAL, DIMENSION(:,:,:), INTENT(IN) :: dum_sfcsumocn          ! ocean composition interface array
    REAL, DIMENSION(:,:,:), INTENT(OUT) :: dum_sfcsed            ! sediment composition interface array
    REAL, DIMENSION(:,:,:), INTENT(OUT) :: dum_sfxocn            ! sediment dissolution flux interface array
    LOGICAL, INTENT(IN) :: dum_reinit_sfxsumsed                  ! reinitialize sedimentation array?

    integer::i,j,l,io,is                                         ! COUNTING AND ARRAY INDEX VARIABLES
    integer::loc_i,loc_tot_i                                     ! array index conversion variables
    real::loc_dtyr                                               ! local time step (in years)
    real::loc_dts                                                ! local time step (in seconds)
    real::loc_tot_A                                              ! local total area
    logical::loc_flag_save                        ! local flag
    REAL,DIMENSION(n_sed)::loc_fracdecay_sed                     ! local reduction factor for decaying sediment tracers
    real,DIMENSION(n_ocn)::loc_fhydrothermal                     ! local dissolved tracer array for hydrothermal input
    real,DIMENSION(n_ocn)::loc_flowTalteration                   ! local dissolved tracer array for low T alteration sink
    real,DIMENSION(n_i,n_j)::loc_phys_sed_mask_deepsea           !
    real::loc_tot,loc_standard                                   !
    real::loc_r7Li,loc_r44Ca                                     !
    real::loc_alpha,loc_R,loc_delta                              !
    real::loc_fsed                                               !
    real,DIMENSION(n_sed,n_i,n_j)::loc_sfxsumsed_OLD                      ! sediment rain flux interface array (COPY)

    ! *** STORE PREVIOUS ITERATION DATA ***
    sed_fsed_OLD(:,:,:) = sed_fsed(:,:,:)
    sed_fdis_OLD(:,:,:) = sed_fdis(:,:,:)
    ! copy current (passed) sediemnt flux
    loc_sfxsumsed_OLD(:,:,:) = dum_sfxsumsed(:,:,:)

    ! *** INITIALIZE RESULTS ARRAYS ***
    dum_sfxocn(:,:,:)  = 0.0     !
    sed_fdis(:,:,:)    = 0.0     !
    sedocn_fnet(:,:,:) = 0.0     !

    ! *** INITIALIZE LOCAL ARRAYS ***
    loc_fhydrothermal(:)   = 0.0
    loc_flowTalteration(:) = 0.0
    loc_phys_sed_mask_deepsea(:,:) = 0.0

    ! *** CALCULATE SEDGEM TIME STEP ***
    IF (ctrl_misc_debug4) print*,'*** CALCULATE SEDGEM TIME ***'
    ! calculate sediment model time step length
    ! NOTE: convert between time units of BioGeM (years) and GOLDSTEIn (use <tsc> scale factor to convert to seconds)
    loc_dts  = dum_dts
    loc_dtyr = loc_dts/conv_yr_s

    ! *** DECAY RADIOACTIVE TRACERS ***
    ! calculate fractional reduction factors for decaying isotopes
    loc_fracdecay_sed(:) = EXP(-loc_dtyr*const_lambda_sed(:))
    ! decay radioactive tracers
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       IF (abs(const_lambda_sed(is)).gt.const_real_nullsmall) THEN
          sed_top(is,:,:) = loc_fracdecay_sed(is)*sed_top(is,:,:)
          sed(is,:,:,:)   = loc_fracdecay_sed(is)*sed(is,:,:,:)
       END if
    end do

    ! *** UpDATE SAVE COUNTER ***
    ! update save counter
    sed_time_save = sed_time_save + loc_dtyr
    sed_time      = sed_time + loc_dtyr
    ! test for whether sedcorenv data should be saved
    if (sed_time_save >= (par_sed_age_save_dt - const_real_nullsmall)) then
       loc_flag_save = .true.
       sed_time_save = 0.0
    else
       loc_flag_save = .false.
    endif

    ! *** UPDATE MASKS ***
    IF (ctrl_misc_debug4) print*,'*** UPDATE MASKS ***'
    DO i=1,n_i
       DO j=1,n_j
          ! catch a zero salinity as an indication that there is no valid corresponding ocean grid point
          ! => permanently amend sediment mask
          IF (sed_mask(i,j) .AND. (dum_sfcsumocn(io_S,i,j) < const_real_nullsmall)) then
             sed_mask(i,j)      = .FALSE.
             sed_mask_reef(i,j) = .FALSE.
             sed_mask_muds(i,j) = .FALSE.
             phys_sed(ips_mask_sed,i,j)      = 0.0
             phys_sed(ips_mask_sed_reef,i,j) = 0.0
             phys_sed(ips_mask_sed_muds,i,j) = 0.0
             sed_save_mask(i,j) = .FALSE.
          end IF
       end DO
    end DO
    ! set local deep-sea mask & calculate total area
    loc_phys_sed_mask_deepsea(:,:) = phys_sed(ips_mask_sed,:,:) - phys_sed(ips_mask_sed_reef,:,:) - phys_sed(ips_mask_sed_muds,:,:)
    loc_tot_A = sum(loc_phys_sed_mask_deepsea(:,:)*phys_sed(ips_A,:,:))

    ! *** UPDATE CARBONATE CHEMSITRY ***
    IF (ctrl_misc_debug4) print*,'*** UPDATE CARBONATE CHEMSITRY ***'
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) then
             CALL sub_calc_carbconst(        &
                  & phys_sed(ips_D,i,j),     &
                  & dum_sfcsumocn(io_T,i,j), &
                  & dum_sfcsumocn(io_S,i,j), &
                  & sed_carbconst(:,i,j)     &
                  & )
             if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                CALL sub_adj_carbconst(          &
                     & dum_sfcsumocn(io_Ca,i,j), &
                     & dum_sfcsumocn(io_Mg,i,j), &
                     & sed_carbconst(:,i,j)      &
                     & )
             end if
             CALL sub_calc_carb(                &
                  & dum_sfcsumocn(io_DIC,i,j),  &
                  & dum_sfcsumocn(io_ALK,i,j),  &
                  & dum_sfcsumocn(io_Ca,i,j),   &
                  & dum_sfcsumocn(io_PO4,i,j),  &
                  & dum_sfcsumocn(io_SiO2,i,j), &
                  & dum_sfcsumocn(io_B,i,j),    &
                  & dum_sfcsumocn(io_SO4,i,j),  &
                  & dum_sfcsumocn(io_F,i,j),    &
                  & dum_sfcsumocn(io_H2S,i,j),  &
                  & dum_sfcsumocn(io_NH4,i,j),  &
                  & sed_carbconst(:,i,j),       &
                  & sed_carb(:,i,j),            &
                  & sed_carbalk(:,i,j)          &
                  & )
             ! re-calculate carbonate system isotopic properties
             if (ocn_select(io_DIC_13C)) then
                CALL sub_calc_carb_r13C(              &
                     & dum_sfcsumocn(io_T,i,j),       &
                     & dum_sfcsumocn(io_DIC,i,j),     &
                     & dum_sfcsumocn(io_DIC_13C,i,j), &
                     & sed_carb(:,i,j),               &
                     & sed_carbisor(:,i,j)            &
                     & )
             end IF
             if (ocn_select(io_DIC_14C)) then
                CALL sub_calc_carb_r14C(              &
                     & dum_sfcsumocn(io_T,i,j),       &
                     & dum_sfcsumocn(io_DIC,i,j),     &
                     & dum_sfcsumocn(io_DIC_14C,i,j), &
                     & sed_carb(:,i,j),               &
                     & sed_carbisor(:,i,j)            &
                     & )
             end IF
          end IF
       end DO
    end DO

    ! *** EARLY DIAGENESIS PROCESSES ***
    ! NOTE: <dum_sfxsumsed> in units of (mol m-2 per time-step)
    ! NOTE: dum_sfxocn(io,:,:) in units of (mol m-2 s-1)
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) THEN
             ! amend sediment rain flux according to prescribed detrital input
             ! NOTE: convert units from (g cm-2 kyr-1) to (mol m-2 (per time-step))
             ! NOTE: add age tracer if selected
             ! NOTE: assuming that not both surface-derived flux and prescribed benthic addition of detrital will be done
             !       (otherwise a flux-weighting of age will be required)
             if (sed_select(is_det)) then
                dum_sfxsumsed(is_det,i,j) = dum_sfxsumsed(is_det,i,j) + &
                     & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_fdet
             endif
             if (sed_select(is_det_age)) then
                dum_sfxsumsed(is_det_age,i,j) = dum_sfxsumsed(is_det_age,i,j) + &
                     & sed_age*conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_fdet
             endif
             ! add ash layer (if selected)
             if (sed_select(is_ash)) then
                if (par_sed_ashevent) then
                   dum_sfxsumsed(is_ash,i,j) = dum_sfxsumsed(is_ash,i,j) + &
                        & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_ashevent_fash
                end if
             endif
             ! tag CaCO3 'color'
             if (sed_select(is_CaCO3_red)) dum_sfxsumsed(is_CaCO3_red,i,j) = par_sed_CaCO3_fred*dum_sfxsumsed(is_CaCO3,i,j)
             if (sed_select(is_CaCO3_blue)) dum_sfxsumsed(is_CaCO3_blue,i,j) = par_sed_CaCO3_fblue*dum_sfxsumsed(is_CaCO3,i,j)
             ! account for clay formation
             If (ocn_select(io_Li)) then
                loc_fsed = par_sed_clay_fLi_alpha*dum_sfxsumsed(is_det,i,j)*dum_sfcsumocn(io_Li,i,j)
                dum_sfxsumsed(is_detLi,i,j) = dum_sfxsumsed(is_detLi,i,j) + loc_fsed
                dum_sfxocn(io_Li,i,j) = dum_sfxocn(io_Li,i,j) - loc_fsed/dum_dts
                if (ocn_select(io_Li_7Li)) then
                   loc_standard = const_standards(ocn_type(io_Li_7Li))
                   if (dum_sfcsumocn(io_Li,i,j) > const_real_nullsmall) then
                      loc_r7Li = dum_sfcsumocn(io_Li_7Li,i,j)/dum_sfcsumocn(io_Li,i,j)
                   else
                      loc_r7Li = 0.0
                   end if
                   loc_alpha = 1.0 + par_sed_clay_7Li_epsilon/1000.0
                   loc_R = loc_r7Li/(1.0 - loc_r7Li)
                   loc_fsed = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_fsed
                   dum_sfxsumsed(is_detLi_7Li,i,j) = dum_sfxsumsed(is_detLi_7Li,i,j) + loc_fsed
                   dum_sfxocn(io_Li_7Li,i,j) = dum_sfxocn(io_Li_7Li,i,j) - loc_fsed/dum_dts
                end if
             end if
          end IF
       end DO
    end DO
    ! deselect ash fall
    if (sed_select(is_ash)) then
       if (par_sed_ashevent) par_sed_ashevent = .false.
    endif

    ! *** FORAM TRACERS ***
    !
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) THEN
             ! add foram tracers
             if (sed_select(is_CaCO3) .AND. sed_select(is_foram_b_13C)) then
                ! calculate 13C/12C fractionation between DIC and CaCO3
                SELECT CASE (opt_sed_foram_b_13C_delta)
                CASE ('NONE')
                   loc_delta = 0.0
                case ('SPERO')
                   ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                   loc_delta = 0.0
                   ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                end SELECT
                loc_alpha = 1.0 + loc_delta/1000.0
                loc_R = sed_carbisor(ici_HCO3_r13C,i,j)/(1.0 - sed_carbisor(ici_HCO3_r13C,i,j))
                loc_fsed = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*dum_sfxsumsed(is_CaCO3,i,j)
                dum_sfxsumsed(is_foram_b_13C,i,j) = loc_fsed
             end if
          end IF
       end DO
    end DO

    ! *** UPDATE SEDIMENTS ***
    IF (ctrl_misc_debug4) print*,'*** UPDATE SEDIMENTS ***'
    DO i=1,n_i
       DO j=1,n_j
          ! calculate sediment rain flux from sediment->ocean flux (convert units)
          ! NOTE: if the sediment grid point lies outside of the sediment mask, then
          !       dissolve all sediment tracers and set the ocean tracer dissolution flux equal to this
          ! convert units of sedimentation flux
          ! NOTE: <dum_sfxsumsed> in units of (mol m-2)
          ! NOTE: <sed_fsed> in units of (mol cm-2)
          sed_fsed(:,i,j) = conv_cm2_m2*dum_sfxsumsed(:,i,j)
          IF (sed_mask(i,j)) THEN
             ! Call sediment composition update
             ! NOTE: the values in both <sed_fsed> and <ocnsed_fnet> are updated by this routine
             if (sed_mask_reef(i,j)) then
                IF (ctrl_misc_debug3) print*,'> UPDATE SED: reef'
                CALL sub_update_sed_reef(    &
                     & loc_dtyr,             &
                     & i,j,                  &
                     & phys_sed(ips_D,i,j),  &
                     & dum_sfcsumocn(:,i,j)  &
                     & )
             elseif (sed_mask_muds(i,j)) then
                IF (ctrl_misc_debug3) print*,'> UPDATE SED: mud'
                CALL sub_update_sed_mud(     &
                     & loc_dtyr,             &
                     & i,j,                  &
                     & dum_sfcsumocn(:,i,j)  &
                     & )
             else
                IF (ctrl_misc_debug3) print*,'> UPDATE SED: (normal)'
                CALL sub_update_sed(         &
                     & loc_dtyr,             &
                     & i,j,                  &
                     & phys_sed(ips_D,i,j),  &
                     & dum_sfcsumocn(:,i,j)  &
                     & )
             end if
             ! save sedcore environmental conditions
             if (sed_save_mask(i,j) .AND. loc_flag_save .AND. ctrl_data_save_sedcorenv) then
                CALL sub_sedgem_save_sedcoreenv( &
                     & loc_dtyr,                 &
                     & i,j,                      &
                     & sed_top(:,i,j),           &
                     & sed_fsed(:,i,j),          &
                     & sed_fdis(:,i,j),          &
                     & dum_sfcsumocn(:,i,j),     &
                     & sed_carb(:,i,j)           &
                     & )
             end if
          else
             ! NO SEDIMENTS HERE
             ! set dissolution flux (as sediment solids)
             sed_fdis(:,i,j) = sed_fsed(:,i,j)
             ! calculate equivalent ocean tracer flux
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                loc_tot_i = conv_sed_ocn_i(0,is)
                do loc_i=1,loc_tot_i
                   io = conv_sed_ocn_i(loc_i,is)
                   sedocn_fnet(io,i,j) = sedocn_fnet(io,i,j) + conv_sed_ocn(io,is)*sed_fsed(is,i,j)
                end do
             end DO
          end if
       end do
    end do

    ! *** HYDROTHERMAL / SEAFLOOR ALTERATION ***
    IF (ctrl_misc_debug4) print*,'*** HYDROTHERMAL / SEAFLOOR ALTERATION ***'
    ! calculate hydrothermal source fluxes (inputs)
    ! NOTE: loc_fhydrothermal_input(io) in units of (mol yr-1)
    ! NOTE: dum_sfxocn(io,:,:) in units of (mol m-2 s-1)
    If (ocn_select(io_Li)) then
       loc_fhydrothermal(io_Li) = par_sed_hydroip_fLi
       if (ocn_select(io_Li_7Li)) then
          loc_tot = loc_fhydrothermal(io_Li)
          loc_standard = const_standards(ocn_type(io_Li_7Li))
          loc_fhydrothermal(io_Li_7Li) = fun_calc_isotope_fraction(par_sed_hydroip_fLi_d7Li,loc_standard)*loc_tot
       end if
    end if
    ! balance Ca input with Mg removal, unless Mg is not selected => add ALK
    If (ocn_select(io_Ca)) then
       loc_fhydrothermal(io_Ca) = par_sed_hydroip_fCa
       if (ocn_select(io_Ca_44Ca)) then
          loc_tot = loc_fhydrothermal(io_Ca)
          loc_standard = const_standards(ocn_type(io_Ca_44Ca))
          loc_fhydrothermal(io_Ca_44Ca) = fun_calc_isotope_fraction(par_sed_hydroip_fCa_d44Ca,loc_standard)*loc_tot
       end if
       If (ocn_select(io_Mg)) then
          loc_fhydrothermal(io_Mg) = -par_sed_hydroip_fCa
       else
          loc_fhydrothermal(io_ALK) = 2.0*par_sed_hydroip_fCa
       end If
    end if
    ! re-scale global total and apread over *valid* grid points
    DO i=1,n_i
       DO j=1,n_j
          if (loc_phys_sed_mask_deepsea(i,j) > const_real_nullsmall) then
             DO l=1,n_l_ocn
                io = conv_iselected_io(l)
                dum_sfxocn(io,i,j) = dum_sfxocn(io,i,j) + loc_fhydrothermal(io)/loc_tot_A/conv_yr_s
             end DO
          end if
       end DO
    end DO
    ! calculate weathering/alteration fluxes (net sink)
    ! NOTE: dum_sfxocn(io,:,:) in units of (mol m-2 s-1)
    DO i=1,n_i
       DO j=1,n_j
          if (loc_phys_sed_mask_deepsea(i,j) > const_real_nullsmall) then
             ! NOTE: the value of par_sed_lowTalt_fLi_alpha is calculated on the basis of a sink of x mol yr-1
             !          where Atot is the total sediment area and [Li] assumed to be 26 umol Li
             !       => e.g. 1.0E10 (mol yr-1) = [Li] * x / Atot / (365.25*24*3600)
             If (ocn_select(io_Li)) then
                loc_flowTalteration(io_Li) = par_sed_lowTalt_fLi_alpha*dum_sfcsumocn(io_Li,i,j)
                dum_sfxocn(io_Li,i,j) = dum_sfxocn(io_Li,i,j) - loc_flowTalteration(io_Li)
                if (ocn_select(io_Li_7Li)) then
                   loc_standard = const_standards(ocn_type(io_Li_7Li))
                   if (dum_sfcsumocn(io_Li,i,j) > const_real_nullsmall) then
                      loc_r7Li = dum_sfcsumocn(io_Li_7Li,i,j)/dum_sfcsumocn(io_Li,i,j)
                   else
                      loc_r7Li = 0.0
                   end if
                   loc_alpha = 1.0 + par_sed_lowTalt_7Li_epsilon/1000.0
                   loc_R = loc_r7Li/(1.0 - loc_r7Li)
                   loc_flowTalteration(io_Li_7Li) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_flowTalteration(io_Li)
                   dum_sfxocn(io_Li_7Li,i,j) = dum_sfxocn(io_Li_7Li,i,j) - loc_flowTalteration(io_Li_7Li)
                end if
             end if
             If (ocn_select(io_Ca)) then
                ! NOTE: the calcium sink is calculated on the basis of a sink of x mol yr-1
                !       where Atot is the total sediment area and [Ca] assumed to be 0.01025 mol Ca
                !       => e.g. [Ca] * x / Atot / (365.25*24*3600)
                loc_flowTalteration(io_Ca) = par_sed_lowTalt_fCa_alpha*dum_sfcsumocn(io_Ca,i,j)
                dum_sfxocn(io_Ca,i,j) = dum_sfxocn(io_Ca,i,j) - loc_flowTalteration(io_Ca)
                If (ocn_select(io_Ca_44Ca)) then
                   loc_standard = const_standards(ocn_type(io_Ca_44Ca))
                   if (dum_sfcsumocn(io_Li,i,j) > const_real_nullsmall) then
                      loc_r44Ca = dum_sfcsumocn(io_Ca_44Ca,i,j)/dum_sfcsumocn(io_Ca,i,j)
                   else
                      loc_r44Ca = 0.0
                   end if
                   loc_alpha = 1.0 + par_sed_lowTalt_44Ca_epsilon/1000.0
                   loc_R = loc_r44Ca/(1.0 - loc_r44Ca)
                   loc_flowTalteration(io_Ca_44Ca) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_flowTalteration(io_Ca)
                   dum_sfxocn(io_Ca_44Ca,i,j) = dum_sfxocn(io_Ca_44Ca,i,j) - loc_flowTalteration(io_Ca_44Ca)
                end If
             end if
          end if
       end DO
    end DO

    ! *** UPDATE INTERFACE ***
    IF (ctrl_misc_debug4) print*,'*** UPDATE INTERFACE ***'
    ! update update sed->ocn interface
    ! NOTE: <sedocn_fnet> in units of (mol cm-2)
    ! NOTE: <dum_sfxocn> in units of (mol m-2 s-1)
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       dum_sfxocn(io,:,:) = dum_sfxocn(io,:,:) + conv_m2_cm2*sedocn_fnet(io,:,:)/loc_dts
    end DO
    ! re-initialize the interfacing integrated sediment rain flux array
    ! NOTE: in normal operation, dum_reinit_sfxsumsed is .true., hence once sediment rain has been added to the sediments
    !       the flux array is zero-ed
    !       (during GEMlite phases, there is no BIOGEM updating of the ocean rain flux, hence need to preserve the flux value)
    if (dum_reinit_sfxsumsed) then
       dum_sfxsumsed(:,:,:) = 0.0
    else
       dum_sfxsumsed(:,:,:) = loc_sfxsumsed_OLD(:,:,:)
    end if
    ! update sediment interface composition data
    dum_sfcsed(:,:,:) = fun_sed_coretop()

    ! *** DEBUG ***
    ! print some debugging info if 'ctrl_misc_debug1' option is selected
    IF (ctrl_misc_debug1) THEN
       i = par_misc_debug_i
       j = par_misc_debug_j
       print*,''
       print*,'--- DEBUG ---'
       print*,'> SEDGEM LOCATION (i,j): ',i,j
       print*,'> TIME, TIME-STEP: ',loc_dts,loc_dtyr
       print*, &
            & phys_sed(ips_D,i,j),               &
            & dum_sfcsumocn(io_T,i,j),           &
            & dum_sfcsumocn(io_S,i,j)
       print*, &
            & dum_sfcsumocn(io_DIC,i,j),         &
            & dum_sfcsumocn(io_ALK,i,j),         &
            & 1.0E+06*sed_carb(ic_dCO3_cal,i,j)
       print*, &
            & 100.0*sed_top(is_CaCO3,i,j),       &
            & 100.0*sed_top(is_opal,i,j)
       print*, &
            & 1.0E+06*sed_fsed(is_CaCO3,i,j)/loc_dtyr, &
            & 1.0E+06*sed_fsed(is_POC,i,j)/loc_dtyr,   &
            & 1.0E+06*sed_fdis(is_CaCO3,i,j)/loc_dtyr, &
            & 1.0E+06*sed_fdis(is_POC,i,j)/loc_dtyr
       print*, &
            & sum(sed_fsed(is_CaCO3,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr, &
            & sum(sed_fsed(is_POC,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr,   &
            & sum(sed_fdis(is_CaCO3,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr, &
            & sum(sed_fdis(is_POC,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr
       print*,'---'
       print*,''
    end if
    ! catch any carbonate chemsitry errors arising in sub_update_sed
    if (error_stop) then
       CALL end_sedgem(     &
            & dum_dts,      &
            & dum_sfcsumocn &
            & )
    end if

    ! *** RUN-TIME OUTPUT ***
    ! GHC 20/05/09 - Save time-series output
    IF (ctrl_timeseries_output) THEN
       ! increment timestep counter
       tstep_count = tstep_count + 1
       ! if output due then change year
       CALL sub_output_year()
       IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN
          CALL sub_data_save_seddiag_GLOBAL(loc_dtyr,dum_sfcsumocn)
       ENDIF
       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
          CALL sub_save_netcdf(year)
          CALL sub_save_netcdf_sed2d(loc_dtyr,dum_sfcsumocn)
          CALL sub_closefile(ntrec_siou)
          ntrec_sout = ntrec_sout + 1
       ENDIF
       ! if output then increment output counter
       CALL sub_output_counters()
    ENDIF

    ! *** UPDATE SEDGEM TIME ***
    IF (ctrl_misc_debug4) print*,'*** UPDATE SEDGEM TIME ***'
    ! update sediment age (== current time in years)
    sed_age = sed_age - loc_dtyr
  END SUBROUTINE step_sedgem


  ! AGE SEDIMENTS
  SUBROUTINE sedgem_dsedage(dum_dts, dum_sfxsumsed)
    USE sedgem_lib
    IMPLICIT NONE
    REAL, INTENT(in) :: dum_dts                                  ! time-step
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: dum_sfxsumsed       ! sediment rain flux interface array

    INTEGER :: i,j
    REAL :: loc_dtyr
    ! set age decrement (years)
    loc_dtyr = dum_dts / conv_yr_s
    ! decrement (CaCO3) sediment age tracer
    DO i = 1, n_i
       DO j = 1, n_j
          IF (sed_mask(i,j)) THEN
             IF (sed_select(is_CaCO3_age)) THEN
                dum_sfxsumsed(is_CaCO3_age,i,j) = dum_sfxsumsed(is_CaCO3_age,i,j) - loc_dtyr*dum_sfxsumsed(is_CaCO3,i,j)
             END IF
             IF (sed_select(is_det_age)) THEN
                dum_sfxsumsed(is_det_age,i,j) = dum_sfxsumsed(is_det_age,i,j) - loc_dtyr*dum_sfxsumsed(is_det,i,j)
             END IF
          END IF
       END DO
    END DO
  END SUBROUTINE sedgem_dsedage


  ! ******************************************************************************************************************************** !
  ! *** RESTART SEDGEM (save data) ************************************************************************************************* !
  ! ******************************************************************************************************************************** !
  SUBROUTINE sedgem_save_rst(dum_genie_clock,dum_sfxocn)
    USE sedgem_lib
    use sedgem_data_netCDF
    IMPLICIT NONE
    INTEGER(KIND=8), INTENT(IN) :: dum_genie_clock               ! genie clock (milliseconds since start) NOTE: 8-byte integer
    REAL, DIMENSION(:,:,:), INTENT(IN) :: dum_sfxocn             ! sediment dissolution flux interface array

    integer::l
    integer::loc_iou
    real::loc_yr                                                 !
    CHARACTER(len=255)::loc_filename
    ! ---------------------------------------------------------- ! calculate local time (years)
    loc_yr = real(dum_genie_clock)/(1000.0*conv_yr_s)
    ! ---------------------------------------------------------- ! test for restart format
    IF (ctrl_ncrst) THEN
       ! ------------------------------------------------------- !
       ! SAVE RESTART DATA: NETCDF FORMAT
       ! ------------------------------------------------------- !
       string_ncrst = TRIM(par_outdir_name)//trim(par_ncrst_name)
       ncrst_ntrec = 0
       CALL sub_data_netCDF_ncrstsave(trim(string_ncrst),loc_yr,loc_iou,dum_sfxocn)
    else
       ! ------------------------------------------------------- !
       ! SAVE RESTART DATA: BINARY DUMP FORMAT
       ! ------------------------------------------------------- !
       ! NOTE: data is saved unformatted for minimal file size
       !       also means that arrays can be written directly to file without needing to loop thought data
       loc_filename = TRIM(par_outdir_name)//trim(par_outfile_name)
       OPEN(unit=out,status='replace',file=loc_filename,form='unformatted',action='write')
       WRITE(unit=out)                                         &
            & n_l_sed,                                         &
            & (conv_iselected_is(l),l=1,n_l_sed),              &
            & (sed(conv_iselected_is(l),:,:,:),l=1,n_l_sed),   &
            & (sed_top(conv_iselected_is(l),:,:),l=1,n_l_sed), &
            & sed_top_h(:,:)
       close(unit=out)
    end IF
  END SUBROUTINE sedgem_save_rst

  SUBROUTINE end_sedgem(dum_dts, dum_sfcsumocn)
    use genie_control
    USE gem_cmn
    USE sedgem_data
    USE sedgem_data_netCDF
    USE genie_util, ONLY: check_iostat
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_dts
    REAL, DIMENSION(:,:,:), INTENT(IN) :: dum_sfcsumocn

    real::loc_dtyr ! local time step in years
    real::loc_dts  ! local time step in seconds
    real::loc_yr
    integer::loc_iou
    ! ---------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! ---------------------------------------------------------- !
    loc_yr = 0.0
    ! ---------------------------------------------------------- !
    ! SHUTDOWN ...
    ! ---------------------------------------------------------- !
    print*,'======================================================='
    print*,' >>> Initialising SEDGEM module shutdown ...'
    ! ------------------------------------------------------- !
    ! SAVE DATA: 2-D netCDF + ASCII output
    ! ------------------------------------------------------- !
    ! calculate sediment model time step length
    loc_dts  = dum_dts
    loc_dtyr = loc_dts/conv_yr_s
    ! save diagnostics
    CALL sub_data_save_seddiag_GLOBAL(loc_dtyr,dum_sfcsumocn)
    ! save requested sediment cores as ASCII
    if (ctrl_data_save_ascii) CALL sub_sedgem_save_sedcore()
    ! save final oecan-sediment interface properties
    ! NOTE: netCDF was set up to be able to save multiple time-slices, but is only being used here to save a single final slice
    if (ctrl_data_save_ascii) CALL sub_data_save_seddiag_2D(loc_dtyr,dum_sfcsumocn)
    CALL sub_save_netcdf(const_real_zero)
    CALL sub_save_netcdf_sed2d(loc_dtyr,dum_sfcsumocn)
    CALL sub_closefile(ntrec_siou)
    ntrec_sout = ntrec_sout + 1
    ! ------------------------------------------------------- !
    ! SAVE DATA: SEDCORES
    ! ------------------------------------------------------- !
    if (nv_sedcore > 0) then
       string_ncsedcorein = TRIM(par_rstdir_name)//trim(par_ncsedcore_name)
       string_ncsedcoreout = TRIM(par_outdir_name)//trim(par_ncsedcore_name)
       ncsedcore_ntrec = 0
       CALL sub_data_netCDF_sedcoresave(trim(string_ncsedcorein),trim(string_ncsedcoreout),loc_yr,loc_iou)
    end if
    ! ---------------------------------------------------------- !
    ! CLEAN UP
    ! ---------------------------------------------------------- !
    ! ---------------------------------------------------------- ! deallocate arrays
    DEALLOCATE(phys_sed,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_mask,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_mask_reef,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_mask_muds,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_top,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_top_h,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_fsed,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_fdis,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sedocn_fnet,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_carb,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_carbconst,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_carbalk,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_carbisor,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_save_mask,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_fsed_OLD,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(sed_fdis_OLD,STAT=dealloc_error)
    CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    DEALLOCATE(vsedcore_store,STAT=alloc_error)
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ! ---------------------------------------------------------- ! deallocate arrays -- lookup tables
    if (par_sed_diagen_CaCO3opt == 'ridgwell2001lookup' .OR. par_sed_diagen_CaCO3opt == 'ridgwell2001lookupvec') then
       DEALLOCATE(lookup_sed_dis_cal,STAT=dealloc_error)
       CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    end if
    if (par_sed_diagen_CaCO3opt == 'ridgwell2001lookupvec') then
       DEALLOCATE(lookup_vec_D,STAT=dealloc_error)
       CALL check_iostat(dealloc_error,__LINE__,__FILE__)
       DEALLOCATE(lookup_vec_dco3,STAT=dealloc_error)
       CALL check_iostat(dealloc_error,__LINE__,__FILE__)
       DEALLOCATE(lookup_vec_frac,STAT=dealloc_error)
       CALL check_iostat(dealloc_error,__LINE__,__FILE__)
       DEALLOCATE(lookup_vec_fCorg,STAT=dealloc_error)
       CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    end if
    if (par_sed_diagen_opalopt == 'ridgwelletal2003lookup') then
       DEALLOCATE(lookup_sed_dis_opal,STAT=dealloc_error)
       CALL check_iostat(dealloc_error,__LINE__,__FILE__)
    end if

    PRINT *, ' <<< Shutdown complete'
    PRINT *, '======================================================='
  END SUBROUTINE end_sedgem

END MODULE sedgem

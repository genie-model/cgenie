! ******************************************************************
! *** GENIE ********************************************************
! ******************************************************************

PROGRAM GENIE

  USE genie_util, ONLY: die
  USE genie_control
  USE genie_global
  USE genie_ini_wrappers
  USE genie_loop_wrappers
  USE genie_end_wrappers
#ifdef INTEL_PROFILE
  use itt_profile
#endif
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  LOGICAL :: command_exists
  CHARACTER(LEN=80) :: command, command_arg
  INTEGER(KIND=8) :: koverall_start = 1, genie_clock_in

  integer :: status_interval

  ! clock timing variables
  INTEGER :: cr, cm
  INTEGER :: clock_starttime, clock_now, clock_last_status
  real :: cpu_starttime, cpu_endtime


  PRINT *
  PRINT *, '*******************************************************'
  PRINT *, '  *** Welcome to cGENIE -- version: ', TRIM(genie_version)
  PRINT *, '*******************************************************'
  PRINT *
  PRINT *, 'MODIFIED IN JOB SRC test'
#ifdef INTEL_PROFILE
  print *, 'Intel Profile API enabled'
  call itt_profile_init()
#endif

 ! First initialize the system_clock
  CALL system_clock(count_rate=cr)
  CALL system_clock(count_max=cm)

  ! set interval write to status file every 5 seconds (in units of the clock rate cr)
  status_interval = 5 * cr

  CALL SYSTEM_CLOCK(clock_starttime)
  call CPU_TIME(cpu_starttime)

  ! set the initial clock_last_status in the past,
  ! so that the first loop iteration triggers a status write
  clock_last_status = clock_starttime - 2*status_interval

  ! *** INITIALIZE ***

  istep_atm = 0
  istep_ocn = 0
  istep_sic = 0
  istep_gem = 0
  gem_yr    = gem_yr_min
  gem_notyr = gem_notyr_max

  CALL read_command(command_exists, command, command_arg)
  IF (command_exists) THEN
     IF (TRIM(command) == 'GUI_RESTART') THEN
        gui_restart = .TRUE.
        PRINT *
        PRINT *, '*******************************************************'
        PRINT *, ' *** Restarting simulation on command from GUI ...'
        PRINT *, '*******************************************************'
        PRINT *
        READ (command_arg, *) koverall_start, genie_clock_in
     END IF
  END IF

  CALL initialise_genie
  IF (gui_restart) genie_clock = genie_clock_in
  CALL allocate_genie_global
  IF (flag_goldsteinocean) CALL initialise_goldocean_wrapper
  IF (flag_ebatmos) CALL initialise_embm_wrapper
  IF (flag_goldsteinseaice) CALL ini_goldsteinseaice_wrapper
  IF (flag_ents) CALL initialise_ents_wrapper
  IF (flag_atchem.or.flag_biogem.or.flag_sedgem.or.flag_rokgem) &
       & CALL initialise_gem_wrapper
  IF (flag_biogem) CALL initialise_biogem_wrapper
  IF (flag_atchem) THEN
     CALL initialise_atchem_wrapper
     CALL cpl_comp_atmocn_wrapper
     CALL cpl_comp_EMBM_wrapper
  END IF
  IF (flag_sedgem) THEN
     CALL initialise_sedgem_wrapper
     CALL cpl_flux_sedocn_wrapper
     CALL cpl_comp_sedocn_wrapper
  END IF
  IF (flag_rokgem) CALL initialise_rokgem_wrapper
  IF (flag_gemlite) CALL initialise_gemlite_wrapper


  ! *** MAIN PROGRAM ***

  PRINT *
  PRINT *, '*******************************************************'
  PRINT *, ' *** Initialisation complete: simulation starting ...'
  PRINT *, '*******************************************************'

  ! *** BIOGEM model - headers and initialization run-time reporting
  IF (flag_biogem) THEN
     CALL biogem_climate_wrapper
     CALL diag_biogem_wrapper
  END IF

  !    *** MAIN TIME-STEPPING LOOP START ***

  ! NOTE: koverall is in hours
  DO koverall = koverall_start, koverall_total
#ifdef INTEL_PROFILE
     call itt_profile_begin(task_timestep)
     call itt_profile_begin(task_status)
#endif
     CALL read_command(command_exists, command, command_arg)
     IF (command_exists) THEN
        SELECT CASE (TRIM(command))
        CASE ('PAUSE')
           CALL write_status('PAUSED')
           PRINT *
           PRINT *, '*******************************************************'
           PRINT *, ' *** Pausing simulation on command from GUI ...'
           PRINT *, '*******************************************************'
           PRINT *
           PRINT *
           PRINT *

           IF (debug_loop > 1) PRINT *, '>>> WRITING GUI RESTART FILES <<<'

           writing_gui_restarts = .TRUE.
           IF (flag_biogem) CALL biogem_save_restart_wrapper
           IF (flag_atchem) CALL atchem_save_restart_wrapper
           IF (flag_sedgem) CALL sedgem_save_restart_wrapper
           IF (flag_rokgem) CALL rokgem_save_restart_wrapper
           IF (flag_ebatmos) CALL embm_save_restart_wrapper
           IF (flag_goldsteinocean) CALL goldstein_save_restart_wrapper
           IF (flag_goldsteinseaice) CALL gold_seaice_save_restart_wrapper
           IF (flag_ents) CALL ents_save_restart_wrapper
           !!! ===> TODO: what about GEMLITE restarts?
           !IF (flag_gemlite) CALL gemlite_save_restart_wrapper
           STOP
        END SELECT
     END IF

     CALL SYSTEM_CLOCK(clock_now)
     IF(clock_now - clock_last_status > status_interval) then
        clock_last_status = clock_now
        CALL write_status('RUNNING')
     ENDIF
#ifdef INTEL_PROFILE
     call itt_profile_end()
#endif

     ! Increment the clock which accumulates total time
     CALL increment_genie_clock

     IF (flag_gemlite) THEN
        IF (MOD(koverall, kgemlite * kocn_loop) == 1) THEN
#ifdef INTEL_PROFILE
           call itt_profile_begin(task_gemlite)
#endif
           ! Increment GEM year count
           istep_gem = istep_gem + 1
           IF (debug_loop > 2) PRINT *, istep_gem, gem_notyr, gem_yr

           ! Test for for GEM count exceeding a complete 1st (normal) cycle
           IF (istep_gem == gem_notyr + 1) THEN
              ! SWITCH TO GEM CYCLE
              IF (debug_loop > 0) PRINT *, '### START GEM CYCLE ###'
              IF (debug_loop > 3) PRINT *, '* gemlite_cycleinit'
              CALL gemlite_cycleinit_wrapper
              ! Copy current state of tracer arrays to GEMlite
              IF (debug_loop > 3) PRINT *, '* cpl_comp_gemglt'
              CALL cpl_comp_gemglt_wrapper
              ! Convert between sediment grids (for integ. sediment rain flux)
              IF (debug_loop > 3) PRINT *, '* cpl_flux_sedsed1'
              CALL cpl_flux_sedsed1_wrapper
              ! Copy climate state variables (here: sea-ice)
              IF (debug_loop > 3) PRINT *, '* gemlite_climate_wrapper'
              CALL gemlite_climate_wrapper
           ELSE IF (istep_gem > gem_notyr + gem_yr) THEN
              ! REVERT TO NORMAL CYCLE
              ! NOTE: <genie_ocn> summed anomaly not re-set at this stage
              ! (OK in the way integration is done and not carried forward?)
              IF (debug_loop > 0) PRINT *, '$$$ REVERT NORMAL CYCLE $$$'
              ! Reset count
              istep_gem = 1
              gem_switch=-1
              ! Adjust GEM cycle count
              gem_yr = gem_yr + gem_dyr
              IF (gem_yr > gem_yr_max) gem_yr = gem_yr_max
              gem_notyr = gem_notyr - gem_dyr
              IF (gem_notyr < gem_notyr_min) gem_notyr = gem_notyr_min
              IF (debug_loop > 1) &
                   & PRINT *, '*** gem_yr, gem_notyr, gem_dyr  :', &
                   & gem_yr, gem_notyr, gem_dyr
              ! Apply *summed* anomalies to <atm>, <ocn>, <ts>, <ts1> so that
              ! the last state of the seasonal cycle is preserved
              ! NOTE: <ocn> has already been incrementally adjusted
              ! (@ each gtl time-step) and does not need the summed anomoly
              ! add summed <docn_sum> anomoly (excluding T,S) to <ts>, <ts1>
              ! NOTE: before cpl_comp_gltgem because <docn_sum> gets re-set
              IF (debug_loop > 3) PRINT *, '* gemlite_ts'
              CALL gemlite_gltts_wrapper
              ! Write summed <datm>, <docn> anomolies to <genie_atm1>,
              ! <genie_ocn>
              ! re-set <datm_sum>, <docn_sum>
              IF (debug_loop > 3) PRINT *, '* cpl_comp_gltgem'
              CALL cpl_comp_gltgem_dsum_wrapper
              ! apply <genie_atm> compositional anomoly to <atm>
              ! re-set <genie_atm1>
              IF (debug_loop > 3) PRINT *, '* cpl_comp_gematm'
              CALL cpl_comp_gematm_wrapper
              ! update <sfcatm1> ocean interface array
              IF (debug_loop > 3) PRINT *, '* cpl_comp_atmocn_wrapper'
              CALL cpl_comp_atmocn_wrapper
              ! reset cumulative (annual average) weathering array
              ! (<dum_sfxsumrok1_gem>)
              ! + atm fluxes (outgassing, weathering CO2 consumption)
              CALL reinit_flux_rokocn_gem_wrapper
              CALL reinit_flux_rokatm_gem_wrapper

              ! TRIP CYCLE PHASE CHANGE
              ! Ensure that the experiment ends on a 'normal' year in order that
              ! restarts (esp. climate modules) are saved
              ! => when remaining time being less than 2 years,
              ! force istep_gem to the end of a cycle (and normal phase start)
              ! NOTE: also ensure that we are not already in a 'normal' phase
           ELSE IF (koverall_total-koverall <= 2 * kgemlite * kocn_loop .AND. &
                &   koverall_total-koverall > kgemlite * kocn_loop .AND. &
                &   istep_gem > gem_notyr) THEN
              IF (debug_loop > 0) &
                   & PRINT *, '### FLAG SWITCH TO NORMAL FOR END -> $$$'
              istep_gem = gem_notyr+gem_yr
           ELSE
              ! *** NOTHING! ***
              ! NO NEED TO SWITCH CYCLE PHASE => keep calm and pony on
           END IF
#ifdef INTEL_PROFILE
           call itt_profile_end()
#endif
        END IF
     END IF

     ! Test for being within 1st part of cycle
     IF (istep_gem <= gem_notyr .OR. .NOT. flag_gemlite) THEN
        ! START NORMAL
#ifdef INTEL_PROFILE
        call itt_profile_begin(task_normal)
#endif
        gem_status = 0
        IF (flag_gemlite .AND. istep_gem == gem_notyr) gem_switch = 1
        IF (flag_gemlite) THEN
           IF (MOD(koverall, kgemlite * kocn_loop) == 0) THEN
              IF (debug_loop > 1) &
                   & PRINT *, 'gem_status, gem_switch :', gem_status, gem_switch
           END IF
        END IF

        ! *** c-GOLDSTEIN surface flux routine
        IF (flag_ebatmos .AND. flag_goldsteinocean) THEN
           ! The next line should read " == 1" so that c-GOLDSTEIN
           ! surflux is executed on the first time-step
           IF (MOD(koverall,kocn_loop) == 1) THEN
#ifdef INTEL_PROFILE
        call itt_profile_begin(task_surflux_wrapper)
#endif
              IF (debug_loop > 2) PRINT *, '>>> SURFLUX @ ', koverall, kocn_loop
              istep_ocn = istep_ocn + 1
              CALL surflux_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
#ifdef INTEL_PROFILE
           call itt_profile_end()
#endif
           END IF
        END IF

        ! *** EMBM atmosphere model
        IF (flag_ebatmos) THEN
           IF (MOD(koverall, katm_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> EMBM @ ', koverall, katm_loop
              istep_atm = istep_atm + 1
              CALL embm_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
           END IF
        END IF

        ! *** c-GOLDSTEIN sea-ice model
        IF (flag_goldsteinseaice) THEN
           IF (MOD(koverall, ksic_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> GOLDICE @ ', koverall, ksic_loop
              istep_sic = istep_sic + 1
              CALL gold_seaice_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
           END IF
        END IF

        ! *** GOLDSTEIN ocean model
        IF (flag_goldsteinocean) THEN
           IF (MOD(koverall, kocn_loop) == 0) THEN
              IF (debug_loop > 2) &
                   & PRINT *, '>>> GOLDSTEIN @ ', koverall, kocn_loop
              CALL goldstein_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
           END IF
        END IF

        ! *** ENTS land model
        IF (flag_ents) THEN
           IF (MOD(koverall, klnd_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> ENTS @ ', koverall, klnd_loop
              CALL ents_wrapper
              CALL cpl_flux_lndatm_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
           END IF
        END IF

        ! *** ROKGEM model - UPDATE
        IF (flag_rokgem) THEN
           IF (MOD(koverall, conv_kocn_krokgem * kocn_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> ROKEGM @ ', &
                   & koverall, conv_kocn_krokgem * kocn_loop
              CALL rokgem_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
              ! If GEMlite: test for the end of the 1st (normal) part of cycle
              ! => create annual averages of weathering <dum_sfxsumrok1_gem>
              ! + atm exchnage (outgassing and weathering CO2 consumption)
              IF (flag_gemlite) THEN
                 IF (istep_gem == gem_notyr) THEN
                    IF (debug_loop > 3) PRINT *, '* cpl_flux_rokgem'
                    CALL cpl_flux_rokatm_gem_wrapper
                    CALL cpl_flux_rokocn_gem_wrapper
                 END IF
              END IF
              ! Couple ocean and atmosphere ROKGEM fluxes
              ! NOTE: carry out after possible pre-GEMlite integration step,
              ! as fluxes passed from ROKGEM are re-set ...
              IF (debug_loop > 3) PRINT *, '* cpl_flux_rokatm_wrapper'
              CALL cpl_flux_rokatm_wrapper
              IF (debug_loop > 3) PRINT *, '* cpl_flux_rokocn_wrapper'
              CALL cpl_flux_rokocn_wrapper
           END IF
        END IF

        ! *** BIOGEM model - UPDATE
        ! NOTE: CALL biogem_climate_wrapper on the very first BIOGEM
        ! time-step in order to initialize solar radiation to biota
        ! NOTE: the calls have to be in this order (phy update after biogem)
        ! so that BIOGEM is normally working with physics matching
        ! the trace field (in terms of last time-step update)
        ! NOTE: if the first or last year of a NORMAL phase (cf. gem_switch)
        ! => force t-series save
        IF (flag_biogem) THEN
           IF (MOD(koverall, conv_kocn_kbiogem * kocn_loop) == 0) THEN
#ifdef INTEL_PROFILE
               call itt_profile_begin(task_biogem)
#endif
              IF (debug_loop > 2) PRINT *, '>>> BIOGEM @ ', &
                   & koverall, conv_kocn_kbiogem * kocn_loop
              ! THIS MOSTLY FIXES THE INITIAL LACK-OF-INSOLATION PROBLEM,
              ! BUT NEEDS BIOGEM TEST REPLACING (AND *CARE* TAKEN)
              IF (koverall == conv_kocn_kbiogem * kocn_loop) &
                   & CALL biogem_climate_sol_wrapper
              CALL biogem_forcing_wrapper
#ifdef INTEL_PROFILE
               call itt_profile_begin(task_biogem_wrapper)
#endif
              CALL biogem_wrapper
#ifdef INTEL_PROFILE
               call itt_profile_end()
               call itt_profile_begin(task_biogem_tracercoupling_wrapper)
#endif

              CALL biogem_tracercoupling_wrapper
#ifdef INTEL_PROFILE
              call itt_profile_end()
              call itt_profile_begin(task_biogem_climate_wrapper)
#endif

              IF (debug_loop > 2) PRINT *, '<<<'

              CALL biogem_climate_wrapper

#ifdef INTEL_PROFILE
              call itt_profile_end()
              call itt_profile_begin(task_diag_biogem_timeslice_wrapper)
#endif

              CALL diag_biogem_timeslice_wrapper
#ifdef INTEL_PROFILE
              call itt_profile_end()
              call itt_profile_begin(task_diag_biogem_timeseries_wrapper)
#endif

              IF (flag_gemlite .AND. (gem_switch.ne.0)) THEN
                 CALL diag_biogem_force_timeseries_wrapper
              ELSE
                 CALL diag_biogem_timeseries_wrapper
              end if

#ifdef INTEL_PROFILE
               call itt_profile_end()
               call itt_profile_begin(task_cpl_flux_ocnatm_wrapper)
#endif
              CALL cpl_flux_ocnatm_wrapper
#ifdef INTEL_PROFILE
               call itt_profile_end()
               call itt_profile_begin(task_cpl_flux_ocnsed_wrapper)
#endif
              CALL cpl_flux_ocnsed_wrapper
#ifdef INTEL_PROFILE
               call itt_profile_end()
               call itt_profile_begin(task_cpl_comp_ocnsed_wrapper)
#endif
              CALL cpl_comp_ocnsed_wrapper
#ifdef INTEL_PROFILE
               call itt_profile_end()
               call itt_profile_begin(task_reinit_flux_rokocn_wrapper)
#endif
              CALL reinit_flux_rokocn_wrapper
#ifdef INTEL_PROFILE
               call itt_profile_end()
#endif
              IF (debug_loop > 0) CALL diag_biogem_wrapper
              ! If GEMlite: test for the end of the 1st (normal) part
              ! of cycle => create annual averages of <ocn> (for later
              ! transfer to GEMlite)
              IF (flag_gemlite .AND. istep_gem == gem_notyr) THEN
                 IF (debug_loop > 3) PRINT *, '* cpl_comp_ocngem'
                 CALL cpl_comp_ocngem_wrapper
              END IF
#ifdef INTEL_PROFILE
              call itt_profile_end() ! end normal
#endif
           END IF
        END IF

        ! *** ATCHEM model - UPDATE
        IF (flag_atchem) THEN
           IF (MOD(koverall, conv_kocn_katchem * kocn_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> ATCHEM @ ', &
                   & koverall, conv_kocn_katchem * kocn_loop
              CALL atchem_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
              IF (debug_loop > 3) PRINT *, '* cpl_comp_atmocn_wrapper'
              CALL cpl_comp_atmocn_wrapper
              CALL cpl_comp_EMBM_wrapper
              CALL cpl_comp_atmlnd_wrapper
              CALL cpl_comp_lndEMBM_wrapper
              ! test for the end of the 1st part of cycle
              ! create annual averages of <atm> (for later transfer to GEMlite)
              IF (istep_gem == gem_notyr) THEN
                 IF (debug_loop > 3) PRINT *, '* cpl_comp_atmgem'
                 CALL cpl_comp_atmgem_wrapper
              END IF
           END IF
        END IF

        ! ADAPT CYCLE STEP
        ! Extend duration of 'normal' cycle if rate of pCO2 change
        ! is above the allowed threshold
        IF (flag_biogem .AND. gem_adapt_auto) THEN
           IF (MOD(koverall, kgemlite * kocn_loop) == 0) THEN
#ifdef INTEL_PROFILE
    call itt_profile_begin(task_biogem_adapt)
#endif
              ! update (@ current time-step, not annual average) value
              ! of gem_pCO2
              CALL diag_biogem_pCO2_wrapper
              ! NOTE: test for excessive rate of pCO2 change only
              ! after first year to avoid reverting to istep_gem = 0
              ! state
              ! NOTE: also avoid last year to allow the end of a NORMAL cycle
              ! to be cleanly predicted (at the start fo the last N cycle)
              IF (istep_gem > 1 .AND. istep_gem < gem_notyr) THEN
                 IF (ABS(gem_pCO2 - gem_pCO2_OLD) > gem_adapt_dpCO2dt) THEN
                    ! decrement cycle phase counter to prolongue
                    ! operation in full (non-GEM) time-stepping cycle
                    ! phase
                    istep_gem = istep_gem - 1
                    IF (debug_loop > 1) &
                         & PRINT *, '$$$ NORMAL phase extended:', istep_gem, &
                         & ' / dpCO2 (ppm) = ', ABS(gem_pCO2 - gem_pCO2_OLD)
                    ! copy current state of tracer arrays to GEMlite
                    ! NOTE: arrays don't actually need copying yet,
                    ! but it wil serve to reset the cumulative annual
                    ! average arrays (now that the Normal phase has
                    ! been extended another yr)
                    IF (debug_loop > 3) PRINT *, '* cpl_comp_gemglt'
                    CALL cpl_comp_gemglt_wrapper
                 END IF
              END IF
              ! save current value of gem_pCO2 as gem_pCO2_OLD
              ! for calculating rate of change of pCO2 between GEM (year) steps
              gem_pCO2_OLD = gem_pCO2
#ifdef INTEL_PROFILE
    call itt_profile_end()
#endif
           END IF
        END IF

        ! *** SEDGEM model - UPDATE
        IF (flag_sedgem) THEN
           IF (MOD(koverall, conv_kocn_ksedgem * kocn_loop) == 0) THEN
              IF (istep_gem /= gem_notyr) THEN
                 IF (debug_loop > 2) PRINT *, '>>> SEDGEM @ ', &
                      & koverall, conv_kocn_ksedgem * kocn_loop
                 CALL sedgem_wrapper
                 IF (debug_loop > 2) PRINT *, '<<<'
              ELSE
                 IF (debug_loop > 2) PRINT *, '>>> SEDEGM_GEM @ ', &
                      & koverall, conv_kocn_ksedgem * kocn_loop
                 CALL sedgem_glt_wrapper
                 IF (debug_loop > 2) PRINT *, '<<<'
              END IF
              CALL cpl_flux_sedocn_wrapper
              CALL cpl_comp_sedocn_wrapper
           END IF
        END IF

        IF (flag_gemlite) THEN
           IF (istep_gem > 1 .AND. istep_gem < gem_notyr) gem_switch = 0
        END IF
#ifdef INTEL_PROFILE
        call itt_profile_end()
#endif
        ! END NORMAL
     ELSE
        ! START GEMLITE
#ifdef INTEL_PROFILE
        call itt_profile_begin(task_gemlite2)
#endif
        gem_status = 1
        gem_switch = 0
        IF (flag_gemlite) THEN
           IF (MOD(koverall, kgemlite * kocn_loop) == 0) THEN
              IF (debug_loop > 1) PRINT *, 'gem_status, gem_switch :', &
                   & gem_status, gem_switch
           END IF
        END IF

        ! *** EMBM - UPDATE
        IF (flag_ebatmos) THEN
           IF (MOD(koverall, katm_loop) == 0) istep_atm = istep_atm + 1
        END IF

        ! *** c-GOLDSTEIN sea-ice - UPDATE
        IF (flag_goldsteinseaice) THEN
           IF (MOD(koverall, ksic_loop) == 0) istep_sic = istep_sic + 1
        END IF

        ! *** c-GOLDSTEIN - UPDATE
        IF (flag_ebatmos) THEN
           IF (MOD(koverall, kocn_loop) == 1) istep_ocn = istep_ocn + 1
        END IF

        ! *** ROKGEM model - UPDATE (GEMlite)
        IF (flag_rokgem) THEN
           IF (MOD(koverall, conv_kocn_krokgem * kocn_loop) == 0) THEN
              ! nothing(!)
              ! NOTE: ROKGEM not called so as to preserve the annual mean
              ! of what might be a highly seasonal weathering flux
              ! NOTE: in any case, climate is invarient (and so too weathering)
              ! NOTE: removal of tracers from the atmosphere in a non
              ! 'short-cut' situation is not currently handled
              ! CALL cpl_flux_rokatm_wrapper
           END IF
        END IF

        ! *** BIOGEM-GEMlite model - UPDATE
        ! NOTE: *DO not* update ATCHEM <atm> until end of GEMLITE phase
        ! (interface values *are* updated for runtime reporting)
        IF (flag_gemlite) THEN
           IF (MOD(koverall, kgemlite * kocn_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> GEMLITE @ ', &
                   & koverall, kgemlite * kocn_loop
              CALL gemlite_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
              ! write <datm>, <docn> anomolies to <genie_atm>, <genie_ocn>
              IF (debug_loop > 2) PRINT *, '* cpl_comp_gltgem'
              CALL cpl_comp_gltgem_d_wrapper
              ! increment <sfcatm1> array with <genie_datm1>
              ! => needed for reporting current <sfcatm1> from BIOGEM
              CALL cpl_comp_gematm1_wrapper
              ! update biogem <ocn> array from <ocn> + <genie_docn>
              ! => needed for reporting current <ocn> from BIOGEM
              CALL cpl_comp_gemocn_wrapper
              ! convert <sfcocn1> (ocn grid) -> <sfcsumocn> (sed grid)
              ! => needed by SEDGEM for update
              ! NOTE: no time-sveraging (GEMLITE, SEDGEM both @ 1yr time-steps)
              CALL cpl_comp_ocnsed_gem_wrapper
           END IF
        END IF
        ! Update BIOGEM diagnostics time points
        ! NOTE: failure to DO this risks BIOGEM losing track of
        ! which time-point to save time-series and slices at
        ! NOTE: BUT ... you will not get an exactly eqilvilant e.g.
        ! annual average as GEMlite onyl updates at the very END
        ! of its (yearly) time-step
        ! (=> you will effectively get the start-of-year value
        ! (repeated each time-step over the year)
        ! rather than the yearly mid-point (or average))
        ! i.e. ... the time-series is out of phase during the GEM segment
        ! NOTE: also update BIOGEM forcing time-series [biogem_forcing]
        ! (even though forcings are not applied in the GEMlite phase)
        ! (although ... sub_update_sig will find the correct point
        ! in the forcing time-series regardless ...)
        IF (flag_biogem) THEN
           IF (MOD(koverall, conv_kocn_kbiogem * kocn_loop) == 0) THEN
              IF (debug_loop > 2) PRINT *, '>>> diag_biogem_gem @ ', &
                   & koverall, conv_kocn_kbiogem * kocn_loop
              ! NOTE: the namelist parameter *gem_adapt_diag_biogem_full*
              ! (an input parameter passed in the diag_biogem loop wrappers)
              ! sets whether to update the current time point alone,
              ! or also calculate a full time-series and slice average and save
              CALL biogem_forcing_wrapper
              CALL diag_biogem_gem_timeslice_wrapper
              CALL diag_biogem_gem_timeseries_wrapper
              IF (debug_loop > 1) CALL diag_biogem_gem_wrapper
              IF (debug_loop > 2) PRINT *, '<<<'
           END IF
        END IF
        ! ADAPT CYCLE STEP
        ! extend GEM cycle phase if the cumulative DpCO2 change
        ! remains below the specified threshold
        IF (flag_gemlite) THEN
           IF (MOD(koverall, kgemlite * kocn_loop) == 0) THEN
              ! update (@ current time-step, not annual average) value
              ! of gem_pCO2
              CALL diag_biogem_pCO2_wrapper
              IF (gem_adapt_auto) THEN
                 ! if first GEM step of sequence => set initial pCO2 value
                 ! if not => test cumulative DpCO2 change for
                 ! excessive pCO2 drift has occurred
                 IF (istep_gem == gem_notyr + 1) THEN
                    gem_pCO2_INIT = gem_pCO2
                    IF (debug_loop > 1) &
                         & PRINT *, '### SETTING: gem_pCO2_INIT', gem_pCO2_INIT
                 ELSE
                    IF (debug_loop > 1) &
                         & PRINT *, '### CURRENT: gem_pCO2', gem_pCO2
                    IF (ABS(gem_pCO2_INIT - gem_pCO2) < gem_adapt_DpCO2) THEN
                       ! decrement cycle phase counter to prolongue
                       ! operation in accelerated (GEM) time-stepping
                       ! cycle phase but ONLY if selected (some risk
                       ! of spurious stuff happening if staying in the
                       ! GEM phase for ever ...)
                       IF (gem_adapt_auto_unlimitedGEM) THEN
                          istep_gem = istep_gem - 1
                          IF (debug_loop > 1) &
                               & PRINT *, '### GEM phase extended :', &
                               & istep_gem, ' / SUM(DpCO2) (ppm) =', &
                               & ABS(gem_pCO2_INIT - gem_pCO2)
                       END IF
                    ELSE
                       IF (istep_gem < gem_notyr + gem_yr - 1) THEN
                          IF (debug_loop > 0) PRINT *, '### EXCESS DpCO2 => $$$'
                          istep_gem = gem_notyr + gem_yr - 1
                       END IF
                    END IF
                 END IF
              END IF
           END IF
        END IF

        ! *** ATCHEM model - UPDATE (GEMlite)
        IF (flag_atchem) THEN
           IF (MOD(koverall, conv_kocn_katchem * kocn_loop) == 0) THEN
              ! DO NOTHING! (currently)
           END IF
        END IF

        ! *** SEDGEM model - UPDATE (GEMlite)
        IF (flag_sedgem) THEN
           IF (MOD(koverall, conv_kocn_ksedgem * kocn_loop) == 0) THEN
              ! Force (CaCO3) sediment aging
              ! (in the absence of BIOGEM sedimentation age updating)
              ! NOTE: strictly, upon the very first time this is called,
              ! only 0.5 years should be subtracted
              ! (cf. mid-point average created by summing BIOGEM output)
              CALL sedgem_dsedage_wrapper
              ! main SEDGEM routine
              IF (istep_gem == gem_notyr + gem_yr) THEN
                 IF (debug_loop > 2) PRINT *, '>>> SEDEGM @ ', &
                      & koverall, conv_kocn_ksedgem * kocn_loop
                 CALL sedgem_wrapper
                 IF (debug_loop > 2) PRINT *, '<<<'
              ELSE
                 IF (debug_loop > 2) PRINT *, '>>> SEDEGM_GLT @ ', &
                      & koverall, conv_kocn_ksedgem * kocn_loop
                 CALL sedgem_glt_wrapper
                 IF (debug_loop > 2) PRINT *, '<<<'
              END IF
              ! coupling routines
              CALL cpl_flux_sedocn_wrapper
              CALL cpl_comp_sedocn_wrapper
           END IF
        END IF
        ! END GEMLITE
#ifdef INTEL_PROFILE
        call itt_profile_end()
#endif
     END IF
#ifdef INTEL_PROFILE
     call itt_profile_end()
#endif

  END DO
  !    *** MAIN TIME-STEPPING LOOP END ***


  ! *** BIOGEOCHEM RUN-END RESTART SAVE ***

  ! *** BIOGEM model - RESTARTS
  IF (flag_biogem) THEN
     IF (debug_loop > 1) PRINT *, '>>> SAVING BIOGEM RESTART <<<'
     CALL biogem_save_restart_wrapper
  END IF

  ! *** ATCHEM model - RESTARTS
  IF (flag_atchem) THEN
     CALL atchem_wrapper
     IF (debug_loop > 1) PRINT *, '>>> SAVING ATCHEM RESTART <<<'
     CALL atchem_save_restart_wrapper
  END IF

  ! *** SEDGEM model - RESTARTS
  IF (flag_sedgem) THEN
     IF (debug_loop > 1) PRINT *, '>>> SAVING SEDEGM RESTART <<<'
     CALL sedgem_save_restart_wrapper
  END IF

  ! *** ROKGEM model - RESTARTS
  IF (flag_rokgem) THEN
     IF (debug_loop > 1) PRINT *, '>>> SAVING ROKGEM RESTART <<<'
     CALL rokgem_save_restart_wrapper
  END IF


  ! *** SHUTDOWN ***

  PRINT *
  PRINT *, '*******************************************************'
  PRINT *, ' *** Simulation complete: shutdown starting ...'
  PRINT *, '*******************************************************'
  PRINT *

  IF (flag_goldsteinocean) CALL end_goldstein_wrapper
  IF (flag_ebatmos) CALL end_embm_wrapper
  IF (flag_goldsteinseaice) CALL end_seaice_wrapper
  IF (flag_biogem) CALL end_biogem_wrapper
  IF (flag_atchem) CALL end_atchem_wrapper
  IF (flag_sedgem) CALL end_sedgem_wrapper
  IF (flag_rokgem) CALL end_rokgem_wrapper
  IF (flag_gemlite) CALL end_gemlite_wrapper
  IF (flag_atchem .OR. flag_biogem .OR. flag_sedgem .OR. flag_rokgem) &
       & CALL end_gem_wrapper
  CALL end_genie
  ! ==================================================================

  PRINT *
  PRINT *, '*******************************************************'
  PRINT *, ' *** Shutdown complete; home time'
  PRINT *, '*******************************************************'
  PRINT *
  CALL write_status('COMPLETE')
  call CPU_TIME(cpu_endtime)
  CALL SYSTEM_CLOCK(clock_now)

  print *, "system_clock : ",(clock_now - clock_starttime)/real(cr)
  print *, 'cpu_clock:', cpu_endtime - cpu_starttime

END PROGRAM GENIE

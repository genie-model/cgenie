!--------------------------------------------------------
!
! File: genie_global.f90
!
! Description:
!
! This Fortran90 module contains the top-level variables
! and routines for the GENIE framework.
!--------------------------------------------------------

MODULE genie_global

  USE genie_control
  USE gem_cmn, ONLY: n_atm_all
  IMPLICIT NONE

  ! STANDARD VARAIBLE NAMES
  ! surf_*_??? = surface propoerties defined over the entire global surface
  ! land_*_??? = land-only surface properties
  ! ocean_*_??? = ocean-only (incl. seaice, i.e. non-land) surface properties
  ! seaice_*_??? = seaice-only surface propoerties
  ! landice_*_??? = landice-only surface properties
  ! atmos_*_??? = atmospheric properties over the entire global surface
  !
  ! *_*_atm = variables on the atmospheric grid
  ! *_*_ocn = variables on the ocean grid
  ! *_*_sic = variables on the seaice grid
  ! *_*_lic = variables on the landice grid
  ! *_*_lnd = variables on the land grid

  ! ============================================================
  ! Declarations
  ! ============================================================

  ! This is the overall timestep counter
  INTEGER(KIND=8) :: koverall
  INTEGER::katm

  INTEGER :: istep_atm, istep_sic, istep_ocn, istep_lic, istep_gsurf, istep_che
  INTEGER :: katmos, kgem
  INTEGER :: istep_gem, istep_tot
  INTEGER :: gem_yr, gem_yr_min, gem_yr_max
  INTEGER :: gem_notyr, gem_notyr_min, gem_notyr_max, gem_dyr
  INTEGER :: gem_status, gem_switch
  LOGICAL :: gem_adapt_auto, gem_adapt_auto_unlimitedGEM
  REAL :: gem_adapt_dpCO2dt, gem_adapt_DpCO2
  REAL :: gem_pCO2, gem_pCO2_INIT, gem_pCO2_OLD
  LOGICAL :: gem_adapt_diag_biogem_full
  INTEGER :: istep_bgm, go_npstp, go_iwstp, go_itstp, go_ianav

  REAL, DIMENSION(:), ALLOCATABLE :: &
       & alon1_atm, alat1_atm, alon1_ocn, alat1_ocn, alon1_sic, alat1_sic, &
       & aboxedge1_lon_atm, aboxedge1_lat_atm, &
       & aboxedge1_lon_ocn, aboxedge1_lat_ocn, &
       & aboxedge1_lon_sic, aboxedge1_lat_sic, &
       & alon2_atm, alat2_atm, alon2_ocn, alat2_ocn, alon2_sic, alat2_sic, &
       & aboxedge2_lon_atm, aboxedge2_lat_atm, &
       & aboxedge2_lon_ocn, aboxedge2_lat_ocn, &
       & aboxedge2_lon_sic, aboxedge2_lat_sic, &
       & alon3_atm, alat3_atm, alon3_ocn, alat3_ocn, alon3_sic, alat3_sic, &
       & aboxedge3_lon_atm, aboxedge3_lat_atm, &
       & aboxedge3_lon_ocn, aboxedge3_lat_ocn, &
       & aboxedge3_lon_sic, aboxedge3_lat_sic, &
       & depth1_ocn, depth2_ocn

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: &
       & ilandmask1_atm, ilandmask2_atm, ilandmask3_atm, &
       & ilandmask1_ocn, ilandmask2_ocn, ilandmask3_ocn, &
       & ilandmask1_sic, ilandmask2_sic, ilandmask3_sic

  ! These are the fluxes which come out of the atmosphere, *_atm
  ! and those which go into goldstein after interpolation onto the
  ! goldstein spatial grid, *_ocn.
  REAL, DIMENSION(:,:), ALLOCATABLE :: tstar_atm, netsolar_atm, netlong_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & tstar_ocn, latent_ocn, sensible_ocn, netsolar_ocn, netlong_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & surf_latent_atm, surf_sensible_atm, &
       & surf_stressx_atm, surf_stressy_atm, surf_evap_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: surf_stressx2_atm, surf_stressy2_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: surf_stressx3_atm, surf_stressy3_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: trest_ocn, srest_ocn

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & land_latent_atm, land_sensible_atm, land_stressx_atm, &
       & land_stressy_atm, land_evap_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & ocean_latent_atm, ocean_sensible_atm, ocean_stressx_atm, &
       & ocean_stressy_atm, ocean_evap_atm
  ! BEWARE!!! I HAVE CHANGED THIS A BIT....
  ! WHY WAS RUNOFF_OCN ON THE ATM GRID??!!

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & ocean_lowestlu2_ocn, ocean_lowestlu3_ocn, &
       & ocean_lowestlv2_ocn, ocean_lowestlv3_ocn, &
       & ocean_stressx2_ocn, ocean_stressx3_ocn, &
       & ocean_stressy2_ocn, ocean_stressy3_ocn
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: stressx_ocn, stressy_ocn

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & surf_orog_atm, zonwind_atm, merwind_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: runoff_ocn
  ! Runoff on land before routing to ocean, for use in rokgem
  REAL, DIMENSION(:,:), ALLOCATABLE :: runoff_land
  REAL, DIMENSION(:,:), ALLOCATABLE :: runoff_sic

  ! For the seaice on the goldstein grid....
  REAL, DIMENSION(:,:), ALLOCATABLE :: seaicefrac_ocn

  ! This variable is so that the stressx_atm and stressy_atm for
  ! goldstein can be passed sensibly.  The atmos one is a dummy
  ! variable for returning the temp and albedo
  REAL, DIMENSION(:,:), ALLOCATABLE :: dummy_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: dummy_atm

  ! Need a variable for the interp mask
  REAL, DIMENSION(:,:), ALLOCATABLE :: interpmask_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: interpmask_ocn

  REAL :: weighttot_atm, weighttot_ocn

  ! Extra fluxes for new c-GOLDSTEIN modules
  REAL, DIMENSION(:,:), ALLOCATABLE :: evap_ocn, precip_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: evap_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: evap_sic, precip_atm, precip_sic
  REAL, DIMENSION(:,:), ALLOCATABLE :: ustar_ocn, vstar_ocn, sstar_ocn

  ! Extra fields for c-GOLDSTEIN sea-ice module
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & hght_sic, frac_sic, temp_sic, albd_sic, dhght_sic, dfrac_sic
  REAL, DIMENSION(:,:), ALLOCATABLE :: waterflux_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: waterflux_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: waterflux_sic

  ! CO2 concentration field for c-GOLDSTEIN surface flux routine -
  ! will be unnecessary in final version of genie.F (when AtCheM is a
  ! genie.F level routine), but necessary at present
  ! Added methane and N2O.
  ! Also now essential for use with the igcm.
  ! With the igcm, these gases are given by genie-fixedchem.
  REAL, DIMENSION(:,:), ALLOCATABLE :: co2_atm, n2o_atm, ch4_atm

  ! 14co2 used in igcm and ichem modules
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: mass14co2, ddtmass14co2, massair

  ! Parameters storing the information about the ocean basins
  ! that needs to be passed from GOLDSTEIN to the EMBM
  INTEGER, DIMENSION(:), ALLOCATABLE :: ips_out, ipf_out, ias_out, iaf_out
  INTEGER jsf_out

  ! Extra fields of "average" ocean cell temperature and roughness
  REAL, DIMENSION(:,:), ALLOCATABLE :: tavg_ocn, rough_ocn

  ! These are the fluxes out of the atmosphere once they have been
  ! averaged onto the seaice (*_atm_meanseaice) or ocean
  ! (*_atm_meanocn) timestep.
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & latent_atm_meanocn, latent_atm_meansic, sensible_atm_meanocn, &
       & sensible_atm_meansic, netsolar_atm_meanocn, netsolar_atm_meansic, &
       & netlong_atm_meanocn, netlong_atm_meansic, stressx_atm_meanocn, &
       & stressx_atm_meansic, stressy_atm_meanocn, stressy_atm_meansic, &
       & precip_atm_meanocn, precip_atm_meansic, evap_atm_meanocn, &
       & evap_atm_meansic

  ! Extra meanocn fields for GOLDOCN-GOLDSIC surflux
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & lowestlt_atm_meanocn, lowestlq_atm_meanocn, lowestlp_atm_meanocn, &
       & lowestlh_atm_meanocn, lowestlu_atm_meanocn, lowestlv_atm_meanocn

  ! EXTRA FIELDS:
  REAL, DIMENSION(:,:), ALLOCATABLE :: runoff_atm_meanocn

  ! These are the fluxes out of the seaice once they have been
  ! averaged onto the ocean
  ! (*_atm_meanocn) timestep.
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & seaicefrac_atm_meanocn, conductflux_atm_meanocn, waterflux_atm_meanocn

  ! This is the sea-ice fraction (=1 or 0 fo the slab case)
  ! Should REALly be renamed seaicefrac_seaice seen as it's on the
  ! seaice grid (this just happens to be the same as the atmos grid
  ! at the moment)
  REAL, DIMENSION(:,:), ALLOCATABLE :: seaicefrac_atm, conductflux_atm

  ! These are the carry-over from ocean to seaice (*_ocn_seaice)
  ! or from seaice to ocean (*_seaice_ocn)
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & dtcarry_ocn_sic, energycarry_ocn_sic, energycarry_sic_ocn

  ! This is the albedo.  In the slab and fixed case, it is calculated
  ! in the ocean or seaice modules.  It could be calculated by the
  ! atmosphere instead, in which case it would not need to be in
  ! genie.f.
  REAL, DIMENSION(:,:), ALLOCATABLE :: albedo_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: albedo_ocn

  ! This is the temperture of the uppermost layer of the ocean,
  ! and its thickness.  This is output from the ocean, and
  ! used in the seaice module to calculate the ocean-seaice heat flux.
  REAL, DIMENSION(:,:), ALLOCATABLE :: temptop_atm, thicktop_atm

  ! Extra files
  REAL, DIMENSION (:,:), ALLOCATABLE :: conductflux_sic
  REAL, DIMENSION (:,:), ALLOCATABLE :: conductflux_ocn
  REAL, DIMENSION (:,:), ALLOCATABLE :: seaicefrac_sic

  REAL, DIMENSION(:,:), ALLOCATABLE :: atmos_lowestlu_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: atmos_lowestlu2_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: atmos_lowestlv3_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & atmos_lowestlv_atm, atmos_lowestlq_atm, atmos_lowestlt_atm, &
       & atmos_lowestlp_atm, atmos_lowestlh_atm

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & land_lowestlu_atm, land_lowestlv_atm, &
       & land_lowestlq_atm, land_lowestlt_atm

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & ocean_lowestlu_atm, ocean_lowestlv_atm, &
       & ocean_lowestlq_atm, ocean_lowestlt_atm, &
       & ocean_lowestlp_atm, ocean_lowestlh_atm

  REAL :: surfsigma, surfdsigma
  REAL, DIMENSION(:), ALLOCATABLE :: psigma

  ! For the fixedatmos grid.  1=igcm, 2=goldstein
  INTEGER :: grid_type_fixedatmos

  ! Inputs to c-GOLDSTEIN surflux routines
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & ocean_lowestlt_ocn, ocean_lowestlq_ocn, ocean_lowestlp_ocn, &
       & ocean_lowestlh_ocn, ocean_atm_netsolar_ocn, ocean_atm_netlong_ocn

  ! Outputs from c-GOLDSTEIN surflux routines
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & ocean_latent_ocn, ocean_sensible_ocn, ocean_netsolar_ocn, &
       & ocean_netlong_ocn, ocean_evap_ocn, ocean_precip_ocn, ocean_runoff_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & atmos_latent_ocn, atmos_sensible_ocn, atmos_netsolar_ocn, &
       & atmos_netlong_ocn, atmos_evap_ocn, atmos_precip_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & atmos_latent_atm, atmos_sensible_atm, atmos_netsolar_atm, &
       & atmos_netlong_atm, atmos_evap_atm, atmos_precip_atm

  ! GOLDOCN-GOLDSIC surflux requires information about average ocean
  ! cell temperature and albedo.  Also, temporary variables needed
  ! after goldstein.F call to feed either ocean-only or ocean+sea-ice
  ! average fields of temperature and albedo out (i.e. slab sea-ice
  ! vs. GOLDSTEIN sea-ice)

  REAL, DIMENSION(:,:), ALLOCATABLE :: albavg_ocn
  REAL, DIMENSION(:,:), ALLOCATABLE :: weight_ocn

  REAL, DIMENSION(:,:), ALLOCATABLE :: surf_qstar_atm

  REAL :: atmos_dt_tim
  REAL :: test_energy_seaice, test_energy_ocean
  REAL :: test_water_seaice, test_water_ocean, test_water_land

  ! Temporary genie-land variable
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & tstar_gb_land, albedo_land, evap_land, fx_le_land, fx_sen_land, &
       & fx_momx_land, fx_momy_land, land_fxco2_atm, ice_icefrac_atm, &
       & land_tice_ice, land_albice_ice

  ! For temporary water-fix.  To be removed.....
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & evap_save1, evap_save2, late_save1, late_save2, sens_save1, sens_save2

  ! This is for the standard genie timestep.
  ! It is the length in seconds of one timestep
  REAL :: genie_timestep

  ! Running total of elapsed time (SECONDS)
  ! An 8-byte INTEGER can count up to 9.2^18
  INTEGER(KIND=8) :: genie_clock

  ! Verbosity level for logging and messages to screen
  ! 0: silent running, 1: basic, 2: chatty, 3: debug
  ! Value is set in genie_control_nml
  INTEGER :: verbosity

  ! solar constant
  REAL :: genie_solar_constant

  ! genie-biogem variables:
  ! This will not be hard-wired soon!!!!!!
  ! Also, to be renamed!!
  REAL :: go_dt, go_saln0, go_rhoair=1.25, go_cd=1.3E-3, go_dphi, go_usc
  REAL, DIMENSION(:), ALLOCATABLE :: go_ds
  ! parameter(go_tsc=go_rsc/go_usc)
  REAL :: go_dsc=5.0E3, go_fsc=2*7.2921E-5, go_rh0sc=1.0E3
  REAL :: go_rhosc, go_cpsc, go_solconst, go_scf
  INTEGER, DIMENSION(:), ALLOCATABLE :: go_ips, go_ipf, go_ias, go_iaf
  INTEGER :: go_jsf
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: go_k1
  REAL, DIMENSION(:), ALLOCATABLE :: go_dz, go_dza
  REAL, DIMENSION(:), ALLOCATABLE :: go_c, go_cv, go_s, go_sv
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: go_ts, go_ts1
  REAL, DIMENSION(:,:), ALLOCATABLE :: go_cost
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: go_uvw
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: go_tau
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: eb_uv
  REAL, DIMENSION(:,:), ALLOCATABLE :: eb_usurf
  REAL, DIMENSION(:), ALLOCATABLE :: go_solfor
  REAL, DIMENSION(:,:), ALLOCATABLE :: go_fxsw
  REAL, DIMENSION(:,:), ALLOCATABLE :: go_mldta
  REAL, DIMENSION(:,:), ALLOCATABLE :: go_psi

  ! genie-ents variables
  INTEGER :: go_istep0, go_nyear
  REAL :: go_rsc, go_syr, eb_dphi, eb_rmax, eb_rdtdim
  REAL, DIMENSION(:,:), ALLOCATABLE :: eb_ca
  CHARACTER(LEN=13) :: go_lin
  REAL, DIMENSION(:,:), ALLOCATABLE :: ea_co2, ea_fxplw
  REAL, DIMENSION(5) :: go_ec
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: go_rho

  ! Variables for surflux_ents
  INTEGER, PARAMETER :: en_ntimes_max=50
  INTEGER :: en_t_orog, en_norog, en_orogsteps
  INTEGER :: en_t_lice, en_nlice, en_licesteps
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: en_orog_vect, en_lice_vect
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & eb_fx0a, eb_fx0o, eb_fxsen, eb_fxlw, eb_evap, eb_pptn, eb_relh
  REAL, DIMENSION(:,:), ALLOCATABLE :: torog_atm, albs_atm
  REAL, DIMENSION(:,:), ALLOCATABLE :: landice_slicemask_lic
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & land_albs_snow_lnd, land_albs_nosnow_lnd, land_snow_lnd, &
       & land_bcap_lnd, land_z0_lnd, land_temp_lnd, land_moisture_lnd

  ! Carbon variables from var_ents (for rokgem)
  ! prefix denotes module name - ENTS Land
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & el_leaf, el_respveg, el_respsoil, el_photo

  ! Ocean-atmosphere tracer interface arrays
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfcatm, &      ! atmosphere-surface tracer composition; atm grid
       & genie_sfxsumatm      ! atmosphere-surface fluxes; integrated, atm grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfcatm1, &     ! atmosphere-surface tracer composition; ocn grid
       & genie_sfxatm1        ! atmosphere-surface fluxes; ocn grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfcatm_lnd, &  ! atmosphere-surface tracer composition; lnd grid
       & genie_sfxatm_lnd     ! land-atmosphere fluxes; lnd grid

  ! Ocean-rock tracer interface arrays (for purposes of getting
  ! weathering flux from rockgem grid into biogem)
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfxrok         ! rock-surface(coastal ocean) fluxes; rok grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfxsumrok1, &  ! rock-surf.(coastal ocean); integrated ocn grid
       & genie_sfxsumrok1_gem ! (version of above for GEMlite)
  REAL,DIMENSION(:,:,:), ALLOCATABLE :: genie_sfxsumatm1_gem

  ! atmosphere-rock tracer interface arrays (for purposes of getting
  ! temp and runoff into rokgem)

  ! oecan-sediment tracer interface arrays
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfcsed, &  ! sed.-surface sediment composition; sed grid
       & genie_sfxsumsed  ! sed.-surface (ocn->sed) fluxes; integrated, sed grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfxsumsed1, & ! sed.-surface (ocn->sed) fluxes; integ., ocn grid
       & genie_sfcsed1, &    ! sediment-surface sediment composition; ocn grid
       & genie_sfxsed1       ! sediment-surface (ocn->sed) fluxes; ocn grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfcsumocn, & ! sed.-surface ocean tracer comp; integ., sed grid
       & genie_sfxocn       ! sediment-surface (sed->ocn) fluxes; sed grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & genie_sfxocn1, &   ! sediment-surface (sed->ocn) fluxes; ocn grid
       & genie_sfcocn1      ! sed.-surface ocean tracer composition; ocn grid

  ! Temporary tracer arrays (for passing to/from GEMlite)
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: genie_atm1   ! atmosphere tracers; ocn grid
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: genie_ocn  ! ocean tracers; ocn grid

  ! These are for whether we have a restart run or not
  LOGICAL :: lrestart_genie

#ifndef REV
#define REV UNKNOWN
#endif

  ! Version number for the model (nastiness is because of dealing with
  ! string preprocessor defines...)
  CHARACTER(LEN=*), PARAMETER :: genie_version='&
&REV&
&'

CONTAINS

  ! subroutine: increment_genie_clock
  ! The GENIE clock keeps track of elapsed model time
  ! (in s) which is available to all components.
  ! This routine increments the accumulated time by
  ! a 'GENIE timestep'
  SUBROUTINE increment_genie_clock
    IMPLICIT NONE
    ! genie_timestep is in MILISECONDS
    ! NOTE: this is to minimize numerical error (drift) when using
    ! time-steps that are not INTEGER number of seconds
    ! e.g. 100 (ocean) time-steps per year (remainder 0.2s)
    ! NOTE: this requires BIOGEM to each time divide this f*cker by
    ! 1000.0 ... :(
    genie_clock = genie_clock + NINT(1000.0 * genie_timestep)
  END SUBROUTINE increment_genie_clock

  ! getversion()
  ! Return the version of the model
  FUNCTION getversion()
    IMPLICIT NONE
    CHARACTER(LEN=256) :: getversion
    getversion = genie_version
  END FUNCTION getversion

  ! print a message and abort the program
  ! optional references to the line number and file of the error
  SUBROUTINE alloc_die(line, file)
    IMPLICIT NONE
    INTEGER,          INTENT(IN), OPTIONAL :: line
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file

    WRITE (6,*) "ERROR: Could not allocate memory"
    IF (PRESENT(line)) THEN
       WRITE (6,*) 'at line: ', line
    END IF
    IF (PRESENT(file)) THEN
       WRITE (6,*) 'in file: ', TRIM(file)
    END IF
    WRITE (6,*) 'stopping'
    CALL flush(6)
    STOP
  END SUBROUTINE alloc_die

  SUBROUTINE allocate_genie_global()
    IMPLICIT NONE

    INTEGER :: status

    ALLOCATE(alon1_atm(ilon1_atm),STAT=status)           ; alon1_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat1_atm(ilat1_atm),STAT=status)           ; alat1_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon1_ocn(ilon1_ocn),STAT=status)           ; alon1_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat1_ocn(ilat1_ocn),STAT=status)           ; alat1_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon1_sic(ilon1_sic),STAT=status)           ; alon1_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat1_sic(ilat1_sic),STAT=status)           ; alat1_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge1_lon_atm(ilon1_atm+1),STAT=status) ; aboxedge1_lon_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge1_lat_atm(ilat1_atm+1),STAT=status) ; aboxedge1_lat_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge1_lon_ocn(ilon1_ocn+1),STAT=status) ; aboxedge1_lon_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge1_lat_ocn(ilat1_ocn+1),STAT=status) ; aboxedge1_lat_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge1_lon_sic(ilon1_sic+1),STAT=status) ; aboxedge1_lon_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge1_lat_sic(ilat1_sic+1),STAT=status) ; aboxedge1_lat_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon2_atm(ilon2_atm),STAT=status)           ; alon2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat2_atm(ilat2_atm),STAT=status)           ; alat2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon2_ocn(ilon2_ocn),STAT=status)           ; alon2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat2_ocn(ilat2_ocn),STAT=status)           ; alat2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon2_sic(ilon2_sic),STAT=status)           ; alon2_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat2_sic(ilat2_sic),STAT=status)           ; alat2_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge2_lon_atm(ilon2_atm+1),STAT=status) ; aboxedge2_lon_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge2_lat_atm(ilat2_atm+1),STAT=status) ; aboxedge2_lat_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge2_lon_ocn(ilon2_ocn+1),STAT=status) ; aboxedge2_lon_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge2_lat_ocn(ilat2_ocn+1),STAT=status) ; aboxedge2_lat_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge2_lon_sic(ilon2_sic+1),STAT=status) ; aboxedge2_lon_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge2_lat_sic(ilat2_sic+1),STAT=status) ; aboxedge2_lat_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon3_atm(ilon3_atm),STAT=status)           ; alon3_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat3_atm(ilat3_atm),STAT=status)           ; alat3_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon3_ocn(ilon3_ocn),STAT=status)           ; alon3_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat3_ocn(ilat3_ocn),STAT=status)           ; alat3_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alon3_sic(ilon3_sic),STAT=status)           ; alon3_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(alat3_sic(ilat3_sic),STAT=status)           ; alat3_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge3_lon_atm(ilon3_atm+1),STAT=status) ; aboxedge3_lon_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge3_lat_atm(ilat3_atm+1),STAT=status) ; aboxedge3_lat_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge3_lon_ocn(ilon3_ocn+1),STAT=status) ; aboxedge3_lon_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge3_lat_ocn(ilat3_ocn+1),STAT=status) ; aboxedge3_lat_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge3_lon_sic(ilon3_sic+1),STAT=status) ; aboxedge3_lon_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(aboxedge3_lat_sic(ilat3_sic+1),STAT=status) ; aboxedge3_lat_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(depth1_ocn(inl1_ocn),STAT=status)           ; depth1_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(depth2_ocn(inl2_ocn),STAT=status)           ; depth2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(ilandmask1_atm(ilon1_atm,ilat1_atm),STAT=status) ; ilandmask1_atm = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask2_atm(ilon2_atm,ilat2_atm),STAT=status) ; ilandmask2_atm = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask3_atm(ilon3_atm,ilat3_atm),STAT=status) ; ilandmask3_atm = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask1_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; ilandmask1_ocn = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask2_ocn(ilon2_ocn,ilat2_ocn),STAT=status) ; ilandmask2_ocn = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask3_ocn(ilon3_ocn,ilat3_ocn),STAT=status) ; ilandmask3_ocn = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask1_sic(ilon1_sic,ilat1_sic),STAT=status) ; ilandmask1_sic = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask2_sic(ilon2_sic,ilat2_sic),STAT=status) ; ilandmask2_sic = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ilandmask3_sic(ilon3_sic,ilat3_sic),STAT=status) ; ilandmask3_sic = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(tstar_atm(ilon1_atm,ilat1_atm),STAT=status)         ; tstar_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netsolar_atm(ilon1_atm,ilat1_atm),STAT=status)      ; netsolar_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netlong_atm(ilon1_atm,ilat1_atm),STAT=status)       ; netlong_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(tstar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; tstar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(latent_ocn(ilon1_ocn,ilat1_ocn),STAT=status)        ; latent_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(sensible_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; sensible_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netsolar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; netsolar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netlong_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; netlong_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_latent_atm(ilon1_atm,ilat1_atm),STAT=status)   ; surf_latent_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_sensible_atm(ilon1_atm,ilat1_atm),STAT=status) ; surf_sensible_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_stressx_atm(ilon1_atm,ilat1_atm),STAT=status)  ; surf_stressx_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_stressy_atm(ilon1_atm,ilat1_atm),STAT=status)  ; surf_stressy_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_evap_atm(ilon1_atm,ilat1_atm),STAT=status)     ; surf_evap_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_stressx2_atm(ilon2_atm,ilat2_atm),STAT=status) ; surf_stressx2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_stressy2_atm(ilon2_atm,ilat2_atm),STAT=status) ; surf_stressy2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_stressx3_atm(ilon3_atm,ilat3_atm),STAT=status) ; surf_stressx3_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(surf_stressy3_atm(ilon3_atm,ilat3_atm),STAT=status) ; surf_stressy3_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(trest_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; trest_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(srest_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; srest_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(land_latent_atm(ilon1_atm,ilat1_atm),STAT=status)    ; land_latent_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_sensible_atm(ilon1_atm,ilat1_atm),STAT=status)  ; land_sensible_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_stressx_atm(ilon1_atm,ilat1_atm),STAT=status)   ; land_stressx_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_stressy_atm(ilon1_atm,ilat1_atm),STAT=status)   ; land_stressy_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_evap_atm(ilon1_atm,ilat1_atm),STAT=status)      ; land_evap_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_latent_atm(ilon1_atm,ilat1_atm),STAT=status)   ; ocean_latent_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_sensible_atm(ilon1_atm,ilat1_atm),STAT=status) ; ocean_sensible_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_stressx_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_stressx_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_stressy_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_stressy_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_evap_atm(ilon1_atm,ilat1_atm),STAT=status)     ; ocean_evap_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(ocean_lowestlu2_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; ocean_lowestlu2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlu3_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; ocean_lowestlu3_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlv2_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; ocean_lowestlv2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlv3_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; ocean_lowestlv3_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_stressx2_ocn(ilon1_ocn,ilat1_ocn),STAT=status)  ; ocean_stressx2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_stressx3_ocn(ilon1_ocn,ilat1_ocn),STAT=status)  ; ocean_stressx3_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_stressy2_ocn(ilon1_ocn,ilat1_ocn),STAT=status)  ; ocean_stressy2_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_stressy3_ocn(ilon1_ocn,ilat1_ocn),STAT=status)  ; ocean_stressy3_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(stressx_ocn(2,ilon1_ocn,ilat1_ocn),STAT=status)       ; stressx_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(stressy_ocn(2,ilon1_ocn,ilat1_ocn),STAT=status)       ; stressy_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(surf_orog_atm(ilon1_atm,ilat1_atm),STAT=status)  ; surf_orog_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(zonwind_atm(ilon1_atm,ilat1_atm),STAT=status)    ; zonwind_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(merwind_atm(ilon1_atm,ilat1_atm),STAT=status)    ; merwind_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(runoff_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; runoff_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(runoff_land(ilon1_ocn,ilat1_ocn),STAT=status)    ; runoff_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(runoff_sic(ilon1_atm,ilat1_atm),STAT=status)     ; runoff_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(seaicefrac_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; seaicefrac_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(dummy_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; dummy_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(dummy_atm(ilon1_atm,ilat1_atm),STAT=status)      ; dummy_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(interpmask_atm(ilon1_atm,ilat1_atm),STAT=status) ; interpmask_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(interpmask_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; interpmask_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; evap_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(precip_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; precip_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_atm(ilon1_atm,ilat1_atm),STAT=status)       ; evap_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_sic(ilon1_atm,ilat1_atm),STAT=status)       ; evap_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(precip_atm(ilon1_atm,ilat1_atm),STAT=status)     ; precip_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(precip_sic(ilon1_atm,ilat1_atm),STAT=status)     ; precip_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ustar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; ustar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(vstar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; vstar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(sstar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; sstar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(hght_sic(ilon1_sic,ilat1_sic),STAT=status)              ; hght_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(frac_sic(ilon1_sic,ilat1_sic),STAT=status)              ; frac_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(temp_sic(ilon1_sic,ilat1_sic),STAT=status)              ; temp_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(albd_sic(ilon1_sic,ilat1_sic),STAT=status)              ; albd_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(dhght_sic(ilon1_sic,ilat1_sic),STAT=status)             ; dhght_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(dfrac_sic(ilon1_sic,ilat1_sic),STAT=status)             ; dfrac_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(waterflux_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; waterflux_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(waterflux_atm(ilon1_atm,ilat1_atm),STAT=status)         ; waterflux_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(waterflux_sic(ilon1_sic,ilat1_sic),STAT=status)         ; waterflux_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(co2_atm(ilon1_atm,ilat1_atm),STAT=status)               ; co2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(n2o_atm(ilon1_atm,ilat1_atm),STAT=status)               ; n2o_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ch4_atm(ilon1_atm,ilat1_atm),STAT=status)               ; ch4_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(mass14co2(ilon1_atm,ilat1_atm,inl1_atm),STAT=status)    ; mass14co2 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ddtmass14co2(ilon1_atm,ilat1_atm,inl1_atm),STAT=status) ; ddtmass14co2 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(massair(ilon1_atm,ilat1_atm,inl1_atm),STAT=status)      ; massair = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(ips_out(ilat1_ocn),STAT=status) ; ips_out = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ipf_out(ilat1_ocn),STAT=status) ; ipf_out = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ias_out(ilat1_ocn),STAT=status) ; ias_out = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(iaf_out(ilat1_ocn),STAT=status) ; iaf_out = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(tavg_ocn(ilon1_ocn,ilat1_ocn),STAT=status)             ; tavg_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(rough_ocn(ilon1_ocn,ilat1_ocn),STAT=status)            ; rough_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(latent_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)   ; latent_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(latent_atm_meansic(ilon1_atm,ilat1_atm),STAT=status)   ; latent_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(sensible_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; sensible_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(sensible_atm_meansic(ilon1_atm,ilat1_atm),STAT=status) ; sensible_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netsolar_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; netsolar_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netsolar_atm_meansic(ilon1_atm,ilat1_atm),STAT=status) ; netsolar_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netlong_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)  ; netlong_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(netlong_atm_meansic(ilon1_atm,ilat1_atm),STAT=status)  ; netlong_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(stressx_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)  ; stressx_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(stressx_atm_meansic(ilon1_atm,ilat1_atm),STAT=status)  ; stressx_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(stressy_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)  ; stressy_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(stressy_atm_meansic(ilon1_atm,ilat1_atm),STAT=status)  ; stressy_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(precip_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)   ; precip_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(precip_atm_meansic(ilon1_atm,ilat1_atm),STAT=status)   ; precip_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)     ; evap_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_atm_meansic(ilon1_atm,ilat1_atm),STAT=status)     ; evap_atm_meansic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(lowestlt_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; lowestlt_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(lowestlq_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; lowestlq_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(lowestlp_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; lowestlp_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(lowestlh_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; lowestlh_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(lowestlu_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; lowestlu_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(lowestlv_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; lowestlv_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(runoff_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)   ; runoff_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(seaicefrac_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)  ; seaicefrac_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(conductflux_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status) ; conductflux_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(waterflux_atm_meanocn(ilon1_atm,ilat1_atm),STAT=status)   ; waterflux_atm_meanocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(seaicefrac_atm(ilon1_atm,ilat1_atm),STAT=status)          ; seaicefrac_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(conductflux_atm(ilon1_atm,ilat1_atm),STAT=status)         ; conductflux_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(dtcarry_ocn_sic(ilon1_atm,ilat1_atm),STAT=status)         ; dtcarry_ocn_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(energycarry_ocn_sic(ilon1_atm,ilat1_atm),STAT=status)     ; energycarry_ocn_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(energycarry_sic_ocn(ilon1_atm,ilat1_atm),STAT=status)     ; energycarry_sic_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(albedo_atm(ilon1_atm,ilat1_atm),STAT=status)              ; albedo_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(albedo_ocn(ilon1_ocn,ilat1_ocn),STAT=status)              ; albedo_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(temptop_atm(ilon1_atm,ilat1_atm),STAT=status)             ; temptop_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(thicktop_atm(ilon1_atm,ilat1_atm),STAT=status)            ; thicktop_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(conductflux_sic(ilon1_sic,ilat1_sic),STAT=status)         ; conductflux_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(conductflux_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; conductflux_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(seaicefrac_sic(ilon1_sic,ilat1_sic),STAT=status)          ; seaicefrac_sic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(atmos_lowestlu_atm(ilon1_atm,ilat1_atm),STAT=status)  ; atmos_lowestlu_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlu2_atm(ilon2_atm,ilat2_atm),STAT=status) ; atmos_lowestlu2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlv3_atm(ilon3_atm,ilat3_atm),STAT=status) ; atmos_lowestlv3_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlv_atm(ilon1_atm,ilat1_atm),STAT=status)  ; atmos_lowestlv_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlq_atm(ilon1_atm,ilat1_atm),STAT=status)  ; atmos_lowestlq_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlt_atm(ilon1_atm,ilat1_atm),STAT=status)  ; atmos_lowestlt_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlp_atm(ilon1_atm,ilat1_atm),STAT=status)  ; atmos_lowestlp_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_lowestlh_atm(ilon1_atm,ilat1_atm),STAT=status)  ; atmos_lowestlh_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_lowestlu_atm(ilon1_atm,ilat1_atm),STAT=status)   ; land_lowestlu_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_lowestlv_atm(ilon1_atm,ilat1_atm),STAT=status)   ; land_lowestlv_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_lowestlq_atm(ilon1_atm,ilat1_atm),STAT=status)   ; land_lowestlq_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_lowestlt_atm(ilon1_atm,ilat1_atm),STAT=status)   ; land_lowestlt_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlu_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_lowestlu_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlv_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_lowestlv_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlq_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_lowestlq_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlt_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_lowestlt_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlp_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_lowestlp_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlh_atm(ilon1_atm,ilat1_atm),STAT=status)  ; ocean_lowestlh_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(psigma(inl1_atm),STAT=status) ; psigma = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(ocean_lowestlt_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; ocean_lowestlt_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlq_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; ocean_lowestlq_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlp_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; ocean_lowestlp_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_lowestlh_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; ocean_lowestlh_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_atm_netsolar_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; ocean_atm_netsolar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_atm_netlong_ocn(ilon1_ocn,ilat1_ocn),STAT=status)  ; ocean_atm_netlong_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_latent_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; ocean_latent_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_sensible_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; ocean_sensible_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_netsolar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; ocean_netsolar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_netlong_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; ocean_netlong_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_evap_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; ocean_evap_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_precip_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; ocean_precip_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ocean_runoff_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; ocean_runoff_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_latent_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; atmos_latent_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_sensible_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; atmos_sensible_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_netsolar_ocn(ilon1_ocn,ilat1_ocn),STAT=status)     ; atmos_netsolar_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_netlong_ocn(ilon1_ocn,ilat1_ocn),STAT=status)      ; atmos_netlong_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_evap_ocn(ilon1_ocn,ilat1_ocn),STAT=status)         ; atmos_evap_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_precip_ocn(ilon1_ocn,ilat1_ocn),STAT=status)       ; atmos_precip_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_latent_atm(ilon1_atm,ilat1_atm),STAT=status)       ; atmos_latent_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_sensible_atm(ilon1_atm,ilat1_atm),STAT=status)     ; atmos_sensible_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_netsolar_atm(ilon1_atm,ilat1_atm),STAT=status)     ; atmos_netsolar_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_netlong_atm(ilon1_atm,ilat1_atm),STAT=status)      ; atmos_netlong_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_evap_atm(ilon1_atm,ilat1_atm),STAT=status)         ; atmos_evap_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(atmos_precip_atm(ilon1_atm,ilat1_atm),STAT=status)       ; atmos_precip_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(albavg_ocn(ilon1_ocn,ilat1_ocn),STAT=status) ; albavg_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(weight_ocn(ilon1_atm,ilat1_atm),STAT=status) ; weight_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(surf_qstar_atm(ilon1_atm,ilat1_atm),STAT=status)  ; surf_qstar_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(tstar_gb_land(ilon1_atm,ilat1_atm),STAT=status)   ; tstar_gb_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(albedo_land(ilon1_atm,ilat1_atm),STAT=status)     ; albedo_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_land(ilon1_atm,ilat1_atm),STAT=status)       ; evap_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(fx_le_land(ilon1_atm,ilat1_atm),STAT=status)      ; fx_le_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(fx_sen_land(ilon1_atm,ilat1_atm),STAT=status)     ; fx_sen_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(fx_momx_land(ilon1_atm,ilat1_atm),STAT=status)    ; fx_momx_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(fx_momy_land(ilon1_atm,ilat1_atm),STAT=status)    ; fx_momy_land = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_fxco2_atm(ilon1_atm,ilat1_atm),STAT=status)  ; land_fxco2_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ice_icefrac_atm(ilon1_atm,ilat1_atm),STAT=status) ; ice_icefrac_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_tice_ice(ilon1_atm,ilat1_atm),STAT=status)   ; land_tice_ice = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_albice_ice(ilon1_atm,ilat1_atm),STAT=status) ; land_albice_ice = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(evap_save1(ilon1_ocn,ilat1_ocn),STAT=status) ; evap_save1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(evap_save2(ilon1_ocn,ilat1_ocn),STAT=status) ; evap_save2 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(late_save1(ilon1_ocn,ilat1_ocn),STAT=status) ; late_save1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(late_save2(ilon1_ocn,ilat1_ocn),STAT=status) ; late_save2 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(sens_save1(ilon1_ocn,ilat1_ocn),STAT=status) ; sens_save1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(sens_save2(ilon1_ocn,ilat1_ocn),STAT=status) ; sens_save2 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(go_ds(ilat1_ocn),STAT=status) ; go_ds = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(go_ips(ilat1_ocn),STAT=status)          ; go_ips = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_ipf(ilat1_ocn),STAT=status)          ; go_ipf = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_ias(ilat1_ocn),STAT=status)          ; go_ias = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_iaf(ilat1_ocn),STAT=status)          ; go_iaf = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_k1(ilon1_ocn,ilat1_ocn),STAT=status) ; go_k1 = 0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(go_dz(1:inl1_ocn),STAT=status)                               ; go_dz = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_dza(1:inl1_ocn),STAT=status)                              ; go_dza = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_c(0:ilat1_ocn),STAT=status)                               ; go_c = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_cv(0:ilat1_ocn),STAT=status)                              ; go_cv = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_s(0:ilat1_ocn),STAT=status)                               ; go_s = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_sv(0:ilat1_ocn),STAT=status)                              ; go_sv = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_ts(intrac_ocn,ilon1_ocn,ilat1_ocn,inl1_ocn),STAT=status)  ; go_ts = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_ts1(intrac_ocn,ilon1_ocn,ilat1_ocn,inl1_ocn),STAT=status) ; go_ts1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_cost(ilon1_ocn,ilat1_ocn),STAT=status)                    ; go_cost = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_uvw(1:3,ilon1_ocn,ilat1_ocn,1:inl1_ocn),STAT=status)      ; go_uvw = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_tau(1:2,ilon1_ocn,ilat1_ocn),STAT=status)                 ; go_tau = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_uv(1:2,ilon1_ocn,ilat1_ocn),STAT=status)                  ; eb_uv = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_usurf(ilon1_ocn,ilat1_ocn),STAT=status)                   ; eb_usurf = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_solfor(ilat1_ocn),STAT=status)                            ; go_solfor = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_fxsw(ilon1_ocn,ilat1_ocn),STAT=status)                    ; go_fxsw = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_mldta(ilon1_ocn,ilat1_ocn),STAT=status)                   ; go_mldta = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_psi(0:ilon1_ocn,0:ilat1_ocn),STAT=status)                 ; go_psi = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(eb_ca(ilon1_ocn,ilat1_ocn),STAT=status)           ; eb_ca = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ea_co2(ilon1_ocn,ilat1_ocn),STAT=status)          ; ea_co2 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(ea_fxplw(ilon1_ocn,ilat1_ocn),STAT=status)        ; ea_fxplw = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(go_rho(ilon1_ocn,ilat1_ocn,inl1_ocn),STAT=status) ; go_rho = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(en_orog_vect(ilon1_ocn,ilat1_ocn,en_ntimes_max),STAT=status) ; en_orog_vect = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(en_lice_vect(ilon1_ocn,ilat1_ocn,en_ntimes_max),STAT=status) ; en_lice_vect = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_fx0a(ilon1_ocn,ilat1_ocn),STAT=status)                    ; eb_fx0a = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_fx0o(ilon1_ocn,ilat1_ocn),STAT=status)                    ; eb_fx0o = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_fxsen(ilon1_ocn,ilat1_ocn),STAT=status)                   ; eb_fxsen = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_fxlw(ilon1_ocn,ilat1_ocn),STAT=status)                    ; eb_fxlw = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_evap(ilon1_ocn,ilat1_ocn),STAT=status)                    ; eb_evap = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_pptn(ilon1_ocn,ilat1_ocn),STAT=status)                    ; eb_pptn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(eb_relh(ilon1_ocn,ilat1_ocn),STAT=status)                    ; eb_relh = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(torog_atm(ilon1_atm,ilat1_atm),STAT=status)                  ; torog_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(albs_atm(ilon1_atm,ilat1_atm),STAT=status)                   ; albs_atm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(landice_slicemask_lic(ilon1_lic,ilat1_lic),STAT=status)      ; landice_slicemask_lic = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_albs_snow_lnd(ilon1_lnd,ilat1_lnd),STAT=status)         ; land_albs_snow_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_albs_nosnow_lnd(ilon1_lnd,ilat1_lnd),STAT=status)       ; land_albs_nosnow_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_snow_lnd(ilon1_lnd,ilat1_lnd),STAT=status)              ; land_snow_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_bcap_lnd(ilon1_lnd,ilat1_lnd),STAT=status)              ; land_bcap_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_z0_lnd(ilon1_lnd,ilat1_lnd),STAT=status)                ; land_z0_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_temp_lnd(ilon1_lnd,ilat1_lnd),STAT=status)              ; land_temp_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(land_moisture_lnd(ilon1_lnd,ilat1_lnd),STAT=status)          ; land_moisture_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(el_leaf(ilon1_ocn,ilat1_ocn),STAT=status)     ; el_leaf = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(el_respveg(ilon1_ocn,ilat1_ocn),STAT=status)  ; el_respveg = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(el_respsoil(ilon1_ocn,ilat1_ocn),STAT=status) ; el_respsoil = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(el_photo(ilon1_ocn,ilat1_ocn),STAT=status)    ; el_photo = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(genie_sfcatm(n_atm_all,ilon1_atm,ilat1_atm),STAT=status)     ; genie_sfcatm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsumatm(n_atm_all,ilon1_atm,ilat1_atm),STAT=status)  ; genie_sfxsumatm = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfcatm1(n_atm_all,ilon1_ocn,ilat1_ocn),STAT=status)    ; genie_sfcatm1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxatm1(n_atm_all,ilon1_ocn,ilat1_ocn),STAT=status)    ; genie_sfxatm1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfcatm_lnd(n_atm_all,ilon1_lnd,ilat1_lnd),STAT=status) ; genie_sfcatm_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxatm_lnd(n_atm_all,ilon1_lnd,ilat1_lnd),STAT=status) ; genie_sfxatm_lnd = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(genie_sfxrok(intrac_ocn_max,ilon1_rok,ilat1_rok),STAT=status)         ; genie_sfxrok = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsumrok1(intrac_ocn_max,ilon1_ocn,ilat1_ocn),STAT=status)     ; genie_sfxsumrok1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsumrok1_gem(intrac_ocn_max,ilon1_ocn,ilat1_ocn),STAT=status) ; genie_sfxsumrok1_gem = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsumatm1_gem(n_atm_all,ilon1_ocn,ilat1_ocn),STAT=status) ; genie_sfxsumatm1_gem = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(genie_sfcsed(intrac_sed_max,ilon1_sed,ilat1_sed),STAT=status)     ; genie_sfcsed = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsumsed(intrac_sed_max,ilon1_sed,ilat1_sed),STAT=status)  ; genie_sfxsumsed = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsumsed1(intrac_sed_max,ilon1_ocn,ilat1_ocn),STAT=status) ; genie_sfxsumsed1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfcsed1(intrac_sed_max,ilon1_ocn,ilat1_ocn),STAT=status)    ; genie_sfcsed1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxsed1(intrac_sed_max,ilon1_ocn,ilat1_ocn),STAT=status)    ; genie_sfxsed1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfcsumocn(intrac_ocn_max,ilon1_sed,ilat1_sed),STAT=status)  ; genie_sfcsumocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxocn(intrac_ocn_max,ilon1_sed,ilat1_sed),STAT=status)     ; genie_sfxocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfxocn1(intrac_ocn_max,ilon1_ocn,ilat1_ocn),STAT=status)    ; genie_sfxocn1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_sfcocn1(intrac_ocn_max,ilon1_ocn,ilat1_ocn),STAT=status)    ; genie_sfcocn1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

    ALLOCATE(genie_atm1(n_atm_all,ilon1_atm,ilat1_atm),STAT=status)         ; genie_atm1 = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)
    ALLOCATE(genie_ocn(intrac_ocn_max,ilon1_ocn,ilat1_ocn,inl1_ocn),STAT=status) ; genie_ocn = 0.0
    IF (status /= 0) CALL alloc_die(__LINE__, __FILE__)

  END SUBROUTINE allocate_genie_global
END MODULE genie_global

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

  use genie_control

  implicit none

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

  REAL :: alon1_atm(ilon1_atm), alat1_atm(ilat1_atm)
  REAL :: alon1_ocn(ilon1_ocn), alat1_ocn(ilat1_ocn)
  REAL :: alon1_sic(ilon1_sic), alat1_sic(ilat1_sic)
  REAL :: aboxedge1_lon_atm(ilon1_atm+1), aboxedge1_lat_atm(ilat1_atm+1)
  REAL :: aboxedge1_lon_ocn(ilon1_ocn+1), aboxedge1_lat_ocn(ilat1_ocn+1)
  REAL :: aboxedge1_lon_sic(ilon1_sic+1), aboxedge1_lat_sic(ilat1_sic+1)
  REAL :: alon2_atm(ilon2_atm), alat2_atm(ilat2_atm)
  REAL :: alon2_ocn(ilon2_ocn), alat2_ocn(ilat2_ocn)
  REAL :: alon2_sic(ilon2_sic), alat2_sic(ilat2_sic)
  REAL :: aboxedge2_lon_atm(ilon2_atm+1), aboxedge2_lat_atm(ilat2_atm+1)
  REAL :: aboxedge2_lon_ocn(ilon2_ocn+1), aboxedge2_lat_ocn(ilat2_ocn+1)
  REAL :: aboxedge2_lon_sic(ilon2_sic+1), aboxedge2_lat_sic(ilat2_sic+1)
  REAL :: alon3_atm(ilon3_atm), alat3_atm(ilat3_atm)
  REAL :: alon3_ocn(ilon3_ocn), alat3_ocn(ilat3_ocn)
  REAL :: alon3_sic(ilon3_sic), alat3_sic(ilat3_sic)
  REAL :: aboxedge3_lon_atm(ilon3_atm+1), aboxedge3_lat_atm(ilat3_atm+1)
  REAL :: aboxedge3_lon_ocn(ilon3_ocn+1), aboxedge3_lat_ocn(ilat3_ocn+1)
  REAL :: aboxedge3_lon_sic(ilon3_sic+1), aboxedge3_lat_sic(ilat3_sic+1)
  REAL :: depth1_ocn(inl1_ocn), depth2_ocn(inl2_ocn)

  INTEGER :: ilandmask1_atm(ilon1_atm,ilat1_atm)
  INTEGER :: ilandmask2_atm(ilon2_atm,ilat2_atm)
  INTEGER :: ilandmask3_atm(ilon3_atm,ilat3_atm)
  INTEGER :: ilandmask1_ocn(ilon1_ocn,ilat1_ocn)
  INTEGER :: ilandmask2_ocn(ilon2_ocn,ilat2_ocn)
  INTEGER :: ilandmask3_ocn(ilon3_ocn,ilat3_ocn)
  INTEGER :: ilandmask1_sic(ilon1_sic,ilat1_sic)
  INTEGER :: ilandmask2_sic(ilon2_sic,ilat2_sic)
  INTEGER :: ilandmask3_sic(ilon3_sic,ilat3_sic)

  ! These are the fluxes which come out of the atmosphere, *_atm
  ! and those which go into goldstein after interpolation onto the
  ! goldstein spatial grid, *_ocn.
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: tstar_atm, netsolar_atm, netlong_atm
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & tstar_ocn, latent_ocn, sensible_ocn, netsolar_ocn, netlong_ocn
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & surf_latent_atm, surf_sensible_atm, &
       & surf_stressx_atm, surf_stressy_atm, surf_evap_atm
  REAL, DIMENSION(ilon2_atm,ilat2_atm) :: surf_stressx2_atm, surf_stressy2_atm
  REAL, DIMENSION(ilon3_atm,ilat3_atm) :: surf_stressx3_atm, surf_stressy3_atm
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: trest_ocn, srest_ocn

  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & land_latent_atm=0.0, land_sensible_atm=0.0, land_stressx_atm=0.0, &
       & land_stressy_atm=0.0, land_evap_atm=0.0
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & ocean_latent_atm=0.0, ocean_sensible_atm=0.0, ocean_stressx_atm=0.0, &
       & ocean_stressy_atm=0.0, ocean_evap_atm=0.0
  ! BEWARE!!! I HAVE CHANGED THIS A BIT....
  ! WHY WAS RUNOFF_OCN ON THE ATM GRID??!!

  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & ocean_lowestlu2_ocn, ocean_lowestlu3_ocn, &
       & ocean_lowestlv2_ocn, ocean_lowestlv3_ocn, &
       & ocean_stressx2_ocn, ocean_stressx3_ocn, &
       & ocean_stressy2_ocn, ocean_stressy3_ocn
  REAL, DIMENSION(2,ilon1_ocn,ilat1_ocn) :: stressx_ocn, stressy_ocn

  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & surf_orog_atm, zonwind_atm, merwind_atm
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: runoff_ocn
  ! Runoff on land before routing to ocean, for use in rokgem
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: runoff_land
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: runoff_sic

  ! For the seaice on the goldstein grid....
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: seaicefrac_ocn

  ! This variable is so that the stressx_atm and stressy_atm for
  ! goldstein can be passed sensibly.  The atmos one is a dummy
  ! variable for returning the temp and albedo
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: dummy_ocn
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: dummy_atm

  ! Need a variable for the interp mask
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: interpmask_atm
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: interpmask_ocn

  REAL :: weighttot_atm, weighttot_ocn

  ! Extra fluxes for new c-GOLDSTEIN modules
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: evap_ocn, precip_ocn
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: evap_atm
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: evap_sic, precip_atm, precip_sic
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: ustar_ocn, vstar_ocn, sstar_ocn

  ! Extra fields for c-GOLDSTEIN sea-ice module
  REAL, DIMENSION(ilon1_sic,ilat1_sic) :: &
       & hght_sic, frac_sic, temp_sic, albd_sic, dhght_sic, dfrac_sic
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: waterflux_ocn
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: waterflux_atm
  REAL, DIMENSION(ilon1_sic,ilat1_sic) :: waterflux_sic

  ! CO2 concentration field for c-GOLDSTEIN surface flux routine -
  ! will be unnecessary in final version of genie.F (when AtCheM is a
  ! genie.F level routine), but necessary at present
  ! Added methane and N2O.
  ! Also now essential for use with the igcm.
  ! With the igcm, these gases are given by genie-fixedchem.
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: co2_atm, n2o_atm, ch4_atm

  ! 14co2 used in igcm and ichem modules
  REAL, DIMENSION(ilon1_atm,ilat1_atm,inl1_atm) :: &
       & mass14co2, ddtmass14co2, massair

  ! Parameters storing the information about the ocean basins
  ! that needs to be passed from GOLDSTEIN to the EMBM
  INTEGER, DIMENSION(ilat1_ocn) :: ips_out, ipf_out, ias_out, iaf_out
  INTEGER jsf_out

  ! Extra fields of "average" ocean cell temperature and roughness
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: tavg_ocn, rough_ocn

  ! These are the fluxes out of the atmosphere once they have been
  ! averaged onto the seaice (*_atm_meanseaice) or ocean
  ! (*_atm_meanocn) timestep.
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & latent_atm_meanocn, latent_atm_meansic, sensible_atm_meanocn, &
       & sensible_atm_meansic, netsolar_atm_meanocn, netsolar_atm_meansic, &
       & netlong_atm_meanocn, netlong_atm_meansic, stressx_atm_meanocn, &
       & stressx_atm_meansic, stressy_atm_meanocn, stressy_atm_meansic, &
       & precip_atm_meanocn, precip_atm_meansic, evap_atm_meanocn, &
       & evap_atm_meansic

  ! Extra meanocn fields for GOLDOCN-GOLDSIC surflux
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & lowestlt_atm_meanocn, lowestlq_atm_meanocn, lowestlp_atm_meanocn, &
       & lowestlh_atm_meanocn, lowestlu_atm_meanocn, lowestlv_atm_meanocn

  ! EXTRA FIELDS:
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: runoff_atm_meanocn

  ! These are the fluxes out of the seaice once they have been
  ! averaged onto the ocean
  ! (*_atm_meanocn) timestep.
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & seaicefrac_atm_meanocn, conductflux_atm_meanocn, waterflux_atm_meanocn

  ! This is the sea-ice fraction (=1 or 0 fo the slab case)
  ! Should REALly be renamed seaicefrac_seaice seen as it's on the
  ! seaice grid (this just happens to be the same as the atmos grid
  ! at the moment)
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: seaicefrac_atm, conductflux_atm

  ! These are the carry-over from ocean to seaice (*_ocn_seaice)
  ! or from seaice to ocean (*_seaice_ocn)
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & dtcarry_ocn_sic, energycarry_ocn_sic, energycarry_sic_ocn

  ! This is the albedo.  In the slab and fixed case, it is calculated
  ! in the ocean or seaice modules.  It could be calculated by the
  ! atmosphere instead, in which case it would not need to be in
  ! genie.f.
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: albedo_atm
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: albedo_ocn

  ! This is the temperture of the uppermost layer of the ocean,
  ! and its thickness.  This is output from the ocean, and
  ! used in the seaice module to calculate the ocean-seaice heat flux.
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: temptop_atm, thicktop_atm

  ! Extra files
  REAL, DIMENSION (ilon1_sic,ilat1_sic) :: conductflux_sic
  REAL, DIMENSION (ilon1_ocn,ilat1_ocn) :: conductflux_ocn
  REAL, DIMENSION (ilon1_sic,ilat1_sic) :: seaicefrac_sic

  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: atmos_lowestlu_atm
  REAL, DIMENSION(ilon2_atm,ilat2_atm) :: atmos_lowestlu2_atm
  REAL, DIMENSION(ilon3_atm,ilat3_atm) :: atmos_lowestlv3_atm
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & atmos_lowestlv_atm, atmos_lowestlq_atm, atmos_lowestlt_atm, &
       & atmos_lowestlp_atm, atmos_lowestlh_atm

  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & land_lowestlu_atm=0.0, land_lowestlv_atm=0.0, &
       & land_lowestlq_atm=0.0, land_lowestlt_atm=0.0

  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & ocean_lowestlu_atm=0.0, ocean_lowestlv_atm=0.0, &
       & ocean_lowestlq_atm=0.0, ocean_lowestlt_atm=0.0, &
       & ocean_lowestlp_atm=0.0, ocean_lowestlh_atm=0.0

  REAL :: surfsigma, surfdsigma, psigma(inl1_atm)

  ! For the fixedatmos grid.  1=igcm, 2=goldstein
  INTEGER :: grid_type_fixedatmos

  ! Inputs to c-GOLDSTEIN surflux routines
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & ocean_lowestlt_ocn, ocean_lowestlq_ocn, ocean_lowestlp_ocn, &
       & ocean_lowestlh_ocn, ocean_atm_netsolar_ocn, ocean_atm_netlong_ocn

  ! Outputs from c-GOLDSTEIN surflux routines
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & ocean_latent_ocn, ocean_sensible_ocn, ocean_netsolar_ocn, &
       & ocean_netlong_ocn, ocean_evap_ocn, ocean_precip_ocn, ocean_runoff_ocn
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & atmos_latent_ocn, atmos_sensible_ocn, atmos_netsolar_ocn, &
       & atmos_netlong_ocn, atmos_evap_ocn, atmos_precip_ocn
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & atmos_latent_atm, atmos_sensible_atm, atmos_netsolar_atm, &
       & atmos_netlong_atm, atmos_evap_atm, atmos_precip_atm

  ! GOLDOCN-GOLDSIC surflux requires information about average ocean
  ! cell temperature and albedo.  Also, temporary variables needed
  ! after goldstein.F call to feed either ocean-only or ocean+sea-ice
  ! average fields of temperature and albedo out (i.e. slab sea-ice
  ! vs. GOLDSTEIN sea-ice)

  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: albavg_ocn
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: weight_ocn

  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: surf_qstar_atm

  REAL :: atmos_dt_tim
  REAL :: test_energy_seaice, test_energy_ocean
  REAL :: test_water_seaice, test_water_ocean, test_water_land

  ! Temporary genie-land variable
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: &
       & tstar_gb_land, albedo_land, evap_land, fx_le_land, fx_sen_land, &
       & fx_momx_land, fx_momy_land, land_fxco2_atm, ice_icefrac_atm, &
       & land_tice_ice, land_albice_ice

  ! For temporary water-fix.  To be removed.....
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
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
  REAL, DIMENSION(ilat1_ocn) :: go_ds
  ! parameter(go_tsc=go_rsc/go_usc)
  REAL :: go_dsc=5.0E3, go_fsc=2*7.2921E-5, go_rh0sc=1.0E3
  REAL :: go_rhosc, go_cpsc, go_solconst, go_scf
  INTEGER, DIMENSION(ilat1_ocn) :: go_ips, go_ipf, go_ias, go_iaf
  INTEGER :: go_jsf
  INTEGER, DIMENSION(ilon1_ocn,ilat1_ocn) :: go_k1
  REAL, DIMENSION(1:inl1_ocn) :: go_dz, go_dza
  REAL, DIMENSION(0:ilat1_ocn) :: go_c, go_cv, go_s, go_sv
  REAL, DIMENSION(intrac_ocn,ilon1_ocn,ilat1_ocn,inl1_ocn) :: go_ts, go_ts1
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: go_cost
  REAL, DIMENSION(1:3,ilon1_ocn,ilat1_ocn,1:inl1_ocn) :: go_uvw
  REAL, DIMENSION(1:2,ilon1_ocn,ilat1_ocn) :: go_tau
  REAL, DIMENSION(1:2,ilon1_ocn,ilat1_ocn) :: eb_uv
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: eb_usurf
  REAL, DIMENSION(ilat1_ocn) :: go_solfor
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: go_fxsw
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: go_mldta
  REAL, DIMENSION(0:ilon1_ocn,0:ilat1_ocn) :: go_psi

  ! genie-ents variables
  INTEGER :: go_istep0, go_nyear
  REAL :: go_rsc, go_syr, eb_dphi, eb_rmax, eb_rdtdim
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: eb_ca
  CHARACTER(LEN=13) :: go_lin
  REAL :: ea_co2(ilon1_ocn,ilat1_ocn)
  REAL :: ea_fxplw(ilon1_ocn,ilat1_ocn)
  REAL, DIMENSION(5) :: go_ec
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn,inl1_ocn) :: go_rho

  ! Variables for surflux_ents
  INTEGER, PARAMETER :: en_ntimes_max=50
  INTEGER :: en_t_orog, en_norog, en_orogsteps
  INTEGER :: en_t_lice, en_nlice, en_licesteps
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn,en_ntimes_max) :: &
       & en_orog_vect, en_lice_vect
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & eb_fx0a, eb_fx0o, eb_fxsen, eb_fxlw, eb_evap, eb_pptn, eb_relh
  REAL, DIMENSION(ilon1_atm,ilat1_atm) :: torog_atm, albs_atm
  REAL, DIMENSION(ilon1_lic,ilat1_lic) :: landice_slicemask_lic
  REAL, DIMENSION(ilon1_lnd,ilat1_lnd) :: &
       & land_albs_snow_lnd, land_albs_nosnow_lnd, land_snow_lnd, &
       & land_bcap_lnd, land_z0_lnd, land_temp_lnd, land_moisture_lnd

  ! Carbon variables from var_ents (for rokgem)
  ! prefix denotes module name - ENTS Land
  REAL, DIMENSION(ilon1_ocn,ilat1_ocn) :: &
       & el_leaf, el_respveg, el_respsoil, el_photo

  ! Ocean-atmosphere tracer interface arrays
  REAL, DIMENSION(intrac_atm_max,ilon1_atm,ilat1_atm) :: &
       & genie_sfcatm, &      ! atmosphere-surface tracer composition; atm grid
       & genie_sfxsumatm      ! atmosphere-surface fluxes; integrated, atm grid
  REAL, DIMENSION(intrac_atm_max,ilon1_ocn,ilat1_ocn) :: &
       & genie_sfcatm1, &     ! atmosphere-surface tracer composition; ocn grid
       & genie_sfxatm1        ! atmosphere-surface fluxes; ocn grid
  REAL, DIMENSION(intrac_atm_max,ilon1_lnd,ilat1_lnd) :: &
       & genie_sfcatm_lnd, &  ! atmosphere-surface tracer composition; lnd grid
       & genie_sfxatm_lnd     ! land-atmosphere fluxes; lnd grid

  ! Ocean-rock tracer interface arrays (for purposes of getting
  ! weathering flux from rockgem grid into biogem)
  REAL, DIMENSION(intrac_ocn_max,ilon1_rok,ilat1_rok) :: &
       & genie_sfxrok         ! rock-surface(coastal ocean) fluxes; rok grid
  REAL, DIMENSION(intrac_ocn_max,ilon1_ocn,ilat1_ocn) :: &
       & genie_sfxsumrok1, &  ! rock-surf.(coastal ocean); integrated ocn grid
       & genie_sfxsumrok1_gem ! (version of above for GEMlite)
  REAL,DIMENSION(intrac_atm_max,ilon1_ocn,ilat1_ocn) :: &
       & genie_sfxsumatm1_gem

  ! atmosphere-rock tracer interface arrays (for purposes of getting
  ! temp and runoff into rokgem)

  ! oecan-sediment tracer interface arrays
  REAL, DIMENSION(intrac_sed_max,ilon1_sed,ilat1_sed) :: &
       & genie_sfcsed, &  ! sed.-surface sediment composition; sed grid
       & genie_sfxsumsed  ! sed.-surface (ocn->sed) fluxes; integrated, sed grid
  REAL, DIMENSION(intrac_sed_max,ilon1_ocn,ilat1_ocn) :: &
       & genie_sfxsumsed1, & ! sed.-surface (ocn->sed) fluxes; integ., ocn grid
       & genie_sfcsed1, &    ! sediment-surface sediment composition; ocn grid
       & genie_sfxsed1       ! sediment-surface (ocn->sed) fluxes; ocn grid
  REAL, DIMENSION(intrac_ocn_max,ilon1_sed,ilat1_sed) :: &
       & genie_sfcsumocn, & ! sed.-surface ocean tracer comp; integ., sed grid
       & genie_sfxocn       ! sediment-surface (sed->ocn) fluxes; sed grid
  REAL, DIMENSION(intrac_ocn_max,ilon1_ocn,ilat1_ocn) :: &
       & genie_sfxocn1, &   ! sediment-surface (sed->ocn) fluxes; ocn grid
       & genie_sfcocn1      ! sed.-surface ocean tracer composition; ocn grid

  ! Temporary tracer arrays (for passing to/from GEMlite)
  REAL, DIMENSION(intrac_atm_max,ilon1_atm,ilat1_atm) :: &
       & genie_atm1 = 0.0   ! atmosphere tracers; ocn grid
  REAL, DIMENSION(intrac_ocn_max,ilon1_ocn,ilat1_ocn,inl1_ocn) :: &
       & genie_ocn = 0.0 ! ocean tracers; ocn grid

  ! These are for whether we have a restart run or not
  LOGICAL :: lrestart_genie

#ifndef REV
#define REV 0
#endif

  ! Version number for the model
  INTEGER, PARAMETER :: genie_version=REV

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
  INTEGER FUNCTION getversion()
    IMPLICIT NONE
    getversion = genie_version
  END FUNCTION getversion

END MODULE genie_global

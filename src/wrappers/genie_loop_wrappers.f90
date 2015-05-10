MODULE genie_loop_wrappers

  USE genie_global

CONTAINS

  SUBROUTINE surflux_wrapper
    USE embm
    IMPLICIT NONE
    ! Surflux module : GOLDSTEIN-EMBM-GOLDSEAICE (parentage = c-GOLDSTEIN)
    !
    ! Inputs :  tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           tstar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    !           albd_sic                 sea-ice albedo
    !           ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    ! Outputs : albedo_ocn               ocean albedo (excl. sea-ice)
    !           latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux
    !           netlong_ocn              net long-wave heat flux
    !           evap_ocn                 evaporation
    !           precip_ocn               precipitation
    !           runoff_ocn               runoff to ocean
    !           runoff_land              runoff on land
    !           surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           netsolar_atm             net short-wave heat flux
    !           netlong_atm              net long-wave heat flux
    !           evap_atm                 evaporation
    !           precip_atm               precipitation (not used normally)
    !           dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    !           atmos_lowestlh_atm       Height of lowest atmos level (m)
    !           atmos_lowestlu2_atm      zonal component of wind speed
    !           atmos_lowestlv3_atm      meridional component of wind speed
    !
    CALL surflux(istep_ocn, tstar_ocn, sstar_ocn, tstar_atm, surf_qstar_atm, &
         & hght_sic, frac_sic, temp_sic, albd_sic, ocean_stressx2_ocn, &
         & ocean_stressy2_ocn, ocean_stressx3_ocn, ocean_stressy3_ocn, &
         & albedo_ocn, latent_ocn, sensible_ocn, netsolar_ocn, netlong_ocn, &
         & evap_ocn, precip_ocn, runoff_ocn, runoff_land, surf_latent_atm, &
         & surf_sensible_atm, netsolar_atm, netlong_atm, evap_atm, precip_atm, &
         & dhght_sic, dfrac_sic, atmos_lowestlh_atm, go_solfor, go_fxsw, &
         & genie_sfcatm1, eb_ca, global_daysperyear, &
         & eb_fx0a, eb_fx0o, eb_fxsen, eb_fxlw, eb_evap, eb_pptn, eb_relh, &
         & eb_uv, eb_usurf, genie_solar_constant, co2_atm, ch4_atm, n2o_atm, &
         & surf_orog_atm, landice_slicemask_lic, albs_atm, land_albs_snow_lnd, &
         & land_albs_nosnow_lnd, land_snow_lnd, land_bcap_lnd, land_z0_lnd, &
         & land_temp_lnd, land_moisture_lnd, flag_ents, atmos_lowestlu2_atm, &
         & atmos_lowestlv3_atm)
  END SUBROUTINE surflux_wrapper

  SUBROUTINE embm_wrapper
    USE EMBM
    IMPLICIT NONE
    ! Atmosphere module : EMBM (parentage = c-GOLDSTEIN)
    !
    ! Inputs  : surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           surf_netsolar_atm        net short-wave heat flux
    !           surf_netlong_atm         net long-wave heat flux
    !           evap_atm                 evaporation
    !           precip_atm               precipitation (not used)
    ! Outputs : ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    !           tatar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           atmos_lowestlu2_atm      zonal component of wind speed
    !           atmos_lowestlv3_atm      meridional component of wind speed
    CALL step_embm(istep_atm, surf_latent_atm, surf_sensible_atm, &
         & netsolar_atm, netlong_atm, evap_atm, precip_atm, &
         & ocean_stressx2_ocn, ocean_stressy2_ocn, &
         & ocean_stressx3_ocn, ocean_stressy3_ocn, &
         & tstar_atm, surf_qstar_atm, torog_atm, surf_orog_atm, &
         & flag_ents, atmos_lowestlu2_atm, atmos_lowestlv3_atm)
  END SUBROUTINE embm_wrapper

  SUBROUTINE gold_seaice_wrapper
    USE gold_seaice
    IMPLICIT NONE
    ! Sea-ice module : GOLDSTEIN sea-ice (parentage = c-GOLDSTEIN)
    !
    ! Inputs  : dhght_sic            change in sea-ice height
    !           dfrac_sic            change in sea-ice fractional cover
    !           ustar_ocn            surface ocean velocity (u)
    !           vstar_ocn            surface ocean velocity (v)
    ! Outputs : hght_sic             sea-ice height
    !           frac_sic             sea-ice fractional cover
    !           temp_sic             sea-ice surface temperature } technically
    !           albd_sic             sea-ice albedo              } inputs
    !           waterflux_ocn        freshwater flux to ocean (melting)
    !           conductflux_ocn      heat flux to ocean (melting)
    !
    CALL step_seaice(istep_sic, dhght_sic, dfrac_sic, ustar_ocn, vstar_ocn, &
         & hght_sic, frac_sic, temp_sic, albd_sic, waterflux_ocn, &
         & conductflux_ocn, test_energy_seaice, test_water_seaice)
  END SUBROUTINE gold_seaice_wrapper

  SUBROUTINE goldstein_wrapper
    USE goldstein
    IMPLICIT NONE
    ! Ocean module : GOLDSTEIN (parentage = c-GOLDSTEIN)
    ! Inputs  : latent_ocn          latent heat flux
    !           sensible_ocn        sensible heat flux
    !           netsolar_ocn        net short-wave heat flux
    !           netlong_ocn         net long-wave heat flux
    !           conductflux_ocn     heat flux to ocean (sea-ice melting)
    !           evap_ocn            evaporation
    !           precip_ocn          precipitation
    !           runoff_ocn          runoff
    !           waterflux_ocn       freshwater flux to ocean (sea-ice melting)
    !           stressx_ocn         surface wind stress (x) on ocean
    !           stressy_ocn         surface wind stress (y) on ocean
    !           frac_sic            sea-ice fractional cover
    !           temp_sic            sea-ice surface temperature
    ! Outputs : tstar_ocn           ocean surface temperature
    !           sstar_ocn           ocean surface salinity
    !           ustar_ocn           surface ocean velocity (u)
    !           vstar_ocn           surface ocean velocity (v)
    CALL step_goldstein(istep_ocn, latent_ocn, sensible_ocn, &
         & netsolar_ocn, netlong_ocn, conductflux_ocn, evap_ocn, &
         & precip_ocn, runoff_ocn, waterflux_ocn, &
         & ocean_stressx2_ocn, ocean_stressy2_ocn, &
         & ocean_stressx3_ocn, ocean_stressy3_ocn, &
         & tstar_ocn, sstar_ocn, ustar_ocn, vstar_ocn, albedo_ocn, &
         & test_energy_ocean, test_water_ocean, &
         & go_ts, go_ts1, go_cost, go_uvw, go_tau, go_psi, go_mldta, go_rho)
  END SUBROUTINE goldstein_wrapper

  SUBROUTINE ents_wrapper
    USE ents
    IMPLICIT NONE
    CALL step_ents(istep_ocn, go_nyear, torog_atm, co2_atm, &
         & go_rh0sc, go_rhosc, go_rsc, go_ds, go_dphi, go_dsc, go_saln0, &
         & go_dz, go_ec, go_rho, eb_fx0a, eb_fx0o, eb_fxsen, eb_fxlw, &
         & eb_evap, eb_pptn, eb_relh, go_istep0, el_photo, el_respveg, &
         & el_respsoil, el_leaf, landice_slicemask_lic, albs_atm, &
         & land_albs_snow_lnd, land_albs_nosnow_lnd, land_snow_lnd, &
         & land_bcap_lnd, land_z0_lnd, land_temp_lnd, land_moisture_lnd, &
         & genie_sfcatm_lnd, genie_sfxatm_lnd)
  END SUBROUTINE ents_wrapper

  SUBROUTINE cpl_flux_ocnatm_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_flux_ocnatm(REAL(conv_kocn_kbiogem * kocn_loop) * genie_timestep, &
         & genie_sfxatm1, genie_sfxsumatm)
  END SUBROUTINE cpl_flux_ocnatm_wrapper

  SUBROUTINE cpl_flux_lndatm_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_flux_lndatm(REAL(klnd_loop) * genie_timestep, genie_sfxatm_lnd, genie_sfxsumatm)
  END SUBROUTINE cpl_flux_lndatm_wrapper

  SUBROUTINE cpl_comp_lndEMBM_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_comp_EMBM(tstar_atm, surf_qstar_atm, genie_sfcatm_lnd)
  END SUBROUTINE cpl_comp_lndEMBM_wrapper

  SUBROUTINE cpl_flux_ocnsed_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL cpl_flux_ocnsed(REAL(conv_kocn_kbiogem * kocn_loop) * genie_timestep, &
         & intrac_sed_max, ilon1_ocn, ilat1_ocn, ilon1_sed, ilat1_sed, &
         & genie_sfxsed1, genie_sfxsumsed)
  END SUBROUTINE cpl_flux_ocnsed_wrapper

  SUBROUTINE cpl_flux_sedocn_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL cpl_flux_sedocn(intrac_ocn_max, ilon1_ocn, ilat1_ocn, &
         & ilon1_sed, ilat1_sed, genie_sfxocn1, genie_sfxocn)
  END SUBROUTINE cpl_flux_sedocn_wrapper

  SUBROUTINE cpl_flux_sedsed1_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL cpl_flux_sedsed1(intrac_sed_max, ilon1_ocn, ilat1_ocn, &
         & ilon1_sed, ilat1_sed, genie_sfxsumsed, genie_sfxsumsed1)
  END SUBROUTINE cpl_flux_sedsed1_wrapper

  SUBROUTINE cpl_comp_ocnsed_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL cpl_comp_ocnsed(INT(koverall / kocn_loop), &
         & conv_kocn_kbiogem, conv_kocn_ksedgem, &
         & ilon1_ocn, ilat1_ocn, ilon1_sed, ilat1_sed, &
         & genie_sfcocn1, genie_sfcsumocn)
  END SUBROUTINE cpl_comp_ocnsed_wrapper

  ! NOTE: set the three time-step integers equal to effectively force:
  !       <genie_sfcsumocn> = <genie_sfcocn1>
  !       (no e.g. annual averaging of the ocean array is needed in
  !       the case of the annual GEMlite tiem-step)
  SUBROUTINE cpl_comp_ocnsed_gem_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL cpl_comp_ocnsed(conv_kocn_ksedgem, conv_kocn_ksedgem, &
         & conv_kocn_ksedgem, ilon1_ocn, ilat1_ocn, &
         & ilon1_sed, ilat1_sed, genie_sfcocn1, genie_sfcsumocn)
  END SUBROUTINE cpl_comp_ocnsed_gem_wrapper

  SUBROUTINE cpl_comp_sedocn_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL cpl_comp_sedocn(ilon1_ocn, ilat1_ocn, &
         & ilon1_sed, ilat1_sed, genie_sfcsed1, genie_sfcsed)
  END SUBROUTINE cpl_comp_sedocn_wrapper

  SUBROUTINE rokgem_wrapper
    USE rokgem
    IMPLICIT NONE
    CALL step_rokgem(REAL(conv_kocn_krokgem * kocn_loop) * genie_timestep, &
         & genie_sfcatm1, runoff_land, el_photo, el_respveg, &
         & genie_sfxrok, genie_sfxatm1)
  END SUBROUTINE rokgem_wrapper

  SUBROUTINE cpl_flux_rokatm_wrapper
    IMPLICIT NONE
    CALL cpl_flux_rokatm(REAL(conv_kocn_krokgem * kocn_loop) * genie_timestep, &
         & intrac_atm_max, ilon1_rok, ilat1_rok, ilon1_atm, ilat1_atm, &
         & genie_sfxatm1, genie_sfxsumatm, .FALSE.)
  END SUBROUTINE cpl_flux_rokatm_wrapper

  SUBROUTINE cpl_flux_rokatm_gem_wrapper
    IMPLICIT NONE
    CALL cpl_flux_rokatm(REAL(conv_kocn_krokgem * kocn_loop) * genie_timestep, &
         & intrac_atm_max, ilon1_rok, ilat1_rok, ilon1_atm, ilat1_atm, &
         & genie_sfxatm1, genie_sfxsumatm1_gem, .TRUE.)
  END SUBROUTINE cpl_flux_rokatm_gem_wrapper

  SUBROUTINE reinit_flux_rokatm_gem_wrapper
    USE rokgem
    IMPLICIT NONE
    CALL reinit_flux_rokatm(genie_sfxsumatm1_gem)
  END SUBROUTINE reinit_flux_rokatm_gem_wrapper

  SUBROUTINE cpl_flux_rokocn_wrapper
    IMPLICIT NONE
    CALL cpl_flux_rokocn(REAL(conv_kocn_krokgem * kocn_loop) * genie_timestep, &
         & intrac_ocn_max, ilon1_rok, ilat1_rok, ilon1_ocn, ilat1_ocn, &
         & genie_sfxrok, genie_sfxsumrok1)
  END SUBROUTINE cpl_flux_rokocn_wrapper

  SUBROUTINE cpl_flux_rokocn_gem_wrapper
    IMPLICIT NONE
    CALL cpl_flux_rokocn(REAL(conv_kocn_krokgem * kocn_loop) * genie_timestep, &
         & intrac_ocn_max, ilon1_rok, ilat1_rok, ilon1_ocn, ilat1_ocn, &
         & genie_sfxrok, genie_sfxsumrok1_gem)
  END SUBROUTINE cpl_flux_rokocn_gem_wrapper

  SUBROUTINE reinit_flux_rokocn_wrapper
    USE rokgem
    IMPLICIT NONE
    CALL reinit_flux_rokocn(genie_sfxsumrok1)
  END SUBROUTINE reinit_flux_rokocn_wrapper

  SUBROUTINE reinit_flux_rokocn_gem_wrapper
    USE rokgem
    IMPLICIT NONE
    CALL reinit_flux_rokocn(genie_sfxsumrok1_gem)
  END SUBROUTINE reinit_flux_rokocn_gem_wrapper

  SUBROUTINE rokgem_save_restart_wrapper
    USE rokgem
    IMPLICIT NONE
    CALL rest_rokgem()
  END SUBROUTINE rokgem_save_restart_wrapper


  ! *** BIOGEM ***

  SUBROUTINE biogem_wrapper
    USE biogem
    IMPLICIT NONE
    CALL step_biogem(REAL(conv_kocn_kbiogem * kocn_loop) * genie_timestep, &
         & genie_clock, genie_sfcatm1, genie_sfxatm1, genie_sfcocn1, &
         & genie_sfxocn1, genie_sfcsed1, genie_sfxsed1, genie_sfxsumrok1)
  END SUBROUTINE biogem_wrapper

  SUBROUTINE biogem_tracercoupling_wrapper
    USE biogem
    IMPLICIT NONE
    CALL biogem_tracercoupling(go_ts, go_ts1)
  END SUBROUTINE biogem_tracercoupling_wrapper

  SUBROUTINE biogem_forcing_wrapper
    USE biogem
    IMPLICIT NONE
    CALL biogem_forcing(genie_clock)
  END SUBROUTINE biogem_forcing_wrapper

  SUBROUTINE biogem_climate_wrapper
    USE biogem
    IMPLICIT NONE
    CALL biogem_climate(hght_sic, frac_sic, go_cost, go_solfor, go_fxsw, &
         & go_uvw, go_tau, go_psi, eb_uv, eb_usurf, go_mldta, eb_evap, &
         & eb_pptn, genie_solar_constant)
  END SUBROUTINE biogem_climate_wrapper

  SUBROUTINE biogem_climate_sol_wrapper
    USE biogem
    IMPLICIT NONE
    CALL biogem_climate_sol(go_solfor, go_fxsw, genie_solar_constant)
  END SUBROUTINE biogem_climate_sol_wrapper

  SUBROUTINE diag_biogem_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem(genie_clock, genie_sfcatm1, .FALSE.)
  END SUBROUTINE diag_biogem_wrapper

  SUBROUTINE diag_biogem_gem_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem(genie_clock, genie_sfcatm1, .TRUE.)
  END SUBROUTINE diag_biogem_gem_wrapper

  SUBROUTINE diag_biogem_pCO2_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem_pCO2(genie_sfcatm1, gem_pCO2)
  END SUBROUTINE diag_biogem_pCO2_wrapper

  SUBROUTINE diag_biogem_timeslice_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem_timeslice(REAL(conv_kocn_kbiogem * kocn_loop) * &
         & genie_timestep, &
         & genie_clock, genie_sfcatm1, genie_sfxatm1, genie_sfxocn1, &
         & genie_sfcsed1, genie_sfxsed1, genie_sfxsumrok1, &
         & .TRUE., .FALSE.)
  END SUBROUTINE diag_biogem_timeslice_wrapper

  SUBROUTINE diag_biogem_gem_timeslice_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem_timeslice(REAL(conv_kocn_kbiogem * kocn_loop) * &
         & genie_timestep, &
         & genie_clock, genie_sfcatm1, genie_sfxatm1, genie_sfxocn1, &
         & genie_sfcsed1, genie_sfxsed1, genie_sfxsumrok1_gem, &
         & gem_adapt_diag_biogem_full, .TRUE.)
  END SUBROUTINE diag_biogem_gem_timeslice_wrapper

  SUBROUTINE diag_biogem_timeseries_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem_timeseries(REAL(conv_kocn_kbiogem * kocn_loop) * &
         & genie_timestep, &
         & genie_clock, genie_sfcatm1, genie_sfxatm1, genie_sfxocn1, &
         & genie_sfcsed1, genie_sfxsed1, genie_sfxsumrok1, &
         & .TRUE., .FALSE., .FALSE.)
  END SUBROUTINE diag_biogem_timeseries_wrapper

  SUBROUTINE diag_biogem_force_timeseries_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem_timeseries(REAL(conv_kocn_kbiogem * kocn_loop) * &
         & genie_timestep, &
         & genie_clock, genie_sfcatm1, genie_sfxatm1, genie_sfxocn1, &
         & genie_sfcsed1, genie_sfxsed1, genie_sfxsumrok1, &
         & .TRUE., .TRUE., .FALSE.)
  END SUBROUTINE diag_biogem_force_timeseries_wrapper

  SUBROUTINE diag_biogem_gem_timeseries_wrapper
    USE biogem
    IMPLICIT NONE
    CALL diag_biogem_timeseries(REAL(conv_kocn_kbiogem * kocn_loop) * &
         & genie_timestep, &
         & genie_clock, genie_sfcatm1, genie_sfxatm1, genie_sfxocn1, &
         & genie_sfcsed1, genie_sfxsed1, genie_sfxsumrok1_gem, &
         & gem_adapt_diag_biogem_full, .FALSE., .TRUE.)
  END SUBROUTINE diag_biogem_gem_timeseries_wrapper

  SUBROUTINE cpl_comp_ocngem_wrapper
    USE biogem
    IMPLICIT NONE
    CALL cpl_comp_ocngem(REAL(conv_kocn_kbiogem * kocn_loop) * genie_timestep, genie_ocn)
  END SUBROUTINE cpl_comp_ocngem_wrapper

  SUBROUTINE cpl_comp_gemocn_wrapper
    USE biogem
    IMPLICIT NONE
    CALL cpl_comp_gemocn(genie_ocn)
  END SUBROUTINE cpl_comp_gemocn_wrapper

  SUBROUTINE cpl_comp_gematm1_wrapper
    USE biogem
    IMPLICIT NONE
    CALL cpl_comp_gematm1(genie_atm1, genie_sfcatm1)
  END SUBROUTINE cpl_comp_gematm1_wrapper

  SUBROUTINE biogem_save_restart_wrapper
    USE biogem
    IMPLICIT NONE
    CALL biogem_save_restart(genie_clock)
  END SUBROUTINE biogem_save_restart_wrapper


  ! *** ATCHEM ***

  SUBROUTINE atchem_wrapper
    USE atchem
    IMPLICIT NONE
    CALL step_atchem(REAL(conv_kocn_katchem * kocn_loop) * genie_timestep, &
         & genie_sfxsumatm, genie_sfcatm)
  END SUBROUTINE atchem_wrapper

  SUBROUTINE cpl_comp_atmgem_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_comp_atmgem(REAL(conv_kocn_katchem * kocn_loop) * genie_timestep, genie_atm1)
  END SUBROUTINE cpl_comp_atmgem_wrapper

  SUBROUTINE cpl_comp_gematm_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_comp_gematm(genie_atm1)
  END SUBROUTINE cpl_comp_gematm_wrapper

  SUBROUTINE cpl_comp_atmocn_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_comp_atmocn(intrac_atm_max, genie_sfcatm, genie_sfcatm1)
  END SUBROUTINE cpl_comp_atmocn_wrapper

  SUBROUTINE cpl_comp_EMBM_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_comp_EMBM(tstar_atm, surf_qstar_atm, genie_sfcatm1)
  END SUBROUTINE cpl_comp_EMBM_wrapper

  SUBROUTINE cpl_comp_atmlnd_wrapper
    USE atchem
    IMPLICIT NONE
    CALL cpl_comp_atmlnd(intrac_atm_max, genie_sfcatm, genie_sfcatm_lnd)
  END SUBROUTINE cpl_comp_atmlnd_wrapper

  SUBROUTINE atchem_save_restart_wrapper
    USE atchem
    IMPLICIT NONE
    CALL atchem_save_rst(genie_clock)
  END SUBROUTINE atchem_save_restart_wrapper


  ! *** SEDGEM ***

  SUBROUTINE sedgem_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL step_sedgem(REAL(conv_kocn_ksedgem * kocn_loop) * genie_timestep, &
         & genie_sfxsumsed, genie_sfcsumocn, genie_sfcsed, genie_sfxocn, &
         & .TRUE.)
  END SUBROUTINE sedgem_wrapper

  SUBROUTINE sedgem_glt_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL step_sedgem(REAL(conv_kocn_ksedgem * kocn_loop) * genie_timestep, &
         & genie_sfxsumsed, genie_sfcsumocn, genie_sfcsed, genie_sfxocn, &
         & .FALSE.)
  END SUBROUTINE sedgem_glt_wrapper

  SUBROUTINE sedgem_dsedage_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL sedgem_dsedage(REAL(conv_kocn_ksedgem * kocn_loop) * genie_timestep, &
         & genie_sfxsumsed)
  END SUBROUTINE sedgem_dsedage_wrapper

  SUBROUTINE sedgem_save_restart_wrapper
    USE sedgem
    IMPLICIT NONE
    CALL sedgem_save_rst(genie_clock, genie_sfxocn)
  END SUBROUTINE sedgem_save_restart_wrapper


  ! *** GEMLITE ***

  SUBROUTINE gemlite_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL step_gemlite(genie_sfcocn1, genie_sfxsumsed1, genie_sfxocn1, &
         & genie_sfxsumrok1_gem, genie_sfxsumatm1_gem)
  END SUBROUTINE gemlite_wrapper

  SUBROUTINE gemlite_climate_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL gemlite_climate(frac_sic)
  END SUBROUTINE gemlite_climate_wrapper

  SUBROUTINE gemlite_cycleinit_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL gemlite_cycleinit()
  END SUBROUTINE gemlite_cycleinit_wrapper

  SUBROUTINE gemlite_gltts_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL gemlite_ts(go_ts, go_ts1)
  END SUBROUTINE gemlite_gltts_wrapper

  SUBROUTINE cpl_comp_gemglt_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL cpl_comp_gemglt(genie_atm1, genie_ocn)
  END SUBROUTINE cpl_comp_gemglt_wrapper

  SUBROUTINE cpl_comp_gltgem_d_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL cpl_comp_gltgem_d(genie_atm1, genie_ocn)
  END SUBROUTINE cpl_comp_gltgem_d_wrapper

  SUBROUTINE cpl_comp_gltgem_dsum_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL cpl_comp_gltgem_dsum(genie_atm1, genie_ocn)
  END SUBROUTINE cpl_comp_gltgem_dsum_wrapper

END MODULE genie_loop_wrappers

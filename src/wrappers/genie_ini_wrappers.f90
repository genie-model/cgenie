! ===========================================================
! This module contains wrapper subroutines to hide arg lists
!             __Initialisation Routines Only__
! ===========================================================

MODULE genie_ini_wrappers

  USE genie_global

CONTAINS

  SUBROUTINE initialise_embm_wrapper
    USE embm
    IMPLICIT NONE
    CALL initialise_embm(alon1_atm, alat1_atm, alon2_atm, alat2_atm, &
         & alon3_atm, alat3_atm, aboxedge1_lon_atm, aboxedge1_lat_atm, &
         aboxedge2_lon_atm, aboxedge2_lat_atm, &
         aboxedge3_lon_atm, aboxedge3_lat_atm, &
         ilandmask1_atm, ilandmask2_atm, ilandmask3_atm, &
         ias_out, iaf_out, ips_out, ipf_out, jsf_out, tstar_ocn, &
         koverall_total, co2_atm, ch4_atm, n2o_atm, &
         ocean_stressx2_ocn, ocean_stressy2_ocn, &
         ocean_stressx3_ocn,  ocean_stressy3_ocn, &
         tstar_atm, surf_qstar_atm, atmos_dt_tim, genie_solar_constant, &
         eb_rmax, eb_dphi, eb_rdtdim, eb_ca, global_daysperyear, &
         torog_atm, surf_orog_atm, landice_slicemask_lic, go_syr, &
         flag_ents, atmos_lowestlu2_atm, atmos_lowestlv3_atm, flag_wind)
  END SUBROUTINE initialise_embm_wrapper

  SUBROUTINE ini_goldsteinseaice_wrapper
    USE gold_seaice
    IMPLICIT NONE
    CALL initialise_seaice(alon1_sic, alat1_sic, alon2_sic, alat2_sic, &
         & alon3_sic, alat3_sic, aboxedge1_lon_sic, aboxedge1_lat_sic, &
         aboxedge2_lon_sic, aboxedge2_lat_sic, &
         aboxedge3_lon_sic, aboxedge3_lat_sic, &
         ilandmask1_sic, ilandmask2_sic, ilandmask3_sic, &
         koverall_total, hght_sic, frac_sic, temp_sic, albd_sic, &
         test_energy_seaice)
  END SUBROUTINE ini_goldsteinseaice_wrapper

  SUBROUTINE initialise_goldocean_wrapper
    USE goldstein
    IMPLICIT NONE
    CALL initialise_goldstein(alon1_ocn, alat1_ocn, alon2_ocn, alat2_ocn, &
         & alon3_ocn, alat3_ocn, aboxedge1_lon_ocn, aboxedge1_lat_ocn, &
         aboxedge2_lon_ocn, aboxedge2_lat_ocn, &
         &aboxedge3_lon_ocn, aboxedge3_lat_ocn, depth1_ocn, depth2_ocn, &
         ilandmask1_ocn, ilandmask2_ocn, ilandmask3_ocn, koverall_total, &
         tstar_ocn, sstar_ocn, ustar_ocn, vstar_ocn, albedo_ocn, &
         ias_out, iaf_out, ips_out, ipf_out, jsf_out, lrestart_genie, &
         go_saln0, go_rhoair, go_cd, go_ds, go_dphi, go_ips, go_ipf, &
         go_usc, go_dsc, go_fsc, go_rh0sc, go_rhosc, go_cpsc, go_scf, &
         go_k1, go_dz, go_dza, go_ias, go_iaf, go_jsf, go_c, go_cv, &
         & go_s, go_sv, go_ts, go_ts1, go_rsc, go_syr, go_nyear, go_lin, &
         & go_ec, go_istep0)
  END SUBROUTINE initialise_goldocean_wrapper

  SUBROUTINE initialise_gem_wrapper
    IMPLICIT NONE
    CALL initialise_gem()
  END SUBROUTINE initialise_gem_wrapper

  SUBROUTINE initialise_biogem_wrapper
    IMPLICIT NONE
    CALL initialise_biogem(go_saln0, go_rhoair, go_cd, go_ds, go_dphi, go_usc, &
         & go_dsc, go_fsc, go_rh0sc, go_rhosc, go_cpsc, genie_solar_constant, &
         & go_scf, go_ips, go_ipf, go_ias, go_iaf, go_jsf, &
         & go_k1, go_dz, go_dza, go_c, go_cv, go_s, go_sv, go_ts, go_ts1, &
         & genie_sfcatm1, genie_sfxatm1, genie_sfcocn1, genie_sfxocn1, &
         & genie_sfcsed1, genie_sfxsed1)
  END SUBROUTINE initialise_biogem_wrapper

  SUBROUTINE initialise_atchem_wrapper
    USE atchem
    IMPLICIT NONE
    CALL initialise_atchem (genie_sfxsumatm, genie_sfcatm)
  END SUBROUTINE initialise_atchem_wrapper

  SUBROUTINE initialise_sedgem_wrapper
    IMPLICIT NONE
    CALL initialise_sedgem (genie_timestep, genie_sfxsumsed, genie_sfcsumocn, &
         & genie_sfcsed, genie_sfxocn)
  END SUBROUTINE initialise_sedgem_wrapper

  SUBROUTINE initialise_ents_wrapper
    USE ents
    IMPLICIT NONE
    CALL initialise_ents(go_lin, go_rsc, go_syr, go_nyear, go_ds, go_dphi, &
         & inl1_ocn, go_k1(1:ilon1_ocn, 1:ilat1_ocn), eb_rmax, eb_rdtdim, &
         & tstar_atm, surf_qstar_atm, eb_ca, co2_atm, global_daysperyear, &
         & alat1_ocn, landice_slicemask_lic, albs_atm, land_albs_snow_lnd, &
         & land_albs_nosnow_lnd, land_snow_lnd, land_bcap_lnd, land_z0_lnd, &
         & land_temp_lnd, land_moisture_lnd, genie_sfcatm_lnd, genie_sfxatm_lnd)
  END SUBROUTINE initialise_ents_wrapper

  SUBROUTINE initialise_rokgem_wrapper
    IMPLICIT NONE
    CALL initialise_rokgem (genie_timestep, genie_sfxrok, genie_sfxsumrok1)
  END SUBROUTINE initialise_rokgem_wrapper

  SUBROUTINE initialise_gemlite_wrapper
    USE gemlite
    IMPLICIT NONE
    CALL initialise_gemlite(go_dsc, go_k1, go_dz, go_dza, go_sv, &
         & genie_sfxsumsed, genie_sfxsumrok1_gem, genie_sfxsumatm1_gem)
  END SUBROUTINE initialise_gemlite_wrapper

END MODULE genie_ini_wrappers

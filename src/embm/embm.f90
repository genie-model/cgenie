MODULE embm

  USE genie_util, ONLY: check_unit, check_iostat
  USE genie_control, &
       & ONLY: dim_GOLDSTEINNLONS, dim_GOLDSTEINNLATS, dim_GOLDSTEINNLEVS

  USE embm_lib
  USE embm_netcdf
  USE embm_data
  USE embm_diag
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_embm
  PUBLIC :: step_embm
  PUBLIC :: end_embm
  PUBLIC :: radfor
  PUBLIC :: surflux

CONTAINS

  SUBROUTINE step_embm(istep, latent_atm, sensible_atm, netsolar_atm, &
       & netlong_atm, evap_atm, pptn_atm, stressxu_atm, stressyu_atm, &
       & stressxv_atm, stressyv_atm, tstar_atm, qstar_atm, &
       & torog_atm, surf_orog_atm, flag_ents, lowestlu2_atm, lowestlv3_atm)
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep
    REAL, DIMENSION(:,:), INTENT(IN) :: &
         & latent_atm, sensible_atm, netsolar_atm, &
         & netlong_atm, pptn_atm, evap_atm
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & stressxu_atm, stressyu_atm, stressxv_atm, stressyv_atm, &
         & tstar_atm, qstar_atm
    REAL, INTENT(OUT) :: torog_atm(maxi,maxj)
    REAL, INTENT(IN) :: surf_orog_atm(maxi,maxj)
    LOGICAL, INTENT(IN) :: flag_ents
    REAL, DIMENSION(:,:), INTENT(INOUT) :: lowestlu2_atm, lowestlv3_atm

    INTEGER :: i, j, itv, iout, ios
    REAL :: sum1(4), sum2(4)
    INTEGER :: isum1(4), isum2(4)
    REAL :: t, fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj), qdrydum(maxi,maxj)
    REAL :: work((maxi+1) * (maxj+1))
    CHARACTER(LEN=3) :: ext
    REAL ::  wateratm, tatm, deltq, qsat

    ! Reset internal atmospheric wind fields
    uatm(1,:,:) = lowestlu2_atm / usc
    uatm(2,:,:) = lowestlv3_atm / usc

    ! *** Heat flux ***

    ! Fluxes copied for output routines
    fx0flux(1,:,:) = netsolar_atm
    fx0flux(2,:,:) = sensible_atm
    fx0flux(3,:,:) = netlong_atm
    fx0flux(4,:,:) = latent_atm

    ! *** Freshwater flux ***

    ! Fluxes copied for output routines (note : leaving in precip for
    ! now, even though it's not used here)
    fwflux(1,:,:) = pptn_atm
    fwflux(2,:,:) = evap_atm

    ! Non-dimensionalize surface fluxes for use in tstepa:
    tqa(1,:,:) = (netsolar_atm + latent_atm + &
         & sensible_atm + netlong_atm) * rfluxsca
    tqa(2,:,:) = evap_atm * mm2m * rpmesca

    ! EMBM model timestep
    call tstipa

    ! Diagnostic fields of precipitation-adjusted humidity (i.e.,
    ! humidity after precipitation)
    ! calculate saturation humidity in line with 'surflux.F' for
    ! diagnostics ('surflux.F' will update it again) and calculate
    ! specific and relative humidity after adjustment for
    ! precipitation has been made
    DO j = 1, maxj
       DO i = 1, maxi
          IF (orogswitch >= 1) THEN
             tatm = tq(1,i,j) + (lapse_rate * surf_orog_atm(i,j))
          else
             tatm = tq(1,i,j)
          END IF
          IF (orogswitch < 2 .AND. flag_ents) THEN
             qsat = const1 * EXP(const4 * tatm / (tatm + const5))
          ELSE
             qsat = const1 * EXP(const4 * tq(1,i,j) / (tq(1,i,j) + const5))
          END IF
          IF (flag_ents) THEN
             deltq = lambdapptn * (tq(2,i,j) - (rmax * qsat))
             q_pa(i,j) = MIN(tq(2,i,j), tq(2,i,j) - deltq)
          ELSE
             q_pa(i,j) = MIN(tq(2,i,j), rmax * qsat)
          END IF
          rq_pa(i,j) = q_pa(i,j) / qsat
       END DO
    END DO

    IF (MOD(istep, npstp * ndta) < 1) THEN
       IF (debug_loop) CALL diaga
       IF (debug_loop) PRINT *
    END IF

    ! Atmosphere diagnostics and output
    CALL outm_netcdf_embm(istep)

    ! write EMBM restart file
    IF (MOD(istep, iwstp * ndta) == 0) THEN
       iw = iw + 1
       IF (debug_loop) PRINT *
    END IF

110 FORMAT(4e15.6,2i5,1e15.6,2i5)

    IF (MOD(istep, itstp * ndta) == 0) THEN
       t = REAL(istep) / REAL(nyear * ndta)
       IF (debug_loop) PRINT *, 'Writing to EMBM time-series files'

       CALL check_unit(41, __LINE__, __FILE__)
       OPEN(41,FILE=outdir_name(1:lenout)//lout//'.'//'airt', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       CALL check_unit(42, __LINE__, __FILE__)
       OPEN(42,FILE=outdir_name(1:lenout)//lout//'.'//'q', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       CALL diag3(sum1, sum2, isum1, isum2)

       IF (debug_loop) &
            & WRITE (41,110,IOSTAT=ios) t, sum1(1), sum1(2), sum1(3), &
            & isum1(1), isum1(2), sum1(4), isum1(3), isum1(4)
       CALL check_iostat(ios, __LINE__, __FILE__)
       IF (debug_loop) &
            & WRITE (42,110,IOSTAT=ios) t, sum2(1), sum2(2), sum2(3), &
            & isum2(1), isum2(2), sum2(4), isum2(3), isum2(4)
       CALL check_iostat(ios, __LINE__, __FILE__)

       CLOSE(41,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(42,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       IF (debug_loop) PRINT *
    END IF

    IF (dosc) THEN
       ! Average the last nyear steps in every ianav steps (if ianav>nyear)
       itv = MOD(istep + (nyear * ndta) - 1, ianav * ndta)
       IF (itv < nyear * ndta .AND. MOD(istep, ndta) < 1) THEN
          ext = conv_num(MOD(iav, 10))
          IF (istep >= nyear * ndta .AND. itv == nyear * ndta - 1) THEN
             iout = 1
          ELSE
             iout = 0
          END IF
          IF (debug_loop) &
               & CALL diagosc_embm(istep, iout, ext, fx0flux, fwflux, wateratm)
       END IF
    END IF

    IF (MOD(istep, iwstp * ndta) == 0) THEN
       IF (debug_loop) PRINT *, 'Writing EMBM netCDF file at time', istep
       CALL ini_netcdf_embm(istep, 1)
       qdrydum = 0.0
       CALL write_netcdf_embm(k1, tq, qdrydum, qdrydum,&
            & fx0flux, fwflux, work, maxi, maxj, 1)
       CALL end_netcdf_embm(1)
       IF (debug_loop) PRINT *
    END IF

    ! Output arguments
    ! Surface air temperature [-> surface fluxes]
    tstar_atm = REAL(tq(1,:,:))
    ! Orography-adjusted surface air temperature
    IF (orogswitch >= 1) THEN
       torog_atm = REAL(tq(1,:,:) + lapse_rate * surf_orog_atm)
    ELSE
       torog_atm = REAL(tq(1,:,:))
    END IF
    ! Surface specific humidity [-> surface fluxes]
    qstar_atm = REAL(tq(2,:,:))
    ! Wind stress x components [-> ocean, surface fluxes]
    stressxu_atm = REAL(us_dztau(1,:,:))
    stressxv_atm = REAL(us_dztav(1,:,:))
    ! Wind stress y components [-> ocean, surface fluxes]
    stressyu_atm = REAL(us_dztau(2,:,:))
    stressyv_atm = REAL(us_dztav(2,:,:))

  END SUBROUTINE step_embm


  SUBROUTINE initialise_embm(alon1, alat1, alon2, alat2, alon3, alat3, &
       & aboxedge1_lon, aboxedge1_lat, aboxedge2_lon, aboxedge2_lat, &
       & aboxedge3_lon, aboxedge3_lat, ilandmask1, ilandmask2, ilandmask3, &
       & ias, iaf, ips, ipf, jsf, tstar_ocn, totsteps, &
       & co2_out, ch4_out, n2o_out, stressxu_atm, stressyu_atm, &
       & stressxv_atm, stressyv_atm, tstar_atm, qstar_atm, &
       & atmos_dt_tim, solconst, eb_rmax, eb_dphi, eb_rdtdim, eb_ca, &
       & gn_daysperyear, torog_atm, surf_orog_atm, landice_slicemask_lic, &
       & syr, flag_ents, lowestlu2_atm, lowestlv3_atm, flag_wind)
    USE gem_cmn, ONLY: alloc_error
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(OUT) :: alon1, alon2, alon3
    REAL, DIMENSION(:), INTENT(OUT) :: alat1, alat2, alat3
    REAL, DIMENSION(:), INTENT(OUT) :: &
         & aboxedge1_lon, aboxedge2_lon, aboxedge3_lon
    REAL, DIMENSION(:), INTENT(OUT) :: &
         & aboxedge1_lat, aboxedge2_lat, aboxedge3_lat
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: &
         & ilandmask1, ilandmask2, ilandmask3
    INTEGER, DIMENSION(:), INTENT(IN) :: ias, iaf, ips, ipf
    INTEGER, INTENT(IN) :: jsf
    REAL, DIMENSION(:,:), INTENT(IN) :: tstar_ocn
    REAL, DIMENSION(:,:), INTENT(OUT) :: co2_out, ch4_out, n2o_out
    INTEGER(KIND=8), INTENT(IN) :: totsteps
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & stressxu_atm, stressyu_atm, stressxv_atm, stressyv_atm, &
         & tstar_atm, qstar_atm
    REAL, INTENT(OUT) :: atmos_dt_tim
    REAL, INTENT(IN) :: solconst
    REAL, INTENT(OUT) :: eb_rmax, eb_dphi, eb_rdtdim, eb_ca(:,:)
    REAL, INTENT(IN) :: gn_daysperyear
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & torog_atm, surf_orog_atm, landice_slicemask_lic
    REAL, INTENT(IN) :: syr
    LOGICAL, INTENT(IN) :: flag_ents
    REAL, DIMENSION(:,:), INTENT(INOUT) :: lowestlu2_atm, lowestlv3_atm

    INTEGER, DIMENSION(:,:), ALLOCATABLE :: bmask
    REAL :: z1, tv, tv1, tv2, tv3, tv4, tv5, tatm, relh0_ocean, &
         & relh0_land, diffamp(2), diffwid, difflin, diffend
    REAL, DIMENSION(:), ALLOCATABLE :: zro, zw
    REAL :: radfor_scl_co2, radfor_pc_co2_rise
    REAL :: radfor_scl_ch4, radfor_pc_ch4_rise
    REAL :: radfor_scl_n2o, radfor_pc_n2o_rise
    REAL :: u_tau_ice, ch_ice, cpo_ice
    REAL :: th0, th1, s0, s1, phix, scl_fwf
    REAL :: theta, thv, dth, dscon
    REAL, PARAMETER :: deg_to_rad = pi / 180.0
    REAL :: area_atl1a, area_atl1b, area_atl1c
    REAL :: area_pac1a, area_pac1b, area_pac1c
    INTEGER :: i, j, k, l, m, natl1a, npac1a, natl1b, npac1b, natl1c, npac1c, n

    CHARACTER(LEN=6) :: world
    INTEGER :: lenworld
    CHARACTER(LEN=127) :: xu_wstress, yu_wstress, xv_wstress, yv_wstress
    INTEGER :: len_xu, len_yu, len_xv, len_yv
    CHARACTER(LEN=20) :: u_wspeed, v_wspeed
    INTEGER :: len_uws, len_vws
    CHARACTER(LEN=1) :: ans, netin

    REAL :: deltq, qsat

    ! File access checks
    INTEGER :: ios
    LOGICAL :: ioex

    ! Antarctic atmospheric diffusivity fudge
    ! Diffusivity scaling factor
    REAL :: diffa_scl
    ! Grid point distance over which scalar is applied (j direction)
    INTEGER :: diffa_len
    ! Planetary albedo modifications
    REAL :: albedop_offs, albedop_amp, albedop_skew
    INTEGER :: albedop_skewp
    REAL :: albedop_mod2, albedop_mod4, albedop_mod6
    ! Planetary albedo modifications: local
    REAL :: albedop_scl

    ! Seasonal fields
    REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: uatml1
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
         & usurfl1, tncep1, pncep1, rhncep1, atm_alb1

    ! Precipitation timescale and land radiation
    REAL :: timepptn, hld, cld

    ! Southern boundaries of regions for FW flux adjustment
    INTEGER :: j1as, j1bs, j1cs

    ! This flag indicates if external wind forcing from 'genie-wind'
    ! module is active
    LOGICAL :: flag_wind

    REAL :: orbitall_vect(5)

    INTEGER, EXTERNAL :: lnsig1

    ! Declare a namelist to store EMBM's initialisation vars
    NAMELIST /ini_embm_nml/ indir_name, outdir_name, rstdir_name
    NAMELIST /ini_embm_nml/ igrid, world
    NAMELIST /ini_embm_nml/ xu_wstress, yu_wstress, xv_wstress
    NAMELIST /ini_embm_nml/ yv_wstress, u_wspeed, v_wspeed
    NAMELIST /ini_embm_nml/ npstp, iwstp, itstp, ianav, ans
    NAMELIST /ini_embm_nml/ yearlen, nyear, ndta, scf
    NAMELIST /ini_embm_nml/ diffamp, diffwid, difflin, betaz, betam
    NAMELIST /ini_embm_nml/ radfor_scl_co2, radfor_pc_co2_rise
    NAMELIST /ini_embm_nml/ radfor_scl_ch4, radfor_pc_ch4_rise
    NAMELIST /ini_embm_nml/ radfor_scl_n2o, radfor_pc_n2o_rise
    NAMELIST /ini_embm_nml/ tatm, relh0_ocean, relh0_land
    NAMELIST /ini_embm_nml/ extra1a, extra1b, extra1c, scl_fwf
    NAMELIST /ini_embm_nml/ z1_embm, tdatafile, qdatafile
    NAMELIST /ini_embm_nml/ tdata_varname, qdata_varname
    NAMELIST /ini_embm_nml/ tdata_missing, qdata_missing
    NAMELIST /ini_embm_nml/ tdata_scaling, qdata_scaling
    NAMELIST /ini_embm_nml/ tdata_offset, qdata_offset
    NAMELIST /ini_embm_nml/ qdata_rhum, tqinterp, lout, netin
    NAMELIST /ini_embm_nml/ rmax, filenetin, dirnetout, atchem_radfor
    NAMELIST /ini_embm_nml/ orbit_radfor, t_orbit, norbit, orbitsteps
    NAMELIST /ini_embm_nml/ filenameorbit, t_co2, nco2, co2steps, filenameco2
    NAMELIST /ini_embm_nml/ diffa_scl, diffa_len, dosc, delf2x
    NAMELIST /ini_embm_nml/ olr_adj0, olr_adj, t_eqm, aerofac, volfac, solfac
    NAMELIST /ini_embm_nml/ useforc, forcname, albedop_offs, albedop_amp
    NAMELIST /ini_embm_nml/ albedop_skew, albedop_skewp
    NAMELIST /ini_embm_nml/ albedop_mod2, albedop_mod4, albedop_mod6
    NAMELIST /ini_embm_nml/ lapse_rate, orogswitch, t_orog, filenameorog
    NAMELIST /ini_embm_nml/ norog, orogsteps, t_lice, filenamelice, nlice
    NAMELIST /ini_embm_nml/ licesteps, lice_k9, t_d18o, nd18o, d18osteps, d18o_k
    NAMELIST /ini_embm_nml/ scale_mwfx, filenamed18o, filenamed18oicethresh
    NAMELIST /ini_embm_nml/ filenamed18oorogmin, filenamed18ooroggrad
    NAMELIST /ini_embm_nml/ ents_seasonswitch, ents_offlineswitch, timepptn
    NAMELIST /ini_embm_nml/ par_runoff_scheme, par_runoff_b, par_runoff_tau
    NAMELIST /ini_embm_nml/ debug_init, debug_end, debug_loop
    NAMELIST /ini_embm_nml/ par_wind_polar_avg, unify_winds, par_sich_max
    NAMELIST /ini_embm_nml/ par_albsic_min, par_albsic_max

    ! Initialize variables
    area_atl1a = 0.0 ; area_atl1b = 0.0 ; area_atl1c = 0.0
    area_pac1a = 0.0 ; area_pac1b = 0.0 ; area_pac1c = 0.0
    j1as = 0 ; j1bs = 0 ; j1cs = 0
    len_xu = 0 ; len_yu = 0 ; len_xv = 0 ; len_yv = 0
    len_uws = 0 ; len_vws = 0

    ! Setting up EMBM
    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising EMBM atmosphere module ...'

    maxi = dim_GOLDSTEINNLONS
    maxj = dim_GOLDSTEINNLATS
    maxk = dim_GOLDSTEINNLEVS

    IF (debug_init) PRINT *

    CALL check_unit(56, __LINE__, __FILE__)
    OPEN(UNIT=56,FILE='data_EMBM',STATUS='old',IOSTAT=ios)
    IF (ios /= 0) THEN
       PRINT *, 'ERROR: could not open EMBM namelist file'
       PRINT *, "ERROR on line ", __LINE__, " in file ", __FILE__
       STOP
    END IF

    READ(UNIT=56,NML=ini_embm_nml,IOSTAT=ios)
    IF (ios /= 0) THEN
       PRINT *, 'ERROR: could not read EMBM namelist'
       PRINT *, "ERROR on line ", __LINE__, " in file ", __FILE__
       STOP
    ELSE
       CLOSE(56,IOSTAT=ios)
       IF (ios /= 0) THEN
          PRINT *, 'ERROR: could not close EMBM namelist file'
          PRINT *, "ERROR on line ", __LINE__, " in file ", __FILE__
          STOP
       END IF
    END IF

    ALLOCATE(k1(0:maxi+1,0:maxj+1),STAT=alloc_error) ; k1 = 0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(ku(2,maxi,maxj),STAT=alloc_error)       ; ku = 0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(mk(maxi+1,maxj),STAT=alloc_error)       ; mk = 0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(dt(maxk),STAT=alloc_error)       ; dt = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(ds(maxj),STAT=alloc_error)       ; ds = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(dsv(1:maxj-1),STAT=alloc_error)  ; dsv = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rds2(2:maxj-1),STAT=alloc_error) ; rds2 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(dz(maxk),STAT=alloc_error)       ; dz = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(s(0:maxj),STAT=alloc_error)      ; s = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(c(0:maxj),STAT=alloc_error)      ; c = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(sv(0:maxj),STAT=alloc_error)     ; sv = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(dzu(2,maxk),STAT=alloc_error)    ; dzu = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(tau(2,maxi,maxj),STAT=alloc_error)    ; tau = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(drag(2,maxi+1,maxj),STAT=alloc_error) ; drag = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(dztau(2,maxi,maxj),STAT=alloc_error)  ; dztau = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(cv(0:maxj),STAT=alloc_error) ; cv = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(dza(maxk),STAT=alloc_error)  ; dza = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tsa0(maxj),STAT=alloc_error) ; tsa0 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(tau0(maxi,maxj),STAT=alloc_error)   ; tau0 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(dztav0(maxi,maxj),STAT=alloc_error) ; dztav0 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tau1(maxi,maxj),STAT=alloc_error)   ; tau1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(dztav1(maxi,maxj),STAT=alloc_error) ; dztav1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(dztav(2,maxi,maxj),STAT=alloc_error) ; dztav = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(rc(0:maxj),STAT=alloc_error)     ; rc = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rcv(1:maxj-1),STAT=alloc_error)  ; rcv = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rds(maxj),STAT=alloc_error)      ; rds = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rdsv(1:maxj-1),STAT=alloc_error) ; rdsv = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(cv2(1:maxj-1),STAT=alloc_error)  ; cv2 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rc2(0:maxj),STAT=alloc_error)    ; rc2 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rdz(maxk),STAT=alloc_error)      ; rdz = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rdza(maxk),STAT=alloc_error)     ; rdza = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(rtv(maxi,maxj),STAT=alloc_error)  ; rtv = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rtv3(maxi,maxj),STAT=alloc_error) ; rtv3 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(us_dztau(2, maxi, maxj),STAT=alloc_error) ; us_dztau = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(us_dztav(2, maxi, maxj),STAT=alloc_error) ; us_dztav = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(asurf(maxj),STAT=alloc_error) ; asurf = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(qsata(maxi,maxj),STAT=alloc_error) ; qsata = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(qsato(maxi,maxj),STAT=alloc_error) ; qsato = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(co2(maxi,maxj),STAT=alloc_error)   ; co2 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(ch4(maxi,maxj),STAT=alloc_error)   ; ch4 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(n2o(maxi,maxj),STAT=alloc_error)   ; n2o = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(tq(2,maxi,maxj),STAT=alloc_error)      ; tq = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tq1(2,maxi,maxj),STAT=alloc_error)     ; tq1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(varice(2,maxi,maxj),STAT=alloc_error)  ; varice = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(varice1(2,maxi,maxj),STAT=alloc_error) ; varice1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tqa(2,maxi,maxj),STAT=alloc_error)     ; tqa = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(albcl(maxi,maxj),STAT=alloc_error)  ; albcl = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxsw(maxi,maxj),STAT=alloc_error)   ; fxsw = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxplw(maxi,maxj),STAT=alloc_error)  ; fxplw = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fx0a(maxi,maxj),STAT=alloc_error)   ; fx0a = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fx0o(maxi,maxj),STAT=alloc_error)   ; fx0o = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxsen(maxi,maxj),STAT=alloc_error)  ; fxsen = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(pmeadj(maxi,maxj),STAT=alloc_error) ; pmeadj = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(pptn(maxi,maxj),STAT=alloc_error)   ; pptn = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(evap(maxi,maxj),STAT=alloc_error)   ; evap = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(usurf(maxi,maxj),STAT=alloc_error)  ; usurf = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxlata(maxi,maxj),STAT=alloc_error) ; fxlata = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxlato(maxi,maxj),STAT=alloc_error) ; fxlato = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxlw(maxi,maxj),STAT=alloc_error)   ; fxlw = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(ca(maxi,maxj),STAT=alloc_error)     ; ca = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(qb(maxi,maxj),STAT=alloc_error)     ; qb = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(qbsic(maxi,maxj),STAT=alloc_error)  ; qbsic = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(diffa(2,2,maxj),STAT=alloc_error) ; diffa = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(fx0sic(maxi,maxj),STAT=alloc_error)   ; fx0sic = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fx0neto(maxi,maxj),STAT=alloc_error)  ; fx0neto = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fwfxneto(maxi,maxj),STAT=alloc_error) ; fwfxneto = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(evapsic(maxi,maxj),STAT=alloc_error)  ; evapsic = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tsfreez(maxi,maxj),STAT=alloc_error)  ; tsfreez = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(uatm(2,maxi,maxj),STAT=alloc_error) ; uatm = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(fxlatavg(maxi,maxj),STAT=alloc_error) ; fxlatavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxsenavg(maxi,maxj),STAT=alloc_error) ; fxsenavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxswavg(maxi,maxj),STAT=alloc_error)  ; fxswavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fxlwavg(maxi,maxj),STAT=alloc_error)  ; fxlwavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fwpptavg(maxi,maxj),STAT=alloc_error) ; fwpptavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fwevpavg(maxi,maxj),STAT=alloc_error) ; fwevpavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(tqavg(2,maxi,maxj),STAT=alloc_error)  ; tqavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fx0avg(4,maxi,maxj),STAT=alloc_error) ; fx0avg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(fwavg(2,maxi,maxj),STAT=alloc_error)  ; fwavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(palb(maxi,maxj),STAT=alloc_error)    ; palb = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(palbavg(maxi,maxj),STAT=alloc_error) ; palbavg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(d18o_ice_thresh(maxi,maxj),STAT=alloc_error) ; d18o_ice_thresh = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(d18o_orog_min(maxi,maxj),STAT=alloc_error)   ; d18o_orog_min = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(d18o_orog_grad(maxi,maxj),STAT=alloc_error)  ; d18o_orog_grad = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(chl(maxi,maxj),STAT=alloc_error) ; chl = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(cel(maxi,maxj),STAT=alloc_error) ; cel = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(q_pa(maxi,maxj),STAT=alloc_error)      ; q_pa = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rq_pa(maxi,maxj),STAT=alloc_error)     ; rq_pa = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(q_pa_avg(maxi,maxj),STAT=alloc_error)  ; q_pa_avg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rq_pa_avg(maxi,maxj),STAT=alloc_error) ; rq_pa_avg = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(iroff(maxi,maxj),STAT=alloc_error) ; iroff = 0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(jroff(maxi,maxj),STAT=alloc_error) ; jroff = 0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(nclon1(maxi),STAT=alloc_error) ; nclon1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(nclon2(maxi),STAT=alloc_error) ; nclon2 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(nclon3(maxi),STAT=alloc_error) ; nclon3 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(nclat1(maxj),STAT=alloc_error) ; nclat1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(nclat2(maxj),STAT=alloc_error) ; nclat2 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(nclat3(maxj),STAT=alloc_error) ; nclat3 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ! Local allocations

    ALLOCATE(bmask(maxi,maxj),STAT=alloc_error) ; bmask = 0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(zro(maxk),STAT=alloc_error)        ; zro = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(zw(0:maxk),STAT=alloc_error)       ; zw = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ALLOCATE(uatml1(2,maxi,maxj,nmth+1),STAT=alloc_error) ; uatml1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(usurfl1(maxi,maxj,nmth+1),STAT=alloc_error)  ; usurfl1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tncep1(maxi,maxj,nmth+1),STAT=alloc_error)   ; tncep1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(pncep1(maxi,maxj,nmth+1),STAT=alloc_error)   ; pncep1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rhncep1(maxi,maxj,nmth+1),STAT=alloc_error)  ; rhncep1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(atm_alb1(maxi,maxj,nmth+1),STAT=alloc_error) ; atm_alb1 = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)

    ! Input directory name
    lenin = lnsig1(indir_name)
    IF (indir_name(lenin:lenin) /= '/') THEN
       lenin = lenin + 1
       indir_name(lenin:lenin) = '/'
    END IF
    IF (debug_init) PRINT *, 'Input dir. name ', indir_name(1:lenin)

    ! Output directory name
    lenout = lnsig1(outdir_name)
    IF (outdir_name(lenout:lenout) /= '/') THEN
       lenout = lenout + 1
       outdir_name(lenout:lenout+1) = '/'
    END IF
    IF (debug_init) PRINT *, 'Output dir. name ', outdir_name(1:lenout)

    ! Restart (input) directory name
    lenrst = lnsig1(rstdir_name)
    IF (rstdir_name(lenrst:lenrst) /= '/') THEN
       lenrst = lenrst + 1
       rstdir_name(lenrst:lenrst+1) = '/'
    END IF
    IF (debug_init) PRINT *, 'Restart dir. name ', rstdir_name(1:lenrst)

    ! Read in topography filename
    lenworld = lnsig1(world)
    IF (debug_init) PRINT *, 'Topography name ', world(1:lenworld)

    ! Read in wind stress filenames
    IF (.NOT. flag_wind) THEN
       len_xu = lnsig1(xu_wstress)
       len_yu = lnsig1(yu_wstress)
       len_xv = lnsig1(xv_wstress)
       len_yv = lnsig1(yv_wstress)
       IF (debug_init) THEN
          PRINT *,  'x windstress at u point ', xu_wstress(1:len_xu)
          PRINT *,  'y windstress at u point ', yu_wstress(1:len_yu)
          PRINT *,  'x windstress at v point ', xv_wstress(1:len_xv)
          PRINT *,  'y windstress at v point ', yv_wstress(1:len_yv)
       END IF
    END IF

    ! Read in wind speed filenames
    IF (.NOT. flag_wind) THEN
       len_uws = lnsig1(u_wspeed)
       len_vws = lnsig1(v_wspeed)
       IF (debug_init) THEN
          PRINT *, 'u wind speed ', u_wspeed(1:len_uws)
          PRINT *, 'v wind speed ', v_wspeed(1:len_vws)
       END IF
    END IF

    ! Time-steps information
    nsteps = totsteps
    IF (debug_init) THEN
       PRINT *, 'npstp iwstp itstp ianav'
       PRINT *, npstp, iwstp, itstp, ianav
       PRINT *, 'new or continuing run ?'
       PRINT *, ans
       PRINT *, 'number of days per EMBM year'
       PRINT *, yearlen
       PRINT *, 'seasonality enabled =', dosc
    END IF

    ! parameters for setting up grid
    ! th is latitude, coords are sin(th), longitude phi, and z
    th0 = -pi/2 ; th1 = pi/2
    s0 = SIN(th0) ; s1 = SIN(th1)
    phix = 2*pi

    dphi = phix / maxi
    IF (igrid < 2) phi0 = -260.0 * deg_to_rad
    rdphi = 1.0 / dphi

    ! Set up horizontal grid: sin and cos factors at rho and v points
    ! (c grid) fix for global domain although only cv and cv2 are
    ! referred to at or beyond limits 24/6/2 if no flow out of N + S
    ! boundaries.
    sv(0) = s0
    cv(0) = COS(th0)
    IF (igrid == 1) THEN
       dth = (th1 - th0) / maxj
       DO j = 1, maxj
          thv = th0 + j * dth
          theta = thv - 0.5 * dth
          sv(j) = SIN(thv)
          s(j) = SIN(theta)
          cv(j) = COS(thv)
       END DO
    ELSE IF (igrid == 0) THEN
       dscon = (s1 - s0) / maxj
       DO j = 1, maxj
          sv(j) = s0 + j * dscon
          cv(j) = SQRT(1 - sv(j) * sv(j))
          s(j) = sv(j) - 0.5 * dscon
       END DO
    END IF
    IF (debug_init) THEN
       PRINT *, 'EMBM latitudes: velocity; tracers'
       PRINT *, 'j, 180/pi*asin(sv(j)), 180/pi*asin(s(j))'
    END IF
    DO j = 1, maxj
       ds(j) = sv(j) - sv(j-1)
       rds(j) = 1.0 / ds(j)
       c(j) = SQRT(1 - s(j) * s(j))
       rc(j) = 1.0 / c(j)
       rc2(j) = rc(j) * rc(j) * rdphi
       IF (j < maxj) THEN
          dsv(j) = s(j+1) - s(j)
          rdsv(j) = 1.0 / dsv(j)
          rcv(j) = 1.0 / cv(j)
          cv2(j) = cv(j) * cv(j) * rdsv(j)
          IF (j > 1) rds2(j) = 2.0 / (dsv(j) + dsv(j-1))
       END IF
       IF (debug_init) PRINT *, j, 180 / pi * ASIN(sv(j)), 180 / pi * ASIN(s(j))
    END DO

    ! area of grid cell (assumes sine(lat) grid)
    DO j = 1, maxj
       asurf(j) = rsc * rsc * ds(j) * dphi
       IF (debug_init) &
            &PRINT *, 'j = ', j, 'EMBM grid cell area is', asurf(j), 'm2'
    END DO

    ! seasonality
    IF (debug_init) PRINT *, 'timesteps per year and A/O dt ratio'
    IF (debug_init) PRINT *, nyear, ndta
    tv = 86400.0 * yearlen / (nyear * tsc)
    ryear = 1.0 / (yearlen * 86400)
    ALLOCATE(solfor(maxj,nyear),STAT=alloc_error)       ; solfor = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(albo(maxj,nyear),STAT=alloc_error)         ; albo = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(uatml(2,maxi,maxj,nyear),STAT=alloc_error) ; uatml = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(usurfl(maxi,maxj,nyear),STAT=alloc_error)  ; usurfl = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(tncep(maxi,maxj,nyear),STAT=alloc_error)   ; tncep = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(pncep(maxi,maxj,nyear),STAT=alloc_error)   ; pncep = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(rhncep(maxi,maxj,nyear),STAT=alloc_error)  ; rhncep = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(atm_alb(maxi,maxj,nyear),STAT=alloc_error) ; atm_alb = 0.0
    CALL check_iostat(alloc_error,__LINE__,__FILE__)


    dtatm = tv / ndta
    IF (debug_init) PRINT *, 'embm timestep (s) =', dtatm * tsc
    atmos_dt_tim = REAL(dtatm * tsc)

    dt = tv
    IF (debug_init) PRINT *, 'dimensional ocean timestep', tv * tsc / 86400
    IF (debug_init) PRINT *, 'dimensionless O/A timesteps', tv, dtatm

    rdtdim = 1.0 / (tsc * dt(maxk))
    IF (debug_init) PRINT *, 'rdtdim = ', rdtdim

    ! Set up grid
    ! For variable (exponential) dz use ez0 > 0, ELSE use ez0 < 0
    ez0 = 0.1
    z1 = ez0*((1.0 + 1 / ez0)**(1.0 / maxk) - 1.0)
    IF (debug_init) PRINT *, 'z1', z1
    tv4 = ez0 * ((z1 / ez0 + 1)**0.5 - 1)
    tv2 = 0
    tv1 = 0
    zro(maxk) = -tv4
    zw(maxk) = tv2
    DO k = 1, maxk
       IF (ez0 > 0) THEN
          tv3 = ez0 * ((z1 / ez0 + 1)**k - 1)
          dz(maxk-k+1) = tv3 - tv2
          tv2 = tv3
          tv5 = ez0 * ((z1 / ez0 + 1)**(k + 0.5) - 1)
          IF (k < maxk) dza(maxk-k) = tv5 - tv4
          tv4 = tv5
          tv1 = tv1 + dz(maxk-k+1)
          ! tv3 is the depth of the kth w level from the top
          ! tv5 is the depth of the k+1th density level from the top
       ELSE
          dz(k) = REAL(1.0D0 / maxk)
          dza(k) = REAL(1.0D0 / maxk)
       END IF
    END DO

    DO k = maxk, 1, -1
       IF (k > 1) zro(k-1) = zro(k) - dza(k-1)
       zw(k-1) = zw(k) - dz(k)
    END DO
    IF (debug_init) WRITE (6,'(i4,3e12.4)') &
         & (k, dsc * zw(k), dsc * zro(k), dsc * dz(k), k = maxk, 1, -1)

    IF (debug_init) PRINT *, 'dzz'
    dzz = dz(maxk) * dza(maxk-1) / 2
    IF (debug_init) PRINT *, dzz

    ! efficiency array
    rdz(1:maxk-1) = 1.0 / dz(1:maxk-1)
    rdza(1:maxk-1) = 1.0 / dza(1:maxk-1)
    rdz(maxk) = 1.0 / dz(maxk)

    ! dza(maxk) never referenced, set to 0 for Andy's biogeo-code
    dza(maxk) = 0.0

    ! Set up sin and cos factors at rho and v points (c grid) fix for
    ! global domain although only cv and cv2 are referred to at or
    ! beyond limits 24/6/2 if no flow out of N + S boundaries.
    IF (debug_init) PRINT *, 'scf'
    IF (debug_init) PRINT *, scf

    ! define forcing

    ! read wind data
    IF (.NOT. flag_wind) THEN
       ! taux,tauy at u-points
       CALL check_unit(96, __LINE__, __FILE__)
       OPEN(96,FILE=indir_name(1:lenin)//xu_wstress(1:len_xu),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CALL check_unit(97, __LINE__, __FILE__)
       OPEN(97,FILE=indir_name(1:lenin)//yu_wstress(1:len_yu),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! taux,tauy at v-points
       CALL check_unit(98, __LINE__, __FILE__)
       OPEN(98,FILE=indir_name(1:lenin)//xv_wstress(1:len_xv),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CALL check_unit(99, __LINE__, __FILE__)
       OPEN(99,FILE=indir_name(1:lenin)//yv_wstress(1:len_yv),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       DO j = 1, maxj
          DO i = 1, maxi
             ! Variables dztau and dztav are renamed here to separate
             ! the unscaled (hence 'us') versions read in from files,
             ! from those scaled versions used in surflux and
             ! GOLDSTEIN.
             READ (96,*,IOSTAT=ios) us_dztau(1,i,j)
             CALL check_iostat(ios, __LINE__, __FILE__)
             READ (97,*,IOSTAT=ios) us_dztau(2,i,j)
             CALL check_iostat(ios, __LINE__, __FILE__)
             READ (98,*,IOSTAT=ios) us_dztav(1,i,j)
             CALL check_iostat(ios, __LINE__, __FILE__)
             READ (99,*,IOSTAT=ios) us_dztav(2,i,j)
             CALL check_iostat(ios, __LINE__, __FILE__)
          END DO
       END DO

       CLOSE(96,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(97,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(98,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(99,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
    ELSE
       us_dztau = 0.0
       us_dztav = 0.0
    END IF

    ntot = 0
    intot = 0

    IF (debug_init) PRINT *, 'Land runoff being read in'
    CALL check_unit(13, __LINE__, __FILE__)
    OPEN(13,FILE=indir_name(1:lenin)//world//'.k1',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! Note k1(i,j) must be periodic ; k1(0,j) - k1(maxi,j) = 0 and
    ! k1(1,j) - k1(maxi+1,j) = 0, as enforced below;
    do j = maxj+1, 0, -1
       READ (13,*,IOSTAT=ios) (k1(i,j), i = 0, maxi+1)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! rotate grid to check b.c.s
       k1(0,j) = k1(maxi,j)
       k1(maxi+1,j) = k1(1,j)
       IF (debug_init .AND. j /= 0 .AND. j /= maxj+1) &
            & WRITE(6,'(i4,32i3)') j, (k1(i,j), i = 1, 32)
    END DO

    ! read ips etc if possible
    close(13,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) PRINT *, 'Land runoff successfully read in'

    ! read parameters
    IF (debug_init) PRINT *, 'diffamp(1),diffamp(2), diffwid, difflin'
    IF (debug_init) PRINT *, diffamp(1), diffamp(2), diffwid, difflin
    ! Parameter beta relates vertically-averaged advective transport
    ! to surface advective transport
    IF (debug_init) PRINT *, 'betaz(1),betam(1),betaz(2),betam(2)'
    IF (debug_init) PRINT *, betaz(1), betam(1), betaz(2), betam(2)

    ! climatological albedo (similar to Weaver et al. 2001)
    IF (debug_init) PRINT *, 'climatological albedo, by latitude'
    DO j = 1, maxj
       albedop_scl = ((albedop_skew - s(j)) / 2.0)**albedop_skewp
       tv = ASIN(s(j))
       albcl(1:maxi,j) = albedop_offs + albedop_amp * 0.5 * &
            & (1.0 - COS(2.0 * tv) + &
            & albedop_scl * albedop_mod2 * COS(2.0 * tv) + &
            & albedop_scl * albedop_mod4 * COS(4.0 * tv) + &
            & albedop_scl * albedop_mod6 * COS(6.0 * tv))
       IF (debug_init) PRINT *, j, albcl(1,j)
    END DO

    ! atmospheric SSW absorption coefficient, value over land purely
    ! diagnostic
    WHERE (k1(1:maxi,1:maxj) <= maxk)
       ca = 0.3
    ELSEWHERE
       ca = 1.0
    END WHERE

    ! read some scalings
    IF (debug_init) THEN
       PRINT *, 'radfor_scl_co2 =', radfor_scl_co2
       PRINT *, 'radfor_pc_co2_rise =', radfor_pc_co2_rise
       PRINT *, 'radfor_scl_ch4 =', radfor_scl_ch4
       PRINT *, 'radfor_pc_ch4_rise =', radfor_pc_ch4_rise
       PRINT *, 'radfor_scl_n2o =', radfor_scl_n2o
       PRINT *, 'radfor_pc_n2o_rise =', radfor_pc_n2o_rise
    END IF

    ! Factor corresponding to radiative forcing of 4 W/m**2 per
    ! doubling of atmospheric CO2 NOTE: value now set via a namelist
    IF (debug_init) PRINT *, 'delf2x =', delf2x
    IF (debug_init) &
         & PRINT *, '=> climate sensitivity =', delf2x * LOG(2.0), ' Wm-2'

    ! initialize greenhouse gas concentrations
    DO j = 1, maxj
       DO i = 1, maxi
          co2(i,j) = radfor_scl_co2 * co20
          ch4(i,j) = radfor_scl_ch4 * ch40
          n2o(i,j) = radfor_scl_n2o * n2o0
          co2_out(i,j) = co2(i,j)
          ch4_out(i,j) = ch4(i,j)
          n2o_out(i,j) = n2o(i,j)
       END DO
    END DO
    ! rate of increase of greenhouse gas concentrations
    rate_co2 = radfor_pc_co2_rise * 0.01 * tsc * dtatm * ndta * ryear
    rate_ch4 = radfor_pc_ch4_rise * 0.01 * tsc * dtatm * ndta * ryear
    rate_n2o = radfor_pc_n2o_rise * 0.01 * tsc * dtatm * ndta * ryear

    ! alternative method for defining time varying co2 from an input
    ! time series read in time varying co2
    IF (t_co2 > 0) THEN
       IF (debug_init) PRINT *,  'co2 defined from ', TRIM(filenameco2)
       IF (debug_init) PRINT *, 't_co2,nco2,co2steps', t_co2, nco2, co2steps
       OPEN(729,FILE=TRIM(filenameco2))
       ALLOCATE(co2_vect(nco2),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       DO i = 1, nco2
          READ (729,*) co2_vect(i)
       END DO
       CLOSE(729)
       co2(:,:) = co2_vect(1)
       co2_out(:,:) = co2_vect(1)
    END IF

    ! depth scale for atmospheric thermal b.l. used by Weaver et al. (2001)
    hatmbl(1) = 8400.0

    ! scaling for heat forcing of atmosphere
    rfluxsca = rsc / (hatmbl(1) * usc * rhoair * cpa)

    ! atmospheric winds
    IF (.NOT. flag_wind) THEN
       CALL check_unit(35, __LINE__, __FILE__)
       OPEN(35,file=indir_name(1:lenin)//u_wspeed(1:len_uws), IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (35,*,IOSTAT=ios)((uatm(1,i,j), i = 1, maxi), j = 1, maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(35,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       OPEN(35,file=indir_name(1:lenin)//v_wspeed(1:len_vws),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (35,*,IOSTAT=ios)((uatm(2,i,j), i = 1, maxi), j = 1, maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(35,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! Conditional zonal average
       ! only when par_wind_polar_avg is 0 (default)
       ! 1 makes the code consistent with genie-winds
       ! 2 removes all conditional zonal averages
       IF (par_wind_polar_avg /= 1 .AND. par_wind_polar_avg /= 2)  then
          IF (debug_init) PRINT *, 'Averaging advective winds near poles'
          DO j = 1, maxj
             IF (j <= 2 .OR. j >= maxj-1) THEN
                DO l = 1, 2
                   tv = SUM(uatm(l,:,j)) / maxi
                   uatm(l,:,j) = tv
                END DO
             END IF
          END DO
       END IF

       ! remove zonal average of v ELSE fail mass conservation (may not be
       ! disastrous).
       uatm = uatm / usc
       IF (par_wind_polar_avg /= 1 .AND. par_wind_polar_avg /= 2) THEN
          uatm(2,:,maxj) = 0.
       END IF
    ELSE
       uatm = 0.0
    END IF

    ! parameters for extra heat diffusion where pptn high
    ppmin = 2.0 / (yearlen * 86400.0)
    ppmax = 4.0 / (yearlen * 86400.0)

    ! nre simpler diffusivity
    diffend = EXP(-(0.5 * pi / diffwid)**2)
    IF (debug_init) PRINT *, &
         & 'atm stability numbers, need to be small for explicit dt'
    DO j = 1, maxj
       tv = ASIN(s(j))
       tv2 = ASIN(sv(j))
       diffa(2,1,j) = diffamp(2)
       diffa(2,2,j) = diffamp(2)
       ! Variable-width and asymmetry (or slope) thermal diffusivity
       diffa(1,1,j) = diffamp(1) * (difflin * 2.0 * (tv + 0.5 * pi) / pi + &
            & (1.0 - difflin) * (EXP(-(tv / diffwid)**2) - diffend) / &
            & (1.0 - diffend))
       diffa(1,2,j) = diffamp(1) * (difflin * 2.0 * (tv2 + 0.5 * pi) / pi + &
            & (1.0 - difflin) * (EXP(-(tv2 / diffwid)**2) - diffend) / &
            & (1.0 - diffend))

       ! Adjust atmospheric diffusivity over S. Ocean and Antarctica
       ! Scale meridional heat diffusion by a factor of diffa_scl as
       ! far as diffa_len in the j direction
       IF (diffa_len < 0) THEN
          IF (SIN(pi * REAL(diffa_len) / 180.0) > sv(j)) THEN
             diffa(1,2,j) = diffa_scl * diffa(1,2,j)
          END IF
       ELSE
          IF (j <= diffa_len) THEN
             diffa(1,2,j) = diffa_scl * diffa(1,2,j)
          END IF
       END IF

       ! non-dimensionalise diffusivities
       diffa(1,1,j) = diffa(1,1,j) / (rsc * usc)
       diffa(1,2,j) = diffa(1,2,j) / (rsc * usc)
       diffa(2,1,j) = diffa(2,1,j) / (rsc * usc)
       diffa(2,2,j) = diffa(2,2,j) / (rsc * usc)

       ! Test for const lat, very unstable diffusivities so control T
       ! diffusion by input params and make sure q diff is no bigger
       ! only if constant lat grid (i.e. allowing compatibility with
       ! 36x36 version)
       IF (igrid == 1 .OR. igrid == 2) THEN
          diffa(2,1,j) = MIN(diffa(2,1,j), diffa(1,1,j))
       END IF
       IF (debug_init .AND. j > 1) WRITE (6,'(i4,4e15.5)') j, &
            & diffa(1,1,j) * dtatm * rc(j) * rc(j) * rdphi * rdphi, &
            & diffa(2,1,j) * dtatm * rc(j) * rc(j) * rdphi * rdphi, &
            & diffa(1,2,j-1) * dtatm * cv(j-1) * cv(j-1) * rdsv(j-1) * rds(j), &
            & diffa(2,2,j-1) * dtatm * cv(j-1) * cv(j-1) * rdsv(j-1) * rds(j)
    END DO

    ! scale height for specific humidity (Peixoto and Oort 1992)
    hatmbl(2) = 1800.

    ! scaling for P-E forcing of atmosphere
    rpmesca = rsc * rho0 / (hatmbl(2) * usc * rhoair)

    ! reconstruct surface wind field for bulk turbulent transfer and
    ! zonally average near poles as for uatm for stability

    DO j = 1, maxj
       tv3 = 0.0
       DO i = 1, maxi
          IF (i == 1) THEN
             tv = (tau(1,i,j) + tau(1,maxi,j)) / 2
          ELSE
             tv = (tau(1,i,j) + tau(1,i-1,j)) / 2
          END IF
          IF (j == 1) THEN
             tv2 = tau(2,i,j) / 2
          ELSE
             tv2 = (tau(2,i,j) + tau(2,i,j-1)) / 2
          END IF
          usurf(i,j) = SQRT((SQRT(tv**2 + tv2**2)) * &
               & rh0sc * dsc * usc * fsc / (rhoair * cd * scf))
          tv3 = tv3 + usurf(i,j)
       END DO
       IF (par_wind_polar_avg /= 2) THEN
          DO i = 1, maxi
             IF (j <= 2 .OR. j >= maxj-1) usurf(i,j) = tv3 / maxi
          END DO
       END IF
    END DO

    ! sea-ice parameter definitions
    IF (debug_init) PRINT *,  'constant ice conductivity, consic =', consic
    ! In parameterization of heat flux at base of sea ice:
    ! empirical constant
    ch_ice = 0.0058
    IF (debug_init) PRINT *,  &
         & 'base of sea-ice empirical constant, ch_ice =', ch_ice
    ! skin friction velocity (m/s)
    u_tau_ice = 0.02
    IF (debug_init) PRINT *,  &
         & 'skin friction velocity, u_tau_ice =', u_tau_ice
    ! specific heat of sea water under ice at constant pressure (J/kg/K)
    cpo_ice = 4044
    IF (debug_init) PRINT *,  &
         & 'specific heat of seawater under ice, cpo_ice =', cpo_ice
    IF (debug_init) PRINT *,  &
         & 'representative ice density, rhoice =', rhoice
    ! useful constant proportional to inverse timscale for surface freezing
    rsictscsf = ch_ice * u_tau_ice * rho0 * cpo_ice
    IF (debug_init) PRINT *, 'rsictscsf = ', rsictscsf
    rsictscsf = dsc * dz(maxk) * rho0 * cpo_ice / (17.5 * 86400.0)
    IF (debug_init) PRINT *, 'rsictscsf = ', rsictscsf
    IF (debug_init) PRINT *,  &
         & 'minimum average sea-ice thickness, hmin =', hmin
    ! density ratios
    IF (debug_init) PRINT *, 'density ratios, rhooi =', rhooi
    IF (debug_init) PRINT *, 'rrholf = ', rrholf
    ! FW flux conversion parameters
    IF (debug_init) PRINT *, 'm to mm conversion factor, m2mm =', m2mm
    IF (debug_init) PRINT *, 'mm to m conversion factor, mm2m =', mm2m
    ! read initial atmos state
    IF (debug_init) PRINT *, 'tatm relh0_ocean relh0_land'
    IF (debug_init) PRINT *, tatm, relh0_ocean, relh0_land
    ! read freshwater flux perturbation data
    IF (debug_init) PRINT *, 'extra1a range1b nsteps_extra1c'
    IF (debug_init) PRINT *, extra1a, extra1b, extra1c
    ! read scaling factor for extra1a, extra1b, extra1c
    IF (debug_init) PRINT *, 'scl_fwf'
    IF (debug_init) PRINT *, scl_fwf
    ! Read the EMBM reference height
    IF (debug_init) PRINT *, 'EMBM reference height, z1 (m)'
    IF (debug_init) PRINT *, z1_embm
    ! apply scl_fwf
    extra1a = scl_fwf * extra1a
    extra1b = scl_fwf * extra1b
    extra1c = scl_fwf * extra1c

    ! Find total no. of Pac/Atl gridboxes

    ! Find southern boundaries of regions for FW flux adjustment
    ! region 1a: 'jsf+1' to 20S
    ! region 1b: 20S to 24N
    ! region 1c: 24N to 90N

    ! Southern boundary of region 1a
    j1as = jsf + 1
    tv = SIN(-20.0 * pi / 180.0)
    tv2 = SIN(24.0 * pi / 180.0)
    DO j = 1, maxj
       ! Southern boundary of region 1b
       IF (tv >= sv(j-1) .AND. tv <= sv(j)) THEN
          ! At least half of box area has to be in region
          IF ((sv(j) - tv) / ds(j) >= 0.5) THEN
             j1bs = j
          ELSE
             j1bs = j + 1
          END IF
       END IF
       ! Southern boundary of region 1c
       IF (tv2 >= sv(j-1) .AND. tv2 <= sv(j)) THEN
          ! at least half of box area has to be in region
          IF ((sv(j)-tv2) / ds(j) >= 0.5) THEN
             j1cs = j
          ELSE
             j1cs = j + 1
          END IF
       END IF
    END DO

    IF (igrid == 0) THEN
       ! In south Atlantic (to 20 deg S)
       npac1a = 0
       natl1a = 0
       DO j = j1as, (j1bs-1)
          npac1a = npac1a + ipf(j) - ips(j) + 1
          natl1a = natl1a + iaf(j) - ias(j) + 1
       END DO

       ! In tropical Atlantic (20 deg S to 24 deg N)
       npac1b = 0
       natl1b = 0
       DO j = j1bs, (j1cs-1)
          npac1b = npac1b + ipf(j) - ips(j) + 1
          natl1b = natl1b + iaf(j) - ias(j) + 1
       END DO

       ! In north Atlantic (north of 24 deg N) NB INCLUDES DRY POINTS
       npac1c = 0
       natl1c = 0
       DO j = j1cs, maxj
          DO i = ips(j), ipf(j)
             IF (k1(i,j) <= maxk) npac1c = npac1c + 1
          END DO
          DO i = ias(j), iaf(j)
             IF (k1(i,j) <= maxk) natl1c = natl1c + 1
          END DO
       END DO
    END IF

    IF (igrid == 1 .OR. igrid == 2) THEN
       area_pac1a = 0.
       area_atl1a = 0.
       DO j = j1as, (j1bs-1)
          DO i = ips(j), ipf(j)
             area_pac1a = area_pac1a + asurf(j)
          END DO
          ! conditionality to take care of 'split Atlantic' (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             DO i = ias(j), maxi
                area_atl1a = area_atl1a + asurf(j)
             END DO
             DO i = 1, iaf(j)
                area_atl1a = area_atl1a + asurf(j)
             END DO
          ELSE
             DO i = ias(j), iaf(j)
                area_atl1a = area_atl1a + asurf(j)
             END DO
          END IF
       END DO

       area_pac1b = 0.
       area_atl1b = 0.
       DO j = j1bs, (j1cs-1)
          DO i = ips(j), ipf(j)
             area_pac1b = area_pac1b + asurf(j)
          END DO
          ! conditionality to take care of 'split Atlantic' (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             DO i = ias(j), maxi
                area_atl1b = area_atl1b + asurf(j)
             END DO
             DO i = 1, iaf(j)
                area_atl1b = area_atl1b + asurf(j)
             END DO
          ELSE
             DO i = ias(j), iaf(j)
                area_atl1b = area_atl1b + asurf(j)
             END DO
          END IF
       END DO

       area_pac1c = 0.
       area_atl1c = 0.
       DO j = j1cs, maxj
          DO i = ips(j), ipf(j)
             IF (k1(i,j) <= maxk) area_pac1c = area_pac1c + asurf(j)
          END DO
          ! conditionality to take care of 'split Atlantic' (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             DO i = ias(j), maxi
                IF (k1(i,j) <= maxk) area_atl1c = area_atl1c + asurf(j)
             END DO
             DO i=  1, iaf(j)
                IF (k1(i,j) <= maxk) area_atl1c = area_atl1c + asurf(j)
             END DO
          ELSE
             DO i = ias(j), iaf(j)
                IF (k1(i,j) <= maxk) area_atl1c = area_atl1c + asurf(j)
             END DO
          END IF
       END DO
    END IF

    IF (debug_init) PRINT *, &
         & natl1a, npac1a, natl1b, npac1b, natl1c, npac1c
    IF (debug_init) PRINT *, &
         & 'natl1a, npac1a, natl1b, npac1b, natl1c, npac1c '

    ! Increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
    ! [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
    ! here add/remove total extra1a, extra1b, extra1c Sv of freshwater
    ! equally by area in Pac/Atl resp.
    pmeadj = 0.0

    ! If 36x36s ...
    IF (igrid == 0) THEN
       DO j = j1as, (j1bs-1)
          pmeadj(ips(j):ipf(j),j) = 1.0E6 * extra1a / (npac1a * asurf(j))
          pmeadj(ias(j):iaf(j),j) = -1.0E6 * extra1a / (natl1a * asurf(j))
       END DO
       DO j = j1bs, (j1cs-1)
          pmeadj(ips(j):ipf(j),j) = 1.0E6 * extra1b / (npac1b * asurf(j))
          pmeadj(ias(j):iaf(j),j) = -1.0E6 * extra1b / (natl1b * asurf(j))
       END DO
       DO j = j1cs, maxj
          DO i = ips(j), ipf(j)
             IF (k1(i,j) <= maxk) &
                  & pmeadj(i,j) = 1.0E6 * extra1c / (npac1c * asurf(j))
          END DO
          DO i = ias(j), iaf(j)
             IF (k1(i,j) <= maxk) &
                  & pmeadj(i,j) = -1.0E6 * extra1c / (natl1c * asurf(j))
          END DO
       END DO
    END IF

    ! If 64x32 or 64x32l ...
    IF (igrid == 1 .OR. igrid == 2) THEN
       DO j = j1as, (j1bs-1)
          pmeadj(ips(j):ipf(j),j) = 1.0E6 * extra1a / area_pac1a
          ! conditionality to take care of 'split Atlantic' (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             pmeadj(ias(j):maxi,j) = -1.0E6 * extra1a / area_atl1a
             pmeadj(1:iaf(j),j) = -1.0E6 * extra1a / area_atl1a
          ELSE
             pmeadj(ias(j):iaf(j),j) = -1.0E6 * extra1a / area_atl1a
          END IF
       END DO

       DO j = j1bs, (j1cs-1)
          pmeadj(ips(j):ipf(j),j) = 1.0E6 * extra1b / area_pac1b
          ! conditionality to take care of 'split Atlantic' (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             pmeadj(ias(j):maxi,j) = -1.0E6 * extra1b / area_atl1b
             pmeadj(1:iaf(j),j) = -1.0E6 * extra1b / area_atl1b
          ELSE
             pmeadj(ias(j):iaf(j),j) = -1.0E6 * extra1b / area_atl1b
          END IF
       END DO

       do j=j1cs,maxj
          do i=ips(j),ipf(j)
             IF (k1(i,j) <= maxk) pmeadj(i,j) = 1.0E6 * extra1c / area_pac1c
          END DO
          ! conditionality to take care of 'split Atlantic' (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             do i=ias(j),maxi
                IF (k1(i,j) <= maxk) pmeadj(i,j) = -1.0E6 * extra1c / area_atl1c
             END DO
             do i=1,iaf(j)
                IF (k1(i,j) <= maxk) pmeadj(i,j) = -1.0E6 * extra1c / area_atl1c
             END DO
          ELSE
             do i=ias(j),iaf(j)
                IF (k1(i,j) <= maxk) pmeadj(i,j) = -1.0E6 * extra1c / area_atl1c
             END DO
          END IF
       END DO
    END IF

    ! *********************************************************************
    ! *** alternaive mask-based methodology *******************************
    ! *********************************************************************

    ! read in mask
    ! basin mask: 3: Atlantic
    !             2: Pacific
    !             1: remaining ocean grid cells
    !             0: land
    ! sub-region 1 (a): 'jsf+1' to 20S
    ! sub-region 2 (b): 20S to 24N
    ! sub-region 3 (c): 24N to 90N

    INQUIRE(FILE=indir_name(1:lenin)//world//'.bmask',EXIST=ioex)
    IF (ioex) THEN
       IF (debug_init) print *, 'reading in basin mask file'
       CALL check_unit(13, __LINE__, __FILE__)
       OPEN(13,FILE=indir_name(1:lenin)//world//'.bmask',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       DO j = maxj, 1, -1
          READ (13,*,IOSTAT=ios) (bmask(i,j), i = 1, maxi)
          CALL check_iostat(ios, __LINE__, __FILE__)
          IF (debug_init) WRITE (6,'(i4,66i3)') j, (bmask(i,j), i = 1, maxi)
       END DO
       CLOSE(13,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! find total no. of Pac/Atl gridboxes
       natl1a = 0 ; npac1a = 0
       natl1b = 0 ; npac1b = 0
       natl1c = 0 ; npac1c = 0
       DO j = 1, maxj
          DO i = 1, maxi
             ! south Pacific, Atlantic (to 20 deg S)
             IF (bmask(i,j) == 31) natl1a = natl1a + 1
             IF (bmask(i,j) == 21) npac1a = npac1a + 1
             ! tropical Pacific, Atlantic (20 deg S to 24 deg N)
             IF (bmask(i,j) == 32) natl1b = natl1b + 1
             IF (bmask(i,j) == 22) npac1b = npac1b + 1
             ! north Pacific, Atlantic (north of 24 deg N)
             IF (bmask(i,j) == 33) natl1c = natl1c + 1
             IF (bmask(i,j) == 23) npac1c = npac1c + 1
          END DO
       END DO

       ! Increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
       ! [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
       ! here add/remove total extra1a, extra1b, extra1c Sv of freshwater
       ! equally by area in Pac/Atl resp.
       pmeadj = 0.0
       DO j = 1, maxj
          DO i = 1, maxi
             SELECT CASE (bmask(i,j))
             CASE (31)
                pmeadj(i,j) = -1.0E6 * extra1a / (natl1a * asurf(j))
             CASE (21)
                pmeadj(i,j) = 1.0E6 * extra1a / (npac1a * asurf(j))
             CASE (32)
                pmeadj(i,j) = -1.0E6 * extra1b / (natl1b * asurf(j))
             CASE (22)
                pmeadj(i,j) = 1.0E6 * extra1b / (npac1b * asurf(j))
             CASE (33)
                pmeadj(i,j) = -1.0E6 * extra1c / (natl1c * asurf(j))
             CASE (23)
                pmeadj(i,j) = 1.0E6 * extra1c / (npac1c * asurf(j))
             END SELECT
          END DO
       END DO
    END IF

    ! Initialize atmosphere
    DO j = 1, maxj
       DO i = 1, maxi
          ! Initial air temperatures
          tq(1,i,j) = tatm
          tq1(1,i,j) = tq(1,i,j)

          ! Initial specific humidities
          ! Set to relh0_ocean*qsat_ocean over ocean and
          ! relh0_land*qsat_atmos over land
          IF (k1(i,j) <= maxk) THEN
             IF (tstar_ocn(i,j) > tsic) THEN
                tq(2,i,j) = relh0_ocean * const1 * &
                     & EXP(const2 * tstar_ocn(i,j) / (tstar_ocn(i,j) + const3))
             ELSE
                tq(2,i,j) = relh0_ocean * const1 * &
                     & EXP(const4 * tstar_ocn(i,j) / (tstar_ocn(i,j) + const5))
             END IF
          ELSE
             IF (tq1(1,i,j) > 0.0) THEN
                tq(2,i,j) = relh0_land * const1 * &
                     & EXP(const2 * tq1(1,i,j) / (tq1(1,i,j) + const3))
             ELSE
                tq(2,i,j) = relh0_land * const1 * &
                     & EXP(const4 * tq1(1,i,j) / (tq1(1,i,j) + const5))
             END IF
          END IF

          tq1(2,i,j) = tq(2,i,j)

          IF (dosc) THEN
             tqavg(:,i,j) = 0.0
             ! Annual average fields for diagnostic of precipitation-adjusted
             ! humidity (i.e., humidity after precipitation)
             q_pa_avg(i,j) = 0.0
             rq_pa_avg(i,j) = 0.0
             fx0avg(:,i,j) = 0.0
             fwavg(:,i,j) = 0.0
             palbavg(i,j)=0.0
          END IF
       END DO
    END DO

    CALL readroff

    ! Read in observational data filenames
    lentdata = lnsig1(tdatafile)
    IF (debug_init) PRINT *, 'Temperature observations filename, ', &
         & tdatafile(1:lentdata)
    lenqdata = lnsig1(qdatafile)
    IF (debug_init) PRINT *, 'Humidity observations filename, ', &
         & qdatafile(1:lenqdata)
    IF (tqinterp) THEN
       PRINT *, 'Interpoate observational dataset'
       lentvar = lnsig1(tdata_varname)
       PRINT *, 'Temperature observations variable name, ', &
            & tdata_varname(1:lentvar)
       PRINT *, 'Temperature observations scaling factor, ', tdata_scaling
       PRINT *, 'Temperature observations offset, ', tdata_offset
       PRINT *, 'Temperature observations missing value, ', tdata_missing
       lenqvar = lnsig1(qdata_varname)
       PRINT *, 'Humidity observations variable name, ', &
            & qdata_varname(1:lenqvar)
       PRINT *, 'Humidity observations scaling factor, ', qdata_scaling
       PRINT *, 'Humidity observations offset, ', qdata_offset
       PRINT *, 'Humidity observations missing value, ', qdata_missing
    END IF
    IF (debug_init) PRINT *

    ! read in time varying orbital forcing
    IF (orbit_radfor == 'y' .OR. orbit_radfor == 'Y') THEN
       OPEN(UNIT=729,FILE=indir_name(1:lenin)//TRIM(filenameorbit),IOSTAT=ios)
       ALLOCATE(orbitecc_vect(norbit),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(orbitobl_vect(norbit),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(orbitpre_vect(norbit),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(orbittau_vect(norbit),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       DO i = 1, norbit
          READ (729,*,IOSTAT=ios) (orbitall_vect(n), n = 1, 5)
          orbitecc_vect(norbit-i+1) = orbitall_vect(2)
          orbitobl_vect(norbit-i+1) = orbitall_vect(3)
          orbitpre_vect(norbit-i+1) = orbitall_vect(4)
          orbittau_vect(norbit-i+1) = orbitall_vect(5)
       END DO
       CLOSE(729)
    END IF

    ! v2 seasonal. Calculate radiative forcing
    ! NOTE: this is where the solar constant is set
    IF (debug_init) PRINT *, 'going to radfor'
    CALL radfor(0, gn_daysperyear, solconst, flag_ents)
    IF (debug_init) PRINT *, 'file extension for output (a3) ?'
    IF (debug_init) PRINT *, lout
    IF (debug_init) PRINT *, 'filename for netCDF restart input ?'
    IF (debug_init) PRINT *, filenetin
    IF (debug_init) PRINT *,  &
         & 'directory name for netCDF restart output ?'
    IF (debug_init) PRINT *, dirnetout
    IF (debug_init) PRINT *, &
         & 'Force EMBM from ATCHEM atmostpheric tracer cons?'
    IF (debug_init) PRINT *, atchem_radfor

    ! Is this a new or continuing run?
    IF (ans == 'n' .OR. ans == 'N') THEN
       IF (debug_init) PRINT *, &
            & 'this is a new run, initial conditions already set up'
       ! But set up initial default time and date....
       iyear_rest = 2000
       imonth_rest = 1
       ioffset_rest = 0
       day_rest = yearlen / (nyear * ndta)
       IF (debug_init) PRINT *, 'day_rest = ', day_rest
    ELSE
       ! This is a continuing run, read in end state filename
       IF (debug_init) PRINT *, 'Reading EMBM restart file'
       CALL inm_netcdf_embm
       tq1 = tq
    END IF


    ! Open output files

    ! Average, etc. values of surface air temperature
    CALL check_unit(41, __LINE__, __FILE__)
    OPEN(41,FILE=outdir_name(1:lenout)//lout//'.'//'airt',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    WRITE (41,*) '% EMBM atmosphere model, air temperature'
    WRITE (41,'(4a15,a10,a15,a10)') '% time        ','N hem air temp', &
         & 'S hem air temp','Max air temp',' Location','Min air temp', &
         & ' Location'
    WRITE (41,'(4a15,2a5,a15,2a5)') '%             ','degrees C', &
         & 'degrees C','degrees C','i','j','degrees C','i','j'
    CLOSE(41,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! Average, etc. values of surface specific humidity
    CALL check_unit(42, __LINE__, __FILE__)
    OPEN(42,FILE=outdir_name(1:lenout)//lout//'.'//'q', IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    WRITE (42,*) '% EMBM atmosphere model, specific humidity'
    WRITE (42,'(4a15,a10,a15,a10)') '% time        ','N hem spec hum', &
         & 'S hem spec hum','Max spec hum',' Location','Min spec hum', &
         & ' Location'
    WRITE (42,'(4a15,2a5,a15,2a5)') '%             ','g / kg','g / kg', &
         & 'g / kg','i','j','g / kg','i','j'
    CLOSE(42,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! count variable for output file numbers
    iw = 1
    iav = 1

313 FORMAT(i3,6f8.2)

    IF (debug_init) PRINT *
    IF (debug_init) PRINT *, 'EMBM/GENIE grid interpolation variables :'

    IF (debug_init) PRINT *, &
         & '* Longitude : alon1, alon2, alon3, abox1, abox2, abox3 *'
    DO i = 1, maxi
       alon1(i) = REAL(360.0 * (i-0.5) / REAL(maxi) + phi0 / deg_to_rad)
       alon2(i) = REAL(360.0 * i / REAL(maxi) + phi0 / deg_to_rad)
       alon3(i) = REAL(360.0 * (i-0.5) / REAL(maxi) + phi0 / deg_to_rad)
       nclon1(i) = alon1(i)
       nclon2(i) = REAL(360.0 * (i-1.0) / REAL(maxi) + phi0 / deg_to_rad)
       nclon3(i) = alon3(i)
    END DO
    DO i = 1, maxi+1
       aboxedge1_lon(i) = REAL(360.0 * (i-1.0) / REAL(maxi) + phi0 / deg_to_rad)
       aboxedge2_lon(i) = REAL(360.0 * (i-0.5) / REAL(maxi) + phi0 / deg_to_rad)
       aboxedge3_lon(i) = REAL(360.0 * (i-1.0) / REAL(maxi) + phi0 / deg_to_rad)
    END DO

    DO i = 1, maxi+1
       IF (i < maxi + 1) THEN
          IF (debug_init) WRITE (*,313) i, alon1(i), alon2(i), alon3(i), &
               & aboxedge1_lon(i), aboxedge2_lon(i), aboxedge3_lon(i)
       ELSE
          IF (debug_init) WRITE (*,313) i, -999.99, -999.99, -999.99, &
               & aboxedge1_lon(i), aboxedge2_lon(i), aboxedge3_lon(i)
       END IF
    END DO

    IF (debug_init) PRINT *, &
         & '* Latitude : alat1, alat2, alat3, abox1, abox2, abox3 *'
    nclat3(1) = REAL(ASIN(sv(0)) * 180.0 / pi)
    DO j = 1, maxj
       alat1(j) = REAL(ASIN(s(j)) * 180.0 / pi)
       alat2(j) = REAL(ASIN(s(j)) * 180.0 / pi)
       alat3(j) = REAL(ASIN(sv(j)) * 180.0 / pi)
       nclat1(j) = alat1(j)
       nclat2(j) = alat2(j)
       IF (j < maxj) nclat3(j + 1) = REAL(ASIN(sv(j)) * 180.0 / pi)
    END DO
    DO j = 1, maxj+1
       aboxedge1_lat(j) = REAL(ASIN(sv(j-1)) * 180.0 / pi)
       aboxedge2_lat(j) = REAL(ASIN(sv(j-1)) * 180.0 / pi)
       ! Following if statement stops bounds error
       IF (j <= maxj) aboxedge3_lat(j) = REAL(ASIN(s(j)) * 180.0 / pi)
    END DO
    aboxedge3_lat(maxj+1) = REAL(ASIN(sv(maxj)) * 180.0 / pi)

    DO j = 1, maxj+1
       IF (j < maxj+1) THEN
          IF (debug_init) WRITE (*,313) j, alat1(j), alat2(j), alat3(j), &
               & aboxedge1_lat(j), aboxedge2_lat(j), aboxedge3_lat(j)
       ELSE
          IF (debug_init) WRITE (*,313) j, -999.99, -999.99, -999.99, &
               & aboxedge1_lat(j), aboxedge2_lat(j), aboxedge3_lat(j)
       END IF
    END DO

    ! This bit is to make the land-sea mask on the genie grid.  The genie
    ! grid is offset from the goldstein grid by maxi/4 in the longitudinal
    ! direction.
    WHERE (k1(1:maxi,1:maxj) >= 90)
       ilandmask1 = 1
       ilandmask2 = 1
       ilandmask3 = 1
    ELSEWHERE
       ilandmask1 = 0
       ilandmask2 = 0
       ilandmask3 = 0
    END WHERE
    IF (debug_init) PRINT *

    ! Output arguments
    DO j = 1, maxj
       DO i = 1, maxi
          ! Surface air temperature [-> surface fluxes]
          tstar_atm(i,j) = REAL(tq1(1,i,j))
          ! Surface specific humidity [-> surface fluxes]
          qstar_atm(i,j) = REAL(tq1(2,i,j))
          ! Variables dztau and dztav are renamed here to separate the unscaled
          ! (hence 'us') versions read in from files, from those scaled versions
          ! used in surflux and GOLDSTEIN.
          ! Wind stress x components [-> ocean, surface fluxes]
          stressxu_atm(i,j) = REAL(us_dztau(1,i,j))
          stressxv_atm(i,j) = REAL(us_dztav(1,i,j))
          ! Wind stress y components [-> ocean, surface fluxes]
          stressyu_atm(i,j) = REAL(us_dztau(2,i,j))
          stressyv_atm(i,j) = REAL(us_dztav(2,i,j))
          eb_ca(i,j) = REAL(ca(i,j))
          lowestlu2_atm(i,j)=REAL(uatm(1,i,j) * usc)
          lowestlv3_atm(i,j)=REAL(uatm(2,i,j) * usc)
       END DO
    END DO

    ! Set values of dummy variables destined for BIOGEM
    eb_rmax = REAL(rmax)
    eb_dphi = REAL(dphi)
    eb_rdtdim = REAL(rdtdim)

    ! read in orography even if orogswitch is off (so orography can be
    ! used for ice sheet melwater as well as climatology)
    IF (t_orog == 1) THEN
       IF (t_d18o == 1) THEN
          PRINT *, 'ERROR: Both transient orography switches are on'
          STOP
       END IF
       OPEN(77,FILE=TRIM(filenameorog))
       ALLOCATE(orog_vect(maxi,maxj,norog),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       DO l = 1, norog
          DO i = 1, maxi
             DO j = 1, maxj
                READ (77,*) orog_vect(i,j,l)
             END DO
          END DO
       END DO
       CLOSE(77)
       surf_orog_atm(:,:) = orog_vect(:,:,1)
    ELSE IF (t_d18o >= 1) THEN
       ! Read in benthic d18o time series and fields to construct transient
       ! ice sheets
       ! See holden et al 2009 Clim Past Disc
       ! d18o time series
       OPEN(77,FILE=TRIM(filenamed18o))
       ALLOCATE(d18o_vect(nd18o),STAT=alloc_error)
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       READ (77,*) (d18o_vect(l), l = 1, nd18o)
       CLOSE(77)
       ! Threshold value of d18o at which cell is ice covered
       OPEN(77,FILE=TRIM(filenamed18oicethresh))
       DO i = 1, maxi
          DO j = 1, maxj
             READ (77,*) d18o_ice_thresh(i,j)
          END DO
       END DO
       CLOSE(77)
       ! Minimum (modern) orography
       OPEN(77,FILE=TRIM(filenamed18oorogmin))
       DO i = 1, maxi
          DO j = 1, maxj
             READ (77,*) d18o_orog_min(i,j)
          END DO
       END DO
       ! 'gradient' of orography wrt d180
       OPEN(77,FILE=TRIM(filenamed18ooroggrad))
       DO i = 1, maxi
          DO j = 1, maxj
             READ (77,*) d18o_orog_grad(i,j)
          END DO
       END DO
       CLOSE(77)
       ! initialise orography
       ! LOOK need to initialise this correctly for restarts
       ! (when ENTS restarts are enabled which they are not as of 2/2/10)
       DO i = 1, maxi
          DO j = 1, maxj
             IF (d18o_ice_thresh(i,j) < 1.0E-5) THEN
                ! ocean (not used but initialised for completeness)
                surf_orog_atm(i,j) = 0.0
             ELSE IF (d18o_vect(1) > d18o_ice_thresh(i,j)) THEN
                ! ice sheet
                surf_orog_atm(i,j) = d18o_orog_min(i,j) + &
                     & d18o_orog_grad(i,j) * &
                     & (d18o_vect(1) - d18o_ice_thresh(i,j)) / &
                     & (d18o_vect(1) - d18o_ice_thresh(i,j) + d18o_k)
             ELSE
                ! land (no ice)
                surf_orog_atm(i,j) = d18o_orog_min(i,j)
             END IF
          END DO
       END DO
    ELSE
       IF (orogswitch >= 1) THEN
          OPEN(77,FILE=TRIM(filenameorog))
          DO i = 1, maxi
             DO j = 1, maxj
                READ (77,*) surf_orog_atm(i,j)
             END DO
          END DO
          CLOSE(77)
       ELSE
          DO i = 1, maxi
             DO j = 1, maxj
                surf_orog_atm(i,j) = 0.0
             END DO
          END DO
       END IF
       IF (debug_init) PRINT *, 'orog:', &
            & SUM(surf_orog_atm) / SIZE(surf_orog_atm)
    END IF

    DO i = 1, maxi
       DO j = 1, maxj
          torog_atm(i,j) = tstar_atm(i,j)
          IF (orogswitch == 1) THEN
             torog_atm(i,j) = torog_atm(i,j) + lapse_rate * surf_orog_atm(i,j)
          END IF
       END DO
    END DO

    ! Read in where ice sheets are (0=ocean, 1=land, 2=ice sheet)
    ! If t_lice is on (i.e. time-varying ice-sheets) THEN file is big.
    IF (t_lice == 1) THEN
       IF (t_d18o == 1) THEN
          PRINT *, 'ERROR: Both transient icemask switches are on'
          STOP
       END IF
       OPEN(77,FILE=TRIM(filenamelice))
       ALLOCATE(lice_vect(maxi,maxj,nlice),STAT=alloc_error) ; lice_vect = 0.0
       CALL check_iostat(alloc_error,__LINE__,__FILE__)
       DO l = 1, nlice
          DO i = 1, maxi
             DO j = 1, maxj
                READ (77,*) lice_vect(i,j,l)
             END DO
          END DO
       END DO
       CLOSE(77)
       landice_slicemask_lic(:,:) = NINT(lice_vect(:,:,1))
    ELSE IF (t_d18o >= 1) THEN
       ! Calculate ice mask from benthic d18o
       ! LOOK need to initialise this correctly for restarts
       ! (when ENTS restarts are enabled which they are not as of 2/2/10)
       DO i = 1, maxi
          DO j = 1, maxj
             IF (d18o_ice_thresh(i,j) < 1.0E-5) THEN
                ! ocean
                landice_slicemask_lic(i,j) = 0.0
             ELSE IF (d18o_vect(1) > d18o_ice_thresh(i,j)) THEN
                ! ice sheets
                landice_slicemask_lic(i,j) = 2.0
             ELSE
                ! land
                landice_slicemask_lic(i,j) = 1.0
             END IF
          END DO
       END DO
    ELSE
       IF (flag_ents) THEN
          OPEN(77,FILE=TRIM(filenamelice))
          DO i = 1, maxi
             DO j = 1, maxj
                READ (77,*) landice_slicemask_lic(i,j)
             END DO
          END DO
          CLOSE(77)
       ELSE
          DO i = 1, maxi
             DO j = 1, maxj
                IF (k1(i,j) <= maxk) THEN
                   landice_slicemask_lic(i,j) = 0.0
                ELSE
                   landice_slicemask_lic(i,j) = 1.0
                END IF
             END DO
          END DO
       END IF
       IF (debug_init) PRINT *, 'lice:', &
            & SUM(landice_slicemask_lic) / SIZE(landice_slicemask_lic)
    END IF

    ! Read in offline NCEP fields if offline version selected
    IF (flag_ents) THEN
       IF (ents_offlineswitch == 1) THEN
          OPEN(77,FILE=indir_name(1:lenin)//'NCEP_airt_monthly.dat')
          OPEN(78,FILE=indir_name(1:lenin)//'NCEP_pptn_monthly.dat')
          OPEN(79,FILE=indir_name(1:lenin)//'NCEP_RH_monthly.dat')
          DO l = 1, nmth+1
             DO i = 1, maxi
                DO j = 1, maxj
                   READ (77,*) tncep1(i,j,l)
                   READ (78,*) pncep1(i,j,l)
                   READ (79,*) rhncep1(i,j,l)
                END DO
             END DO
          END DO
          CLOSE(77)
          CLOSE(78)
          CLOSE(79)
       END IF

       ! Create atmospheric albedo fields
       OPEN(77,FILE=indir_name(1:lenin)//'atm_albedo_monthly.dat')
       DO l = 1, nmth+1
          DO i = 1, maxi
             DO j = 1, maxj
                READ (77,*) atm_alb1(i,j,l)
             END DO
          END DO
       END DO
       CLOSE(77)

       ! Read in and initialise monthly winds
       OPEN(35,FILE=indir_name(1:lenin)//'uvic_windx.silo')
       READ (35,*)(((uatml1(1,i,j,l), i = 1, maxi), j = 1, maxj), l = 1, nmth+1)
       CLOSE(35)
       OPEN(35,FILE=indir_name(1:lenin)//'uvic_windy.silo')
       READ (35,*)(((uatml1(2,i,j,l), i = 1, maxi), j = 1, maxj), l = 1, nmth+1)
       CLOSE(35)

       ! read in wind speeds for use in radiation/evap calc.
       OPEN(35,FILE=indir_name(1:lenin)//'monthly_windspd.silo')
       READ (35,*)(((usurfl1(i,j,l), i = 1, maxi), j = 1, maxj), l = 1, nmth+1)
       CLOSE(35)

       ! Preprocess for use in tsetpa/tstipa
       ! conditional zonal average
       ! Step avoided if par_wind_polar_avg is 2
       IF (par_wind_polar_avg /= 2)  then
          DO m = 1, nmth+1
             DO j = 1, maxj
                IF (j <= 2 .OR. j >= maxj-1) THEN
                   DO l = 1, 2
                      tv = SUM(uatml1(l,1:maxi,j,m)) / maxi
                      tv1 = SUM(usurfl1(1:maxi,j,m)) / maxi
                      uatml1(l,1:maxi,j,m) = tv
                      usurfl1(1:maxi,j,m) = tv1
                   END DO
                END IF
             END DO
          END DO
       END IF

       ! Remove zonal average of v ELSE fail mass conservation (may not be
       ! disastrous).
       ! Step avoided if par_wind_polar_avg is 2
       DO l = 1, nmth+1
          DO i = 1, maxi
             uatml1(1,i,1:maxj,l) = uatml1(1,i,1:maxj,l) / usc
             uatml1(2,i,1:maxj,l) = uatml1(2,i,1:maxj,l) / usc
             IF (par_wind_polar_avg /= 2)  then
                uatml1(2,i,maxj,l) = 0.
             END IF
          END DO
       END DO
       ! Interpolate prescribed model fields
       CALL field_interp(uatml1, usurfl1, tncep1, pncep1, rhncep1, atm_alb1)
       ! I removed this part so that we keep ents winds separate from embm
       ! winds even with unify_winds
       IF (unify_winds >= 1) THEN
          ! Replace ents uatm with embm fields
          DO l = 1, nyear
             uatml(:,:,:,l) = uatm
          END DO
       END IF
       IF (unify_winds  ==  1) THEN
          ! Replace ents usurf by embm fields
          DO l = 1, nyear
             usurfl(:,:,l) = usurf
          END DO
       END IF
    ELSE
       uatml = 0.0
       usurfl = 0.0
       tncep = 0.0
       pncep = 0.0
       rhncep = 0.0
       atm_alb = 0.0
    END IF

    ! Precipitation timescale and land radiation

    ! Define lambda i.e. atm timestep/pptn timescale converted to secs
    IF (debug_init) PRINT *, 'timepptn=', timepptn
    lambdapptn = MIN(1.0, gn_daysperyear / (REAL(nyear) * timepptn))
    IF (debug_init) PRINT *, 'lambdapptn=', lambdapptn
    ! Typical land depth scale
    hld = 1.0
    ! Typical land heat capacity (J/m3/K)
    cld = 3.3E5
    rhcld = syr / (nyear * hld * cld)
    IF (debug_init) PRINT *, 'rhcld ', rhcld, nyear, hld, cld

    ! Diagnostic fields of precipitation-adjusted humidity (i.e., humidity
    ! after precipitation)
    ! calculate saturation humidity in line with 'surflux.F' for
    ! diagnostics ('surflux.F' will update it again) and calculate
    ! specific and relative humidity after adjustment for precipitation
    ! has been made
    DO j = 1, maxj
       DO i = 1, maxi
          IF (orogswitch < 2 .AND. flag_ents) THEN
             qsat = const1 * &
                  & EXP(const4 * torog_atm(i,j) / (torog_atm(i,j) + const5))
          ELSE
             qsat = const1 * &
                  & EXP(const4 * tstar_atm(i,j) / (tstar_atm(i,j) + const5))
          END IF
          IF (flag_ents) THEN
             deltq = lambdapptn * (qstar_atm(i,j) - rmax * qsat)
             q_pa(i,j) = MIN(qstar_atm(i,j), REAL(qstar_atm(i,j) - deltq))
          ELSE
             q_pa(i,j) = MIN(qstar_atm(i,j), REAL(rmax * qsat))
          END IF
          rq_pa(i,j) = q_pa(i,j) / qsat
       END DO
    END DO

    ! Scheme to allocate land runoff: 0 - bucket overflow (default); 1 -
    ! Leaky bucket (Meissner et al '03); 2 - soil moisture 'half-life'
    IF (debug_init) PRINT *, 'runoff scheme ', par_runoff_scheme
    IF (debug_init) PRINT *,  &
         & 'Clapp-Hornberger exponent  (for runoff scheme = 1) = ', &
         & par_runoff_b
    IF (debug_init) PRINT *,  &
         & 'soil moisture half-life (for runoff scheme = 2) = ', &
         & par_runoff_tau
    IF (debug_init) PRINT *, 'syr = ', syr

    runoff_factor_1 = 2 * par_runoff_b + 3
    runoff_factor_2 = 1 - EXP(-0.693 * dtatm * ndta * tsc /  &
         & (par_runoff_tau * syr / 12.0))
    IF (debug_init) PRINT *, 'runoff factor 1 = ', runoff_factor_1
    IF (debug_init) PRINT *, 'runoff factor 2 = ', runoff_factor_2

    DEALLOCATE(bmask)
    DEALLOCATE(zro)
    DEALLOCATE(zw)
    DEALLOCATE(uatml1)
    DEALLOCATE(usurfl1)
    DEALLOCATE(tncep1)
    DEALLOCATE(pncep1)
    DEALLOCATE(rhncep1)
    DEALLOCATE(atm_alb1)

    PRINT *, ' <<< Initialisation complete'
    PRINT *, '======================================================='
  END SUBROUTINE initialise_embm


  SUBROUTINE end_embm
    IMPLICIT NONE

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising EMBM module shutdown ...'

    IF (debug_end) CALL diagend_embm

    PRINT *, ' <<< Shutdown complete'
    PRINT *, '======================================================='
  END SUBROUTINE end_embm


  ! Atmospheric timestep: iterative implicit version
  ! Coefficient of implicitness cimp: cimp=1 fully implicit, cimp=0
  ! explicit.
  ! Coeffs for iterative implicit scheme are defined at cell faces.
  ! eg flux across east face = cie(i)*T(i+1) + ciw(i)*T(i)
  SUBROUTINE tstipa
    IMPLICIT NONE

    REAL :: tv, ups, pec, diffpp, centre, dtloc
    REAL, DIMENSION(0:maxi,0:maxj) :: cie, ciw, cin, cis
    REAL :: tq2(0:maxi+1,0:maxj+1)
    INTEGER :: iits
    INTEGER, PARAMETER :: nii=4         ! iterations to solve timestep
    REAL, PARAMETER :: ups0=999
    REAL, PARAMETER :: cimp=0.5
    INTEGER :: i, j, l
    LOGICAL, PARAMETER :: correct=.TRUE.
    dtloc = dtatm

    ! set b.c's on local variables
    cin(0:maxi,0) = 0.0
    cis(0:maxi,0) = 0.0
    tq2(0:maxi,0) = 0.0
    cin(0:maxi,maxj) = 0.0
    cis(0:maxi,maxj) = 0.0
    tq2(0:maxi,maxj+1) = 0.0

    DO l = 1, 2
       DO j = 1, maxj
          DO i = 1, maxi
             ! Flux to east
             cie(i,j) = betaz(l) * uatm(1,i,j) * rc(j) * 0.5 * rdphi
             diffpp = diffa(l,1,j) + &
                  & (2-l) * diffmod0 * MAX(0.0, MIN(1.0, &
                  & (pptn(i,j) - ppmin) / (ppmax - ppmin)))
             tv = rc(j) * rc(j) * rdphi * diffpp * rdphi
             pec = betaz(l) * uatm(1,i,j) * dphi / diffpp
             ups = pec / (2.0 + ABS(pec))
             ciw(i,j) = cie(i,j) * (1 + ups) + tv
             cie(i,j) = cie(i,j) * (1 - ups) - tv
             ! Flux to north
             cin(i,j) = cv(j) * betam(l) * uatm(2,i,j) * 0.5
             diffpp = diffa(l,2,j) + &
                  & (2-l) * diffmod0 * MAX(0.0, MIN(1.0, &
                  & (pptn(i,j) - ppmin) / (ppmax - ppmin)))
             ! cv(maxj) = 0 but dsv not defined so mask needed
             IF (j < maxj) THEN
                tv = cv(j) * cv(j) * rdsv(j) * diffa(l,2,j)
                pec = betam(l) * uatm(2,i,j) * dsv(j) / diffpp
                ups = pec / (2.0 + ABS(pec))
             ELSE
                tv = 0.0
                ups = 0.0
             END IF
             cis(i,j) = cin(i,j) * (1 + ups) + tv
             cin(i,j) = cin(i,j) * (1 - ups) - tv
          END DO
       END DO
       cie(0,1:maxj) = cie(maxi,1:maxj)
       ciw(0,1:maxj) = ciw(maxi,1:maxj)

       ! iterate to solve timestep
       DO iits = 1, nii
          tq2(1:maxi,1:maxj) = cimp * tq(l,:,:) + (1.0 - cimp) * tq1(l,:,:)
          tq2(0,1:maxj) = tq2(maxi,1:maxj)
          tq2(maxi+1,1:maxj) = tq2(1,1:maxj)
          DO j = 1, maxj
             DO i = 1, maxi
                centre = dtloc * (ciw(i,j) - cie(i-1,j) + &
                     & (cis(i,j) - cin(i,j-1)) * rds(j))
                tq(l,i,j) = (tq1(l,i,j) * (1.0 - (1.0 - cimp) * centre) &
                     & - dtloc * (-tqa(l,i,j) &
                     &  + cie(i,j) * tq2(i+1,j) &
                     &  - ciw(i-1,j) * tq2(i-1,j) &
                     &  + (cin(i,j) * tq2(i,j+1) &
                     &  - cis(i,j-1) * tq2(i,j-1)) * rds(j))) /  &
                     & (1 + cimp * centre)
             END DO
          END DO
       END DO
       IF (correct) THEN
          tq2(1:maxi,1:maxj) = 0.5 * (tq2(1:maxi,1:maxj) + &
               & cimp * tq(l,:,:) + (1.0 - cimp) * tq1(l,:,:))
          tq2(0,1:maxj) = tq2(maxi,1:maxj)
          tq2(maxi+1,1:maxj) = tq2(1,1:maxj)
          DO j = 1, maxj
             DO i = 1, maxi
                ! Explicit and conservative corrector step
                tq(l,i,j) = tq1(l,i,j) &
                     & - dtloc * (-tqa(l,i,j) &
                     &  + cie(i,j) * tq2(i+1,j) &
                     &  - ciw(i-1,j) * tq2(i-1,j) &
                     &  + (cin(i,j) * tq2(i,j+1) &
                     &  - cis(i,j-1) * tq2(i,j-1)) * rds(j)) &
                     & - dtloc * tq2(i,j) * &
                     &   (ciw(i,j) - cie(i-1,j) + (cis(i,j) - cin(i,j-1)) * &
                     &    rds(j))
             END DO
          END DO
       END IF
    END DO

    ! update tq1
    tq1 = tq
  END SUBROUTINE tstipa


  ! Subroutine to interpolate between monthly mean
  ! fields. The interpolation method is calculated as
  ! in Killworth (1995)
  ! Ref: P. D. Killworth (1995). Time interpolation of
  !      forcing fields in ocean models. J. Phys. Ocn.
  !      26, 136-143.
  ! This method preserves the monthly mean of the fields
  ! whereas linear interpolation does not.
  ! MSW 8/2/5
  SUBROUTINE field_interp(uatml1, usurfl1, tncep1, pncep1, rhncep1, atm_alb1)
    IMPLICIT NONE
    REAL, INTENT(IN) :: uatml1(2,maxi,maxj,nmth+1)
    REAL, DIMENSION(:,:,:), INTENT(IN) :: &
         & usurfl1, tncep1, pncep1, rhncep1, atm_alb1

    REAL :: invmat(nmth,nmth)
    REAL :: puatml(2,maxi,maxj,nmth)
    REAL, DIMENSION(maxi,maxj,nmth) :: &
         & pusurfl, ptncep, ppncep, prhncep, patm_alb
    REAL :: midpoint(0:nmth+1)
    REAL :: xint, x1int, x2int, y1int(8), y2int(8), gradint(8)
    INTEGER :: i, j, m, istep, l

    ! Read in inverse of linear interpolation matrix. Calculation of this
    ! matrix and its inverse is assuming equal month length
    OPEN(1,FILE=indir_name(1:lenin)//'inv_linterp_matrix.dat')
    DO j = 1, nmth
       DO i = 1, nmth
          READ (1,*) invmat(i,j)
       END DO
    END DO
    CLOSE(1)

    ! Calculate pseudo data
    DO j = 1, maxj
       DO i = 1, maxi
          puatml(1,i,j,:) = MATMUL(invmat, uatml1(1,i,j,1:12))
          puatml(2,i,j,:) = MATMUL(invmat, uatml1(2,i,j,1:12))
          pusurfl(i,j,:) = MATMUL(invmat, usurfl1(i,j,1:12))
          ptncep(i,j,:) = MATMUL(invmat, tncep1(i,j,1:12))
          ppncep(i,j,:) = MATMUL(invmat, pncep1(i,j,1:12))
          prhncep(i,j,:) = MATMUL(invmat, rhncep1(i,j,1:12))
          patm_alb(i,j,:) = MATMUL(invmat, atm_alb1(i,j,1:12))
       END DO
    END DO

    ! Linearly interpolate based on the pseudo data
    ! First find exact istep for midpoint of each month
    DO m = 0, nmth+1
       midpoint(m) = 0.5 * ((2.0 * m) - 1) * REAL(nyear) / REAL(nmth)
    END DO

    DO j = 1, maxj
       DO i = 1, maxi
          m = 0
          DO istep = 1, nyear
             IF (REAL(istep) >= midpoint(m)) m = m + 1
             ! x terms (i.e. time)
             xint = istep
             x1int = midpoint(m-1)
             x2int = midpoint(m)
             ! y terms (i.e. field values)
             IF (m == 1) THEN
                y1int(1) = puatml(1,i,j,nmth)
                y1int(2) = puatml(2,i,j,nmth)
                y1int(3) = pusurfl(i,j,nmth)
                y1int(4) = ptncep(i,j,nmth)
                y1int(5) = ppncep(i,j,nmth)
                y1int(6) = prhncep(i,j,nmth)
                y1int(7) = patm_alb(i,j,nmth)

                y2int(1) = puatml(1,i,j,m)
                y2int(2) = puatml(2,i,j,m)
                y2int(3) = pusurfl(i,j,m)
                y2int(4) = ptncep(i,j,m)
                y2int(5) = ppncep(i,j,m)
                y2int(6) = prhncep(i,j,m)
                y2int(7) = patm_alb(i,j,m)
             ELSE IF (m > nmth) THEN
                y1int(1) = puatml(1,i,j,m-1)
                y1int(2) = puatml(2,i,j,m-1)
                y1int(3) = pusurfl(i,j,m-1)
                y1int(4) = ptncep(i,j,m-1)
                y1int(5) = ppncep(i,j,m-1)
                y1int(6) = prhncep(i,j,m-1)
                y1int(7) = patm_alb(i,j,m-1)

                y2int(1) = puatml(1,i,j,1)
                y2int(2) = puatml(2,i,j,1)
                y2int(3) = pusurfl(i,j,1)
                y2int(4) = ptncep(i,j,1)
                y2int(5) = ppncep(i,j,1)
                y2int(6) = prhncep(i,j,1)
                y2int(7) = patm_alb(i,j,1)
             ELSE
                y1int(1) = puatml(1,i,j,m-1)
                y1int(2) = puatml(2,i,j,m-1)
                y1int(3) = pusurfl(i,j,m-1)
                y1int(4) = ptncep(i,j,m-1)
                y1int(5) = ppncep(i,j,m-1)
                y1int(6) = prhncep(i,j,m-1)
                y1int(7) = patm_alb(i,j,m-1)

                y2int(1) = puatml(1,i,j,m)
                y2int(2) = puatml(2,i,j,m)
                y2int(3) = pusurfl(i,j,m)
                y2int(4) = ptncep(i,j,m)
                y2int(5) = ppncep(i,j,m)
                y2int(6) = prhncep(i,j,m)
                y2int(7) = patm_alb(i,j,m)
             END IF
             DO l = 1, 8
                gradint(l) = (y2int(l) - y1int(l)) / (x2int - x1int)
             END DO
             uatml(1,i,j,istep) = (gradint(1) * (xint - x1int)) + y1int(1)
             uatml(2,i,j,istep) = (gradint(2) * (xint - x1int)) + y1int(2)
             usurfl(i,j,istep) = (gradint(3) * (xint - x1int)) + y1int(3)
             tncep(i,j,istep) = (gradint(4) * (xint - x1int)) + y1int(4)
             pncep(i,j,istep) = (gradint(5) * (xint - x1int)) + y1int(5)
             rhncep(i,j,istep) = (gradint(6) * (xint - x1int)) + y1int(6)
             atm_alb(i,j,istep) = (gradint(7) * (xint - x1int)) + y1int(7)

             ! If just want annual average forcing then make all
             ! elements in array the same in the time direction
             IF (ents_seasonswitch == 0) THEN
                uatml(1,i,j,istep) = uatml1(1,i,j,nmth+1)
                uatml(2,i,j,istep) = uatml1(2,i,j,nmth+1)
                usurfl(i,j,istep) = usurfl1(i,j,nmth+1)
                tncep(i,j,istep) = tncep1(i,j,nmth+1)
                pncep(i,j,istep) = pncep1(i,j,nmth+1)
                rhncep(i,j,istep) = rhncep1(i,j,nmth+1)
                atm_alb(i,j,istep) = atm_alb1(i,j,nmth+1)
             END IF
          END DO
       END DO
    END DO

  END SUBROUTINE field_interp


  ! Used to calculate mean daily ocean albedo zenith angle dependence
  ! Calculation performed at initialisation stage for every latitude and
  ! istep.
  SUBROUTINE ocean_alb(oscss, osccc, oscday, j, istep)
    IMPLICIT NONE
    REAL, INTENT(IN) :: oscss, osccc, oscday
    INTEGER, INTENT(IN) :: j, istep

    REAL :: h, a, b, sum, old, int, radout, xp
    INTEGER n, p, x
    REAL, PARAMETER :: tol=1.0E-5

    sum = 0.0
    p = 0

    ! Integration loop (adaptive extended trapezium rule) returns the
    ! integrated value when converges to a specified tolerance. See
    ! Numerical Recipes Ch. 4

    ! Initial values
    old = -1.0E30
    int = 1.0E30
    ! Integration limits
    a = 0.0
    b = oscday

    IF (b <= 0.) THEN
       albo(j,istep) = 1.0
    ELSE
       DO n = 1, 15
          ! Initial guess based on end points
          IF (n == 1) THEN
             p = 1
             CALL rad_out(radout, a, oscss, osccc)
             sum = 0.5 * radout
             CALL rad_out(radout, b, oscss, osccc)
             sum = sum + (0.5 * radout)
             old = (b - a) * sum
          ELSE
             old = int
          END IF
          ! Interval size doubles with each iteration
          h = (b - a) / (2 * p)
          ! Calculate for new points only then add to the running sum
          DO x = 1, p
             xp = a + h + (2 * h * (x - 1))
             CALL rad_out(radout, xp, oscss, osccc)
             sum = sum + radout
          END DO
          ! Calculate new value of integral
          int = h * sum
          ! Check tolerance
          IF (ABS(int - old) < tol * ABS(old)) EXIT
          ! Double number of points to evaluate
          p = 2 * p
       END DO

       ! Work out ocean albedo (outgoing radiation/incoming radiation)
       albo(j,istep) = int / (oscss * (oscday - TAN(oscday)))
    END IF
  END SUBROUTINE ocean_alb


  ! Function to work out instantaneous outgoing radiation   *
  SUBROUTINE rad_out(radout, h, oscss, osccc)
    IMPLICIT NONE
    REAL, INTENT(IN) :: h, oscss, osccc
    REAL, INTENT(OUT) :: radout

    REAL :: czsol, rspec, rtot
    REAL, PARAMETER :: rdiff=0.06

    ! Cosine of zenith angle
    czsol = oscss + (osccc * COS(h))
    IF (czsol > 1.0) czsol = 1.0
    IF (czsol < 0.0) czsol = 0.0

    ! specular reflectance according to Briegleb et al. (1986)
    rspec = (0.026 / ((czsol**1.7) + 0.065)) +  &
         & (0.15 * (czsol - 0.1) * (czsol - 0.5) * (czsol - 1.0))

    ! total reflectance
    rtot = rspec + rdiff

    ! Instantaneous outgoing radiation (but without the constants)
    radout = rtot * czsol
  END SUBROUTINE rad_out


  ! Routine to calculate radiative forcing for c-goldstein
  ! started 30/5/3 Neil R. Edwards
  ! loosely based on Peter Cox's SUNNY.f or see Peixoto and Oort 1992 p.99
  !
  ! nyear = no. dt per year
  ! osct  = angular time of year (0,..,2pi)
  ! oscsind = sin of declination
  ! oscsob = sin of obliquity
  ! osce = eccentricity
  ! oscgam = ??
  ! oscday = 0.5 * sunlit (angular) fraction of day, ie 1/2 length of day
  ! solfor = solar forcing = scaling factor * integral of cosine of solar
  !          elevation during daylight
  SUBROUTINE radfor(iistep, gn_daysperyear, solconst, flag_ents)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: iistep
    REAL, INTENT(IN) :: gn_daysperyear
    REAL, INTENT(IN) :: solconst
    LOGICAL, INTENT(IN) :: flag_ents

    INTEGER :: istep, j
    REAL :: rpi, osce, oscsob, oscgam, tv, osce1, osce2, osce3, osce4
    REAL :: oscryr, osct, oscv, oscsolf, oscsind, oscss, osccc, osctt
    REAL :: oscday

    ! Orbital variables
    INTEGER :: time_1, time_2
    REAL :: time_frac, osctau0, osctau1

    REAL :: solavg(maxj)
    REAL :: alboavg(maxj)

    osce = 0.0167
    oscsob = 0.397789
    oscgam = 1.352631
    osctau0 = -0.5

    IF (orbit_radfor == 'y' .OR. orbit_radfor == 'Y') THEN
       IF (t_orbit == 2) THEN
          osce = orbitecc_vect(1)
          oscsob = orbitobl_vect(1)
          oscgam = orbitpre_vect(1)
          osctau0 = orbittau_vect(1)
          PRINT *, 'orbitvars:', iistep
          PRINT *, 'orbitosce,oscsob:', osce, oscsob
          PRINT *, 'orbitoscgam,orbitosctau0',oscgam, osctau0
       END IF
       IF (t_orbit == 1) THEN
          time_1 = INT(iistep / REAL(orbitsteps)) + 1
          time_2 = time_1 + 1
          time_frac = (MOD(iistep, orbitsteps)) / REAL(orbitsteps)
          IF (time_2 <= norbit) THEN
             osce = (1 - time_frac) * orbitecc_vect(time_1) + &
                  & time_frac * orbitecc_vect(time_2)
             oscsob = (1 - time_frac) * orbitobl_vect(time_1) + &
                  & time_frac * orbitobl_vect(time_2)

             IF (ABS(orbitpre_vect(time_1) - orbitpre_vect(time_2)) > pi) THEN
                IF (orbitpre_vect(time_1) > orbitpre_vect(time_2)) THEN
                   oscgam = MOD((1 - time_frac) * orbitpre_vect(time_1) + &
                        & time_frac * (orbitpre_vect(time_2) + 2 * pi), 2 * pi)
                ELSE
                   oscgam = MOD((1 - time_frac) * &
                        & (orbitpre_vect(time_1) + 2 * pi) + &
                        & time_frac * (orbitpre_vect(time_2)), 2 * pi)
                END IF
             ELSE
                oscgam = (1 - time_frac) * orbitpre_vect(time_1) + &
                     & time_frac * orbitpre_vect(time_2)
             END IF

             IF (ABS(orbittau_vect(time_1) - orbittau_vect(time_2)) > &
                  & gn_daysperyear / 2.0) THEN
                IF (orbittau_vect(time_1) > orbittau_vect(time_2)) THEN
                   osctau0 = MOD((1 - time_frac) * orbittau_vect(time_1) + &
                        & time_frac * (orbittau_vect(time_2) + &
                        & gn_daysperyear), gn_daysperyear)
                ELSE
                   osctau0 = MOD((1 - time_frac) *  &
                        & (orbittau_vect(time_1) + gn_daysperyear) + &
                        & time_frac * (orbittau_vect(time_2)), gn_daysperyear)
                END IF
             ELSE
                osctau0 = (1 - time_frac) * orbittau_vect(time_1) + &
                     & time_frac * orbittau_vect(time_2)
             END IF
          ELSE
             IF (time_frac /= 0) PRINT *, 'Time out of bounds for orbit'
             osce = orbitecc_vect(norbit)
             oscsob = orbitobl_vect(norbit)
             oscgam = orbitpre_vect(norbit)
             osctau0 = orbittau_vect(norbit)
          END IF

          IF (MOD(iistep-1, 10000) == 0) THEN
             IF (debug_loop) THEN
                PRINT *, 'orbitvars:', iistep, time_1, time_frac
                PRINT *, 'orbitosce,oscsob:', osce, oscsob
                PRINT *, 'orbitoscgam,orbitosctau0',oscgam, osctau0
             END IF
          END IF
       END IF
    END IF

    rpi = 1.0 / pi
    tv = osce * osce
    osce1 = osce * (2.0 - 0.25 * tv)
    osce2 = 1.25 * tv
    osce3 = osce * tv * 13. / 12.
    osce4 = ((1.0 + 0.5 * tv) / (1.0 - tv))**2
    oscryr = 2.0 * pi / REAL(nyear)
    osctau1 = osctau0 + 0.5

    DO istep = 1, nyear
       ! Dan's offset for angular time of year
       osct = (REAL(MOD(istep-1, nyear) + 1) - &
            & (nyear * osctau1 / gn_daysperyear)) * oscryr
       DO j = 1, maxj
          oscv = osct + osce1 * SIN(osct) + osce2 * SIN(2.0 * osct) + &
               & osce3 * SIN(3.0 * osct)
          oscsolf = osce4 * (1.0 + osce * COS(oscv))**2
          oscsind = oscsob * SIN(oscv - oscgam)

          oscss = oscsind * s(j)
          osccc = SQRT(1.0 - oscsind**2) * c(j)
          osctt = MIN(1.0, MAX(-1.0, oscss / osccc))

          oscday = ACOS(-osctt)

          solfor(j,istep) = solconst * oscsolf * rpi * &
               & (oscss * oscday + osccc * SIN(oscday))
          IF (flag_ents) CALL ocean_alb(oscss, osccc, oscday, j, istep)
       END DO
    END DO

    IF (.NOT. dosc) THEN
       ! Replace variable forcing by its average
       DO j = 1, maxj
          solavg(j) = 0.0
          DO istep = 1, nyear
             solavg(j) = solavg(j) + solfor(j,istep)
             IF (flag_ents) alboavg(j) = alboavg(j) + albo(j,istep)
          END DO
       END DO
       DO j = 1, maxj
          DO istep = 1, nyear
             solfor(j,istep) = solavg(j) / nyear
             IF (flag_ents) albo(j,istep) = alboavg(j) / nyear
          END DO
       END DO
    END IF

  END SUBROUTINE radfor


  ! surflux
  !
  ! Notation; fx = heat flux, lh = latent, sh = sensible, sw = shortwave/solar,
  !           lw = long wave, o = ocean, a = atm., sic = sea ice
  !           thus eg fxsho = sensible heat flux into (???) ocean
  !           dthsic,dtareasic are rates of change (thermodynamic only) of
  !           sea-ice height and area
  !
  ! Note evap array passed to the ocean is the NET evaporation, NOT the
  ! evaporation from the ocean alone, hence renamed
  ! AY (10/12/03) : output fluxes now dimensional (ie SI)
  !
  ! PPH (01/09/04) : Partially removed use of Dland precompiler options.
  ! Under genie.F structure the Dland option is unsupported and you are
  ! strongly advised to not use it.  Instead set flag_land=.true. in the
  ! job script.
  !
  ! Notes for coupling; for IGCM pptn and atm state variables atemp, ashum
  ! should become unalterable input fields. Ouput heat flux to atm includes
  ! both surface fluxes like surface LW (same for IGCM) and interior fluxes
  ! like planetary OLWR (not needed for IGCM. Net fluxes are only diagnostic,
  ! however in componentised, GENIE version.
  !
  SUBROUTINE surflux(istep, otemp, osaln, atemp, ashum, sich, sica, &
       & tice, albice, stressxu_ocn, stressyu_ocn, &
       & stressxv_ocn, stressyv_ocn, albedo, fxlho, fxsho, fxswo, fxlwo, &
       & evap_ocn, pptn_ocn, runoff_ocn, runoff_land, fxlha, fxsha, &
       & fxswa, fxlwa, evap_atm, pptn_atm, dthsic, dtareasic, &
       & atmos_lowestlh_atm, go_solfor, go_fxsw, dum_sfcatm, &
       & eb_ca, gn_daysperyear, eb_fx0a, eb_fx0o, eb_fxsen, eb_fxlw, &
       & eb_evap, eb_pptn, eb_relh, eb_uv, eb_usurf, solconst, &
       & co2_out, ch4_out, n2o_out, surf_orog_atm, landice_slicemask_lic, &
       & albs_atm, land_albs_snow_lnd, land_albs_nosnow_lnd, &
       & land_snow_lnd, land_bcap_lnd, land_z0_lnd, land_temp_lnd, &
       & land_moisture_lnd, flag_ents, lowestlu2_atm, lowestlv3_atm)
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep
    REAL, DIMENSION(:,:), INTENT(IN) :: otemp, osaln, atemp, sich, sica, &
         & stressxu_ocn, stressyu_ocn, stressxv_ocn, stressyv_ocn
    REAL, DIMENSION(:,:), INTENT(OUT) :: ashum, tice, albice, &
         & albedo, fxlho, fxsho, fxswo, fxlwo, pptn_ocn, &
         & runoff_ocn, runoff_land, fxlha, fxsha, fxswa, fxlwa, &
         & dthsic, dtareasic, co2_out, ch4_out, n2o_out, &
         & evap_ocn, evap_atm, pptn_atm, atmos_lowestlh_atm
    REAL, INTENT(OUT) :: go_solfor(maxj), go_fxsw(maxi,maxj)
    REAL, INTENT(IN), DIMENSION(:,:,:) :: dum_sfcatm
    REAL, INTENT(IN) :: gn_daysperyear
    REAL, DIMENSION(:,:), INTENT(IN) :: eb_ca
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & eb_fx0a, eb_fx0o, eb_fxsen, eb_fxlw, &
         & eb_evap, eb_pptn, eb_relh, eb_usurf
    REAL, INTENT(OUT) :: eb_uv(2,maxi,maxj)
    REAL, INTENT(IN) :: solconst
    REAL, INTENT(INOUT) :: surf_orog_atm(maxi,maxj)
    REAL, DIMENSION(:,:), INTENT(OUT) :: landice_slicemask_lic
    REAL, DIMENSION(:,:), INTENT(INOUT) :: &
         & albs_atm, land_snow_lnd, land_bcap_lnd, land_z0_lnd, &
         & land_temp_lnd, land_moisture_lnd
    REAL, DIMENSION(:,:), INTENT(IN) :: &
         & land_albs_snow_lnd, land_albs_nosnow_lnd
    LOGICAL, INTENT(IN) :: flag_ents
    REAL, DIMENSION(:,:), INTENT(INOUT) :: lowestlu2_atm, lowestlv3_atm


    REAL, DIMENSION(maxi,maxj) :: orog_exact, lice_exact

    ! For interpolation between time series d18o input
    REAL :: d18o_exact, surf_orog_atm_previous(maxi,maxj)

    ! For ice sheet melt runoff
    REAL :: ice_runoff(maxi,maxj)

    ! Variables required for/from ENTS
    REAL :: beta, tldlast, dtld, dtld1, dfxsens, dfxlw, devap, dqsato
    REAL :: dum_talt, dum_hum, dum_pptn, qsalt, fxswsica

    INTEGER itld, devapcheck
    INTEGER, PARAMETER :: itldmax=21
    REAL, PARAMETER :: tolld=1.0E-3
    REAL :: deltq, talt, lmd

    ! For orography
    REAL :: surf_tq2, surf_qsata

    REAL :: runoff(maxi,maxj)
    REAL :: ce, ch, cesic, chsic, rq, tv0, tv, tv1, tv2, tv3, tol
    REAL :: albsic, fxswsic , fxlwsic, fxsensic , fx0oa, fx0sica
    REAL :: qsatsic, alw, ticold, cfxsensic, salt, dho, dhsic
    REAL :: tieqn, dtieq
    REAL :: atm_latent, atm_sensible, atm_netsol, atm_netlong
    REAL :: atm_latenti, atm_sensiblei, atm_netsoli, atm_netlongi
    REAL, PARAMETER :: zeroc=273.15
    INTEGER :: i, j, iter, istot, imth, ios
    REAL :: meantemp
    CHARACTER(LEN=3) :: ext

    ! edit to sea-ice convergence tolerance
    INTEGER, PARAMETER :: itice=21

    INTEGER :: my_year, io
    REAL :: co2lev, solar, vol, aero
    CHARACTER(LEN=40) :: header
    ! Radiative forcing terms
    REAL :: ch4_term, n2o_term

    ! For time series co2
    INTEGER :: time_1, time_2
    REAL :: time_frac, co2_exact

    ! Dimensionalised surflux timestep
    REAL :: dtsurfdim, rdtsurfdim

    talt = 0.0
    lmd = 0.0
    surf_tq2 = 0.0
    surf_qsata = 0.0
    fxswsica = 0.0
    dum_talt = 0.0
    dum_hum = 0.0
    dum_pptn = 0.0
    beta = 0.0
    imth = 1

    ! Dimensionalised surflux timestep
    dtsurfdim = dtatm * ndta * tsc
    rdtsurfdim = 1.0 / dtsurfdim

    istot = istep

    ! If running on real*4, need to use a lower tolerance than usual
    tol = 1.0E-10

    ! initialise ice sheet meltwater
    ice_runoff = 0.0

    IF (flag_ents) THEN
       ! istot defined currently as zero
       ! N.B. RESTARTS WILL NOT WORK!!!
       imth = MOD(istep+0-1, nyear) + 1

       IF (t_orog == 1) THEN
          time_1 = INT(istot / REAL(orogsteps)) + 1
          time_2 = time_1 + 1
          time_frac = (MOD(istot, orogsteps)) / REAL(orogsteps)
          IF (time_2 <= norog) THEN
             orog_exact = (1 - time_frac) * orog_vect(:,:,time_1) + &
                  & time_frac * orog_vect(:,:,time_2)
          ELSE
             IF (time_frac /= 0) PRINT *, 'Time out of bounds for orog'
             orog_exact = orog_vect(:,:,norog)
          END IF
          IF (MOD(istot, 10000) == 0) THEN
             PRINT *, 'orog:', istot, time_1, time_frac, &
                  & SUM(orog_exact) / SIZE(orog_exact)
          END IF
       END IF

       IF (t_lice == 1) THEN
          time_1 = INT(istot / REAL(licesteps)) + 1
          time_2 = time_1 + 1
          time_frac = (MOD(istot, licesteps)) / REAL(licesteps)
          IF (time_2 <= nlice) THEN
             lice_exact = (1 - time_frac) * lice_vect(:,:,time_1) + &
                  & time_frac * lice_vect(:,:,time_2)
          ELSE
             IF (time_frac /= 0) PRINT *, 'Time out of bounds for lice'
             lice_exact = lice_vect(:,:,nlice)
          END IF
          IF (MOD(istot, 10000) == 0) THEN
             PRINT *, 'lice:', istot, time_1, time_frac, &
                  & SUM(lice_exact) / SIZE(lice_exact)
          END IF
       END IF

       ! interpolate d18o for icesheets
       IF (t_d18o == 1) THEN
          time_1 = INT(istot / REAL(d18osteps)) + 1
          time_2 = time_1 + 1
          time_frac = (MOD(istot, d18osteps)) / REAL(d18osteps)
          IF (time_2 <= nd18o) THEN
             d18o_exact = (1 - time_frac) * d18o_vect(time_1) + &
                  & time_frac * d18o_vect(time_2)
          ELSE
             IF (time_frac /= 0) PRINT *, 'Time out of bounds for d18o'
             d18o_exact = d18o_vect(nlice)
          END IF
          ! d18o orography and icemask.
          ! store orography at previous timestep to derive meltwater
          surf_orog_atm_previous = surf_orog_atm
          DO i = 1, maxi
             DO j = 1, maxj
                IF (d18o_ice_thresh(i,j) < 1.0E-5) THEN
                   ! ocean
                   surf_orog_atm(i,j) = 0.0
                   landice_slicemask_lic(i,j) = 0.0
                ELSE IF (d18o_exact > d18o_ice_thresh(i,j)) THEN
                   ! ice sheet
                   surf_orog_atm(i,j) = d18o_orog_min(i,j) +  &
                        & d18o_orog_grad(i,j) *  &
                        & (d18o_exact - d18o_ice_thresh(i,j)) /  &
                        & (d18o_exact - d18o_ice_thresh(i,j) + d18o_k)
                   landice_slicemask_lic(i,j) = 2.0
                ELSE
                   ! land (no ice)
                   surf_orog_atm(i,j) = d18o_orog_min(i,j)
                   landice_slicemask_lic(i,j) = 1.0
                END IF
             END DO
          END DO

          ! ice runoff
          DO i = 1, maxi
             DO j = 1, maxj
                ice_runoff(iroff(i,j),jroff(i,j)) = &
                     & ice_runoff(iroff(i,j),jroff(i,j)) - &
                     & (surf_orog_atm(i,j) - surf_orog_atm_previous(i,j)) *  &
                     & (rhoice / 1000.0) * rdtsurfdim *  &
                     & (asurf(j) / asurf(jroff(i,j))) * scale_mwfx
             END DO
          END DO

          IF (MOD(istot, 10000) == 0) THEN
             IF (debug_loop) &
                  & PRINT *,  'd18o:', istot, time_1, time_frac, d18o_exact
          END IF
       END IF
    END IF

    ! Input field modifications

    ! This next section executes routines that calculate usurf from wind
    ! stresses.  If the winds are fixed, as they are with the EMBM, this
    ! only needs be run just prior to the first iteration.  If the winds
    ! are time-variant, as they are with the IGCM, this will need to be
    ! run every time-step.
    DO j = 1, maxj
       DO i = 1, maxi
          dztau(1,i,j) = &
               & scf * stressxu_ocn(i,j) / (rh0sc * dsc * usc * fsc) / dzz
          dztau(2,i,j) = &
               & scf * stressyu_ocn(i,j) / (rh0sc * dsc * usc * fsc) / dzz
          dztav(1,i,j) = &
               & scf * stressxv_ocn(i,j) / (rh0sc * dsc * usc * fsc) / dzz
          dztav(2,i,j) = &
               & scf * stressyv_ocn(i,j) / (rh0sc * dsc * usc * fsc) / dzz
          tau(1,i,j) = dztau(1,i,j) * dzz
          tau(2,i,j) = dztav(2,i,j) * dzz

          ! Transfer modified coefficients from ENTS
          IF (flag_ents) THEN
             ca(i,j) = REAL(eb_ca(i,j), KIND(ca))
             ! don't use ents winds when winds are unified
             IF (unify_winds  ==  0) THEN
                lowestlu2_atm(i,j) = REAL(uatml(1,i,j,imth) * usc, &
                     & KIND(lowestlu2_atm))
                lowestlv3_atm(i,j) = REAL(uatml(2,i,j,imth) * usc, &
                     & KIND(lowestlv3_atm))
             END IF
             IF (unify_winds == 0 .OR. unify_winds == 2) THEN
                usurf(i,j) = usurfl(i,j,imth)
             END IF
          END IF
       END DO
    END DO

    IF (unify_winds == 1 .OR. .NOT. flag_ents) THEN
       DO j = 1, maxj
          tv3 = 0.0
          DO i = 1, maxi
             IF (i == 1) THEN
                tv = (tau(1,i,j) + tau(1,maxi,j)) / 2
             ELSE
                tv = (tau(1,i,j) + tau(1,i-1,j)) / 2
             END IF
             IF (j == 1) THEN
                tv2 = tau(2,i,j) / 2
             ELSE
                tv2 = (tau(2,i,j) + tau(2,i,j-1)) / 2
             END IF
             ! use embm winds even with ents (unify_winds=1
             usurf(i,j) = SQRT((SQRT(tv**2 + tv2**2)) * &
                  & rh0sc * dsc * usc * fsc / (rhoair * cd * scf))
             tv3 = tv3 + usurf(i,j)
          END DO
          ! added wind_polar_avg that undoes the zonal average of usurf near
          ! poles when eq 2
          IF (par_wind_polar_avg /= 2) THEN
             IF (j <= 2 .OR. j >= maxj-1) usurf(1:maxi,j) = tv3 / maxi
          END IF
       END DO
       ! and now replace usufl with usurf for the rest of the calculations
       usurfl(:,:,imth) = usurf
    END IF

    atmos_lowestlh_atm = REAL(z1_embm)

    ! Set up local atmospheric greenhouse gas arrays
    !
    !     Options:
    !     (1) Test for climate feedback with ATCHEM (BIOGEM) is selected
    !         NOTE: current atmospheric tracer assignment is:
    !               CO2 == #3
    !               CH4 == #10
    !               N2O == #14
    !         NOTE: catch situation where tracers are not selected in ATCHEM
    !               (=> gas concentrations will be zero)
    !     (2) Test for JDA forcings
    !     (3) Time series co2 input (from Dan Lunt's goldstein option)
    !     (4) ELSE, utilize original compound increase in CO2 (if specified)
    ! *** 1 ***
    IF (atchem_radfor == 'y' .OR. atchem_radfor == 'Y') THEN
       DO j = 1, maxj
          DO i = 1, maxi
             IF (dum_sfcatm(3,i,j) < (co20 / 1000.0)) THEN
                co2(i,j) = co20
             ELSE
                co2(i,j) = dum_sfcatm(3,i,j)
             END IF
             IF (dum_sfcatm(10,i,j) < (ch40 / 1000.0)) THEN
                ch4(i,j) = ch40
             ELSE
                ch4(i,j) = dum_sfcatm(10,i,j)
             END IF
             IF (dum_sfcatm(14,i,j) < (n2o0 / 1000.0)) THEN
                n2o(i,j) = n2o0
             ELSE
                n2o(i,j) = dum_sfcatm(14,i,j)
             END IF
          END DO
       END DO
       ! *** 2 ***
       ! JDA reading forcings (if appropriate)
    ELSE IF (useforc) THEN
       IF (MOD(istep, nyear) == 1) THEN
          my_year = istep / nyear + 1
          co2lev = co2(1,1) * 1.0E6
          aero = 0.0
          vol = 0.0
          OPEN(25,FILE=rstdir_name(1:lenrst)//forcname)
          READ (25,*) header
          ! file exists, will read contents and use
          j = 0
          DO WHILE (j < my_year)
             READ (25,*,IOSTAT=io) i, co2lev, vol, aero, solar
             IF (io < 0) THEN
                ! reached end of file
                PRINT *, 'Warning: reached end of forcing file'
                j = my_year
             END IF
             j = j + 1
          END DO
          ! now set forcings
          PRINT *, i, co2lev, vol, aero, solar
          co2 = co2lev * 1.0E-6
          ! update global values
          co2_out = co2

          CALL radfor(0, REAL(nyear), &
               & REAL((solar - 1368.0) * solfac + 1368.0 + &
               &      (aero * aerofac + vol * volfac) * 4.0 / 0.7), flag_ents)
          CLOSE(25)
       END IF
       ! *** 3 ***
       ! time series input for co2
    ELSE IF (t_co2 > 0) THEN
       IF (t_co2 == 1) THEN
          time_1 = INT(istot / REAL(co2steps)) + 1
          time_2 = time_1 + 1
          time_frac = (MOD(istot, co2steps)) / REAL(co2steps)
          IF (time_2 <= nco2) THEN
             co2_exact = (1 - time_frac) * co2_vect(time_1) + &
                  & time_frac * co2_vect(time_2)
          ELSE
             IF (time_frac /= 0) PRINT *, 'Time out of bounds for co2'
             co2_exact = co2_vect(nco2)
          END IF
          IF (MOD(istot, 10000) == 0) THEN
             PRINT *, 'co2', istot, time_1, time_frac, co2_exact
          END IF
          co2 = co2_exact
          ! update global values
          co2_out = co2
       END IF
    ELSE
       ! *** 4 ***
       ! original simple compound increase
       DO j = 1, maxj
          DO i = 1, maxi
             co2(i,j) = (1.0 + rate_co2) * co2(i,j)
             ch4(i,j) = (1.0 + rate_ch4) * ch4(i,j)
             n2o(i,j) = (1.0 + rate_n2o) * n2o(i,j)
          END DO
       END DO
       ! update global values
       co2_out = co2
       ch4_out = ch4
       n2o_out = n2o
    END IF


    ! SURFLUX model timestep

    ! initialize integrated runoff (and other arrays)
    evap = 0.0
    runoff = 0.0
    runoff_land = 0.0
    fxlho = 0.0
    fxsho = 0.0
    fxswo = 0.0
    fxlwo = 0.0
    IF (flag_ents) pptn = 0.0
    pptn_ocn = 0.0
    runoff_ocn = 0.0
    fxlha = 0.0
    fxsha = 0.0
    fxswa = 0.0
    fxlwa = 0.0
    dthsic = 0.0
    dtareasic = 0.0
    IF (.NOT. flag_ents) albedo = REAL(albcl)
    albice = 0.0

    ! make global mean temp for use in sensitivity adjustment
    meantemp = SUM(atemp) / REAL(maxj * maxi)

    ! main i,j loop to compute surface flux terms
    ! note for IGCM ; the first section defines P, OLWR, rel hum. CO2, all
    ! irrelevant to IGCM
    DO i = 1, maxi
       DO j = 1, maxj
          IF (flag_ents) THEN
             ! Optional effect of altitude in ENTS.
             ! Air temp calculated at altitude using lapse
             ! rate. This air temp used in calc.s of SVP and
             ! longwave radiation.
             IF (orogswitch >= 1) THEN
                talt = atemp(i,j) + (lapse_rate * surf_orog_atm(i,j))
             ELSE
                talt = atemp(i,j)
             END IF
             lmd = lambdapptn
          END IF
          ! pptn (over land and ocean)
          ! - need saturation vapour pressure, relative humidity

          ! no orography for precipitation (orography only affects surface
          ! processes if orogswitch=2)
          IF (orogswitch < 2 .AND. flag_ents) THEN
             qsata(i,j) = const1 * EXP(const4 * talt / (talt + const5))
          ELSE
             qsata(i,j) = const1 * &
                  & EXP(const4 * atemp(i,j) / (atemp(i,j) + const5))
          END IF

          IF (flag_ents) THEN
             ! surface saturation humidity for evaporation (used if
             ! orogswitch=2)
             surf_qsata = const1 * EXP(const4 * talt / (talt + const5))

             deltq = lmd * (ashum(i,j) - rmax * qsata(i,j))

             pptn(i,j) = MAX(0.0, deltq * rhoao * hatmbl(2) * rdtsurfdim)
          ELSE
             pptn(i,j) = MAX(0.0, (ashum(i,j) - rmax * qsata(i,j)) *  &
                  & rhoao * hatmbl(2) * rdtdim)
          END IF

          ! Instantaneous precipitation
          ! Calc relative humidity rq after P not before

          ! The next two lines cause problems for surflux as they alter
          ! atmosphere properties independently of embm.F and the fluxes (but
          ! for good reasons)
          IF (flag_ents) THEN
             ashum(i,j) = MIN(ashum(i,j), REAL(ashum(i,j) - deltq))
          ELSE
             ashum(i,j) = MIN(ashum(i,j), REAL(rmax * qsata(i,j)))
          END IF
          tq1(2,i,j) = ashum(i,j)
          tq(2,i,j) = tq1(2,i,j)

          rq = ashum(i,j) / qsata(i,j)
          IF (flag_ents) THEN
             eb_relh(i,j) = REAL(rq)

             ! Surface specific humidity, derived at talt assuming vertically
             ! constant relh (orogswitch=2)
             surf_tq2 = rq * surf_qsata
          END IF

          ! Use climatological albedo in calculating incoming
          ! shortwave radiation shortwave radiation NB modified later
          ! over ice
          IF (flag_ents) albedo(i,j) = atm_alb(i,j,imth)
          ! cgv2 seasonal
          IF (flag_ents) THEN
             fxsw(i,j) = solfor(j, MOD(istot-1,nyear) + 1) * (1.0 - albedo(i,j))
          ELSE
             fxsw(i,j) = solfor(j, MOD(istot-1,nyear) + 1) * (1.0 - albcl(i,j))
          END IF
          IF (flag_ents) THEN
             IF (t_orog == 1) THEN
                ! store orography at previous timestep to derive meltwater
                surf_orog_atm_previous(i,j) = surf_orog_atm(i,j)
                surf_orog_atm(i,j) = orog_exact(i,j)
             END IF
             IF (t_lice == 1) THEN
                landice_slicemask_lic(i,j) = CEILING(lice_exact(i,j))
             ELSE
                lice_exact(i,j) = landice_slicemask_lic(i,j)
             END IF
          END IF

          ! Outgoing planetary longwave

          !     NOTE: JDA added in olr_adj term (equivalent to olr_1 in holden
          !           (clim dyn) but with opposite sign
          !           pbh added in olr_adj0 term - globally uniform perturbation
          !           (reduction) to clear skies OLR

          ! calculate coefficients
          tv0 = b00 + rq * (b10 + b20 * rq)
          tv1 = b01 + rq * (b11 + b21 * rq)
          tv2 = b02 + rq * (b12 + b22 * rq)
          tv3 = b03 + rq * (b13 + b23 * rq)
          ! calculate CH4 and N2O contributions
          ! NOTE units are ppb for CH4 and N2O calculation
          ch4_term = &
               & alphach4 * (SQRT(1.0E9 * ch4(i,j)) - SQRT(1.0E9 * ch40)) &
               & - ch4_func(1.0E9 * ch4(i,j), 1.0E9 * n2o0) &
               & + ch4_func(1.0E9 * ch40, 1.0E9 * n2o0)
          n2o_term = &
               & alphan2o * (SQRT(1.0E9 * n2o(i,j)) - SQRT(1.0E9 * n2o0)) &
               & - ch4_func(1.0E9 * ch40, 1.0E9 * n2o(i,j)) &
               & + ch4_func(1.0E9 * ch40, 1.0E9 * n2o0)
          ! calculate outgoign longwave
          ! talt dependence removed for olr (orography only affects surface
          ! proceses now if orogswitch=2)
          IF (orogswitch < 2 .AND. flag_ents) THEN
             fxplw(i,j) = tv0 + &
                  & talt * (tv1 + talt * (tv2 + talt * tv3)) &
                  & - delf2x * LOG(co2(i,j) / co20) &
                  & - ch4_term - n2o_term &
                  & + olr_adj * (meantemp-t_eqm) - olr_adj0
          ELSE
             fxplw(i,j) = tv0 + &
                  & atemp(i,j) * (tv1 + atemp(i,j) * (tv2 + atemp(i,j) * tv3)) &
                  & - delf2x * LOG(co2(i,j) / co20) &
                  & - ch4_term - n2o_term &
                  & + olr_adj * (meantemp-t_eqm) - olr_adj0
          END IF

          ! Latent heat flux

          ! latent heat flux into atmos associated with condensation no account
          ! taken of snow melting, must assume all pptn is rain
          fxlata(i,j) = rho0 * pptn(i,j) * hlv

          ! calculate terms over ocean or ice
          IF (k1(i,j) <= maxk) THEN
             ! longwave radiation
             alw = atemp(i,j) + zeroc
             alw = alw * alw
             alw = alw * alw
             alw = ema * alw

             ! surface salinity-dependent freezing point:
             salt = saln0 + osaln(i,j)
             tsfreez(i,j) = &
                  & salt * (-0.0575 + 0.0017 * SQRT(salt) - 0.0002 * salt)

             ! maximum amount of heat available in first layer
             ! nre rsictscsf must be changed if dt>17.5 days, see gseta
             qb(i,j) = rsictscsf * (tsfreez(i,j) - otemp(i,j))
             qbsic(i,j) = qb(i,j)

             ! calculate terms over ice
             IF (sica(i,j) > 0.0) THEN
                ! * Sea-ice present *

                ! Let albedo over sea ice vary as a function of tair
                ! (Holland et al. 1993)
                albsic = MAX(par_albsic_min, &
                     & MIN(par_albsic_max, 0.40 - 0.04 * atemp(i,j)))
                IF (flag_ents) THEN
                   fxswsic = solfor(j, MOD(istot-1,nyear) + 1) * &
                        & (1.0 - albsic) * (1.0 - albedo(i,j))

                   fxswsica = solfor(j, MOD(istot-1,nyear) + 1) * &
                        & (1.0 - albedo(i,j))
                ELSE
                   fxswsic = solfor(j, MOD(istot-1,nyear) + 1) * (1.0 - albsic)
                END IF

                ! first need to calculate T_ice
                DO iter = 1, itice
                   ticold = tice(i,j)
                   ! Dalton number
                   cesic = 1.0E-3 * (1.0022 - 0.0822 * (atemp(i,j) &
                        & - ticold) + 0.0266 * usurf(i,j))
                   cesic = MAX(6.0E-5, MIN(2.19E-3, cesic))
                   chsic = 0.94 * cesic

                   ! sensible heat flux
                   cfxsensic = rhoair * chsic * cpa * usurf(i,j)

                   qsatsic = const1 * EXP(const2 * ticold / (ticold + const3))

                   evapsic(i,j) = MAX(0.0, (qsatsic - ashum(i,j)) * &
                        & rhoao * cesic * usurf(i,j))

                   tieqn = sich(i,j) * &
                        &    ((1 - ca(i,j)) * fxswsic + alw &
                        &     - emo * (ticold + zeroc)**4 &
                        &     - cfxsensic * (ticold - atemp(i,j)) &
                        &     - rho0 * hls * evapsic(i,j)) + &
                        &  consic * (tsfreez(i,j) - ticold)

                   dtieq = sich(i,j) * &
                        & (-4.0 * emo * (ticold + zeroc)**3 &
                        &  - cfxsensic &
                        &  - hls * rhoair * cesic * usurf(i,j) * qsatsic * &
                        &     const2 * const3 / ((ticold + const3)**2) * &
                        &     0.5 * (1.0 + SIGN(1.0,qsatsic - ashum(i,j)))) &
                        & - consic

                   tice(i,j) = REAL(ticold - tieqn / dtieq)

                   IF (ABS(tice(i,j) - ticold) < tol .OR. &
                        & (ticold > tfreez .AND. tieqn > 0.0)) GOTO 10
                END DO

                PRINT *, 'warning sea-ice iteration failed at', istot, i, j &
                     & ,tice(i,j), ticold, tice(i,j)-ticold, tieqn, dtieq
                PRINT *, 'cesic ', cesic, 'cfxsensic ', cfxsensic, &
                     & 'qsatsic ', qsatsic, 'evapsic ', evapsic(i,j), &
                     & 'tieqn ', tieqn, 'dtieq ', dtieq
                PRINT *, 'sich ', sich(i,j), 'sica ', sica(i,j)
                IF (debug_loop) STOP
10              tice(i,j) = MIN(REAL(tfreez), tice(i,j))

                ! recalc everything in case of resetting of tice
                fxlwsic = emo * (tice(i,j) + zeroc )**4 - alw

                cesic = 1.0E-3 * (1.0022 - 0.0822 * (atemp(i,j) &
                     & - tice(i,j)) + 0.0266 * usurf(i,j))
                cesic = MAX(6.0E-5, MIN(2.19E-3, cesic))

                chsic = 0.94 * cesic

                cfxsensic = rhoair * chsic * cpa * usurf(i,j)

                fxsensic = cfxsensic * (tice(i,j) - atemp(i,j))

                qsatsic = const1 * &
                     & EXP(const2 * tice(i,j)  / (tice(i,j) + const3))

                evapsic(i,j) = MAX(0.0, (qsatsic - ashum(i,j)) * &
                     & rhoao * cesic * usurf(i,j))

                fx0sic(i,j) = (1 - ca(i,j)) * fxswsic - fxsensic &
                     & - fxlwsic - rho0 * hls * evapsic(i,j)

                IF (flag_ents) THEN
                   fx0sica = ca(i,j) * fxswsica + fxlata(i,j) &
                        & + fxsensic + fxlwsic - fxplw(i,j)
                ELSE
                   fx0sica = ca(i,j) * fxswsic + fxlata(i,j) &
                        & + fxsensic + fxlwsic - fxplw(i,j)
                END IF
                IF (flag_ents) THEN
                   fx0sica = ca(i,j) * fxswsica + fxlata(i,j) &
                        & + fxsensic + fxlwsic - fxplw(i,j)
                END IF

                ! components of heat flux into atmosphere over sea-ice
                atm_latenti = +fxlata(i,j)
                atm_sensiblei = +fxsensic
                IF (flag_ents) THEN
                   atm_netsoli = +ca(i,j) * fxswsica
                ELSE
                   atm_netsoli = +ca(i,j) * fxswsic
                END IF
                atm_netlongi  = +fxlwsic - fxplw(i,j)

                dhsic = rrholf * (qb(i,j) - fx0sic(i,j)) - rhooi * evapsic(i,j)

                ! Assign a dummy value of qb(i,j) calculated to give
                ! no further sea-ice growth if existing thickness
                ! exceeds a given threshold (par_sich_max)
                IF (sich(i,j) >= par_sich_max) THEN
                   IF (dhsic > 0.0) THEN
                      qbsic(i,j) = (0.0 + rhooi * evapsic(i,j)) / rrholf + &
                           & fx0sic(i,j)
                      dhsic = rrholf * (qbsic(i,j) - fx0sic(i,j)) &
                           & - rhooi * evapsic(i,j)
                   END IF
                END IF
             ELSE
                ! * Sea-ice absent *
                albsic = 0.0
                fx0sica = 0.0
                dhsic = 0.0
                evapsic(i,j) = 0.0
                tice(i,j) = 0.0
                atm_latenti = 0.0
                atm_sensiblei = 0.0
                atm_netsoli = 0.0
                atm_netlongi = 0.0
             END IF

             ! over open ocean
             fxlw(i,j) = emo * (otemp(i,j) + zeroc)**4 - alw

             ! Dalton number
             ce = 1.0E-3 * (1.0022 - 0.0822 * (atemp(i,j) - &
                  & otemp(i,j)) + 0.0266 * usurf(i,j))
             ce = MAX(6.0E-5, MIN(2.19E-3, ce))
             ch = 0.94 * ce

             ! sensible heat flux from ocean to atmosphere
             fxsen(i,j) = rhoair * ch * cpa * usurf(i,j) * &
                  & (otemp(i,j) - atemp(i,j))

             ! evaporation/sublimation rate
             qsato(i,j) = const1 * &
                  & EXP(const4 * otemp(i,j) / (otemp(i,j) + const5))
             evap(i,j) = MAX(0.0, (qsato(i,j) - ashum(i,j)) * &
                  & rhoao * ce * usurf(i,j))

             ! net heat flux into atmosphere
             fx0oa = ca(i,j) * fxsw(i,j) + fxlata(i,j) + fxsen(i,j) + &
                  & fxlw(i,j) - fxplw(i,j)

             ! set up fluxes -> atmosphere
             atm_latent = +fxlata(i,j)
             atm_sensible = +fxsen(i,j)
             atm_netsol = +ca(i,j) * fxsw(i,j)
             atm_netlong = +fxlw(i,j) - fxplw(i,j)

             ! add proportions over open ocean and sea ice
             fx0a(i,j) = (1 - sica(i,j)) * fx0oa + sica(i,j) * fx0sica

             ! add fluxes to include sea-ice and ocean components
             fxlha(i,j) = REAL((sica(i,j) * atm_latenti) + &
                  & ((1 - sica(i,j)) * atm_latent))
             fxsha(i,j) = REAL((sica(i,j) * atm_sensiblei) + &
                  & ((1 - sica(i,j)) * atm_sensible))
             fxswa(i,j) = REAL((sica(i,j) * atm_netsoli) + &
                  & ((1 - sica(i,j)) * atm_netsol))
             fxlwa(i,j) = REAL((sica(i,j) * atm_netlongi) + &
                  & ((1 - sica(i,j)) * atm_netlong))

             ! heat flux from atmosphere into open ocean
             IF (flag_ents) THEN
                fx0o(i,j) = ((1 - ca(i,j)) * fxsw(i,j) * (1.0 - albo(j,imth))) &
                     & - fxsen(i,j) - fxlw(i,j) - rho0 * hlv * evap(i,j)
             ELSE
                fx0o(i,j) = (1 - ca(i,j)) * fxsw(i,j) - fxsen(i,j) &
                     & - fxlw(i,j) - rho0 * hlv * evap(i,j)
             END IF

             ! net heat flux into ocean from atmosphere and sea ice
             ! including possible ice growth over open ocean
             fx0neto(i,j) = sica(i,j) * qbsic(i,j) &
                  & + (1 - sica(i,j)) * MAX(qb(i,j), fx0o(i,j))

             ! set up fluxes -> ocean
             fxlho(i,j) = REAL((1 - sica(i,j)) * (-rho0 * hlv * evap(i,j) + &
                  & MAX(0.0, qb(i,j) - fx0o(i,j))) + sica(i,j) * qbsic(i,j))
             fxsho(i,j) = -REAL((1 - sica(i,j)) * fxsen(i,j))
             IF (flag_ents) THEN
                fxswo(i,j) = REAL((1 - sica(i,j)) * (1 - ca(i,j)) * &
                     & fxsw(i,j) * (1.0 - albo(j,imth)))
             ELSE
                fxswo(i,j) = REAL((1 - sica(i,j)) * (1 - ca(i,j)) * fxsw(i,j))
             END IF
             fxlwo(i,j)  = -REAL((1 - sica(i,j)) * fxlw(i,j))

             dho = MAX(0.0, rrholf * (qb(i,j) - fx0o(i,j)))

             dthsic(i,j) = REAL(sica(i,j) * dhsic + (1 - sica(i,j)) * dho)

             dtareasic(i,j) = REAL(MAX(0.0, rhmin * dho * (1 - sica(i,j))))
             IF (sich(i,j) > 1.0E-12) THEN
                dtareasic(i,j) = REAL(dtareasic(i,j) + &
                     & MIN(0.0, 0.5 * sica(i,j) * sica(i,j) * &
                     & dhsic / sich(i,j)))
             END IF

             IF (flag_ents) THEN
                ! Calculate planetary albedo over ocean boxes
                albs_atm(i,j) = (albo(j,imth) &
                     & - (sica(i,j) * (albo(j,imth) - albsic)))
                palb(i,j) = (1.0 - albedo(i,j)) * (albo(j,imth) &
                     & - (sica(i,j) * (albo(j,imth) - albsic))) &
                     & + albedo(i,j)
             END IF

             ! albedo array, purely diagnostic variable
             IF (.NOT. flag_ents) THEN
                albedo(i,j) = &
                     & REAL(sica(i,j) * albsic + (1 - sica(i,j)) * albcl(i,j))
             END IF

             ! sea-ice albedo now output
             albice(i,j) = REAL(albsic)
          ELSE
             ! calculate terms over land
             IF (.NOT. flag_ents) THEN
                fx0a(i,j) = fxsw(i,j) + fxlata(i,j) - fxplw(i,j)
             END IF

             ! set up fluxes -> atmosphere
             IF (.NOT. flag_ents) THEN
                fxlha(i,j) = +REAL(fxlata(i,j))
                fxsha(i,j) = +0.0
                fxswa(i,j) = +REAL(fxsw(i,j))
                fxlwa(i,j) = -REAL(fxplw(i,j))
             END IF

             ! runoff; add pptn to appropriate coastal grid cell
             IF (.NOT. flag_ents) THEN
                IF (igrid /= 0) THEN
                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j)) + &
                        & pptn(i,j) * ds(j) * rds(jroff(i,j))
                ELSE
                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j)) + pptn(i,j)
                END IF
             END IF

             IF (.NOT. flag_ents) runoff_land(i,j) = REAL(pptn(i,j))

             IF (flag_ents) THEN
                ! ENTS radiation and hydrology

                ! Offline model: switch variables
                IF (ents_offlineswitch == 1) THEN
                   dum_talt = talt
                   dum_hum = ashum(i,j)
                   dum_pptn = pptn(i,j)
                   ! Replace with offline data
                   IF (orogswitch == 1) THEN
                      talt = tncep(i,j,imth) + (lapse_rate * surf_orog_atm(i,j))
                   ELSE
                      talt = tncep(i,j,imth)
                   END IF
                   qsalt = const1 * EXP(const4 * talt / (talt + const5))
                   ashum(i,j) = rhncep(i,j,imth) * qsalt
                   pptn(i,j) = pncep(i,j,imth)
                END IF

                ! Snow
                ! also update land surface albedo values according to
                ! changes here
                IF (land_temp_lnd(i,j) < -5 .AND. talt < -5 .AND. &
                     & pptn(i,j) > 0.0) THEN
                   land_snow_lnd(i,j) = 1
                   albs_atm(i,j) = land_albs_snow_lnd(i,j)
                END IF

                ! Snow melt
                ! also update land surface albedo values according to
                ! changes here
                IF (land_temp_lnd(i,j) >= -5 .AND. talt >= -5 .AND. &
                     & land_snow_lnd(i,j) == 1) THEN
                   land_snow_lnd(i,j) = 0
                   albs_atm(i,j) = land_albs_nosnow_lnd(i,j)
                END IF

                ! transfer coefficients
                ! should this be moved after affects of slicemask calculated?
                chl(i,j) = 1.0 / (((1.0 / 0.41) * &
                     & LOG(10.0 / land_z0_lnd(i,j)))**2)
                cel(i,j) = chl(i,j)

                ! Albedo functionality
                IF (ABS(landice_slicemask_lic(i,j) - 2.0) < 1.0E-19) THEN
                   albs_atm(i,j) = &
                        & albs_atm(i,j) * (2 - lice_exact(i,j)) + &
                        & 0.8 * (lice_exact(i,j) - 1)
                   land_z0_lnd(i,j) = &
                        & land_z0_lnd(i,j) * (2 - lice_exact(i,j)) +  &
                        & 0.001 * (lice_exact(i,j) - 1)
                   land_bcap_lnd(i,j) = &
                        & land_bcap_lnd(i,j) * (2 - lice_exact(i,j)) + &
                        & lice_k9 * (lice_exact(i,j) - 1)
                END IF

                ! Newton-Raphson iteration loop to solve for eqm land
                ! temperature. Justified for 1 ocean timestep.
                DO itld = 1, itldmax
                   ! sensible heat flux from land to atmosphere
                   fxsen(i,j) = rhoair * chl(i,j) * cpa * usurfl(i,j,imth) *  &
                        & (land_temp_lnd(i,j) - talt)

                   ! longwave radiation
                   IF (ents_offlineswitch == 1) THEN
                      alw = tncep(i,j,imth) + zeroc
                   ELSE
                      alw = tq1(1,i,j) + zeroc
                   END IF
                   alw = alw  *  alw
                   alw = alw * alw
                   alw = ema * alw
                   ! Net longwave over land (positive upward)
                   fxlw(i,j) = eml * ((land_temp_lnd(i,j) + zeroc)**4) - alw

                   ! Evaporation
                   IF (land_temp_lnd(i,j) <= 0.) THEN
                      qsato(i,j) = const1 * EXP(const2 * land_temp_lnd(i,j) / &
                           & (land_temp_lnd(i,j) + const3))
                   ELSE
                      qsato(i,j) = const1 * EXP(const4 * land_temp_lnd(i,j) / &
                           & (land_temp_lnd(i,j) + const5))
                   END IF

                   ! devapcheck used to find out if evap has a
                   ! differential (0=N, 1=Y)
                   IF (land_moisture_lnd(i,j) > 0.0) THEN
                      beta = MIN(1.0, (land_moisture_lnd(i,j) / &
                           &           land_bcap_lnd(i,j))**4)

                      ! evap calculated with surface humidity if orogswitch=2
                      IF (orogswitch < 2) THEN
                         evap(i,j) = MAX(0.0, (qsato(i,j) - ashum(i,j)) * &
                              & rhoao * cel(i,j) * usurfl(i,j,imth) * beta)
                      ELSE
                         evap(i,j) = MAX(0.0, (qsato(i,j) - surf_tq2) * &
                              & rhoao * cel(i,j) * usurfl(i,j,imth) * beta)
                      END IF

                      devapcheck = 1
                   ELSE
                      evap(i,j) = 0.0
                      devapcheck = 0
                   END IF

                   IF (evap(i,j) * dtsurfdim > land_moisture_lnd(i,j)) THEN
                      evap(i,j) = land_moisture_lnd(i,j) * rdtsurfdim
                      devapcheck  =0
                   END IF

                   ! ODE for land temp wrt to time
                   dtld = rhcld * ((1 - ca(i,j)) * fxsw(i,j) * &
                        & (1 - albs_atm(i,j)) &
                        & - fxsen(i,j) - fxlw(i,j) - rho0 * hlv * evap(i,j))
                   ! Sensible heat derivative
                   dfxsens = rhoair * chl(i,j) * cpa * usurfl(i,j,imth)

                   ! net longwave derivative
                   dfxlw = 4.0 * eml * ((land_temp_lnd(i,j) + zeroc)**3)

                   ! evap derivative
                   IF (devapcheck == 0) THEN
                      devap = 0.0
                   ELSE
                      IF (land_temp_lnd(i,j) <= 0.0) THEN
                         dqsato = ((const1 * const2 * const3) /  &
                              & (land_temp_lnd(i,j) + const3)**2) &
                              &  * EXP(const2 * land_temp_lnd(i,j) &
                              &  / (land_temp_lnd(i,j) + const3))
                      ELSE
                         dqsato = ((const1 * const4 * const5) /  &
                              & (land_temp_lnd(i,j) + const5)**2) &
                              &  * EXP(const4 * land_temp_lnd(i,j) &
                              &  / (land_temp_lnd(i,j) + const5))
                      END IF

                      ! Calculate evaporation diff. (bare soil) or
                      ! evapotranspiration diff. (veg) depending on
                      ! whether carbon feedbacks on climate chosen
                      ! (carbonswitch)
                      devap = rhoao * cel(i,j) * &
                           & usurfl(i,j,imth) * beta * dqsato
                   END IF

                   ! total derivative
                   dtld1 = -rhcld * (dfxsens + dfxlw + (rho0 * hlv * devap))

                   ! update last value
                   tldlast = land_temp_lnd(i,j)

                   ! calculate new value of tqld(1,i,j)
                   land_temp_lnd(i,j) = land_temp_lnd(i,j) - (dtld / dtld1)

                   IF (ABS(tldlast - land_temp_lnd(i,j)) < tolld) EXIT

                   IF (itld == itldmax) THEN
                      PRINT *, &
                           & 'Land rad. calc. did not converge to specified tol'
                      PRINT *, 'at point', i, j
                      PRINT *, 'final value was ', land_temp_lnd(i,j)
                      PRINT *, 'last iteration value was ', tldlast
                      PRINT *, 'istep ', istep
                      PRINT *, ' '
                   END IF
                END DO

                ! Radiation fluxes
                fx0a(i,j) = ca(i,j) * fxsw(i,j) + fxlata(i,j) + &
                     & fxlw(i,j) + fxsen(i,j) - fxplw(i,j)
                fx0o(i,j) = (1 - ca(i,j)) * fxsw(i,j) * (1 - albs_atm(i,j)) - &
                     & fxsen(i,j) - fxlw(i,j) - rho0 * hlv * evap(i,j)

                ! Bucket land hydrology: update bucket sizes
                land_moisture_lnd(i,j) = land_moisture_lnd(i,j) + &
                     & ((pptn(i,j) - evap(i,j)) * dtsurfdim)

                ! runoff scheme: in the case of land water bucket gt
                ! bucket capacity, bcap, find nearest ocean
                ! gridbox/gridboxes and add the surplus as runoff
                ! there
                IF (land_moisture_lnd(i,j) > land_bcap_lnd(i,j)) THEN
                   IF (igrid /= 0) THEN
                      runoff(iroff(i,j),jroff(i,j)) = &
                           & runoff(iroff(i,j),jroff(i,j)) + &
                           & ((land_moisture_lnd(i,j) - land_bcap_lnd(i,j)) * &
                           & rdtsurfdim) * ds(j) * rds(jroff(i,j))
                      ! need the excess surface water skimmed off
                      ! anyway before trying other schemes
                      IF (par_runoff_scheme > 0) THEN
                         runoff_land(i,j) = runoff_land(i,j) +  &
                              & ((land_moisture_lnd(i,j) - &
                              &   land_bcap_lnd(i,j)) * &
                              & rdtsurfdim) * ds(j)  * rds(jroff(i,j))
                      END IF
                   ELSE
                      runoff(iroff(i,j),jroff(i,j)) = &
                           & runoff(iroff(i,j),jroff(i,j)) + &
                           & ((land_moisture_lnd(i,j) - land_bcap_lnd(i,j)) * &
                           & rdtsurfdim)
                      ! need the excess surface water skimmed off
                      ! anyway before trying other schemes
                      IF (par_runoff_scheme > 0) THEN
                         runoff_land(i,j) = runoff_land(i,j) +  &
                              & ((land_moisture_lnd(i,j) - &
                              &   land_bcap_lnd(i,j)) * rdtsurfdim)
                      END IF
                   END IF
                   land_moisture_lnd(i,j) = land_bcap_lnd(i,j)
                END IF

                ! Meissner et al (2003) leaky bucket scheme.
                ! 6.63 = b = Clapp-Hornberger exponent (=4.50 for
                ! coarse grain soil and 11.20 for fien grain)
                ! 0.0047 kg/m2/s = K_s = 0.0047 mm s-1 = 0.0000047 m s-1 =
                ! Saturated hydraulic conductivity
                ! Y = runoff; M = moisture; M_sat = saturated soil moisture
                ! Y = K_s(M/M_Sat)^(2b+3) = 0.0047*(M/M_Sat)^15.2
                ! [2b+3 = {12.00, 16.26, 25.40} = runoff_factor_1]
                ! kg m-2 is mm for rainfall, which rokgem uses.
                ! need to divide rather than multiply by rdtdim (=
                ! 1/ocean timestep in
                ! sec = 3.1688087814028956E-006 s-1),
                ! or just leave out altogether(!) as have a s-1 in K_s? Yes, am
                ! multiplying by a factor of 1000 below (m2mm) so all
                ! in all getting the right magnitude for the wrong
                ! reason (should be ~300x more!)
                ! could adjust K_s, or even have a 2d field for it...
                ! /(11.0-MIN(10,istep)) is used to ramp up runoff to
                ! full steam so as to avoid crashing the model
                IF (par_runoff_scheme == 2) THEN
                   runoff_land(i,j) = runoff_land(i,j) +  &
                        & ((0.0000047 / (11.0 - MIN(10, istep))) *  &
                        & (land_moisture_lnd(i,j) / &
                        &  land_bcap_lnd(i,j))**runoff_factor_1)

                   ! balance water budget
                   land_moisture_lnd(i,j) = land_moisture_lnd(i,j) - &
                        & runoff_land(i,j) * dtsurfdim / (11.0 - MIN(10, istep))

                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j)) + runoff_land(i,j)
                END IF

                ! Try Phil Holden's idea of soil moiture half-life
                ! (residence time) of 1-2 months (call it 1.5) from
                ! Pidwirny, M. (2006). 'The Hydrologic Cycle'. Fundamentals of
                ! Physical Geography, 2nd Edition. Date
                ! Viewed. http://www.physicalgeography.net/fundamentals/8b.html
                ! residence time = tau = 1.5 months = 1/8 year =
                ! 3944700sec. dtdim =
                ! timestep = 315576sec
                ! runoff_factor_2 = [1-exp(-tln2/tau)] =0.054 with 100
                ! tsteps/year
                IF (par_runoff_scheme == 3) THEN
                   runoff_land(i,j) = runoff_land(i,j) +  &
                        & land_moisture_lnd(i,j) * runoff_factor_2 * rdtsurfdim

                   ! balance water budget
                   land_moisture_lnd(i,j) = land_moisture_lnd(i,j) - &
                        & runoff_land(i,j) * dtsurfdim

                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j)) + runoff_land(i,j)
                END IF

                IF (par_runoff_scheme == 0) runoff_land(i,j) = REAL(pptn(i,j))

                ! Calculate planetary albedo over land
                palb(i,j) = ((1.0 - albedo(i,j)) * albs_atm(i,j)) + albedo(i,j)

                ! add fluxes to include sea-ice and ocean components
                fxlha(i,j) = REAL(fxlata(i,j))
                fxsha(i,j) = REAL(fxsen(i,j))
                fxswa(i,j) = REAL((ca(i,j) * fxsw(i,j)))
                fxlwa(i,j) = REAL((fxlw(i,j) - fxplw(i,j)))

                ! Offline model: switch variables back
                IF (ents_offlineswitch == 1) THEN
                   talt = dum_talt
                   ashum(i,j) = dum_hum
                   pptn(i,j) = dum_pptn
                END IF

                ! Update global heat souce over land
                ghs = ghs + fx0a(i,j) + fx0o(i,j)
             END IF
          END IF
       END DO
    END DO

    ! Add in effects of runoff (non-local in i,j) and set up fluxes
    ! freshwater forcing in m/s open ocean P-E over ocean gridboxes
    ! evap zero over land,
    ! nre P goes straight through as no snow allowed, but E is E total
    ! for atm model and solely E_ocean for the fwflux
    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) <= maxk) THEN
             ! wet points;
             pptn_ocn(i,j) = REAL(pptn(i,j))
             runoff_ocn(i,j) = REAL(runoff(i,j)) + REAL(ice_runoff(i,j))
             evap_atm(i,j) = REAL(evap(i,j) * (1 - sica(i,j)) + &
                  & evapsic(i,j) * sica(i,j))

             ! note that the FW anomaly normally applied to ocean FW
             ! fluxes has an odd status in a split-apart model.  As
             ! present it is added here via the precipitation term.
             ! It may make sense to add it differently or not at all
             ! in the future.
             pptn_ocn(i,j) = REAL(pptn_ocn(i,j) + pmeadj(i,j))
          ELSE
             ! set 'ocean' fluxes to zero over land
             IF (flag_ents) THEN
                pptn_ocn(i,j) = REAL(pptn(i,j))
                runoff_ocn(i,j) = REAL(runoff(i,j))
                evap_atm(i,j) = REAL(evap(i,j))
             ELSE
                pptn_ocn(i,j) = 0.0
                runoff_ocn(i,j) = 0.0
                evap_atm(i,j) = 0.0
             END IF
          END IF

          ! All cells (no evaporation over land - in this scheme)
          pptn_atm(i,j) = REAL(pptn(i,j))
          evap_ocn(i,j) = -evap_atm(i,j)
          pptn_ocn(i,j) = REAL(pptn_ocn(i,j) * m2mm)
          evap_ocn(i,j) = REAL(evap_ocn(i,j) * m2mm)
          runoff_ocn(i,j) = REAL(runoff_ocn(i,j) * m2mm)
          runoff_land(i,j) = REAL(runoff_land(i,j) * m2mm)
          pptn_atm(i,j) = REAL(pptn_atm(i,j) * m2mm)
          evap_atm(i,j) = REAL(evap_atm(i,j) * m2mm)
       END DO
    END DO

    ! call routine to output surface flux fields (in the same way as a
    ! restart file)
    IF (MOD(istep, iwstp) == 0) THEN
       ext = conv_num(MOD(iw, 10))
       IF (debug_loop) PRINT *, &
            & 'Writing SURFLUX output file at time', istep
       CALL check_unit(2, __LINE__, __FILE__)
       OPEN(2,FILE=outdir_name(1:lenout)//lout//'.sfx.'//ext,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       REWIND 2
       CALL outm_surf(2, co2, albedo, usurf, fxlho, fxsho, fxswo, fxlwo, &
            & evap_atm, pptn_ocn, runoff_ocn, fxlha,fxsha, fxswa, fxlwa, &
            & evap_atm, pptn_atm, dthsic, dtareasic)
       CLOSE(2,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       IF (debug_loop) PRINT *
    END IF

    ! Set values of dummy variables destined for BIOGEM & ENTS
    ! NOTE: convert precision between GOLDSTEIn (real*8) and GENIE
    !       (which can be real*4 or real* depending on the IGCM)
    go_fxsw = REAL(fxsw)
    eb_fx0a  = REAL(fx0a)
    eb_fx0o  = REAL(fx0o)
    eb_fxsen = REAL(fxsen)
    eb_fxlw  = REAL(fxlw)
    eb_evap  = REAL(evap)
    eb_pptn  = REAL(pptn)
    eb_uv  = usc * uatm
    eb_usurf = usurf
    DO j = 1, maxj
       go_solfor(j) = REAL(solfor(j, MOD(istot-1, nyear) + 1))
    END DO

    ! annual call to radfor if transient orbit is applied
    IF (t_orbit == 1) THEN
       IF (MOD(istep-1, nyear) == 0) THEN
          CALL radfor(istep + 0, REAL(gn_daysperyear), solconst, flag_ents)
       END IF
    END IF

  END SUBROUTINE surflux


  FUNCTION conv_num(i)
    IMPLICIT NONE
    CHARACTER(LEN=3) :: conv_num
    INTEGER :: i

    INTEGER :: i1, i2, itemp, i3
    CHARACTER(LEN=1) :: a, b, c
    IF (i < 10) THEN
       a = CHAR(i + 48)
       conv_num = a // '  '
    ELSE IF (i < 100) THEN
       i1 = i / 10
       i2 = i - i1 * 10
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       conv_num = a // b // ' '
    ELSE
       i1 = i / 100
       itemp = i - 100 * i1
       i2 = itemp / 10
       i3 = itemp - 10 * i2
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       c = CHAR(i3 + 48)
       conv_num = a // b // c
    END IF
  END FUNCTION conv_num


  ! see: IPCC [1990, 2001]
  FUNCTION ch4_func(ch4, n2o)
    IMPLICIT NONE
    REAL :: ch4_func
    REAL :: ch4, n2o
    ch4_func = 0.47 * LOG(1.0 + 2.01E-5 * (ch4 * n2o)**0.75 + &
         & 5.31E-15 * ch4 * (ch4 * n2o)**1.52)
  END FUNCTION ch4_func


  ! Define runoff matrix for EMBM
  !
  ! The dry points in the k1 file now define a compass direction
  ! for the runoff from that point/cell. By following this direction
  ! to the sea (where k1 <= maxk) we build up the
  ! matrix (iroff(i,j),jroff(i,j))
  ! which defines where to put the runoff from point (i,j)
  subroutine readroff
    implicit none

    integer i, j, loop, iroe, iros, irow, iron

    parameter (iroe=91, iros=92, irow=93, iron=94)

    IF (debug_init) PRINT *, 'Calculating runoff routing'

    DO j = 1, maxj
       DO i = 1, maxi
          iroff(i,j) = i
          jroff(i,j) = j
          loop = 0
          do while(k1(iroff(i,j),jroff(i,j)) > maxk)
             IF (k1(iroff(i,j),jroff(i,j)) == iroe) THEN
                iroff(i,j) = iroff(i,j) + 1
             ELSE IF (k1(iroff(i,j),jroff(i,j)) == iros) THEN
                jroff(i,j) = jroff(i,j) - 1
             ELSE IF (k1(iroff(i,j),jroff(i,j)) == irow) THEN
                iroff(i,j) = iroff(i,j) - 1
             ELSE IF (k1(iroff(i,j),jroff(i,j)) == iron) THEN
                jroff(i,j) = jroff(i,j) + 1
             END IF
             ! periodic b.c.
             IF (iroff(i,j) == maxi+1) THEN
                iroff(i,j) = 1
             ELSE IF (iroff(i,j) == 0) THEN
                iroff(i,j) = maxi
             END IF
             ! avoid inf. loops
             loop = loop + 1
             IF (loop > 100000) THEN
                PRINT *, 'There is a problem calculating runoff'
                PRINT *, 'Located at k1(',i,',',j,')'
                PRINT *, 'k1(',i,',',j,') = ',k1(i,j)
                PRINT *, 'iroff(i,j) = ',iroff(i,j)
                PRINT *, 'jroff(i,j) = ',jroff(i,j)
                PRINT *, 'k1(iroff(i,j),jroff(i,j)) = ', &
                     & k1(iroff(i,j),jroff(i,j)),' (maxk = ',maxk,')'
                stop 'problem calculating runoff'
             END IF
          END DO
       END DO
    END DO

    IF (debug_init) PRINT *, 'Runoff routing successfully calculated'
  end subroutine readroff

END MODULE embm

MODULE goldstein

  USE genie_util, ONLY: check_unit, check_iostat
  USE goldstein_lib
  USE goldstein_netcdf
  USE goldstein_data
  USE goldstein_diag
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_goldstein
  PUBLIC :: step_goldstein
  PUBLIC :: end_goldstein

CONTAINS

  SUBROUTINE step_goldstein(istep, latent_ocn, sensible_ocn, netsolar_ocn, &
       & netlong_ocn, fx0sic_ocn, evap_ocn, pptn_ocn, runoff_ocn, fwsic_ocn, &
       & stressxu_ocn, stressyu_ocn, stressxv_ocn, stressyv_ocn, tsval_ocn, &
       & ssval_ocn, usval_ocn, vsval_ocn, albedo_ocn, test_energy_ocean, &
       & test_water_ocean, go_ts, go_ts1, go_cost, go_u, go_tau, &
       & go_psi, go_mldta, go_rho)
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep
    REAL, DIMENSION(:,:), INTENT(INOUT) :: &
         & latent_ocn, sensible_ocn, netsolar_ocn, netlong_ocn, fx0sic_ocn, &
         & evap_ocn, pptn_ocn, runoff_ocn, fwsic_ocn, stressxu_ocn, &
         & stressyu_ocn, stressxv_ocn, stressyv_ocn, tsval_ocn, ssval_ocn, &
         & usval_ocn, vsval_ocn, albedo_ocn
    REAL, INTENT(OUT) :: test_energy_ocean, test_water_ocean
    ! Dummy variables to be passed to/from BIOGEM via genie.F
    REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: go_ts, go_ts1
    REAL, DIMENSION(:,:), INTENT(INOUT) :: go_cost, go_mldta
    REAL, INTENT(INOUT) :: go_u(3,maxi,maxj,maxk)
    REAL, INTENT(INOUT) :: go_tau(2,maxi,maxj)
    REAL, INTENT(INOUT) :: go_psi(0:maxi,0:maxj)
    REAL, INTENT(INOUT) :: go_rho(maxi,maxj,maxk)

    LOGICAL, PARAMETER :: flag_igcmatmos=.FALSE.
    INTEGER :: i, j, k, isl, itv, iout, ios
    REAL :: time, avn, avs, sums(8*maxl), rms, tv4, tv2, tv3

    ! Stream function variables
    REAL, DIMENSION(0:maxj,0:maxk) :: opsi, opsia, opsip
    REAL :: omina, omaxa, ominp, omaxp
    INTEGER iposa(2)

    ! Surface flux variables
    REAL, DIMENSION(maxi,maxj) :: fx0neto, fwfxneto

    ! Extra fields for flux and wind stress passing
    REAL :: fx0flux(5,maxi,maxj), fwflux(4,maxi,maxj)
    REAL :: wstress(4,maxi,maxj)

    REAL :: work(0:maxi+1, 0:maxj+1, 0:maxk+1)

    ! Heat and freshwater time-series fluxes
    REAL :: fw_flx(5), fx_flx(5), t_area(5), r_itstp

    CHARACTER(LEN=3) :: ext

    ! For the energy calculations.....
    REAL :: tot_energy, tot_water, vsc
    REAL, SAVE :: ini_energy, ini_water
    LOGICAL :: lfirst=.TRUE.

    opsi = 0.0 ; opsia = 0.0 ; opsip = 0.0
    fx0neto = 0.0 ; fwfxneto = 0.0

    ! This bit just calculates the inital diagnostic of the total ocean
    !   thermal energy and water.  It is simply the temperature/density of
    !   each ocean gridbox multiplied by its vertical depth.
    ! The water bit has units of kg
    ! Note the -ve sign in the water part!!
    ! The energy bit has units of J

    ! First of all, set up this constant ....
    IF (lfirst) THEN
       vsc = dphi * rsc * rsc
       ini_energy = 0.0
       ini_water = 0.0
       DO k = 1, maxk
          DO j = 1, maxj
             DO i = 1, maxi
                ini_energy = ini_energy + ts(1,i,j,k) * dz(k) * ds(j)
                ini_water = ini_water - ts(2,i,j,k) * dz(k) * ds(j)
             END DO
          END DO
       END DO
       ! The m2mm is because internally, goldstein uses m, but genie
       ! uses mm.
       ini_energy = ini_energy * vsc * dsc * rh0sc * cpsc
       ini_water = m2mm * ini_water * vsc * dsc / saln0
       lfirst = .FALSE.
    END IF

    ! Pass back in BIOGEM-reset running count of cost
    cost = go_cost

    ! Need to calculate wind stresses first
    dztau(1,:,:) = scf * stressxu_ocn / (rh0sc * dsc * usc * fsc) / dzz
    dztau(2,:,:) = scf * stressyu_ocn / (rh0sc * dsc * usc * fsc) / dzz
    dztav(1,:,:) = scf * stressxv_ocn / (rh0sc * dsc * usc * fsc) / dzz
    dztav(2,:,:) = scf * stressyv_ocn / (rh0sc * dsc * usc * fsc) / dzz
    tau(1,:,:) = dztau(1,:,:) * dzz
    tau(2,:,:) = dztav(2,:,:) * dzz
    wstress(1,:,:) = stressxu_ocn
    wstress(2,:,:) = stressyu_ocn
    wstress(3,:,:) = stressxv_ocn
    wstress(4,:,:) = stressyv_ocn

    ! Wind calculations needed for mld
    IF (imld == 1) THEN
       ! Following code taken from genie-embm/src/fortan/surflux.F
       ! but min(j,maxj-1): avoids effect of N Pole singularity
       DO j = 1, maxj
          tv3 = 0.0
          DO i = 1, maxi
             IF (i == 1) THEN
                tv4 = (tau(1,i,j) + tau(1,maxi,j)) * 0.5
             ELSE
                tv4 = (tau(1,i,j) + tau(1,i-1,j)) * 0.5
             END IF
             IF (j == 1) THEN
                tv2 = tau(2,i,j) * 0.5
             ELSE
                tv2 = (tau(2,i,MIN(j, maxj-1)) + tau(2,i,j-1)) * 0.5
             END IF

             ! Wind stress used for mld calcs is consistent with that
             ! for ocean advection, NOT for surface heat fluxes.  Only
             ! matters if physically interpreting mldketaucoeff
             mldketau(i,j) = mldketaucoeff * (SQRT(SQRT(tv4**2 + tv2**2)))**3
             tv3 = tv3 + mldketau(i,j)
          END DO
          DO i = 1, maxi
             IF (j <= 2 .OR. j >= maxj-1) mldketau(i,j) = tv3 / maxi
          END DO
       END DO
    END IF

    CALL get_hosing(istep)

    ! Combine individual components of heat and freshwater fluxes into
    ! net fluxes needed by tstepo/tstipo

    ! *** Heat flux ***
    fx0neto = netsolar_ocn + sensible_ocn + netlong_ocn + &
         & latent_ocn + fx0sic_ocn
    fx0flux(1,:,:) = netsolar_ocn
    fx0flux(2,:,:) = sensible_ocn
    fx0flux(3,:,:) = netlong_ocn
    fx0flux(4,:,:) = latent_ocn
    fx0flux(5,:,:) = fx0sic_ocn

    ! *** Freshwater flux ***
    fwfxneto = pptn_ocn + evap_ocn + runoff_ocn + fwsic_ocn + &
         & fw_hosing + fw_anom
    fwfxneto = fwfxneto * mm2m
    fwflux(1,:,:) = pptn_ocn
    fwflux(2,:,:) = evap_ocn
    fwflux(3,:,:) = runoff_ocn
    fwflux(4,:,:) = fwsic_ocn

    ! Put these fluxes into correct array location for tstepo/tstipo
    ts(1,1:maxi,1:maxj,maxk+1) = -fx0neto * rfluxsc
    ts(2,1:maxi,1:maxj,maxk+1) = fwfxneto * rpmesco
    ts1(1,1:maxi,1:maxj,maxk+1) = ts(1,1:maxi,1:maxj,maxk+1)
    ts1(2,1:maxi,1:maxj,maxk+1) = ts(2,1:maxi,1:maxj,maxk+1)

    ! Input biogeochemical tracers arrays
    ! Don't let anyone ELSE change Temperature and Salinity !!
    ! NO - let BIOGEM change Temperature and Salinity
    ! it will DO it ever so nicely and gently ... ;)
    ts(:,1:maxi,1:maxj,1:maxk)  = go_ts
    ts1(:,1:maxi,1:maxj,1:maxk) = go_ts1

    ! Create b.c.
    ! NOTE: BIOGEM only passes in/out the core grid array
    !       (no boundaries) and does not update tracers at boundaries
    DO j = 1, maxj
       ts1(:,0,j,k1(0,j):maxk) = ts(:,maxi,j,k1(0,j):maxk)
       DO k = k1(maxi+1,j), maxk
          ts1(:,maxi+1,j,k) = ts(:,1,j,k)
       END DO
    END DO

    ! GOLDSTEIN MODEL TIMESTEP

    ! Ocean momentum

    ! This next section executes routines that calculate wind-driven
    ! circulation.  If the winds are fixed, as they are with the EMBM,
    ! this only needs be run just prior to the first iteration.  If
    ! the winds are time-variant, as they are with the IGCM, this will
    ! need to be run every time-step.  Hence the use of flags.
    CALL wind
    CALL jbar
    CALL ubarsolv(ub, psi)

    ! Find island path integral due to wind and jbar terms
    DO isl = 1, isles
       CALL island(ub, erisl(isl,isles+1), isl, 1)
    END DO

    ! Solve system of simultaneous equations. Zero division here might
    ! suggest not enough islands in the .psiles file
    IF (isles > 1) THEN
       CALL matmult(isles, erisl(:,1:isles), erisl(:,isles+1))
       DO isl = 1, isles
          psibc(isl) = -erisl(isl,isles+1)
       END DO
    ELSE
       psibc(1) = -erisl(1,2) / erisl(1,1)
    END IF

    DO j = 1, maxj
       DO i = 0, maxi+1
          ub(1,i,j) = ub(1,i,j) + SUM(ubisl(1,i,j,1:isles) * psibc(1:isles))
          ub(2,i,j) = ub(2,i,j) + SUM(ubisl(2,i,j,1:isles) * psibc(1:isles))
       END DO
    END DO

    ! Update diagnostic psi, not always necessary
    DO j = 0, maxj
       DO i = 0, maxi
          psi(i,j) = psi(i,j) + SUM(psisl(i,j,1:isles) * psibc(1:isles))
       END DO
    END DO

    ! Update velocities
    CALL velc

    CALL tstepo

    ! OCEAN DIAGNOSTICS AND OUTPUT

    ! "Healthcheck" and model rate of change
    IF (MOD(istep, npstp) == 0) THEN
       IF (debug_loop) THEN
          PRINT *, 'step ', istep
          PRINT *, 'psi on islands ', (psibc(isl), isl = 1, isles)
          CALL diag
       END IF
       IF (MOD(istep, npstp) == 0) THEN
          ts_store = ts(:,1:maxi,1:maxj,1:maxk)
       ELSE IF (MOD(istep, npstp) == 1 .AND. istep > 1) THEN
          rms = SUM((ts_store - ts(:,1:maxi,1:maxj,1:maxk))**2)
          rms = SQRT(rms / maxl / ntot / dt(maxk) / dt(maxk))
          IF (debug_loop) PRINT *, 'r.m.s. r.o.c.', rms
       END IF
       IF (debug_loop) PRINT *
    END IF

    ! Model restart (ASCII and netCDF)
    CALL outm_netcdf(istep)

    IF (MOD(istep, iwstp) == 0) THEN
       ext = conv(MOD(iw, 10))
       ! Oscillating streamfunction
       IF (.NOT. flat) THEN
          IF (debug_loop) &
               & PRINT *, 'Writing GOLDSTEIN oscillating streamfunction ', &
               & 'file at time', istep
          OPEN(2,FILE=outdir_name(1:lenout)//lout//'.psi.'//ext,IOSTAT=ios)
          CALL check_iostat(ios, __LINE__, __FILE__)
          DO j = 0, maxj
             DO i = 0, maxi
                IF (debug_loop) WRITE (2,*,IOSTAT=ios) psi(i,j)
                CALL check_iostat(ios, __LINE__, __FILE__)
             END DO
          END DO
          CLOSE(2,IOSTAT=ios)
          CALL check_iostat(ios, __LINE__, __FILE__)
       END IF
       iw = iw + 1
       IF (debug_loop) PRINT *
    END IF

    ! Time-series outputs (T, S, THC, FW flux)
100 FORMAT(5e14.6,2i5)
110 FORMAT(11e14.6)
120 FORMAT(11e14.6)

    IF (MOD(istep, itstp) == 0) THEN
       time = REAL(REAL(istep) / REAL(nyear))
       IF (debug_loop) &
            & PRINT *, 'Writing GOLDSTEIN time-series files at time', istep
       CALL check_unit(4, __LINE__, __FILE__)
       OPEN(4,FILE=outdir_name(1:lenout)//lout//'.'//'t', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       OPEN(14,FILE=outdir_name(1:lenout)//lout//'.'//'s', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       OPEN(40,FILE=outdir_name(1:lenout)//lout//'.'//'opsit', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CALL diag2(sums, avn, avs)
       IF (debug_loop) &
            & WRITE (4,110,IOSTAT=ios) time, (sums(i), i = 1, 8), avn, avs
       CALL check_iostat(ios, __LINE__, __FILE__)
       IF (debug_loop) &
            & WRITE (14,110,IOSTAT=ios) time, (sums(i), i = 9, 16), avn, avs
       CALL check_iostat(ios, __LINE__, __FILE__)
       CALL diagopsi(ominp, omaxp, omina, omaxa, opsi, opsia, opsip, iposa)
       IF (debug_loop) &
            & WRITE (40,100,IOSTAT=ios) time, ominp * opsisc, omaxp * opsisc, &
            & omina * opsisc, omaxa * opsisc, iposa(1), iposa(2)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(4,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(14,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(40,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       IF (debug_loop) PRINT *
    END IF

    IF (istep == 1) THEN
       fw_flx = 0.0
       fx_flx = 0.0
       t_area = 0.0
    END IF

    DO j = 1, maxj
       DO i = 1, maxi
          ! Ocean cells only
          IF (k1(i,j) <= maxk) THEN
             ! Global ocean
             fw_flx(1) = fw_flx(1) + (fwfxneto(i,j) * asurf(j))
             fx_flx(1) = fx_flx(1) + (fx0neto(i,j) * asurf(j))
             t_area(1) = t_area(1) + asurf(j)
             ! Atlantic basin cells
             IF (j > jsf .AND. i >= ias(j) .AND. i <= iaf(j)) THEN
                fw_flx(2) = fw_flx(2) + (fwfxneto(i,j) * asurf(j))
                fx_flx(2) = fx_flx(2) + (fx0neto(i,j) * asurf(j))
                t_area(2) = t_area(2) + asurf(j)
                ! Pacific basin cells
             ELSEIF (j > jsf .AND. i >= ips(j) .AND. i <= ipf(j)) THEN
                fw_flx(3) = fw_flx(3) + (fwfxneto(i,j) * asurf(j))
                fx_flx(3) = fx_flx(3) + (fx0neto(i,j) * asurf(j))
                t_area(3) = t_area(3) + asurf(j)
                ! Indian basin cells
             ELSEIF (j > jsf .AND. (i < ips(j) .OR. i > iaf(j))) THEN
                fw_flx(4) = fw_flx(4) + (fwfxneto(i,j) * asurf(j))
                fx_flx(4) = fx_flx(4) + (fx0neto(i,j) * asurf(j))
                t_area(4) = t_area(4) + asurf(j)
                ! Southern basin cells
             ELSE
                fw_flx(5) = fw_flx(5) + (fwfxneto(i,j) * asurf(j))
                fx_flx(5) = fx_flx(5) + (fx0neto(i,j) * asurf(j))
                t_area(5) = t_area(5) + asurf(j)
             END IF
          END IF
       END DO
    END DO

    IF (MOD(istep, itstp) == 0) THEN
       ! Open flux file
       CALL check_unit(41, __LINE__, __FILE__)
       OPEN(41,FILE=outdir_name(1:lenout)//lout//'.'//'flux', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       r_itstp = REAL(itstp)

       ! Write out regional areas (m2) if this is the first timestep
       IF (istep == itstp) THEN
          IF (debug_loop) WRITE (41,120,IOSTAT=ios) 0.0, &
               & t_area(1) / r_itstp, t_area(2) / r_itstp, &
               & t_area(3) / r_itstp, t_area(4) / r_itstp, &
               & t_area(5) / r_itstp, t_area(1) / r_itstp, &
               & t_area(2) / r_itstp, t_area(3) / r_itstp, &
               & t_area(4) / r_itstp, t_area(5) / r_itstp
          CALL check_iostat(ios, __LINE__, __FILE__)
       END IF

       ! Write out freshwater and heat fluxes
       IF (debug_loop) WRITE (41,120,IOSTAT=ios) time, &
            & fw_flx(1) / (1.0E6 * r_itstp), fw_flx(2) / (1.0E6 * r_itstp), &
            & fw_flx(3) / (1.0E6 * r_itstp), fw_flx(4) / (1.0E6 * r_itstp), &
            & fw_flx(5) / (1.0E6 * r_itstp), fx_flx(1) / (1.e15 * r_itstp), &
            & fx_flx(2) / (1.e15 * r_itstp), fx_flx(3) / (1.e15 * r_itstp), &
            & fx_flx(4) / (1.e15 * r_itstp), fx_flx(5) / (1.e15 * r_itstp)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(41,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       fw_flx = 0.0
       fx_flx = 0.0
       t_area = 0.0
    END IF

    ! Averaging of model state
    IF (dosc) THEN
       ! Average the last nyear steps in every ianav steps (if ianav>nyear)
       itv = MOD(istep+nyear-1, ianav)
       IF (itv < nyear) THEN
          ext = conv(MOD(iav, 10))
          IF (istep >= nyear .AND. itv == nyear - 1) THEN
             iout = 1
          ELSE
             iout = 0
          END IF
          CALL diagosc(istep, iout, ext, fx0flux, fwflux, wstress)
       END IF
    END IF

    ! Defunct model output (instantaneous version of averaging above)
    IF (MOD(istep, iwstp) == 0) THEN
       IF (debug_loop) &
            & PRINT *, 'Writing GOLDSTEIN netCDF file at time', istep

       ! Get streamfunction data
       CALL diagopsi(ominp, omaxp, omina, omaxa, opsi, opsia, opsip, iposa)

       CALL ini_netcdf_ocn(istep, 1)
       CALL write_netcdf_ocn(k1, ncdepth1, &
            & opsi, opsia, opsip, ts, u, rho, fx0flux, fwflux, work, &
            & dsc, usc, rsc, saln0, maxi, maxj, maxk, maxl, 1)
       CALL end_netcdf_ocn(1)
       IF (debug_loop) PRINT *
    END IF

    ! Output arguments
    usval_ocn = REAL(u(1,1:maxi,1:maxj,maxk))
    vsval_ocn = REAL(u(2,1:maxi,1:maxj,maxk))
    WHERE (k1(1:maxi,1:maxj) <= maxk)
       tsval_ocn = REAL(ts(1,1:maxi,1:maxj,maxk))
       ssval_ocn = REAL(ts(2,1:maxi,1:maxj,maxk))
       albedo_ocn = REAL(albocn)
    ELSEWHERE
       tsval_ocn = 0.0
       ssval_ocn = 0.0
       albedo_ocn = 0.0
    END WHERE

    ! Set values of dummy variables destined for BIOGEM
    ! NOTE: convert precision between GOLDSTEIn (REAL*8) and GENIE
    !       (which can be REAL*4 or REAL* depending on the IGCM)
    go_ts = REAL(ts(:,1:maxi,1:maxj,1:maxk))
    go_ts1 = REAL(ts1(:,1:maxi,1:maxj,1:maxk))
    go_u = REAL(u(:,1:maxi,1:maxj,1:maxk))
    go_rho = REAL(rho(1:maxi,1:maxj,1:maxk))
    go_cost = REAL(cost(1:maxi,1:maxj))
    go_tau = REAL(tau(:,1:maxi,1:maxj))
    go_mldta = REAL(-5000.0 * mld)
    go_psi = 1592.5 * REAL(psi)

    ! This bit just calculates the diagnostic of the total ocean
    !   thermal energy and water.  It is simply the temperature/density of
    !   each ocean gridbox multiplied by its vertical depth.
    ! The water bit has units of kg, relative to an initial value.
    ! Note the -ve sign in the water part!!
    ! The energy bit has units of J, relative to an initial value.
    IF (MOD(istep, conserv_per) == 0) THEN
       vsc = dphi * rsc * rsc
       tot_energy = 0.0
       tot_water = 0.0
       DO k = 1, maxk
          DO j = 1, maxj
             tot_energy = tot_energy + SUM(ts(1,1:maxi,j,k)) * dz(k) * ds(j)
             tot_water = tot_water - SUM(ts(2,1:maxi,j,k)) * dz(k) * ds(j)
          END DO
       END DO
       ! The m2mm is because internally, goldstein uses m, but genie
       ! uses mm.
       tot_energy = tot_energy * vsc * dsc * rh0sc * cpsc
       tot_water = m2mm * tot_water * vsc * dsc / saln0
       test_energy_ocean = REAL(tot_energy - ini_energy)
       test_water_ocean = REAL(tot_water - ini_water)
       IF (debug_loop) THEN
          PRINT *, 'Goldstein energy diagnostic: ', test_energy_ocean
          PRINT *, 'Goldstein water diagnostic: ', test_water_ocean
       END IF
    END IF
  END SUBROUTINE step_goldstein


  ! Formatting function
  FUNCTION conv(i)
    IMPLICIT NONE
    CHARACTER(LEN=3) :: conv
    INTEGER :: i

    CHARACTER(LEN=1) :: a, b, c
    INTEGER :: i1, i2, i3, itemp

    IF (i < 10) THEN
       a = CHAR(i + 48)
       conv = a // '  '
    ELSE IF (i < 100) THEN
       i1 = i / 10
       i2 = i - i1 * 10
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       conv = a // b // ' '
    ELSE
       i1 = i / 100
       itemp = i - 100 * i1
       i2 = itemp / 10
       i3 = itemp - 10 * i2
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       c = CHAR(i3 + 48)
       conv = a // b // c
    END IF
  END FUNCTION conv


  ! Initialise GOLDSTEIN
  SUBROUTINE initialise_goldstein(olon1, olat1, olon2, olat2, olon3, olat3, &
       & oboxedge1_lon, oboxedge1_lat, oboxedge2_lon, oboxedge2_lat, &
       & oboxedge3_lon, oboxedge3_lat, depth, depth1, &
       & ilandmask1, ilandmask2, ilandmask3, totsteps, &
       & tstar_ocn, sstar_ocn, ustar_ocn, vstar_ocn, albedo_ocn, &
       & ias_out, iaf_out, ips_out, ipf_out, jsf_out, lrestart_genie, &
       & go_saln0, go_rhoair, go_cd, go_ds, go_dphi, go_ips, go_ipf, &
       & go_usc, go_dsc, go_fsc, go_rh0sc, go_rhosc, go_cpsc, go_scf, &
       & go_k1, go_dz, go_dza, go_ias, go_iaf, go_jsf, go_c, go_cv, &
       & go_s, go_sv, go_ts, go_ts1, go_rsc, go_syr, go_nyear, go_lin, &
       & go_ec, go_istep0)
    USE genie_util, ONLY: check_unit, check_iostat, message, die
    USE genie_control, ONLY: &
         & dim_GOLDSTEINNLONS, dim_GOLDSTEINNLATS, dim_GOLDSTEINNLEVS, &
         & dim_GOLDSTEINNTRACS

    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(OUT) :: olon1, olon2, olon3
    REAL, DIMENSION(:), INTENT(OUT) :: olat1, olat2, olat3
    REAL, DIMENSION(:), INTENT(OUT) :: &
         & oboxedge1_lon, oboxedge2_lon, oboxedge3_lon
    REAL, DIMENSION(:), INTENT(OUT) :: &
         & oboxedge1_lat, oboxedge2_lat, oboxedge3_lat
    REAL, DIMENSION(:), INTENT(OUT) :: depth, depth1
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: &
         & ilandmask1, ilandmask2, ilandmask3
    INTEGER(KIND=8), INTENT(IN) :: totsteps
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & tstar_ocn, sstar_ocn, ustar_ocn, vstar_ocn, albedo_ocn
    INTEGER, DIMENSION(:), INTENT(OUT) :: ias_out, iaf_out, ips_out, ipf_out
    INTEGER, INTENT(OUT) :: jsf_out
    LOGICAL, INTENT(IN) :: lrestart_genie
    REAL, DIMENSION(:), INTENT(OUT) :: go_ds
    REAL, INTENT(OUT) :: go_saln0, go_rhoair, go_cd, go_dphi
    INTEGER, DIMENSION(:), INTENT(OUT) :: go_ips, go_ipf
    REAL, INTENT(OUT) :: &
         & go_usc, go_dsc, go_fsc, go_rh0sc, go_rhosc, go_cpsc, go_scf
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: go_k1
    REAL, DIMENSION(:), INTENT(OUT) :: go_dz, go_dza
    INTEGER, DIMENSION(:), INTENT(OUT) :: go_ias, go_iaf
    INTEGER, INTENT(OUT) :: go_jsf
    REAL, DIMENSION(:), INTENT(OUT) :: go_c, go_cv, go_s, go_sv
    REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: go_ts, go_ts1
    REAL, INTENT(OUT) :: go_rsc, go_syr
    INTEGER, INTENT(OUT) :: go_nyear
    CHARACTER(LEN=13), INTENT(OUT) :: go_lin
    REAL, INTENT(OUT) :: go_ec(5)
    INTEGER, INTENT(OUT) :: go_istep0

    INTEGER :: lenrst
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: bmask
    INTEGER :: i, j, k, l, kmxdrg, jeb, isol, isl, kk
    REAL :: phix, th0, th1, z1, tv, tv1, tv2, tv3, tv4, tv5
    REAL :: temp0, temp1, adrag, drgf, s0, s1
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: h
    REAL :: theta, thv, dth, dscon, deg_to_rad

    ! Extras for hosing.
    REAL :: area_hosing, syr
    INTEGER :: nyears_hosing, j_hosing(2)

    ! Extra for freshwater flux anomalies.
    REAL, DIMENSION(:,:), ALLOCATABLE :: fw_anom_in
    CHARACTER(LEN=13) :: lin
    CHARACTER ans, fwanomin

    ! Grid type
    INTEGER :: igrid

    CHARACTER(LEN=6) :: world
    CHARACTER(LEN=3) :: cmip_model
    INTEGER :: lenworld, lencmip_model

    ! ssmax parameters
    REAL :: ssmaxmid, ssmaxdiff, ssmaxtanhefold, ssmaxtanh0dep, zssmax

    INTEGER :: ios, status
    LOGICAL :: ioex
    CHARACTER(LEN=200) :: msgStr

    INTEGER, EXTERNAL :: lnsig1

    ! Namelist for reading initialisation data
    NAMELIST /ini_gold_nml/ indir_name, outdir_name, rstdir_name
    NAMELIST /ini_gold_nml/ igrid, world
    NAMELIST /ini_gold_nml/ npstp, iwstp, itstp, ianav
    NAMELIST /ini_gold_nml/ conserv_per, ans, yearlen, nyear
    NAMELIST /ini_gold_nml/ temp0, temp1, rel, scf, diff, adrag
    NAMELIST /ini_gold_nml/ hosing, hosing_trend, nyears_hosing
    NAMELIST /ini_gold_nml/ fwanomin, cmip_model, albocn
    NAMELIST /ini_gold_nml/ gust, ene_tune, iconv
    NAMELIST /ini_gold_nml/ imld, mldpebuoycoeff, mldketaucoeff
    NAMELIST /ini_gold_nml/ mldwindkedec
    NAMELIST /ini_gold_nml/ iediff, ediff0, ediffpow1, ediffpow2
    NAMELIST /ini_gold_nml/ ediffvar
    NAMELIST /ini_gold_nml/ ieos
    NAMELIST /ini_gold_nml/ ssmaxsurf, ssmaxdeep
    NAMELIST /ini_gold_nml/ tdatafile, sdatafile
    NAMELIST /ini_gold_nml/ tdata_varname, sdata_varname
    NAMELIST /ini_gold_nml/ tdata_missing, sdata_missing
    NAMELIST /ini_gold_nml/ tdata_scaling, sdata_scaling
    NAMELIST /ini_gold_nml/ tdata_offset, sdata_offset
    NAMELIST /ini_gold_nml/ tsinterp, lout
    NAMELIST /ini_gold_nml/ filenetin, dirnetout
    NAMELIST /ini_gold_nml/ dosc, diso, debug_init, debug_end, debug_loop
    NAMELIST /ini_gold_nml/ ctrl_diagend
    NAMELIST /ini_gold_nml/ saln0
    NAMELIST /ini_gold_nml/ rst_reset_T

    j_hosing = 0

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising GOLDSTEIN ocean module ...'

    maxi = dim_GOLDSTEINNLONS
    maxj = dim_GOLDSTEINNLATS
    maxk = dim_GOLDSTEINNLEVS
    maxl = dim_GOLDSTEINNTRACS
    mpxi = maxi
    mpxj = maxj + 1
    mpi = 2 * (maxi + maxj)

    IF (debug_init) PRINT *

    ! read DATA (i.e. namelist) file
    CALL check_unit(56, __LINE__, __FILE__)
    OPEN(unit=56,FILE='data_GOLD',STATUS='old',IOSTAT=ios)
    IF (ios /= 0) THEN
       CALL die('could not open GOLDSTEIN namelist file', __LINE__, __FILE__)
    END IF

    ! read in namelist
    READ (UNIT=56,NML=ini_gold_nml,IOSTAT=ios)
    IF (ios /= 0) THEN
       CALL die('could not read GOLDSTEIN namelist', __LINE__, __FILE__)
    ELSE
       CLOSE(56,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
    END IF

    ALLOCATE(k1(0:maxi+1,0:maxj+1),STAT=status) ; k1 = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ku(2,maxi,maxj),STAT=status)       ; ku = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mk(maxi+1,maxj),STAT=status)       ; mk = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ips(maxj),STAT=status)             ; ips = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ipf(maxj),STAT=status)             ; ipf = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ias(maxj),STAT=status)             ; ias = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(iaf(maxj),STAT=status)             ; iaf = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(icosd(maxi,maxj),STAT=status)      ; icosd = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mldk(maxi,maxj),STAT=status)       ; mldk = 0
    IF (status /= 0) CALL die("Could not allocate memory")

    ALLOCATE(dt(maxk),STAT=status)                               ; dt = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ds(maxj),STAT=status)                               ; ds = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dsv(1:maxj-1),STAT=status)                          ; dsv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rds2(2:maxj-1),STAT=status)                         ; rds2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dz(maxk),STAT=status)                               ; dz = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(u(3,0:maxi,0:maxj,maxk),STAT=status)                ; u = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ts(maxl,0:maxi+1,0:maxj+1,0:maxk+1),STAT=status)    ; ts = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(s(0:maxj),STAT=status)                              ; s = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(c(0:maxj),STAT=status)                              ; c = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dzu(2,maxk),STAT=status)                            ; dzu = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(tau(2,maxi,maxj),STAT=status)                       ; tau = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(drag(2,maxi+1,maxj),STAT=status)                    ; drag = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dztau(2,maxi,maxj),STAT=status)                     ; dztau = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ratm(mpxi*mpxj,mpxi+1),STAT=status)                 ; ratm = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ub(2,0:maxi+1,0:maxj),STAT=status)                  ; ub = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rho(0:maxi+1,0:maxj+1,0:maxk),STAT=status)          ; rho = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ts1(maxl,0:maxi+1,0:maxj+1,0:maxk+1),STAT=status)   ; ts1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(sv(0:maxj),STAT=status)                             ; sv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(cv(0:maxj),STAT=status)                             ; cv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dza(maxk),STAT=status)                              ; dza = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dztav(2,maxi,maxj),STAT=status)                     ; dztav = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(gb(mpxi*mpxj),STAT=status)                          ; gb = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(gap(mpxi*mpxj,2*mpxi+3),STAT=status)                ; gap = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(cost(maxi,maxj),STAT=status)                        ; cost = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rh(3,0:maxi+1,0:maxj+1),STAT=status)                ; rh = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(gbold(mpxi*mpxj),STAT=status)                       ; gbold = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(tau0(maxi,maxj),STAT=status)                        ; tau0 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dztav0(maxi,maxj),STAT=status)                      ; dztav0 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(tau1(maxi,maxj),STAT=status)                        ; tau1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dztav1(maxi,maxj),STAT=status)                      ; dztav1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(tsa0(maxj),STAT=status)                             ; tsa0 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fw_hosing(maxi,maxj),STAT=status)                   ; fw_hosing = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rhosing(maxi,maxj),STAT=status)                     ; rhosing = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(zro(maxk),STAT=status)                              ; zro = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(zw(0:maxk),STAT=status)                             ; zw = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dzg(maxk,maxk),STAT=status)                         ; dzg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(z2dzg(maxk,maxk),STAT=status)                       ; z2dzg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rdzg(maxk,maxk),STAT=status)                        ; rdzg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fw_anom(maxi,maxj),STAT=status)                     ; fw_anom = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fw_anom_rate(maxi,maxj),STAT=status)                ; fw_anom_rate = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(psi(0:maxi,0:maxj),STAT=status)                     ; psi = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(u1(3,0:maxi,0:maxj,maxk),STAT=status)               ; u1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rc(0:maxj),STAT=status)                             ; rc = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rc2(0:maxj),STAT=status)                            ; rc2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rtv(maxi,maxj),STAT=status)                         ; rtv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rtv3(maxi,maxj),STAT=status)                        ; rtv3 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rcv(1:maxj-1),STAT=status)                          ; rcv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rdsv(1:maxj-1),STAT=status)                         ; rdsv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(cv2(1:maxj-1),STAT=status)                          ; cv2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rds(maxj),STAT=status)                              ; rds = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rdz(maxk),STAT=status)                              ; rdz = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rdza(maxk),STAT=status)                             ; rdza = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(bp(maxi+1,maxj,maxk),STAT=status)                   ; bp = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(sbp(maxi+1,maxj),STAT=status)                       ; sbp = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(asurf(maxj),STAT=status)                            ; asurf = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(tsavg(maxl,0:maxi+1,0:maxj+1,0:maxk+1),STAT=status) ; tsavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(uavg(3,0:maxi,0:maxj,maxk),STAT=status)             ; uavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rhoavg(0:maxi+1,0:maxj+1,0:maxk),STAT=status)       ; rhoavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fx0avg(5,maxi,maxj),STAT=status)                    ; fx0avg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fwavg(4,maxi,maxj),STAT=status)                     ; fwavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(windavg(4,maxi,maxj),STAT=status)                   ; windavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ts_store(maxl,maxi,maxj,maxk),STAT=status)          ; ts_store = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(albcl(maxi,maxj),STAT=status)                       ; albcl = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(evap_save1(maxi,maxj),STAT=status)                  ; evap_save1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(late_save1(maxi,maxj),STAT=status)                  ; late_save1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(sens_save1(maxi,maxj),STAT=status)                  ; sens_save1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(evap_save2(maxi,maxj),STAT=status)                  ; evap_save2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(late_save2(maxi,maxj),STAT=status)                  ; late_save2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(sens_save2(maxi,maxj),STAT=status)                  ; sens_save2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mldpebuoy(maxi,maxj),STAT=status)                   ; mldpebuoy = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mldpeconv(maxi,maxj),STAT=status)                   ; mldpeconv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mldpelayer1(maxi,maxj),STAT=status)                 ; mldpelayer1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mldketau(maxi,maxj),STAT=status)                    ; mldketau = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mldemix(maxi,maxj),STAT=status)                     ; mldemix = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mld(maxi,maxj),STAT=status)                         ; mld = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(mlddec(maxk), mlddecd(maxk),STAT=status)            ; mlddec = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ediff1(maxi,maxj,maxk-1),STAT=status)               ; ediff1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(diffmax(maxk),STAT=status)                          ; diffmax = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ssmax(maxk-1),STAT=status)                          ; ssmax = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    ALLOCATE(getj(maxi,maxj),STAT=status) ; getj = .FALSE.
    IF (status /= 0) CALL die("Could not allocate memory")

    ALLOCATE(nclon1(maxi),STAT=status)     ; nclon1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclon2(maxi),STAT=status)     ; nclon2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclon3(maxi),STAT=status)     ; nclon3 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclat1(maxj),STAT=status)     ; nclat1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclat2(maxj),STAT=status)     ; nclat2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclat3(maxj),STAT=status)     ; nclat3 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ncdepth(maxk),STAT=status)    ; ncdepth = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ncdepth1(maxk+1),STAT=status) ; ncdepth1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    ! Local allocations
    ALLOCATE(bmask(maxi,maxj),STAT=status)       ; bmask = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(h(3,0:maxi+1,0:maxj+1),STAT=status) ; h = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fw_anom_in(maxi,maxj),STAT=status)  ; fw_anom_in = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    ! syr re-defined here
    syr = yearlen * 86400

    ! Directory names
    lenin = lnsig1(indir_name)
    IF (indir_name(lenin:lenin) /= '/') THEN
       lenin = lenin + 1
       indir_name(lenin:lenin) = '/'
    END IF
    lenout = lnsig1(outdir_name)
    IF (outdir_name(lenout:lenout) /= '/') THEN
       lenout = lenout + 1
       outdir_name(lenout:lenout+1) = '/'
    END IF
    lenrst = lnsig1(rstdir_name)
    IF (rstdir_name(lenrst:lenrst) /= '/') THEN
       lenrst = lenrst + 1
       rstdir_name(lenrst:lenrst+1) = '/'
    END IF
    lenworld = lnsig1(world)

    ! Time-steps information
    nsteps = totsteps
    IF (debug_init) THEN
       PRINT *, 'syr = ', syr
       PRINT *, 'Input dir. name ', indir_name(1:lenin)
       PRINT *, 'Output dir. name ', outdir_name(1:lenout)
       PRINT *, 'Restart dir. name ', rstdir_name(1:lenrst)
       PRINT *, 'Topography name ', world(1:lenworld)
       PRINT *, 'nsteps npstp iwstp itstp ianav'
       PRINT *, nsteps, npstp, iwstp, itstp, ianav
       PRINT *, 'period of water and energy checks:'
       PRINT *, conserv_per
       PRINT *, 'new or continuing run ?'
       PRINT *, ans
       PRINT *, 'number of days per GOLDSTEIN year'
       PRINT *, yearlen
       PRINT *, 'seasonality enabled =', dosc
       PRINT *, 'CALL diagend? =', ctrl_diagend
       PRINT *, 'Reference salinity =', saln0
    END IF

    ! EMBM scaling for freshwater forcing of ocean
    rpmesco = rsc * saln0 / (dsc * usc)

    ! Parameters for setting up grid
    ! th is latitude, coords are sin(th), longitude phi, and z
    th0 = -pi / 2
    th1 = pi / 2
    s0 = SIN(th0)
    s1 = SIN(th1)
    phix = 2 * pi
    deg_to_rad = pi / 180.0

    ! Grid dimensions must be no greater than array dimensions in var.cmn
    dphi = phix / maxi
    rdphi = 1.0 / dphi

    ! Set up horizontal grid: sin and cos factors at rho and v points
    ! (c grid) fix for global domain although only cv and cv2 are
    ! referred to at or beyond limits 24/6/2 if no flow out of N + S
    ! boundaries.
    sv(0) = s0
    cv(0) = COS(th0)
    IF (igrid == 1) THEN
       ! Set up const dlat grid
       dth = (th1 - th0) / maxj
       DO j = 1, maxj
          thv = th0 + j * dth
          theta = thv - 0.5 * dth
          sv(j) = SIN(thv)
          s(j) = SIN(theta)
          cv(j) = COS(thv)
       END DO
    ELSEIF (igrid == 0) THEN
       ! Set up const dsinlat grid
       dscon = (s1 - s0) / maxj
       DO j = 1, maxj
          sv(j) = s0 + j * dscon
          cv(j) = SQRT(1 - sv(j) * sv(j))
          s(j) = sv(j) - 0.5 * dscon
       END DO
    END IF
    IF (debug_init) THEN
       PRINT *, 'GOLDSTEIN latitudes: velocity; tracers'
       PRINT *, 'j, 180/pi*ASIN(sv(j)), 180/pi*ASIN(s(j))'
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

    ! Area of grid cell (assumes sine(lat) grid)
    DO j = 1, maxj
       asurf(j) = rsc * rsc * ds(j) * dphi
       IF (debug_init) &
            & PRINT *, 'j = ', j, 'GOLDSTEIN grid cell area is', asurf(j), 'm2'
    END DO

    ! v2 seasonality
    IF (debug_init) PRINT *, 'timesteps per year'
    IF (debug_init) PRINT *, nyear
    tv = 86400.0 * yearlen / (nyear * tsc)

    ! Variable timestep option not recommended
    dt = tv
    dzu = 0
    IF (debug_init) PRINT *, 'dimensional ocean timestep', tv * tsc / 86400

    ! Set up grid
    ! For variable (exponential) dz use ez0 > 0, ELSE use ez0 < 0
    ez0 = 0.1
    z1 = ez0 * ((1.0 + 1 / ez0)**(1.0 / maxk) - 1.0)
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

    ! 2D "depth" grids of difference between each pair of levels
    ! or difference between squares (needed for PE calculation)
    DO k = maxk, 1, -1
       DO kk = maxk, 1, -1
          dzg(k,kk) = zw(k) - zw(kk-1)
          z2dzg(k,kk) = -zw(k) * zw(k) + zw(kk-1) * zw(kk-1)
          IF (k /= kk - 1) THEN
             rdzg(k,kk) = 1.0 / dzg(k,kk)
          ELSE
             ! This number should be inifinite. Large number used instead
             rdzg(k,kk) = 1.0E10
          END IF
       END DO
    END DO

    ! Write dimensional vertical grid
    IF (debug_init) THEN
       PRINT *, 'layer #, layer top, layer mid, layer thick'
       WRITE (6,'(i4,3e12.4)') &
            & (k, dsc * zw(k), dsc * zro(k), dsc * dz(k), k = maxk, 1, -1)
    END IF

    dzz = dz(maxk) * dza(maxk-1) / 2

    ! when max/min overturning are calculated for the OPSIT file, they
    ! are usually full water column and include surface, wind-driven
    ! circulation

    ! here, a variable is set to control the level below which max/min
    ! overturning is calculated - currently it is set for overturning
    ! values on levels > 500 m deep
    overdep = 8
    DO k = maxk, 1, -1
       tv1 = dsc * zw(k)
       IF (tv1 > -500.0) overdep = k - 1
    END DO
    IF (debug_init) &
         & PRINT *, 'depth levels for OPSIT max/min overturning : 1 to', overdep

    ! efficiency array
    DO k = 1, maxk-1
       rdz(k) = 1.0 / dz(k)
       rdza(k) = 1.0 / dza(k)
    END DO
    rdz(maxk) = 1.0 / dz(maxk)

    ! dza(maxk) never referenced, set to 0 for Andy's biogeo-code
    dza(maxk) = 0.0

    ! Set up coeffs for state equation following WS 1993
    ec(1) = -0.0559 / rhosc
    ec(2) = 0.7968 / rhosc
    ec(3) = -0.0063 / rhosc
    ec(4) = 3.7315E-5 / rhosc
    ! Thermobaricity (T*z) term added as option. Optimised for -1<deep
    ! T<6, S=34.9, but is an order of magnitude improvement even in
    ! other parts of parameter space. It does not change surface
    ! densities which are fine anyway.
    IF (ieos == 1) THEN
       ec(5) = 2.5E-5 * dsc / rhosc
    ELSE
       ec(5) = 0.0
    END IF
    go_ec = REAL(ec)

    IF (debug_init) THEN
       PRINT *, 'temp0, temp1, rel, scf'
       PRINT *, temp0, temp1, rel, scf
       PRINT *, 'diff(1), diff(2)'
       PRINT *, diff(1), diff(2)
       PRINT *, 'inverse minimum drag in days'
       PRINT *, adrag
       PRINT *, 'initial hosing =', hosing
       PRINT *, 'hosing_trend =', hosing_trend
       PRINT *, 'nyears_hosing =', nyears_hosing
    END IF

    ! convert hosing_trend from Sv/ky to Sv/s
    hosing_trend = hosing_trend / (1.0E3 * syr)

    ! convert hosing_period from yr to ocean timesteps
    nsteps_hosing = nyears_hosing * nyear

    ! Parameters for (restricted) time-dependent forcing
    ! set sda1 < 1e05 for steady forcing
    sda1 = 0.0
    sdomg = 2 * pi / 10.0

    ! Seabed depth h needed BEFORE forcing if coastlines are non-trivial
    ! note k1(i,j) must be periodic ; k1(0,j) - k1(maxi,j) = 0 and
    ! k1(1,j) - k1(maxi+1,j) = 0
    ntot = 0
    intot = 0

    IF (debug_init) PRINT *, 'ocean bathymetry being read in'
    CALL check_unit(13, __LINE__, __FILE__)
    OPEN(13,FILE=indir_name(1:lenin)//world//'.k1',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    ! note k1(i,j) must be periodic ; k1(0,j) - k1(maxi,j) = 0 and
    ! k1(1,j) - k1(maxi+1,j) = 0, as enforced below;

    DO j = maxj+1, 0, -1
       READ (13,*,IOSTAT=ios) (k1(i,j), i = 0, maxi+1)
       CALL check_iostat(ios, __LINE__, __FILE__)
       k1(0,j) = k1(maxi,j)
       k1(maxi+1,j) = k1(1,j)
       ! boundary condition
       h(:,:,j) = 0
       rh(:,:,j) = 0
       IF (debug_init) WRITE (6,'(i4,66i3)')j, (k1(i, j), i = 0, maxi+1)
    END DO

    ! read ips etc if possible
    CLOSE(13,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! count wet cells
    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) <= maxk) THEN
             ntot = ntot + maxk - k1(i,j) + 1
             intot = intot + maxk - k1(i,j)
          END IF
       END DO
    END DO

    INQUIRE(FILE=indir_name(1:lenin)//world//'.bmask',EXIST=ioex)
    IF (ioex) THEN
       IF (debug_init) PRINT *, 'reading in basin mask file'
       CALL check_unit(13, __LINE__, __FILE__)
       OPEN(13,FILE=indir_name(1:lenin)//world//'.bmask',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       DO j = maxj, 1, -1
          READ (13,*,IOSTAT=ios) (bmask(i, j), i = 1, maxi)
          CALL check_iostat(ios, __LINE__, __FILE__)
          IF (debug_init) WRITE (6,'(i4,66i3)')j, (bmask(i,j), i = 1, maxi)
       END DO
       ! basin mask: 0: land
       !             2: Pacific
       !             3: Atlantic
       !             1: remaining ocean grid cells
       ! for now, derive ias(:), iaf(:), ips(:), ipf(:) north of j=jsf from
       ! the basin mask, jsf is set to be the first grid row south of the
       ! southern extent of the Atlantic basin
       ips = 0
       ipf = 0
       ias = 0
       iaf = 0
       jsf = 1
       DO j = 1, maxj
          DO i = 1, maxi
             IF (ips(j) == 0 .AND. bmask(i,j) == 2) ips(j) = i
             IF (bmask(i,j) == 2) ipf(j) = i
             IF (ias(j) == 0 .AND. bmask(i,j) == 3) ias(j) = i
             IF (bmask(i,j) == 3) iaf(j) = i
             IF (jsf == 1 .AND. bmask(i,j) == 3) jsf = j-1
          END DO
          IF (ips(j) == 0) ips(j) = 1
          IF (ias(j) == 0) ias(j) = 1
       END DO
       CLOSE(13,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
    ELSE
       IF (debug_init) &
            & PRINT *, 'no basin mask available - trying to detect ' // &
            & 'Atlantic and Pacific basins from land mask'
       WHERE (k1(1:maxi,1:maxj) <= maxk)
          bmask = 1
       ELSEWHERE
          bmask = 0
       END WHERE

       ! Find ocean positions semi-automatically, must start with a
       ! longitude i which is in the right ocean for all j, tricky in north
       ias = 0
       iaf = 0
       ips = 0
       ipf = 0
       ias(maxj) = NINT(maxi * 24.0 / 36.0)
       ips(maxj) = NINT(maxi * 10.0 / 36.0)
       jsf = 1
       ! For 64x32l grid (15/9/05):
       IF (igrid /= 0) THEN
          ias(maxj) = 61
          ips(maxj) = 36
          jsf = 10
       END IF
       IF (debug_init) PRINT *, 'ips ipf ias iaf '
       DO j = 1, maxj
          ips(j) = ips(maxj)
          ipf(j) = ips(j)
          ias(j) = ias(maxj)
          iaf(j) = ias(j)
          ! This bit to get the Southern tip of Greenland into the Atlantic
          IF (j > NINT(maxj * 34.0 / 36.0) .AND. &
               & j <= NINT(maxj * 35.0 / 36.0)) THEN
             ias(j) = NINT(maxi * 20.0 / 36.0)
          END IF
          DO i = 1, maxi
             IF (k1(ips(j)-1,j) <= maxk) ips(j) = ips(j) - 1
             IF (k1(ipf(j)+1,j) <= maxk) ipf(j) = ipf(j) + 1
             IF (k1(ias(j)-1,j) <= maxk) ias(j) = ias(j) - 1
             IF (k1(iaf(j)+1,j) <= maxk) iaf(j) = iaf(j) + 1
             ips(j) = 1 + MOD(ips(j)-1+maxi, maxi)
             ipf(j) = 1 + MOD(ipf(j)-1+maxi, maxi)
             ias(j) = 1 + MOD(ias(j)-1+maxi, maxi)
             iaf(j) = 1 + MOD(iaf(j)-1+maxi, maxi)
          END DO
          IF (igrid == 0) THEN
             IF (ias(j) >= iaf(j) .AND. j <= maxj / 2) jsf = j
             IF (ips(j) >= ipf(j) .AND. j <= maxj / 2) jsf = j
          END IF
       END DO

       IF (igrid == 0) THEN
          ! This bit to get the Arctic all Atlantic
          DO j = 1, maxj
             IF (j > NINT(maxj * 35.0 / 36.0)) THEN
                ips(j) = 1
                ipf(j) = 0
                ias(j) = 1
                iaf(j) = maxi
             END IF
             ! This bit to get the Southern tip of Greenland out of the Pacific
             IF (j > NINT(maxj * 34.0 / 36.0) .AND. &
                  & j <= NINT(maxj * 35.0 / 36.0)) THEN
                ips(j) = 1
                ipf(j) = 0
             END IF
          END DO
       END IF

       IF (igrid /= 0) THEN
          ips(maxj) = 1
          ipf(maxj) = 0
          ips(maxj-1) = 1
          ipf(maxj-1) = 0
          ias(maxj) = 1
          iaf(maxj) = maxi
       END IF
    END IF

    IF (debug_init) PRINT *, 'jsf ', jsf
    IF (debug_init) WRITE (6,'(5i4)') &
         & (j, ips(j), ipf(j), ias(j), iaf(j), j = maxj, 1, -1)

    DO j = 1, maxj
       ips_out(j) = ips(j)
       ipf_out(j) = ipf(j)
       ias_out(j) = ias(j)
       iaf_out(j) = iaf(j)
    END DO
    jsf_out = jsf

    ! Freshwater hosing: Initialise the denominators which are used to
    ! convert the total area-integrated freshwater-hosing flux in units of
    ! Sv into freshwater fluxes in units of m/s in the grid cells in a
    ! latitudinal band in the high-latitude Atlantic

    ! Find the region where the freshwater hosing will be applied by
    ! selecting grid cells in the northern Atlantic which have at least 50%
    ! of their surface area in the latitudinal band between 50N and 70N
    tv1 = SIN(50.0 * pi / 180.0)
    tv2 = SIN(70.0 * pi / 180.0)
    DO j = 1, maxj
       ! southern boundary
       IF (tv1 >= sv(j-1) .AND. tv1 <= sv(j)) THEN
          ! at least half of box area has to be in latitudinal band
          IF (((sv(j) - tv1) / ds(j)) >= 0.5) THEN
             j_hosing(1) = j
          ELSE
             j_hosing(1) = j + 1
          END IF
       END IF
       ! northern boundary
       IF (tv2 >= sv(j-1) .AND. tv2 <= sv(j)) THEN
          ! at least half of box area has to be in latitudinal band
          IF (((tv2 - sv(j-1)) / ds(j)) >= 0.5) THEN
             j_hosing(2) = j
          ELSE
             j_hosing(2) = j - 1
          END IF
       END IF
    END DO

    rhosing = 0.0
    area_hosing = 0.0
    DO j = j_hosing(1), j_hosing(2)
       DO i = ias(j), iaf(j)
          IF (k1(i,j) <= maxk) area_hosing  = area_hosing + asurf(j)
       END DO
    END DO
    DO j = j_hosing(1), j_hosing(2)
       DO i = ias(j), iaf(j)
          IF (k1(i,j) <= maxk) rhosing(i,j)  = 1e6 / area_hosing
       END DO
    END DO

    IF (igrid == 2) THEN
       PRINT *, 'rhosing(i,j) for grid 2:'
       DO i = 1, maxi
          DO j = 1, maxj
             PRINT *, i, j, rhosing(i,j)
          END DO
       END DO
    END IF

    ! FW flux anomalies to be used?
    IF (debug_init) PRINT *, 'Apply FW flux anomalies ?'
    IF (debug_init) PRINT *, fwanomin

    ! Read in FW flux anomaly filename, from CMIP/PMIP OAGCM (HadCM3 by default)
    lencmip_model = lnsig1(cmip_model)
    IF (debug_init) PRINT *, 'CMIP model ', cmip_model(1:lencmip_model)

    ! initialise FW flux anomalies
    fw_anom = 0.0
    IF (fwanomin == 'y' .OR. fwanomin == 'Y') THEN
       ! Freshwater flux anomalies
       CALL check_unit(13, __LINE__, __FILE__)
       OPEN(23,FILE=indir_name(1:lenin)//cmip_model//'.fwanom', &
            & STATUS='old',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! get anomalies in units m/y
       DO j = 1, maxj
          DO i = 1, maxi
             READ (23,100,IOSTAT=ios) fw_anom_in(i,j)
             CALL check_iostat(ios, __LINE__, __FILE__)
          END DO
       END DO
100    FORMAT(e14.7)
       CLOSE(23,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! convert units to mm/s
       fw_anom_in = m2mm * fw_anom_in / syr
       ! get linear rate of change of fw flux anomalies in mm/s/s
       fw_anom_rate = fw_anom_in / (130.0 * syr)
    ELSE
       fw_anom_rate = 0.0
    END IF

    ! initialize psi
    psi = 0.0
    ub = 0

    ! seabed depth h
    DO j = maxj+1, 0, -1
       DO i = 0, maxi+1
          IF (k1(i,j) <= maxk) THEN
             DO k = k1(i,j), maxk
                h(3,i,j) = h(3,i,j) + dz(k)
             END DO
             rh(3,i,j) = 1.0 / h(3,i,j)
          END IF
       END DO
    END DO

    DO j = 0, maxj+1
       DO i = 0, maxi
          h(1,i,j) = MIN(h(3,i,j), h(3,i+1,j))
          IF (max(k1(i,j), k1(i+1,j)) <= maxk) rh(1,i,j) = 1.0 / h(1,i,j)
       END DO
    END DO

    DO j = 0, maxj
       DO i = 0, maxi+1
          h(2,i,j) = MIN(h(3,i,j), h(3,i,j+1))
          IF (max(k1(i,j), k1(i,j+1)) <= maxk) rh(2,i,j) = 1.0 / h(2,i,j)
       END DO
    END DO

    DO j = 1, maxj
       DO i = 1, maxi
          ku(1,i,j) = MAX(k1(i,j), k1(i+1,j))
          ku(2,i,j) = MAX(k1(i,j), k1(i,j+1))
       END DO
    END DO
    tv2 = 0

    ! set up drag and diffusion values
    ! drag takes the value adrag in the interior, rising twice by factor
    ! drgf per gridpoint close to equator and in regions of
    ! shallow water (k1>kmxdrg) ie land in the case kmxdrg=maxk
    ! jeb = 1/2 width of equatorial region of maximum drag

    adrag = 1.0 / (adrag * 86400 * fsc)
    ! cross equator need * 4 if drag is constant ie if drgf=1
    drgf = 3.0
    kmxdrg = maxk / 2
    jeb = 1
    CALL drgset(adrag, drgf, kmxdrg, jeb)
    diff(1) = diff(1) / (rsc * usc)
    diff(2) = diff(2) * rsc / (usc * dsc * dsc)

    ! arrays for efficiency
    DO j = 1, maxj
       DO i = 1, maxi
          rtv(i,j) = 1.0 / (s(j) * s(j) + drag(1,i,j) * drag(1,i,j))
          rtv3(i,j) = 1.0 / (sv(j) * sv(j) + drag(2,i,j) * drag(2,i,j))
       END DO
    END DO

    IF (debug_init) PRINT *, 'dphi ds diff(1) diff(2)'
    IF (debug_init) PRINT *, dphi, ds(1), diff(1), diff(2)

    ! initialize some arrays to zero
    u = 0
    u1 = 0

    IF (dosc) THEN
       ! v2 seasonal. Annual averages
       tsavg(:,1:maxi,1:maxj,1:maxk) = 0.0
       uavg(:,1:maxi,1:maxj,:) = 0.0
       rhoavg(1:maxi,1:maxj,1:maxk) = 0.0
       fx0avg = 0.0
       fwavg = 0.0
    END IF

    ! initial conditions
    DO i = 0, maxi+1
       DO j = 0, maxj+1
          DO k = 0, maxk+1
             ! initial uniform temperature T0 large favours thermally
             ! direct solutions
             IF (j <= maxj / 2) THEN
                ts(1,i,j,k) = temp0 * 0.5 * (1 + SIGN(1, k-k1(i,j)))
             ELSE
                ts(1,i,j,k) = temp1 * 0.5 * (1 + SIGN(1, k-k1(i,j)))
             END IF
             ! initial salinity
             ts(2,i,j,k) =  0.0
             ts1(1,i,j,k) = ts(1,i,j,k)
             ts1(2,i,j,k) = ts(2,i,j,k)
          END DO
          DO k = 1, maxk
             CALL eos(ec, ts(1,i,j,k), ts(2,i,j,k), zro(k), ieos, rho(i,j,k))
          END DO
       END DO
    END DO

    ! forcing fields and some more initialisation
    cost = 0
    icosd = 0
    rho(1:maxi,1:maxj,0) = 0

    ! array to determine limit of easy part of double p integral in J term
    ! use INTEGER wetpoint indicator
    ! (1+SIGN(1,maxk-k1(i,j)))/2
    ! mk is largest of surrounding wet k1 values if i,j is wet, ELSE 0
    DO j = 1, maxj
       DO i = 1, maxi
          mk(i,j) = MAX( &
               & k1(i,j) * (1+SIGN(1, maxk - k1(i,j))) / 2, &
               & k1(i+1,j) * (1+SIGN(1, maxk - k1(i+1,j))) / 2, &
               & k1(i-1,j) * (1+SIGN(1, maxk - k1(i-1,j))) / 2, &
               & k1(i,j+1) * (1+SIGN(1, maxk - k1(i,j+1))) / 2, &
               & k1(i,j-1) * (1+SIGN(1, maxk - k1(i,j-1))) / 2)
          mk(i,j) = mk(i,j) * (1+SIGN(1, maxk - k1(i,j))) / 2
       END DO
    END DO

    ! initialize bp, only strictly needed at k=k1
    bp = 0.0

    ! periodic b.c. required for Antarctic island integral
    mk(maxi+1,1:maxj) = mk(1,1:maxj)

    ! array to avoid J term in flat regions
    ! For non-trivial coasts essential to avoid adding J term at non-Psi points.
    ! Hence first condition ensures (i,j) is a wet Psi point, 2nd that bottom
    ! is not flat.
    DO j = 1, maxj
       DO i = 1, maxi
          getj(i,j) = (MAX(k1(i,j), k1(i+1,j), &
               &           k1(i,j+1), k1(i+1,j+1)) <= maxk) .AND. &
               &      (k1(i,j) /= k1(i,j+1) .OR. &
               &       k1(i,j) /= k1(i+1,j) .OR. &
               &       k1(i,j) /= k1(i+1,j+1))
       END DO
    END DO

    ! read island geometry file or write out for manual editing
    ! setting wet to zero and 1 on 1st landmass, 2 on 2nd landmass (1st island)
    ! etc nb narrow channels may have no wet psi points and hence not show up on
    ! psi grid
    IF (debug_init) PRINT *, 'island geometry being read in'
    CALL check_unit(23, __LINE__, __FILE__)
    OPEN(23,FILE=indir_name(1:lenin)//world//'.psiles',STATUS='old',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! isles is now a variable, determine here how
    ! many islands (i.e. landmasses-1) there are (maximum number in .psiles
    ! file minus 1)
    isles = 0
    DO j = maxj, 0, -1
       READ (23,*,IOSTAT=ios) (gbold(i+j*maxi), i = 1, maxi)
       CALL check_iostat(ios, __LINE__, __FILE__)
       DO i = 1, maxi
          IF (gbold(i+j*maxi) > REAL(isles)) isles = INT(gbold(i+j*maxi))
       END DO
    END DO
    CLOSE(23,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    isles = isles - 1
    IF (debug_init) &
         & PRINT *, 'Number of landmasses present in .psiles file:', &
         & isles+1, '(i.e.', isles, 'islands)'
    ALLOCATE(lpisl(mpi,isles),STAT=status)                 ; lpisl = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ipisl(mpi,isles),STAT=status)                 ; ipisl = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(jpisl(mpi,isles),STAT=status)                 ; jpisl = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(npi(isles),STAT=status)                       ; npi = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(psisl(0:maxi,0:maxj,isles),STAT=status)       ; psisl = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ubisl(2,0:maxi+1,0:maxj,isles),STAT=status)   ; ubisl = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(erisl(isles,isles+1),STAT=status)             ; erisl = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(psibc(isles),STAT=status)                     ; psibc = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    ! read island path integral data, read isles+1 paths only if want last path
    ! for testing
    IF (isles > 0) THEN
       IF (debug_init) PRINT *, 'island path integrals being read in'
       CALL check_unit(24, __LINE__, __FILE__)
       OPEN(24,FILE=indir_name(1:lenin)//world//'.paths',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (24,*,IOSTAT=ios) (npi(i), i = 1, isles)
       CALL check_iostat(ios, __LINE__, __FILE__)
       DO i = 1, isles
          READ (24,*,IOSTAT=ios)
          CALL check_iostat(ios, __LINE__, __FILE__)
          IF (npi(i) > mpi) THEN
             CALL die('path integral around island too long', &
                  & __LINE__, __FILE__)
          END IF
          DO j = 1, npi(i)
             READ (24,*,IOSTAT=ios) lpisl(j,i), ipisl(j,i), jpisl(j,i)
             CALL check_iostat(ios, __LINE__, __FILE__)
             ! rotate grid to check b.c.s
             IF (ABS(lpisl(j,i)) /= 1 .AND. ABS(lpisl(j,i)) /= 2) THEN
                CALL die('', __LINE__, __FILE__)
             END IF
             IF (ipisl(j,i) > maxi .OR. ipisl(j,i) < 0) THEN
                CALL die('bad path', __LINE__, __FILE__)
             END IF
             IF (jpisl(j,i) > maxj .OR. jpisl(j,i) < 0) THEN
                CALL die('bad path', __LINE__, __FILE__)
             END IF
             IF (k1(ipisl(j,i),jpisl(j,i)) > maxk) THEN
                WRITE (msgStr,*) 'dry path', j, i, ipisl(j, i), jpisl(j, i), &
                     & k1(ipisl(j, i), jpisl(j, i)), maxk
                CALL die(msgStr, __LINE__, __FILE__)
             END IF
          END DO
       END DO
    END IF
    CLOSE(24,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    IF (debug_init) THEN
       PRINT *, 'ocean variables'
       PRINT *, 'horizontal diffusivity', diff(1) * rsc * usc, ' m**2/s'
       PRINT *, 'vertical diffusivity', diff(2) * usc * dsc * dsc/rsc, ' m**2/s'
       PRINT *, 'basic drag coefficient', adrag * fsc, ' /s'
       PRINT *, 'wind stress scale', fsc * usc * dsc, ' m**2/s**2'
       PRINT *, 'or', fsc * usc * dsc * rh0sc, ' N/m**2'
       PRINT *, 'density variation scale', rhosc, ' kg/m**3'
       PRINT *, 'vertical velocity scale', usc * dsc / rsc, ' m/s'
       PRINT *, 'time scale', tsc / 86400 / yearlen, ' yrs'
       PRINT *, 'overturning scale', dsc * usc * rsc * 1e-6, ' Sv'
       PRINT *, 'vertical heat flux scale', &
            & dsc * usc * rh0sc * cpsc / rsc, ' W/m**2'
       PRINT *, 'integrated energy scale', &
            & rh0sc * fsc * usc * rsc**3 * dsc, ' J'
       PRINT *, 'integrated northward heat flux scale (W)'
       WRITE (6, '(e15.5)') usc * rh0sc * cpsc * rsc * dsc
    END IF

    ! This climatological albedo calculation has been reintroduced to
    ! the ocean model because albedo appears to be a desired output
    ! from the ocean model.  It may turn out that albedo is supplied
    ! instead by the surflux.F routines.

    ! Climatological albedo (similar to Weaver et al. 2001)
    DO j = 1, maxj
       tv = ASIN(s(j))
       tv2 = 0.2 + 0.36 * 0.5 * (1.0 - COS(2.0 * tv))
       albcl(1:maxi,j) = tv2
    END DO

    ! SETTING UP ADDITIONAL PARAMETERS FOR GENIE'S NEW surflux ROUTINE

    ! The following portion of code has been added to define the
    ! values of parameters required in the ocean and sea-ice only
    ! surflux routine.  They shouldn't interfere with the existing
    ! code, so GENIE c-GOLDSTEIN should still work as usual.

    IF (debug_init) THEN
       PRINT *, 'ocean albedo, albocn =', albocn
       PRINT *, 'gustiness factor, gust =', gust
       PRINT *, 'flux factor =', ene_tune
       PRINT *, 'choice of convection scheme =', iconv
    END IF

    ! Drag coefficent
    cd = 0.0013
    ! Useful constant proportional to inverse timsecale for surface
    ! freezing.
    rsictscsf = dsc * dz(maxk) * rho0 * cpo_ice / (17.5 * 86400.0)

    IF (debug_init) THEN
       PRINT *
       PRINT *, 'GENIE surflux parameters (for ocean and sea-ice fluxes)'
       PRINT *, 'ocean albedo, albocn =', albocn
       PRINT *, 'gustiness factor, gust =', gust
       PRINT *, 'drag coefficient, cd =', cd
       PRINT *, 'air density, rhoair =', rhoair
       PRINT *, 'specific heat capacity of air, cpa =', cpa
       PRINT *, 'constants for saturation specific humidity ...'
       PRINT *, '     const1 =', const1
       PRINT *, '     const2 =', const2
       PRINT *, '     const3 =', const3
       PRINT *, '     const4 =', const4
       PRINT *, '     const5 =', const5
       PRINT *, 'water density, rho0 =', rho0
       PRINT *, 'ratio of air/ocean density, rhoao =', rhoao
       PRINT *, 'Stefan-Boltzmann constant, sigma =', sigma
       PRINT *, 'ocean shortwave radiation emission constant, emo =', emo
       PRINT *, 'upper limit for ice temperature, tfreez =', tfreez
       PRINT *, 'latent heat of vapourization, hlv =', hlv
       PRINT *, 'latent heat of fusion, hlf =', hlf
       PRINT *, 'latent heat of sublimation, hls =', hls
       PRINT *, 'constant ice conductivity, consic =', consic
       PRINT *, 'Kelvin temperature constant, zeroc =', zeroc
       PRINT *, 'base of sea-ice empirical constant, ch_ice =', ch_ice
       PRINT *, 'skin friction velocity, u_tau_ice =', u_tau_ice
       PRINT *, 'specific heat of seawater under ice, cpo_ice =', cpo_ice
       PRINT *, 'representative ice density, rhoice =', rhoice
       PRINT *, 'useful inverse timescale for surface freezing, ', &
            & 'rsictscsf =', rsictscsf
       PRINT *, 'minimum average sea-ice thickness, hmin =', hmin
       PRINT *, 'density ratios, rhooi =', rhooi
       PRINT *, 'melting factor, rrholf =', rrholf
       PRINT *, 'm to mm conversion factor, m2mm =', m2mm
       PRINT *, 'mm to m conversion factor, mm2m =', mm2m
       PRINT *
    END IF

    ! MIXED LAYER DEPTH (mld) scheme (added to trunk 01/07/08), KICO
    ! mld scheme coefficients for mixing due to buoyancy and wind energy
    ! These are tunable parameters and should eventually be moved to genie-main
    ! mldpebuoycoeff = 0.15 and mldketaucoeff = 100.0 SHOULD be good starting
    ! points, according to theory. mldpebuoycoeff is an efficiency so should not
    ! exceed 1. mldwindkedec is the depth scale of exponential decay of wind
    ! energy efficiency, dec/dsc, where dec is the decay scale in metres. These
    ! paramters are now defined in xml in genie-main (e.g. definition.xml).
    mldwindkedec = mldwindkedec / dsc

    ! mld scheme - calculate wind decay efficiency
    DO k = maxk, 1, -1
       mlddec(k) = EXP(zro(k) / mldwindkedec)
       IF (k < maxk) THEN
          mlddecd(k) = mlddec(k) / mlddec(k+1)
       ELSE
          mlddecd(maxk) = mlddec(maxk)
       END IF
    END DO

    ! read in observational data filenames
    lentdata = lnsig1(tdatafile)
    IF (debug_init) PRINT *, &
         & 'Temperature observations filename, ', tdatafile(1:lentdata)
    lensdata = lnsig1(sdatafile)
    IF (debug_init) PRINT *, &
         & 'Salinity observations filename, ', sdatafile(1:lensdata)
    IF (debug_init) PRINT *
    IF (tsinterp) THEN
       lentvar = lnsig1(tdata_varname)
       lensvar = lnsig1(sdata_varname)
       IF (debug_init) THEN
          PRINT *, 'Interpoate observational dataset'
          PRINT *, 'Temperature observations variable name, ', &
               & tdata_varname(1:lentvar)
          PRINT *, 'Temperature observations scaling factor, ', tdata_scaling
          PRINT *, 'Temperature observations offset, ' , tdata_offset
          PRINT *, 'Temperature observations missing value, ', tdata_missing
          PRINT *, 'Salinity observations variable name, ', &
               & sdata_varname(1:lensvar)
          PRINT *, 'Salinity observations scaling factor, ', sdata_scaling
          PRINT *, 'Salinity observations offset, ' , sdata_offset
          PRINT *, 'Salinity observations missing value, ', sdata_missing
       END IF
    END IF

    IF (debug_init) THEN
       PRINT *, 'file extension for output (a3) ?'
       PRINT *, lout
       PRINT *, 'filename for NETCDF restart input ?'
       PRINT *, filenetin
       PRINT *, 'directory name for NETCDF restart output ?'
       PRINT *, dirnetout
    END IF

    ! Is this a new or continuing run?
    IF (ans == 'n' .OR. ans == 'N') THEN
       ! This is a new run, initial conditions already set up
       ! But set up initial default time and date....
       ! If we're not re-starting, then assume that
       !   we are at the beginning of a year....:
       iyear_rest = 2000
       imonth_rest = 1
       ioffset_rest = 0
       day_rest = yearlen / REAL(nyear)
       IF (debug_init) PRINT *, 'day_rest = ', day_rest
    ELSE
       ! This is a continuing run, read in end state filename
       IF (debug_init) PRINT *, 'input file extension for input (a6)'
       IF (debug_init) PRINT *, lin
       IF (debug_init) PRINT *, 'Reading GOLDSTEIN restart file'
       CALL inm_netcdf(lrestart_genie)

       DO k = 1, maxk
          DO j = 1, maxj
             DO i = 1, maxi
                ! added option for resetting temperature value
                ! (code having been copied from initialization avove)
                IF (rst_reset_t) THEN
                   IF (j <= maxj / 2) THEN
                      ts(1,i,j,k) = temp0 * 0.5 * (1 + SIGN(1, k - k1(i,j)))
                   ELSE
                      ts(1,i,j,k) = temp1 * 0.5 * (1 + SIGN(1, k - k1(i,j)))
                   END IF
                END IF
                ts1(:,i,j,k) = ts(:,i,j,k)
                CALL eos(ec, ts(1,i,j,k), ts(2,i,j,k), zro(k), ieos, rho(i,j,k))
             END DO
          END DO
       END DO
    END IF

    ! periodic b.c. (required for implicit code)
    DO k = 1, maxk
       DO j = 1, maxj
          rho(0,j,k) = rho(maxi,j,k)
          rho(maxi+1,j,k) = rho(1,j,k)
          DO l = 1, maxl
             ts(l,0,j,k) = ts(l,maxi,j,k)
             ts(l,maxi+1,j,k) = ts(l,1,j,k)
             ! for cimp /= 1 need
             ts1(l,0,j,k) = ts(l,maxi,j,k)
             ts1(l,maxi+1,j,k) = ts(l,1,j,k)
          END DO
       END DO
    END DO

    ! oscillating forcing
    flat = .TRUE.
    DO i = 1, maxi
       DO j = 1, maxj
          IF (k1(i,j) > 1 .AND. k1(i,j) <= maxk) flat = .FALSE.
       END DO
    END DO
    IF (flat) THEN
       IF (debug_init) PRINT *, 'flat bottom'
    ELSE
       IF (debug_init) PRINT *, 'topography present'
    END IF

    CALL invert

    DO isol = 1, isles
       ! set source term to 1 on the ith island (i+1th landmass) only
       DO j = 0, maxj
          DO i = 1, maxi
             k = i + j * maxi
             IF (INT(gbold(k)) == isol + 1) THEN
                gb(k) = 1.0
             ELSE
                gb(k) = 0.0
             END IF
          END DO
       END DO
       CALL ubarsolv(ubisl(1,0,0,isol), psisl(0,0,isol))

       ! find island path integral due to unit source on boundary
       DO isl = 1, isles
          CALL island(ubisl(1,0,0,isol), erisl(isl,isol), isl, 0)
       END DO
    END DO

    IF (debug_init) PRINT *, 'island path integrals due to unit sources', &
         & ((erisl(isl,isol), isl = 1, isles), isol = 1, isles)

    ! partially invert inland integral error matrix for psi bc calc
    CALL matinv_gold(isles, erisl)

    ! c-goldstein calls wind.f here
    !  replicate this only by fetching tau data for sake of
    !    duplicating c-goldstein output with genie.exe

    iw  = 1
    iav = 1

    ! istep0 added for hdefoutput. Currently set to zero
    ! so RESTARTS WITH ENTS WILL NOT WORK!!!
    go_istep0 = 0

    ! SETTING UP GRID STRUCTURE

    ! This code is copied from an earlier incarnation of c-GOLDSTEIN
    ! and IGCM coupling.  It calculates grid structure for use in
    ! conversion between atmosphere and ocean grids.  Previously it
    ! was inserted just after gseta.F was executed, but I've moved it
    ! here instead.

111 FORMAT(i3,6f8.2)
112 FORMAT(i3,2f8.2)

    IF (debug_init) PRINT *
    IF (debug_init) PRINT *, 'GOLDSTEIN/GENIE grid interpolation variables :'

    IF (debug_init) PRINT *, &
         & '* Longitude : olon1, olon2, olon3, obox1, obox2, obox3 *'
    IF (igrid == 0 .OR. igrid == 1) THEN
       phi0 = -260.0 * deg_to_rad
       DO i = 1, maxi
          olon1(i) = REAL(360.0 * (i - 0.5) / REAL(maxi) + phi0 / deg_to_rad)
          olon2(i) = REAL(360.0 * i / REAL(maxi) + phi0 / deg_to_rad)
          olon3(i) = REAL(360.0 * (i - 0.5) / REAL(maxi) + phi0 / deg_to_rad)
       END DO
       DO i = 1, maxi+1
          oboxedge1_lon(i) = &
               & REAL(360.0 * (i - 1.0) / REAL(maxi) + phi0 / deg_to_rad)
          oboxedge2_lon(i) = &
               & REAL(360.0 * (i - 0.5) / REAL(maxi) + phi0 / deg_to_rad)
          oboxedge3_lon(i) = &
               & REAL(360.0 * (i - 1.0) / REAL(maxi) + phi0 / deg_to_rad)
       END DO
    END IF

    DO i = 1, maxi+1
       IF (i < maxi+1) THEN
          nclon1(i) = olon1(i)
          IF (igrid == 0 .OR. igrid == 1) THEN
             nclon2(i) = &
                  & REAL(360.0 * (i - 1.0) / REAL(maxi) + phi0 / deg_to_rad)
          ELSE
             nclon2(i) = olon2(i)
          END IF
          nclon3(i) = olon3(i)
          IF (debug_init) WRITE (*,111) i, olon1(i), olon2(i), olon3(i), &
               & oboxedge1_lon(i), oboxedge2_lon(i), oboxedge3_lon(i)
       ELSE
          IF (debug_init) WRITE (*,111) i, -999.99, -999.99, -999.99, &
               & oboxedge1_lon(i), oboxedge2_lon(i), oboxedge3_lon(i)
       END IF
    END DO

    IF (debug_init) PRINT *, &
         & '* Latitude : olat1, olat2, olat3, obox1, obox2, obox3 *'
    nclat3(1) = REAL(ASIN(sv(0)) * 180.0 / pi)
    DO j = 1, maxj
       olat1(j) = REAL(ASIN(s(j)) * 180.0 / pi)
       olat2(j) = REAL(ASIN(s(j)) * 180.0 / pi)
       olat3(j) = REAL(ASIN(sv(j)) * 180.0 / pi)
       nclat1(j) = olat1(j)
       nclat2(j) = olat2(j)
       IF (j < maxj) nclat3(j+1) = REAL(ASIN(sv(j)) * 180.0 / pi)
    END DO
    DO j = 1, maxj+1
       oboxedge1_lat(j) = REAL(ASIN(sv(j-1)) * 180.0 / pi)
       oboxedge2_lat(j) = REAL(ASIN(sv(j-1)) * 180.0 / pi)
       IF (j <= maxj) oboxedge3_lat(j) = REAL(ASIN(s(j)) * 180.0 / pi)
    END DO
    oboxedge3_lat(maxj+1) = REAL(ASIN(sv(maxj)) * 180.0 / pi)

    DO j = 1, maxj+1
       IF (j < maxj+1) THEN
          IF (debug_init) WRITE (*,111) j, olat1(j), olat2(j), olat3(j), &
               & oboxedge1_lat(j), oboxedge2_lat(j), oboxedge3_lat(j)
       ELSE
          IF (debug_init) WRITE (*,111) j, -999.99, -999.99, -999.99, &
               & oboxedge1_lat(j), oboxedge2_lat(j), oboxedge3_lat(j)
       END IF
    END DO

    ! This bit is to make the land-sea mask on the genie grid.
    ! The genie grid is offset from the goldstein grid by maxi/4
    ! in the longitudinal direction.
    WHERE (k1(1:maxi,1:maxj) >= 90)
       ilandmask1 = 1
       ilandmask2 = 1
       ilandmask3 = 1
    ELSEWHERE
       ilandmask1 = 0
       ilandmask2 = 0
       ilandmask3 = 0
    END WHERE

    IF (debug_init) PRINT *, '* Depth : depth, depth1 *'
    DO l = 1, maxk
       depth(l) = REAL(ABS(dsc * zro(maxk+1-l)))
       ncdepth(l) = depth(l)
    END DO
    DO l = 0, maxk
       depth1(l+1) = REAL(ABS(dsc * zw(maxk-l)))
       ncdepth1(l+1) = depth1(l+1)
    END DO

    DO l = 0, maxk
       IF (l > 0) THEN
          IF (debug_init) WRITE (*,112) l, depth(l), depth1(l+1)
       ELSE
          IF (debug_init) WRITE (*,112) l, -999.99, depth1(l+1)
       END IF
    END DO
    IF (debug_init) PRINT *

    ! OPEN OUTPUT FILES

    ! Average values of ocean T
    CALL check_unit(4, __LINE__, __FILE__)
    OPEN(4,FILE=outdir_name(1:lenout)//lout//'.'//'t',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (4,*)'% GOLDSTEIN ocean model, ocean temperature'
    IF (debug_init) WRITE (4,'(11a14)',IOSTAT=ios) &
         & '% time       ', ' Pac_T_d   ', ' Atl_T_d   ', ' Ind_T_d   ', &
         & ' Sou_T_d   ', ' Pac_T_u   ', ' Atl_T_u   ', ' Ind_T_u   ', &
         & ' Sou_T_u   ', ' drho/dz   ', ' speed     '
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(4,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    ! Average values of ocean S
    CALL check_unit(14, __LINE__, __FILE__)
    OPEN(14,FILE=outdir_name(1:lenout)//lout//'.'//'s',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (14,*)'% GOLDSTEIN ocean model, ocean salinity'
    IF (debug_init) WRITE (14,'(11a14)',IOSTAT=ios) &
         & '% time       ', ' Pac_S_d   ', ' Atl_S_d   ', ' Ind_S_d   ', &
         & ' Sou_S_d   ', ' Pac_S_u   ', ' Atl_S_u   ', ' Ind_S_u   ', &
         & ' Sou_S_u   ', ' drho/dz   ', ' speed     '
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(14,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! Values of ocean maximum/minimum circulation
    CALL check_unit(40, __LINE__, __FILE__)
    OPEN(40,FILE=outdir_name(1:lenout)//lout//'.'//'opsit',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (40,*)'% GOLDSTEIN ocean model, ocean overturning'
    IF (debug_init) WRITE (40,'(5a14,a10)',IOSTAT=ios) &
         & '% time       ', 'Pacific min', 'Pacific max', 'Atlantic min', &
         & 'Atlantic max', ' Location'
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (40,'(5a14,2a5)',IOSTAT=ios) &
         & '%            ', 'Sv', 'Sv', 'Sv', 'Sv', 'j', 'k'
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(40,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! Values of ocean basin FW and heat fluxes
    CALL check_unit(41, __LINE__, __FILE__)
    OPEN(41,FILE=outdir_name(1:lenout)//lout//'.'//'flux',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (41,'(3a28)',IOSTAT=ios) &
         & '% GOLDSTEIN ocean model, FW ', 'and heat fluxes, first data ', &
         & 'line is region area (m2)    '
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (41,'(11a14)',IOSTAT=ios) &
         & '% time        ', ' ', ' ', ' Total FW ', ' ', ' ', ' ', &
         & ' ', 'Total heat', ' ', ' '
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (41,'(11a14)',IOSTAT=ios) &
         & '%             ', 'Global', 'Atlantic', 'Pacific', 'Indian', &
         & 'Southern', 'Global', 'Atlantic', 'Pacific', 'Indian', 'Southern'
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (41,'(11a14)',IOSTAT=ios) &
         & '%             ', 'Sv', 'Sv', 'Sv', 'Sv', 'Sv', &
         & 'PW', 'PW', 'PW', 'PW', 'PW'
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(41,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! file for writing extra freshwater forcing
    CALL check_unit(44, __LINE__, __FILE__)
    OPEN(44,FILE=outdir_name(1:lenout)//lout//'.'//'hose',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_init) WRITE (44,*)'% GOLDSTEIN ocean model, extra FW forcing'
    CLOSE(44,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)


    ! OUTPUT ARGUMENTS

    ! Sea surface temperature [-> surface fluxes]
    tstar_ocn = REAL(ts(1,1:maxi,1:maxj,maxk))
    ! Sea surface salinity [-> surface fluxes]
    sstar_ocn = REAL(ts(2,1:maxi,1:maxj,maxk))
    ! Surface velocity (u component) [-> sea-ice]
    ustar_ocn = REAL(u(1,1:maxi,1:maxj,maxk))
    ! Surface velocity (v component) [-> sea-ice]
    vstar_ocn = REAL(u(2,1:maxi,1:maxj,maxk))
    ! Ocean albedo
    albedo_ocn = REAL(albocn)

    ! Set values of dummy variables destined for BIOGEM
    go_saln0 = REAL(saln0)
    go_rhoair = REAL(rhoair)
    go_cd = REAL(cd)
    go_dphi = REAL(dphi)
    go_ips = ips
    go_ipf = ipf
    go_ias = ias
    go_iaf = iaf
    go_ds = REAL(ds)
    go_jsf = jsf
    go_usc = REAL(usc)
    go_dsc = REAL(dsc)
    go_fsc = REAL(fsc)
    go_rh0sc = REAL(rh0sc)
    go_rhosc = REAL(rhosc)
    go_cpsc = REAL(cpsc)
    go_scf = REAL(scf)
    go_k1 = k1(1:maxi,1:maxj)
    go_dz = REAL(dz)
    go_dza = REAL(dza)
    go_c  = REAL(c)
    go_cv = REAL(cv)
    go_s  = REAL(s)
    go_sv = REAL(sv)
    go_ts = REAL(ts(:,1:maxi,1:maxj,1:maxk))
    go_ts1 = REAL(ts1(:,1:maxi,1:maxj,1:maxk))

    ! Choose diapycnal mixing scheme. iediff=0 (needed for tests to pass),
    ! is uniform diffusivity.
    IF (iediff > 0) CALL ediff

    ! Define ssmax for isopycnal mixing as function of depth
    IF (ssmaxsurf - ssmaxdeep < 1.0E-7 .AND. &
         & ssmaxsurf - ssmaxdeep > -1.0E-7) THEN
       ssmax = ssmaxdeep
    ELSE
       ssmaxmid = 0.5 * (LOG(ssmaxsurf) + LOG(ssmaxdeep))
       ssmaxdiff = 0.5 * (LOG(ssmaxsurf) - LOG(ssmaxdeep))
       ssmaxtanhefold = 200 / dsc
       ssmaxtanh0dep = -300 / dsc
       DO k = 1, maxk-1
          zssmax = (zw(k) - ssmaxtanh0dep) / ssmaxtanhefold
          ssMAX(k) = EXP(ssmaxmid+ssmaxdiff * TANH(zssmax))
       END DO
    END IF

    ! Set values for ENTS
    go_rsc = REAL(rsc)
    go_nyear = nyear
    go_syr = REAL(syr)
    go_lin = lin

    DEALLOCATE(bmask)
    DEALLOCATE(h)
    DEALLOCATE(fw_anom_in)

    PRINT *, ' <<< Initialisation complete'
    PRINT *, '======================================================='
  END SUBROUTINE initialise_goldstein


  ! Terminate GOLDSTEIN run
  SUBROUTINE end_goldstein
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE

    INTEGER :: i, j, k, l, ios
    REAL :: avn, avs, sums(8*maxl)

    ! Stream function variables
    REAL, dimension(0:maxj,0:maxk) :: opsi, opsia, opsip
    REAL :: omina, omaxa, ominp, omaxp

    REAL :: zpsi(0:maxi,0:maxk), zu(maxi,maxk)
    REAL :: hft(3), hfp(3), hfa(3), phfmax, tv2, tv3
    REAL :: rhoout(maxk,maxi,maxj)
    INTEGER :: iposa(2)

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising GOLDSTEIN module shutdown ...'

    IF (ctrl_diagend .AND. debug_end) CALL diagend

    ! write out convective frequency array. Divide by 2*nsteps if CALL co twice
    CALL check_unit(3, __LINE__, __FILE__)
    OPEN(3,FILE=outdir_name(1:lenout)//lout//'.cost',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (nsteps > 0) THEN
       IF (debug_end) WRITE (3,'(e15.8)',IOSTAT=ios) &
            & ((cost(i,j), i = 1, maxi), j = 1, maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
    END IF
    CLOSE(3,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! write out barotropic streamfunction
    OPEN(3,FILE=outdir_name(1:lenout)//lout//'.psi',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    DO j = 0, maxj
       DO i = 0, maxi
          IF (debug_end) WRITE (3,*,IOSTAT=ios) psi(i,j)
          CALL check_iostat(ios, __LINE__, __FILE__)
       END DO
    END DO
    CLOSE(3,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! The following routines are normally only called in goldstein.F, but
    ! their outputs are written to file here.  Rather than have them passed
    ! from goldstein.F to end_goldstein.F, am re-calculating them here.
    IF (debug_end) CALL diag2(sums, avn, avs)

    IF (debug_end) &
         & CALL diagopsi(ominp, omaxp, omina, omaxa, opsi, opsia, opsip, iposa)

    CALL check_unit(10, __LINE__, __FILE__)
    OPEN(10,FILE=outdir_name(1:lenout)//lout//'.opsi',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) &
         & WRITE (10,100,IOSTAT=ios) ((opsi(j,k), j = 0, maxj), k = 0, maxk)
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(10,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    OPEN(10,FILE=outdir_name(1:lenout)//lout//'.opsip',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) &
         & WRITE (10,100,IOSTAT=ios) ((opsip(j,k), j = 0, maxj), k = 0, maxk)
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(10,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    OPEN(10,FILE=outdir_name(1:lenout)//lout//'.opsia',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) &
         & WRITE (10,100,IOSTAT=ios) ((opsia(j,k), j = 0, maxj), k = 0, maxk)
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(10,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! zonal overturning streamfunction
    zpsi = 0
    DO i = 1, maxi-1
       DO k = 1, maxk-1
          zu(i,k) = SUM(u(1,i,1:maxj,k) / c(1:maxj) * ds)
          zpsi(i,k) = zpsi(i,k-1) - dz(k) * zu(i,k)
       END DO
    END DO

    OPEN(10,FILE=outdir_name(1:lenout)//lout//'.zpsi',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) &
         & WRITE (10,100,IOSTAT=ios) ((zpsi(i,k), i = 0, maxi), k = 0, maxk)
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(10,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

100 FORMAT(e14.7)
110 FORMAT(11e14.6)

    ! write poleward heat flux in Atlantic and Pacific and total
    CALL check_unit(15, __LINE__, __FILE__)
    OPEN(15,FILE=outdir_name(1:lenout)//lout//'.fofy',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) WRITE (15,'(10(a11,3x))',IOSTAT=ios) &
         & ' latitude  ', ' tot_tot   ', ' Pac_tot   ', ' Atl_tot   ', &
         & ' tot_adv   ', ' Pac_adv   ', ' Atl_adv   ', ' tot_dif   ', &
         & ' Pac_dif   ', ' Atl_dif   '
    CALL check_iostat(ios, __LINE__, __FILE__)
    phfmax = 0
    DO j = 1, maxj-1
       hft = 0
       hfp = 0
       hfa = 0
       DO i = 1, maxi
          IF (k1(i,j) <= maxk .AND. k1(i,j+1) <= maxk) THEN
             tv2 = 0
             tv3 = 0
             DO k = k1(i,j), maxk
                tv2 = tv2 + 0.5 * cv(j) * u(2,i,j,k) * &
                     & (ts(1,i,j+1,k) + ts(1,i,j,k)) * dz(k) * dphi
                tv3 = tv3 - cv(j) * cv(j) * &
                     & (ts(1,i,j+1,k) - ts(1,i,j,k)) * &
                     & rds(j) * diff(1) * dz(k) * dphi
             END DO
             hft(1) = hft(1) + tv2 + tv3
             hft(2) = hft(2) + tv2
             hft(3) = hft(3) + tv3
             IF (i >= ips(j) .AND. i <= ipf(j)) THEN
                hfp(1) = hfp(1) + tv2 + tv3
                hfp(2) = hfp(2) + tv2
                hfp(3) = hfp(3) + tv3
             ELSEIF (i >= ias(j) .AND. i <= iaf(j)) THEN
                hfa(1) = hfa(1) + tv2 + tv3
                hfa(2) = hfa(2) + tv2
                hfa(3) = hfa(3) + tv3
             END IF
          END IF
       END DO
       IF (debug_end) WRITE (15,110,IOSTAT=ios) &
            & 180.0 / pi * ASIN(s(j)), (hft(l), hfp(l), hfa(l), l = 1, 3)
       CALL check_iostat(ios, __LINE__, __FILE__)
       IF (ABS(hft(3)) > phfmax) phfmax = ABS(hft(3))
    END DO

    IF (debug_end) WRITE (6,*) 'max poleward heat flux ', phfmax

    CLOSE(15,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! write out potential density
    rhoout = 0.0

    CALL check_unit(11, __LINE__, __FILE__)
    OPEN(11,FILE=outdir_name(1:lenout)//lout//'.rho',IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    DO j = 1, maxj
       DO i = 1, maxi
          DO k = 1, maxk
             IF (k >= k1(i,j)) THEN
                rhoout(k,i,j) = rho(i,j,k)
             ELSE
                rhoout(k,i,j) = 0.0
             END IF
          END DO
       END DO
    END DO

    IF (debug_end) WRITE (11,fmt='(e21.13)')rhoout
    CLOSE(11,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    tv2 = dsc * usc * rsc * 1.0E-6
    IF (debug_end) WRITE (6,'(a)',IOSTAT=ios) 'overturning extrema in Sv'
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) WRITE (6,'(a)',IOSTAT=ios) 'ominp,omaxp,omina,omaxa,avn'
    CALL check_iostat(ios, __LINE__, __FILE__)
    IF (debug_end) WRITE (6,'(5e15.5)',IOSTAT=ios) &
         & ominp * tv2, omaxp * tv2, omina * tv2, omaxa * tv2, avn
    CALL check_iostat(ios, __LINE__, __FILE__)

    PRINT *, ' <<< Shutdown complete'
    PRINT *, '======================================================='
  END SUBROUTINE end_goldstein


  ! GOLSTEIN timestep
  ! flux version fully explicit one step second order variable depth
  ! version with isoneutral diffusion 21/5/1
  ! notes; fe/fn/fa in previous versions could have been scalars
  ! error in isoneutral coeffs corrected 29/8/2
  ! upstream weighting included 29/8/2
  ! variable ds 6/12/4 nre
  SUBROUTINE tstepo
    IMPLICIT NONE

    REAL, PARAMETER :: ups0=0.0
    INTEGER :: i, j, k, l

    ! Mixed layer scheme needs:
    REAL :: mldtsold(maxl,maxi,maxj,maxk), mldrhoold(maxi,maxj,maxk)
    REAL :: mldrhonew(maxi,maxj,maxk), mldtstmp(2), mldrhotmp
    INTEGER :: mldpk(2,maxi,maxj)

    REAL, DIMENSION(maxl,maxi,maxj,maxk) :: ts_t1, ts1_t1, ts_t2, ts1_t2
    REAL, DIMENSION(maxi,maxj,maxk) :: rho_t1, rho_t2

    IF (imld == 1) THEN
       ! before calculating fluxes, calculate energy consumed or
       ! released in mixing surface forcing over top layer. Needed for
       ! mld calculation, especially if mixed layer is <1 cell thick.
       ! NOTE: for now, all "energies" in this scheme are calculated
       ! in units of density x height^2, or
       ! (Energy/area)/2*acceleration (due to gravity).
       DO j = 1, maxj
          DO i = 1, maxi
             mldtstmp(1) = ts(1,i,j,maxk) - ts(1,i,j,maxk+1)
             mldtstmp(2) = ts(2,i,j,maxk) - ts(2,i,j,maxk+1)
             CALL eos(ec, mldtstmp(1), mldtstmp(2), zro(maxk), ieos, mldrhotmp)
             mldpelayer1(i,j) = (mldrhotmp - rho(i,j,maxk)) * z2dzg(maxk,maxk)
          END DO
       END DO
    END IF

    ts_t1 = ts(:,1:maxi,1:maxj,1:maxk)
    ts1_t1 = ts1(:,1:maxi,1:maxj,1:maxk)
    ts_t2 = ts(:,1:maxi,1:maxj,1:maxk)
    ts1_t2 = ts1(:,1:maxi,1:maxj,1:maxk)
    rho_t1 = rho(1:maxi,1:maxj,1:maxk)
    rho_t2 = rho(1:maxi,1:maxj,1:maxk)
    CALL tstepo_flux()

    IF (imld == 1) THEN
       ! remember old T and S only so we can calculate PE
       ! change. Code inefficient at present because only surface mixed points
       ! are compared (energy released due to eg bottom mixed layers is
       ! excluded, although surface plumes from coshuffle are accounted for),
       ! but T and S remembered everywhere.
       DO i = 1, maxi
          DO j = 1, maxj
             IF (k1(i,j) <= maxk) THEN
                mldtsold(:,i,j,:) = ts(:,i,j,1:maxk)
             END IF
          END DO
       END DO
       ! convection scheme itself not changed, but diagnostic
       ! for mixed layer depth precursor diagnosed in mldpk(1,:,:). If
       ! coshuffle used, maxmium depth of surface plume is in mldpk(2,:,:)
       CALL co(ts, mldpk)
       ! calculate PE released and multiply it by efficiency
       ! of recycling of PE. Need to check non-linearities properly
       ! represented before any commit??!!
       DO i = 1, maxi
          DO j = 1, maxj
             IF (k1(i,j) <= maxk) THEN
                mldpeconv(i,j) = 0
                k = maxk
                DO WHILE (k > 0)
                   CALL eos(ec, mldtsold(1,i,j,k), mldtsold(2,i,j,k), &
                        & zro(k), ieos, mldrhoold(i,j,k))
                   CALL eos(ec, ts(1,i,j,k), ts(2,i,j,k), &
                        & zro(k), ieos, mldrhonew(i,j,k))
                   mldpeconv(i,j) = mldpeconv(i,j) + &
                        & (mldrhonew(i,j,k) - mldrhoold(i,j,k)) * z2dzg(k,k)
                   k = k - 1
                END DO
                mldpebuoy(i,j) = mldpeconv(i,j) + mldpelayer1(i,j)
                IF (mldpebuoy(i,j) > 0.0) THEN
                   mldpebuoy(i,j) = mldpebuoy(i,j) * mldpebuoycoeff
                END IF
                ! Add wind energy
                mldemix(i,j) = mldpebuoy(i,j) + mldketau(i,j) * mlddec(maxk)
             END IF
          END DO
       END DO

       ! Kraus Turner scheme added. Static instability
       ! driven convection  has already been done in co, and will differ from
       ! standard KT if iconv == 1. krausturner uses PE released
       ! in co and KE from the wind to deepen the mixed layer further.
       DO i = 1, maxi
          DO j = 1, maxj
             IF (k1(i,j) <= maxk) THEN
                IF (mldemix(i,j) > 0.0) THEN
                   ! Apply krausturner
                   CALL krausturner(ts(1:maxl,i,j,1:maxk), mldpebuoy(i,j), &
                        & mldketau(i,j), mldpk(1,i,j), mld(i,j), &
                        & mldk(i,j), k1(i,j))
                ELSE
                   ! Not enough energy even to homogenise first
                   ! layer. The first layer *is* still homogeneous for
                   ! all tracers, but an mld shallower than the first
                   ! cell is output as a diagnostic
                   mldk(i,j) = maxk
                   IF (mldpelayer1(i,j) < 0) THEN
                      mld(i,j) = &
                           & zw(maxk-1) * (1 - mldemix(i,j) / mldpelayer1(i,j))
                   ELSE
                      mld(i,j) = zw(maxk-1)
                   END IF
                END IF
             END IF
          END DO
       END DO
    ELSE
       ! Not applying mixed layer scheme. Just CALL convection scheme
       CALL co(ts, mldpk)
    END IF

    ! if thermobaricity is on, make sure rho calculation is vertically local
    IF (ieos /= 0) THEN
       DO i = 1, maxi
          DO j = 1, maxj
             IF (k1(i,j) <= maxk) THEN
                DO k = 1,maxk
                   CALL eos(ec, ts(1,i,j,k), ts(2,i,j,k), &
                        & zro(k), ieos, rho(i,j,k))
                END DO
             END IF
          END DO
       END DO
    END IF

    ! periodic b.c. for rho (required at wet points)
    ! isoneutral code also needs ts1 bc.
    DO j = 1, maxj
       DO k = k1(0,j), maxk
          rho(0,j,k) = rho(maxi,j,k)
          ts1(:,0,j,k) = ts(:,maxi,j,k)
       END DO
       DO k = k1(maxi+1,j), maxk
          rho(maxi+1,j,k) = rho(1,j,k)
          ts1(:,maxi+1,j,k) = ts(:,1,j,k)
       END DO
    END DO

    DO k = 1, maxk
       DO j = 1, maxj
          DO i = 1, maxi
             DO l = 1, maxl
                IF (k >= k1(i,j)) ts1(l,i,j,k) = ts(l,i,j,k)
             END DO
          END DO
       END DO
    END DO
  END SUBROUTINE tstepo


  ! Ocean flux calculation
  SUBROUTINE tstepo_flux
    IMPLICIT NONE

    REAL :: tv, ups(3), pec(3)
    REAL, DIMENSION(maxl) :: fe, fw, fn, fa, fwsave
    REAL :: fs(maxl,maxi), fb(maxl,maxi,maxj)
    INTEGER :: i, j, k, l
    REAL, PARAMETER :: ups0=0.0

    ! ediff calc needs
    REAL :: diffv

    REAL :: tec, scc, dzrho, rdzrho, slim, tv1
    REAL :: dxrho(4), dxts(maxl,4), dyrho(4), dyts(maxl,4), dzts(maxl)
    INTEGER :: ina, nnp, knp

    scc = 0.0
    rdzrho = 0.0
    IF (diso) THEN
       scc = ec(2)
       limps = 0
    END IF
    diffv = diff(2)
    dmax = 0

    ! 2nd order explicit step

    ! lower boundary fluxes
    fb = 0

    DO k = 1, maxk
       ! southern boundary fluxes
       j = 1
       fs = 0
       DO j = 1, maxj
          ! western boundary fluxes
          i = 1
          pec(1) = u(1,maxi,j,k) * dphi / diff(1)
          ups(1) = pec(1) / (2.0 + ABS(pec(1)))
          DO l = 1, maxl
             IF (k >= MAX(k1(maxi,j), k1(1,j))) THEN
                ! western doorway
                fw(l) = u(1,maxi,j,k) * rc(j) * &
                     & ((1.0 - ups(1)) * ts1(l,1,j,k) + &
                     &  (1.0 + ups(1)) * ts1(l,maxi,j,k)) * 0.5
                fw(l) = fw(l) - (ts1(l,1,j,k) - ts1(l,maxi,j,k)) * &
                     & rc2(j) * diff(1)
             ELSE
                fw(l) = 0
             END IF
             fwsave(l) = fw(l)
          END DO
          DO i = 1, maxi
             ! calculate local vertical diffusivity
             IF (k >= k1(i,j) .AND. k < maxk) THEN
                ! First get vertical density gradient (also needed for
                ! isopycnal diff)
                CALL eosd(ec, ts1(1,i,j,k), ts1(1,i,j,k+1), &
                     & ts1(2,i,j,k), ts1(2,i,j,k+1), &
                     & zw(k), rdza(k), ieos, dzrho, tec)
                IF (dzrho < -1.0E-12) THEN
                   rdzrho = 1.0 / dzrho
                ELSE
                   rdzrho = -1.0E12
                END IF
                IF (iediff > 0 .AND. iediff < 3) THEN
                   ! Value of diffv fine for applying diffusivity, but
                   ! peclet number calc is a 1st order approximation
                   ! if diffusivity is variable.
                   IF (ediffpow2i == 0) THEN
                      diffv = ediff0 + ediff1(i,j,k)
                   ELSEIF (ediffpow2i == 1) THEN
                      diffv = ediff0 + ediff1(i,j,k) * (-rdzrho)
                   ELSEIF (ediffpow2i == 2) THEN
                      diffv = ediff0 + ediff1(i,j,k) * SQRT(-rdzrho)
                   ELSE
                      diffv = ediff0 + ediff1(i,j,k) * ((-rdzrho)**ediffpow2)
                   END IF
                   IF (diffv > diffmax(k+1)) diffv = diffmax(k+1)
                END IF
             END IF
             pec(1) = u(1,i,j,k) * dphi / diff(1)
             ups(1) = pec(1) / (2.0 + ABS(pec(1)))
             ! rather untidy mask to avoid undefined dsv at maxj nre
             pec(2) = u(2,i,j,k) * dsv(MIN(j, maxj-1)) / diff(1)
             ups(2) = pec(2) / (2.0 + ABS(pec(2)))
             pec(3) = u(3,i,j,k) * dza(k) / diffv
             ups(3) = pec(3) / (2.0 + ABS(pec(3)))
             DO l = 1, maxl
                ! flux to east
                IF (i == maxi) THEN
                   ! eastern edge(doorway or wall)
                   fe(l) = fwsave(l)
                ELSEIF (k < MAX(k1(i,j), k1(i+1,j))) THEN
                   fe(l) = 0
                ELSE
                   fe(l) = u(1,i,j,k) * rc(j) * &
                        & ((1.0 - ups(1)) * ts1(l,i+1,j,k) + &
                        &  (1.0 + ups(1)) * ts1(l,i,j,k)) * 0.5
                   fe(l) = fe(l) - (ts1(l,i+1,j,k) - ts1(l,i,j,k)) * &
                        & rc2(j) * diff(1)
                END IF
                ! flux to north
                IF (k < MAX(k1(i,j), k1(i,j+1))) THEN
                   fn(l) = 0
                ELSE
                   fn(l) = cv(j) * u(2,i,j,k) * &
                        & ((1.0 - ups(2)) * ts1(l,i,j+1,k) + &
                        &  (1.0 + ups(2)) * ts1(l,i,j,k)) * 0.5
                   fn(l) = fn(l) - cv2(j) * &
                        & (ts1(l,i,j+1,k) -ts1(l,i,j,k)) * diff(1)
                END IF
                ! flux above
                IF (k < k1(i,j)) THEN
                   fa(l) = 0
                ELSEIF (k == maxk) THEN
                   fa(l) = ts(l,i,j,maxk+1)
                ELSE
                   fa(l) = u(3,i,j,k) * &
                        & ((1.0 - ups(3)) * ts1(l,i,j,k+1) + &
                        &  (1.0 + ups(3)) * ts1(l,i,j,k)) * 0.5
                   fa(l) = fa(l) - (ts1(l,i,j,k+1) - ts1(l,i,j,k)) * &
                        & rdza(k) * diffv
                END IF
             END DO
             IF (diso) THEN
                ! isoneutral diffusion
                IF (k >= k1(i,j) .AND. k < maxk) THEN
                   IF (dzrho < -1.0E-12) THEN
                      tv1 = 0.0
                      ! tracer loop
                      DO knp = 0, 1
                         DO nnp = 0, 1
                            ina = 1+nnp + 2 * knp
                            ! phi derivatives
                            DO l = 1, maxl
                               IF (k+knp >= k1(i-1+2*nnp,j)) THEN
                                  dxts(l,ina) = (ts1(l,i+nnp,j,k+knp) - &
                                       & ts1(l,i+nnp-1,j,k+knp)) * rc(j) * rdphi
                               ELSE
                                  dxts(l,ina) = 0.0
                               END IF
                               ! s-derivatives
                               IF (k+knp >= k1(i,j-1+2*nnp)) THEN
                                  dyts(l,ina) = (ts1(l,i,j+nnp,k+knp) - &
                                       & ts1(l,i,j+nnp-1,k+knp)) * &
                                       & cv(j-1+nnp) * rdsv(j+nnp-1)
                               ELSE
                                  dyts(l,ina) = 0.0
                               END IF
                            END DO
                            dxrho(ina) = scc * dxts(2,ina) - tec * dxts(1,ina)
                            dyrho(ina) = scc * dyts(2,ina) - tec * dyts(1,ina)
                            ! calculate diagonal part
                            tv1 = tv1 + dxrho(ina) * dxrho(ina) + &
                                 & dyrho(ina) * dyrho(ina)
                         END DO
                      END DO
                      tv1 = 0.25 * tv1 * rdzrho * rdzrho
                      ! limit flux by factor slim for large slope
                      IF (tv1 > ssMAX(k)) THEN
                         slim = ssMAX(k) * ssMAX(k) / (tv1 * tv1)
                         ! count flux-limited points
                         limps = limps + 1
                      ELSE
                         slim = 1.0
                      END IF
                      tv1 = tv1 * slim * diff(1) * rdza(k)
                      ! test vertical diffusion number
                      tv = tv1 * dt(k) * rdza(k)
                      IF (tv > dmax) THEN
                         dmax = tv
                      END IF
                      DO l = 1, maxl
                         dzts(l) = (ts1(l,i,j,k+1)- ts1(l,i,j,k)) * rdza(k)
                         ! add isoneutral vertical flux
                         tv = 0
                         DO ina = 1, 4
                            tv = tv + (2 * dzrho * dxts(l,ina) - &
                                 & dxrho(ina) * dzts(l)) * dxrho(ina) + &
                                 & (2 * dzrho * dyts(l,ina) - &
                                 & dyrho(ina) * dzts(l)) * dyrho(ina)
                         END DO
                         tv = 0.25 * slim * diff(1) * tv / (dzrho * dzrho)
                         fa(l) = fa(l) + tv
                      END DO
                   END IF
                END IF
             END IF
             DO l = 1, maxl
                tv = 0
                IF (k >= k1(i,j)) THEN
                   ts(l,i,j,k) = ts1(l,i,j,k) - dt(k) * &
                        & (-tv + (fe(l) - fw(l)) * rdphi + &
                        & (fn(l) - fs(l,i)) * rds(j) + &
                        & (fa(l) - fb(l,i,j)) * rdz(k))
                END IF
                fw(l) = fe(l)
                fs(l,i) = fn(l)
                fb(l,i,j) = fa(l)
             END DO

             CALL eos(ec, ts(1,i,j,k), ts(2,i,j,k), zro(k), ieos, rho(i,j,k))
          END DO
       END DO
    END DO
  END SUBROUTINE tstepo_flux


  ! Convection code simplified for GOLDSTEIN
  !      suitable for arbitrary functions rho(T,S) variable depth
  ! cost counts occurences of mixing at each point not including the point
  ! at the top of each mixed region
  ! if cost is 2-d it counts the average number of convecting points at
  ! each horizontal point (divide by nsteps in mains.f)
  ! 22/5/2 maxl > 2 allowed
  ! 10/6/2 ts array passed as argument
  ! 16/5/5 coshuffle from Simon Mueller's Bern version added with edits nre
  ! 1/8/06 old/new convection scheme option added (rma)
  ! 04/08/08 KICO modified to handle thermobaricity
  ! 05/08/08 KICO merging of duplicated code
  SUBROUTINE co(tv, mldpk)
    IMPLICIT NONE

    REAL :: dzm(maxk), sum(maxl), tv(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
    INTEGER :: mldpk(2,maxi,maxj)
    INTEGER :: i, j, k(0:maxk), lastmix, m, n, ni, icond

    icond = 0

    IF (iconv == 1) THEN
       ! Mueller convection scheme:
       ! first apply coshuffle to mix directly down to density level
       ! would be faster to inline this call
       ! don't comment out the CALL without ensuring icosd is initialised
       CALL coshuffle(tv, mldpk)
    END IF

    DO j = 1, maxj
       DO i = 1, maxi
          ! initialize the index array k and mixed region sizes dzm
          ! wet points only
          IF (k1(i,j) <= maxk) THEN
             IF (iconv == 1) icond = 0

             k(k1(i,j)-1) = 0
             DO m = k1(i,j), maxk
                k(m) = m
                dzm(m) = dz(m)
             END DO

             m = maxk
             lastmix = 0

             ! main loop 'normally' decreasing in m
             DO WHILE (k(m-1) > 0 .OR. (lastmix /= 0 .AND. k(m) /= maxk))
                ! added code for thermobaricity
                IF (ieos /= 0) THEN
                   CALL eos(ec, tv(1,i,j,k(m)), tv(2,i,j,k(m)), &
                        & zw(k(m-1)), ieos, rho(i,j,k(m)))
                   CALL eos(ec, tv(1,i,j,k(m-1)), tv(2,i,j,k(m-1)), &
                        & zw(k(m-1)), ieos, rho(i,j,k(m-1)))
                END IF

                IF (rho(i,j,k(m)) < rho(i,j,k(m-1)) .OR. k(m-1) == 0) THEN
                   ! this may need changing, unless as rho(i,j,0) dimensioned
                   IF (lastmix == 0 .OR. k(m) == maxk) THEN
                      m = m - 1
                   ELSE
                      m = m + 1
                   END IF
                   lastmix = 0
                ELSE
                   lastmix = 1

                   ! look for instability before mixing
                   n = m - 1

                   ! added code for thermobaricity
                   IF (ieos /= 0) THEN
                      CALL eos(ec, tv(1,i,j,k(n)), tv(2,i,j,k(n)), &
                           & zw(k(n-1)), ieos, rho(i,j,k(n)))
                      CALL eos(ec, tv(1,i,j,k(n-1)), tv(2,i,j,k(n-1)), &
                           & zw(k(n-1)), ieos, rho(i,j,k(n-1)))
                   END IF

                   DO WHILE (k(n-1) > 0 .AND. rho(i,j,k(n)) >= rho(i,j,k(n-1)))
                      n = n - 1
                      ! added code for thermobaricity
                      IF (ieos /= 0) THEN
                         CALL eos(ec, tv(1,i,j,k(n)), tv(2,i,j,k(n)), &
                              & zw(k(n-1)), ieos, rho(i,j,k(n)))
                         CALL eos(ec, tv(1,i,j,k(n-1)), tv(2,i,j,k(n-1)), &
                              & zw(k(n-1)), ieos, rho(i,j,k(n-1)))
                      END IF
                   END DO
                   sum = tv(:,i,j,k(m)) * dzm(k(m))
                   DO ni = 1, m-n
                      sum = sum + tv(:,i,j,k(m-ni)) * dzm(k(m-ni))
                      dzm(k(m)) = dzm(k(m)) + dzm(k(m-ni))
                   END DO
                   tv(:,i,j,k(m)) = sum / dzm(k(m))
                   CALL eos(ec, tv(1,i,j,k(m)), tv(2,i,j,k(m)), &
                        & zw(k(m-1)), ieos, rho(i,j,k(m)))
                   ! reindex k(m)
                   ni = m - 1
                   DO WHILE (k(ni+1) > 0)
                      k(ni) = k(ni-m+n)
                      ni = ni - 1
                   END DO
                END IF
             END DO

             ! fill in T,S values in mixed regions
             m = maxk - 1
             DO n = maxk-1, k1(i,j), -1
                IF (n > k(m)) THEN
                   tv(:,i,j,n) = tv(:,i,j,k(m+1))
                   CALL eos(ec, tv(1,i,j,n), tv(2,i,j,n), &
                        & zw(k(n-1)), ieos, rho(i,j,n))
                   IF (iconv == 1) THEN
                      icond = icond + 1
                   ELSE
                      cost(i,j) = cost(i,j) + 1.0
                   END IF
                ELSE
                   m = m - 1
                END IF
             END DO

             IF (iconv == 1) THEN
                icosd(i,j) = MAX(icosd(i,j), icond)
                ! Fix for convection diagnostic needed by biogem
                cost(i,j) = dsc * zw(maxk-1-icosd(i,j))
             END IF

             mldpk(1,i,j) = k(m+1)
             ! wet points only
          END IF
       END DO
    END DO
  END SUBROUTINE co


  ! coshuffle.f - Bern3D+C
  SUBROUTINE coshuffle(tv, mldpk)
    IMPLICIT NONE

    REAL :: tv(maxl,0:maxi+1,0:maxj+1,0:maxk+1), tv_temp
    INTEGER :: mldpk(2,maxi,maxj)
    INTEGER :: i, j, k, k0, l, ipass, maxpass

    maxpass = maxk

    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) <= maxk) THEN
             icosd(i,j) = 0
             k0 = 0
             ipass = 0
             DO WHILE (k0 < maxk .AND. ipass < maxpass)
                ipass = ipass + 1
                k = maxk-1

                ! added code for thermobaricity
                IF (ieos /= 0) THEN
                   CALL eos(ec, tv(1,i,j,maxk), tv(2,i,j,maxk), &
                        & zw(k), ieos, rho(i,j,maxk))
                   CALL eos(ec, tv(1,i,j,k), tv(2,i,j,k), &
                        & zw(k), ieos, rho(i,j,k))
                END IF

                DO while((rho(i,j,maxk) > rho(i,j,k)) .AND. (k >= k1(i,j)))
                   k = k-1
                   ! added code for thermobaricity
                   IF (ieos /= 0) THEN
                      CALL eos(ec, tv(1,i,j,maxk), tv(2,i,j,maxk), &
                           & zw(k), ieos, rho(i,j,maxk))
                      CALL eos(ec, tv(1,i,j,k), tv(2,i,j,k), &
                           & zw(k), ieos, rho(i,j,k))
                   END IF
                END DO
                mldpk(2,i,j) = k0
                k0 = k+1
                IF (k0 < maxk) THEN
                   DO l = 1, maxl
                      tv_temp = tv(l,i,j,maxk)
                      DO k = maxk, k0+1, -1
                         tv(l,i,j,k) = &
                              & ((dz(k) - dz(maxk)) * tv(l,i,j,k) + &
                              & dz(maxk) * tv(l,i,j,k-1)) * rdz(k)
                      END DO
                      tv(l,i,j,k0) = ((dz(k0) - dz(maxk)) * tv(l,i,j,k0) + &
                           & dz(maxk) * tv_temp) * rdz(k0)
                   END DO
                   DO k = k0, maxk
                      CALL eos(ec, tv(1,i,j,k), tv(2,i,j,k), &
                           & zw(maxk-1), ieos, rho(i,j,k))
                   END DO
                   icosd(i,j) = MAX(icosd(i,j), maxk-k0)
                END IF
             END DO
          END IF
       END DO
    END DO
  END SUBROUTINE coshuffle


  ! Define drag matrix for GOLDSTEIN
  SUBROUTINE drgset(adrag, drgf, kmxdrg, jeb)
    IMPLICIT NONE
    REAL, INTENT(IN) :: adrag, drgf
    INTEGER, INTENT(IN) :: kmxdrg, jeb

    REAL :: tmpdrg(0:maxi,0:maxj)
    INTEGER :: i, j, i1, i1p, j1, kloc2, kloc4

    ! calculate drag at psi points (temporary variable)
    ! first find if there is shallow water (k1>kmxdrg) in
    ! 2x2 cell neighbourhood then in 4x4 nbhd.
    ! Increase drag near equator, assuming domain is symmetric
    DO j = 0, maxj
       DO i = 0, maxi
          kloc2 = MAX(k1(i,j), k1(i+1,j), k1(i,j+1), k1(i+1,j+1))
          kloc4 = k1(i,j)
          DO j1 = MAX(0, j-1), MIN(maxj+1, j+2)
             DO i1 = i-1, i+2
                i1p = 1 + MOD(maxi + i1-1, maxi)
                kloc4 = MAX(kloc4, k1(i1p,j1))
             END DO
          END DO
          IF (kloc2 > kmxdrg .OR. ABS(j - maxj / 2) <= jeb) THEN
             tmpdrg(i,j) = adrag * drgf * drgf
          ELSE IF (kloc4 > kmxdrg .OR. ABS(j - maxj / 2) == jeb + 1) THEN
             tmpdrg(i,j) = adrag * drgf
          ELSE
             tmpdrg(i,j) = adrag
          END IF
       END DO
    END DO

    ! interpolate to velocity points
    DO j = 1, maxj
       drag(1,1:maxi,j) = 0.5 * (tmpdrg(1:maxi,j) + tmpdrg(1:maxi,j-1))
       drag(2,1:maxi,j) = 0.5 * (tmpdrg(1:maxi,j) + tmpdrg(0:maxi-1,j))
    END DO

    ! boundary conditions, assuming no flow out of N or S bdy
    drag(2,maxi+1,:) = drag(2,1,:)
  END SUBROUTINE drgset


  ! Scheme for stratification-dependent
  ! and/or spatially variable diapycnal mixing. The standard
  ! option of horizontally uniform diapycnal mixing can
  ! be recovered (except for rounding differences), by iediff=1,
  ! ediff0=diff(2), or by iediff=1, ediffpow1=0.0, ediffpow2=0.0,
  ! ediffvar=0.0

  ! If iediff=1, a very simple stratification-dependent mixing
  ! scheme is applied:

  ! ediff1p = ediff0 +
  !   (diff(2)-ediff0)*ediffk0^ediffpow1*(dzrho_lev/dzrho)^ediffpow2

  ! where ediff is implemented diffusivity; ediff0 (tunable)
  ! is "minimum diffusivity" applied everywhere; ediffk0 is a
  ! reference diffusivity profile, exponentially growing with
  ! depth and equal to 1 at a depth of 2500 m;
  ! ediffpow1 (tunable) effectively modifies the efolding
  ! scale - ediffpow1=0 yields vertically uniform ediff for the
  ! "standard" density profile, ediffpow1=1 yields an e-folding
  ! scale of 700 m (but effectively slightly greater due to
  ! limitation at high diffusivities); dzrho is density gradient;
  ! dzrho_lev is a density gradient profile obtained by fitting an
  ! exponential decay profile to globally averaged Levitus data,
  ! ediffpow2 (tunable) is the stratification dependent mixing
  ! paramter - ediffpow2=0 yields no stratification dependence,
  ! ediffpow2=1 yields constant PE production in diapycnal mixing
  ! (excluding background mixing in ediff0 and diapycnal leakage).

  ! For all values of ediff0, ediffpow1, and ediffpow2, diff(2) is
  ! the diffusivity at 2500 m when dzrho=dzrho_lev.

  ! Additional spatial variability may be added by creating an
  ! maxi*maxj*(maxk-1) file ediffvargrid.dat. If ediffvar is
  ! different from zero, ediff is then modified according to:

  ! ediff1=ediff0+(ediff1p-ediff0)*(1+ediffvar*(ediffvargrid-1))

  ! Note that there is a maximum threshold value of ediff applied
  ! in tstepo.F, needed because dzrho can be small or zero.

  ! iediff=2 is identical to iediff=1, except that ediffk0 is
  ! a Bryan & Lewis profile (where diffusivity at a positive
  ! infinite depth is ediff0 and diffusivity at 2500 m is
  ! diff(2)). Not recommended.

  ! Future mixing schemes may be added using iediff=3 etc....

  SUBROUTINE ediff
    IMPLICIT NONE

    REAL :: ediff10, dzrho_lev(maxk), ediffk0(maxk), ediff1p(maxk)
    REAL :: ediffvargrid(maxi,maxj,maxk), ediffvartemp(maxi), ediffklim
    INTEGER :: i, j, k

    IF (iediff > 0 .AND. iediff < 3) THEN
       ! ediff0 units are initially m^2/s, other paramters are dimensionless
       ! convert ediff0 to dimensionless number
       ediff0 = ediff0 * rsc / (usc * dsc * dsc)

       ediff10 = diff(2) - ediff0
       IF (ediff10 < 0.0) THEN
          PRINT *, '****Warning: risk of negative diffusivities, ' // &
               & 'increasediff(2) or decrease ediff0****'
       END IF

       ! Create Levitus global mean density profile, dzrho_lev
       DO k = 1, maxk-1
          dzrho_lev(k) = (-5.5E-3 / rhosc * dsc) * EXP(zw(k) * (dsc / 650.0))
       END DO

       ! Create standard mixing profile
       IF (iediff == 1) THEN
          ! Use exponential growth profile, roughly consistent with observations
          ! (see papers by e.g. Polzin, Naveira Garabato).
          DO k = 1, maxk-1
             ediffk0(k) = EXP(-(zw(k) + 2500.0 / dsc) * (dsc / 700.0))
             ! 08/07/08 However, the profile is limited to a maximum that is
             ! 1/ediffklim=3 times greater than the diffusivity at 2500 m.
             ! Note that this limit is raised to the power of ediffpow1.
             ediffklim = 1 / 3.0E0
             ediffk0(k) = 1 / ((1 - ediffklim) / ediffk0(k) + ediffklim)
          END DO
       ELSEIF (iediff == 2) THEN
          ! Use Bryan & Lewis (1979) type profile instead. Not recommended. The
          ! exact profile used by Bryan & Lewis is recovered using
          ! diff(2)=0.8e-4 and ediff0=0.275e-4, with ediffpow2 and ediffvar set
          ! to zero and ediffpow1=1.
          DO k = 1, maxk-1
             ediffk0(k) = 1 + (2 / pi) * &
                  & ATAN(-(zw(k) + 2500.0 / dsc) * (4.5E-3 * dsc))
          END DO
       END IF

       ! Create precursor (before any manual mods from ediffvargrid)
       ! of profile to be used in tstepo.F
       DO k = 1, maxk-1
          ediff1p(k) = ediff10 * &
               & (ediffk0(k)**ediffpow1) * ((-dzrho_lev(k))**ediffpow2)
       END DO

       ! Make any manual mods from ediffvargrid
       IF (ediffvar < -1.0E-7 .OR. ediffvar > 1.0E-7) THEN
          ! Read in var file. The default file is a copy of
          ! ediffvar_36_36_08.dat; to use files for different grids or
          ! with different variation, copy (e.g.)
          ! ~/genie/genie-goldstein/data/input/ediffvar_36_36_16.dat
          ! to ~/genie/genie-goldstein/data/input/ediffvargrid.dat, or
          ! create new file.
          OPEN(unit=87,FILE=indir_name(1:lenin)//'ediffvargrid.dat', &
               & STATUS='old')
          DO k = 1, maxk-1
             DO j = 1, maxj
                READ (87,*) ediffvartemp
                ediffvargrid(:,j,k) = ediffvartemp
             END DO
          END DO
          CLOSE(unit=87)
          ! Apply mods
          DO i = 1, maxi
             DO j = 1, maxj
                DO k = 1, maxk-1
                   ediff1(i,j,k) = ediff1p(k) * &
                        & (1 + ediffvar * (ediffvargrid(i,j,k) - 1))
                END DO
             END DO
          END DO
       ELSE
          ! No mods needed....
          DO k = 1, maxk-1
             ediff1(:,:,k) = ediff1p(k)
          END DO
       END IF

       ! Make code more efficient by checking whether ediffpow2 is
       ! very close to 0, 1/2, or 1. If so, tstepo.F will not need to
       ! raise 1/dzrho to the power of a REAL number
       IF (ediffpow2 > -1.0E-7 .AND. ediffpow2 < 1.0E-7) THEN
          ediffpow2i = 0
       ELSE IF (ediffpow2 > (1.0 - 1.0E-7) .AND. &
             &  ediffpow2 < (1.0 + 1.0E-7)) THEN
          ediffpow2i = 1
       ELSE IF (ediffpow2 > (0.5 - 1.0E-7) .AND. &
             &  ediffpow2 < (0.5 + 1.0E-7)) THEN
          ediffpow2i = 2
       ELSE
          ! In this eventuality, every w point at every timestep will
          ! involve raising 1/dzrho to the power of a REAL number
          ediffpow2i = -999
       END IF

       ! Set up grid of maximum diffusivity (theoretical limit is
       ! 0.125*dz^2/dt). Note that this means max diffusivity will be
       ! a function of the number of vertical levels.
       diffmax = 0.5 * 0.125 * dz * dz / dt
    END IF
  END SUBROUTINE ediff


  ! Equation of state
  SUBROUTINE eos(ec, t, s, z, ieos, rho)
    IMPLICIT NONE
    REAL, INTENT(IN) :: ec(5), t, s, z
    INTEGER, INTENT(IN) :: ieos
    REAL, INTENT(OUT) :: rho

    IF (ieos == 0) THEN
       ! No thermobaricity term
       rho = ec(1) * t + ec(2) * s + ec(3) * t**2 + ec(4) * t**3
    ELSEIF (ieos == 1) THEN
       ! Thermobaricity term is in
       rho = ec(1) * t + ec(2) * s + ec(3) * t**2 + ec(4) * t**3 + ec(5) * t * z
    END IF
  END SUBROUTINE eos


  SUBROUTINE eosd(ec, t1, t2, s1, s2, z, rdz, ieos, dzrho, tec)
    IMPLICIT NONE

    REAL, INTENT(IN) :: ec(5), t1, t2, s1, s2, z, rdz
    INTEGER :: ieos
    REAL, INTENT(OUT) :: dzrho, tec
    REAL tatw

    ! Calculate dzrho (vertical density gradient).
    tatw = 0.5 * (t1 + t2)
    IF (ieos == 0) THEN
       ! No thermobaricity term
       tec = -ec(1) - ec(3) * tatw * 2 - ec(4) * tatw * tatw * 3
    ELSEIF (ieos == 1) THEN
       ! Thermobaricity term is in
       tec = -ec(1) - ec(3) * tatw * 2 - ec(4) * tatw * tatw * 3 - ec(5) * z
    END IF
    dzrho = (ec(2) * (s2 - s1) - tec * (t2 - t1)) * rdz
  END SUBROUTINE eosd


  ! Set additional freshwater forcing
  SUBROUTINE get_hosing(istep)
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep

    INTEGER :: i, j, ios

    ! set up freshwater fluxes according to parameters set up in
    ! genie_example.job:

    !${go_hosing:=0.00}        initial/constant value of hosing flux (Sv)
    !${go_hosing_trend:=0.00}  rate of increase/decrease of hosing flux (Sv/kyr)
    !${go_nyears_hosing:=0}    time period of hosing (yr)

    ! Update extra freshwater forcing
    hosing = hosing + hosing_trend * tsc * dt(maxk)

    IF (MOD(istep, itstp) < 1) THEN
       CALL check_unit(44, __LINE__, __FILE__)
       OPEN(44,FILE=outdir_name(1:lenout)//lout//'.'//'hose', &
            & STATUS='old',POSITION='append',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (44,*,IOSTAT=ios) istep, hosing
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(44,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
    END IF

    DO i = 1, maxi
       DO j = 1, maxj
          ! update fw_hosing Sv
          ! over gridboxes within convective regions (50-70N) in N.Atlantic
          ! and scaling as other freshwater fluxes:
          IF (istep <= nsteps_hosing) THEN
             fw_hosing(i,j) = m2mm * hosing * rhosing(i,j)
          ELSE
             fw_hosing(i,j) = 0.0
          END IF

          ! FW flux anomaly (from CMIP/PMIP model)
          fw_anom(i,j) = fw_anom(i,j) + fw_anom_rate(i,j) * tsc * dt(maxk)
       END DO
    END DO
  END SUBROUTINE get_hosing


  ! Invert matrix for barotropic streamfunction
  ! c grid version variable depth last change 20/10/94
  ! version for non-trivial coastlines started 10/4/00
  ! wind part removed to wind.f 9/2/1
  ! variable drag 7/5/2
  ! variable ds 6/12/4 nre
  SUBROUTINE invert
    IMPLICIT NONE

    INTEGER :: i, j, k, l, n, m, im
    REAL :: tv, tv1, rat

    n = maxi
    m = maxj + 1
    gap = 0

    ! Set equation at Psi points, assuming periodic b.c. in i.
    ! Cannot solve at both i=0 and i=maxi as periodicity => would
    ! have singular matrix. At dry points equation is trivial.
    DO i = 1, maxi
       DO j = 0, maxj
          k = i + j * n
          IF (MAX(k1(i,j), k1(i+1,j), k1(i,j+1), k1(i+1,j+1)) <= maxk) THEN
             tv = (s(j+1) * rh(1,i,j+1) - s(j) * rh(1,i,j)) / &
                  & (2.0 * dsv(j) * dphi)
             tv1 = (sv(j) * rh(2,i+1,j) - sv(j) * rh(2,i,j)) / &
                  & (2.0 * dphi * dsv(j))

             gap(k,2) = drag(1,i,j) * c(j) * c(j) * rh(1,i,j) / &
                  & (ds(j) * dsv(j)) + tv1

             l = n + 1
             ! for periodic boundary in i
             IF (i == 1) l = 2 * n + 1

             gap(k,l) = drag(2,i,j) * rcv(j) * rcv(j) * &
                  & rdphi * rdphi * rh(2,i,j) - tv

             gap(k,n+2) = -(drag(2,i,j) * rh(2,i,j) + &
                  & drag(2,i+1,j) * rh(2,i+1,j)) / &
                  & (cv(j) * cv(j) * dphi * dphi) - &
                  & (drag(1,i,j) * c(j) * c(j) * rh(1,i,j) / ds(j) + &
                  & drag(1,i,j+1) * c(j+1) * c(j+1) * rh(1,i,j+1) / &
                  & ds(j+1)) / dsv(j)

             l = n + 3
             ! for periodic boundary in i
             IF (i == maxi) l = 3

             gap(k,l) = drag(2,i+1,j) * rh(2,i+1,j) / &
                  & (cv(j) * cv(j) * dphi * dphi) + tv
             gap(k,2*n+2) = drag(1,i,j+1) * c(j+1) * c(j+1) * rh(1,i,j+1) / &
                  & (ds(j+1) * dsv(j)) - tv1
          ELSE
             gap(k,n+2) = 1
          END IF
       END DO
    END DO

    ! now invert the thing
    DO i = 1, n*m-1
       im = MIN(i+n+1, n * m)
       DO j = i+1, im
          rat = gap(j,n+2-j+i) / gap(i,n+2)
          ratm(j,j-i) = rat
          IF (rat /= 0) THEN
             DO k = n+2-j+i, 2*n+3-j+i
                gap(j,k) = gap(j,k) - rat * gap(i,k+j-i)
             END DO
          END IF
       END DO
    END DO
  END SUBROUTINE invert


  ! Calculate jbar forcing for streamfunction
  ! variable depth last change 21/10/94
  ! adds time dependent pressure torque term to forcing in psi eqn
  ! error corrected 14/6/97, split in two 9/2/1
  ! altered for non-trivial islands 17/2/2, should give same answers
  ! slower with old code (not yet checked)
  ! uninitialised bottom pressures where k1=maxk could cause loss of
  ! significance, although they exactly cancel, hence bp now defined
  ! at all wet points 16/6/3
  ! updated for generalised grid (RMA, 10/5/05)
  SUBROUTINE jbar
    IMPLICIT NONE

    INTEGER :: i, j, k, l, n, ip1
    REAL :: tv1, tv2, tv3, tv4

    n = maxi

    ! calculate easy part of double p integral
    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) <= maxk) THEN
             DO k = k1(i,j)+1, maxk
                bp(i,j,k) = bp(i,j,k-1) - &
                     & (rho(i,j,k) + rho(i,j,k-1)) * dza(k-1) * 0.5
             END DO
          END IF
       END DO
    END DO

    ! sbp now defined if mk > 0 ie all wet points
    DO j = 1, maxj
       DO i = 1, maxi
          IF (mk(i,j) > 0) THEN
             sbp(i,j) = 0
             DO k = mk(i,j)+1, maxk
                sbp(i,j) = sbp(i,j) + bp(i,j,k) * dz(k)
             END DO
          END IF
       END DO
    END DO

    ! periodic b.c. required for Antarctic island integral
    ! (if bp was defined everywhere would not need ifs)
    DO j = 1, maxj
       IF (k1(1,j) < maxk) THEN
          DO k = k1(1,j), maxk
             bp(maxi+1,j,k) = bp(1,j,k)
          END DO
       END IF
       IF (k1(1,j) <= maxk) THEN
          sbp(maxi+1,j) = sbp(1,j)
       END IF
    END DO

    ! calc tricky bits and add to source term for Psi, ip1 copes with
    ! periodicity in i, j bdy points are ignored assuming no flow out
    ! of north or south of domain.
    DO j = 1, maxj-1
       DO i = 1, maxi
          ip1 = MOD(i, maxi) + 1
          l = i + j * n
          IF (getj(i,j)) THEN
             tv1 = 0
             DO k = ku(2,ip1,j), mk(ip1,j+1)
                tv1 = tv1 + bp(ip1,j+1,k) * dz(k)
             END DO
             tv2 = 0
             DO k = ku(2,ip1,j), mk(ip1,j)
                tv2 = tv2 + bp(ip1,j,k) * dz(k)
             END DO
             tv3 = 0
             DO k = ku(2,i,j), mk(i,j+1)
                tv3 = tv3 + bp(i,j+1,k) * dz(k)
             END DO
             tv4 = 0
             DO k = ku(2,i,j), mk(i,j)
                tv4 = tv4 + bp(i,j,k) * dz(k)
             END DO
             gb(l) = gbold(l) + &
                  & ((tv3 + sbp(i,j+1) - tv4 - sbp(i,j)) * rh(2,i,j) - &
                  & (tv1 + sbp(ip1,j+1) - tv2 - sbp(ip1,j)) * &
                  & rh(2,ip1,j)) * rdphi * rdsv(j)
             tv1 = 0
             DO k = ku(1,i,j+1), mk(ip1,j+1)
                tv1 = tv1 + bp(ip1,j+1,k) * dz(k)
             END DO
             tv2 = 0
             DO k = ku(1,i,j), mk(ip1,j)
                tv2 = tv2 + bp(ip1,j,k) * dz(k)
             END DO
             tv3 = 0
             DO k = ku(1,i,j+1), mk(i,j+1)
                tv3 = tv3 + bp(i,j+1,k) * dz(k)
             END DO
             tv4 = 0
             DO k = ku(1,i,j), mk(i,j)
                tv4 = tv4 + bp(i,j,k) * dz(k)
             END DO
             gb(l) = gb(l) + &
                  & ((tv1 + sbp(ip1,j+1) - tv3 - sbp(i,j+1)) * rh(1,i,j+1) - &
                  & (tv2 + sbp(ip1,j) - tv4 - sbp(i,j)) * rh(1,i,j)) * &
                  & rdphi * rdsv(j)
          ELSE
             gb(l) = gbold(l)
          END IF
       END DO
    END DO
  END SUBROUTINE jbar


  ! Simplest Kraus-Turner mixed layer scheme, carried out after
  ! buoyancy-driven convection. The PE release from convection is
  ! calculated in co.f (to be multiplied by an efficiency here).
  ! The KE input from wind is in the field kewind, pre-multiplied
  ! by an efficiency, which is not a function of mixed layer depth
  ! (mld). Internal KE sources are neglected. It is not necessary
  ! to determine mld, because the scheme works by partially mixing
  ! the last layer. However mld is output each time-step as a
  ! diagnostic for use by (e.g.) biogem. KICO 18/09/07
  ! Inputs:
  !   tv (ts) - tracers including T and S
  !   empe    - pe energy input
  !   emke    - ke energy input
  !   mldpk   - deepest layer to be mixed from surface by buoyancy
  !             forcing alone
  ! Outputs:
  !   tv (ts) - tracers including T and S
  !   mldt    - mixed layer depth (-ve value) for use by biogem
  !   mldkt   - index of layer containing mld
  SUBROUTINE krausturner(tv, pebuoy, ketau, mldpk, mldt, mldkt, tvkl)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: tv(maxl,1,1,1:maxk)
    REAL, INTENT(IN) :: pebuoy, ketau
    INTEGER, INTENT(OUT) :: mldpk
    REAL, INTENT(OUT) :: mldt
    INTEGER, INTENT(OUT) :: mldkt
    INTEGER, INTENT(IN) :: tvkl

    REAL :: em, empe, emke, eneed, emr, smix, tmix, qmix(1:maxl)
    REAL :: rhou, rhol, rhomix, mlda, mldb, mldtadd
    INTEGER :: k, l, n, partmix

    ! delete following line (more efficient) once mldpk calc sorted
    mldpk = maxk
    ! initialise variables
    k = mldpk
    empe = pebuoy
    emke = ketau * mlddec(k)
    em = empe + emke
    eneed = 0.0
    tmix = tv(1,1,1,k)
    smix = tv(2,1,1,k)
    qmix = tv(:,1,1,k)
    CALL eos(ec, tmix, smix, zw(k), ieos, rhomix)
    partmix = 1
    ! find layer to be only partially mixed
    DO WHILE(eneed < em .AND. em > 0)
       IF (k < mldpk) THEN
          ! apply mixing from last step if complete
          qmix(1) = tmix
          qmix(2) = smix
          DO l = 3, maxl
             qmix(l) = rdzg(maxk,k) * &
                  & (qmix(l) * dzg(maxk,k+1) + tv(l,1,1,k) * dz(k))
          END DO
       END IF
       IF (k == tvkl) THEN
          ! Mixed layer reaches bottom
          mldkt = k
          mldt = zw(k-1)
          partmix = 0
          em = -1.0E-8
          IF (k < maxk) THEN
             DO l = 1, maxl
                DO n = k, maxk
                   tv(l,1,1,n) = qmix(l)
                END DO
             END DO
          END IF
       ELSE
          ! Try to completely mix next layer
          k = k-1
          emr = (em - eneed) / em
          empe = empe * emr
          emke = emke * emr * mlddecd(k)
          em = empe + emke
          ! T and S and rho
          IF (ieos /= 0) THEN
             CALL eos(ec, tmix, smix, zw(k), ieos, rhou)
          ELSE
             rhou = rhomix
          END IF
          tmix = rdzg(maxk,k) * (tmix * dzg(maxk,k+1) + tv(1,1,1,k) * dz(k))
          smix = rdzg(maxk,k) * (smix * dzg(maxk,k+1) + tv(2,1,1,k) * dz(k))
          CALL eos(ec, tv(1,1,1,k), tv(2,1,1,k), zw(k), ieos, rhol)
          CALL eos(ec, tmix, smix, zw(k), ieos, rhomix)
          ! Energy needed to completely mix layer
          eneed = z2dzg(maxk,k+1) * rhou+z2dzg(k,k) * rhol - &
               & z2dzg(maxk,k) * rhomix
       END IF
    END DO

    IF (partmix == 1 .AND. em > 0) THEN
       ! Partially mix next layer. This scheme is taken from Unified
       ! Model documentation paper No41. Ocean model mixed layer
       ! formulation (S. J. Foreman, 17 Sept 1990). Model Vers <2.0.
       ! (24) is the key equation, but it should read
       ! a=(h_(n-1)/h_n)*(M/E_mix).
       mldkt = k
       mlda = dzg(maxk,k+1) * rdzg(maxk,k) * em / eneed
       mldb = (dzg(maxk,k) * rdzg(maxk,k+1)-1) * mlda
       DO l = 1, maxl
          tv(l,1,1,maxk) = (1 - mldb) * qmix(l) + mldb * tv(l,1,1,k)
          DO n = k+1, maxk-1
             tv(l,1,1,n) = tv(l,1,1,maxk)
          END DO
          tv(l,1,1,k) = mlda * qmix(l) + (1 - mlda) * tv(l,1,1,k)
       END DO
       ! Actual mixed layer depth. The key equation is (26), but
       ! again the eqn is wrong (see above ref). It should read
       ! d = 2M / (g h_(n-1) (rho_n - rho_mix) )
       IF (k < maxk) THEN
          mldtadd = em / (zw(k) * (rhol - rhou))
          mldt = zw(k) + mldtadd
       ELSE
          ! Shouldn't happen, delete this line when confident it doesn't
          mldt = 0.0
          PRINT *, 'Warning: inconsistent result in mixed layer' // &
               & 'scheme surface layer partmixed: see krausturner.f'
       END IF
    ELSE IF (partmix == 1 .AND. k < maxk) THEN
       mldkt = k + 1
       mldt = zw(k)
    END IF
  END SUBROUTINE krausturner


  ! solve a set of n linear equations by direct inversion
  !          checked for 4x4 random matrix Neil Edwards 13/5/4
  !          solves amat*x = rhs, putting result into rhs
  !          included in goldstein for case of multiple islands
  !          split in to two parts to invert and multiply separately
  !          to save a lot of cpu if matrix is constant in time

  SUBROUTINE matinv_gold(nvar, amat)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nvar
    REAL, INTENT(INOUT) :: amat(:,:)

    INTEGER :: i, j, k

    ! elimination
    DO i = 1, nvar-1
       DO j = i+1, nvar
          DO k = i+1, nvar
             amat(j,k) = amat(i,i) * amat(j,k) - amat(j,i) * amat(i,k)
          END DO
       END DO
    END DO
  END SUBROUTINE matinv_gold


  SUBROUTINE matmult(nvar, amat, rhs)
    IMPLICIT NONE
    INTEGER, intent(in) :: nvar
    REAL, INTENT(IN) :: amat(:,:)
    REAL, INTENT(OUT) :: rhs(:)

    INTEGER :: i, j

    DO i = 1, nvar-1
       DO j = i+1, nvar
          rhs(j) = amat(i,i) * rhs(j) - amat(j,i) * rhs(i)
       END DO
    END DO

    ! back substitution
    rhs(nvar) = rhs(nvar) / amat(nvar,nvar)
    DO i = nvar-1, 1, -1
       DO j = i+1, nvar
          rhs(i) = rhs(i) - amat(i,j) * rhs(j)
       END DO
       rhs(i) = rhs(i) / amat(i,i)
    END DO
  END SUBROUTINE matmult


  ! Calculate barotropic velocity on c grid
  ! variable depth last change 21/10/94
  ! adds time dependent pressure torque term to forcing in psi eqn
  ! error corrected 14/6/97, domain extended 9/2/1
  ! updated for generalised grid (RMA, 10/5/05)
  SUBROUTINE ubarsolv(ubloc, psiloc)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: ubloc(2,0:maxi+1,0:maxj)
    REAL, INTENT(OUT) :: psiloc(0:maxi,0:maxj)

    INTEGER :: i, j, k, n, m, km, im

    n = maxi
    m = maxj + 1

    ! solve Psi equation
    DO i = 1, n*m-1
       im = MIN(i+n+1, n * m)
       DO j = i+1, im
          gb(j) = gb(j) - ratm(j,j-i) * gb(i)
       END DO
    END DO
    gb(n*m) = gb(n*m) / gap(n*m, n+2)
    DO i = n*m-1, 1, -1
       km = MIN(n+1, n * m-i)
       DO k = 1, km
          gb(i) = gb(i) - gap(i,n+2+k) * gb(i+k)
       END DO
       gb(i) = gb(i) / gap(i,n+2)
    END DO

    ! write to Psi for convenience (useful if ACC)
    DO j = 0, maxj
       DO i = 1, maxi
          k = i + j * n
          psiloc(i,j) = gb(k)
       END DO
       psiloc(0,j) = psiloc(maxi,j)
    END DO

    ! calculate barotropic vely where Psi (and ub) defined
    DO j = 1, maxj
       DO i = 1, maxi
          ubloc(1,i,j) = -rh(1,i,j) * c(j) * &
               & (psiloc(i,j) - psiloc(i,j-1)) * rds(j)
       END DO
    END DO

    DO j = 1, maxj-1
       DO i = 1, maxi
          ubloc(2,i,j) = rh(2,i,j) * &
               & (psiloc(i,j) - psiloc(i-1,j)) * rcv(j) * rdphi
       END DO
    END DO

    ! set velocity to zero at N and S boundaries
    DO i = 1, maxi
       ubloc(2,i,maxj) = 0.0
       ubloc(2,i,0) = 0.0
    END DO

    ! periodic b.c. for ub(2) required only for island integral
    DO j = 1, maxj
       ubloc(2,maxi+1,j) = ubloc(2,1,j)
       ubloc(1,0,j) = ubloc(1,maxi,j)
       ubloc(1,maxi+1,j) = ubloc(1,1,j)
       ubloc(2,0,j) = ubloc(2,maxi,j)
    END DO
    ubloc(2,maxi+1,0) = ubloc(2,1,0)
    ubloc(2,0,0) = ubloc(2,maxi,0)
  END SUBROUTINE ubarsolv


  SUBROUTINE velc
    IMPLICIT NONE

    REAL :: tv, tv1, tv2, tv4, tv5, sum(2)
    INTEGER :: i, j, k, l

    DO j = 1, maxj
       DO i = 1, maxi
          sum = 0
          ! u calc
          DO k = k1(i,j), maxk
             IF (k1(i+1,j) > k) THEN
                tv1 = 0
                tv2 = 0
             ELSE
                tv2 = -(rho(i+1,j,k) - rho(i,j,k)) * rdphi * rc(j)
                IF (MAX(k1(i,j-1), k1(i,j+1), &
                     &  k1(i+1,j-1), k1(i+1,j+1)) <= k) THEN
                   tv1 = -c(j) * (rho(i+1,j+1,k) - rho(i+1,j-1,k) + &
                        &         rho(i,j+1,k) - rho(i,j-1,k)) * rds2(j) * 0.25
                ELSEIF (MAX(k1(i,j-1), k1(i+1,j-1)) <= k) THEN
                   tv1 = -c(j) * (rho(i+1,j,k) - rho(i+1,j-1,k) + &
                        &         rho(i,j,k) - rho(i,j-1,k)) * rdsv(j-1) * 0.5
                ELSEIF (MAX(k1(i,j+1), k1(i+1,j+1)) <= k) THEN
                   tv1 = -c(j) * (rho(i+1,j+1,k) - rho(i+1,j,k) + &
                        &         rho(i,j+1,k) - rho(i,j,k)) * rdsv(j) * 0.5
                ELSE
                   tv1 = 0
                END IF
             END IF

             ! v calc
             IF (k1(i,j+1) > k) THEN
                tv4 = 0
                tv5 = 0
             ELSE
                tv4 = - cv(j) * (rho(i,j+1,k) - rho(i,j,k)) * rdsv(j)
                IF (MAX(k1(i-1,j), k1(i-1,j+1), &
                     &  k1(i+1,j), k1(i+1,j+1)) <= k) THEN
                   tv5 = -(rho(i+1,j+1,k) - rho(i-1,j+1,k) + &
                        &  rho(i+1,j,k) - rho(i-1,j,k)) * rdphi * 0.25 * rcv(j)
                ELSEIF (MAX(k1(i-1,j), k1(i-1,j+1)) <= k) THEN
                   tv5 = -(rho(i,j+1,k) - rho(i-1,j+1,k) + &
                        &  rho(i,j,k) - rho(i-1,j,k)) * rdphi * 0.5 * rcv(j)
                ELSEIF (MAX(k1(i+1,j), k1(i+1,j+1)) <= k) THEN
                   tv5 = -(rho(i+1,j+1,k) - rho(i,j+1,k) + &
                        &  rho(i+1,j,k) - rho(i,j,k)) * rdphi * 0.5 * rcv(j)
                ELSE
                   tv5 = 0
                END IF
             END IF

             ! add wind
             IF (k == maxk) THEN
                IF (k1(i+1,j) <= k) THEN
                   tv1 = tv1 - dztau(2,i,j)
                   tv2 = tv2 - dztau(1,i,j)
                END IF
                IF (k1(i,j+1) <= k) THEN
                   tv4 = tv4 - dztav(2,i,j)
                   tv5 = tv5 - dztav(1,i,j)
                END IF
             END IF

             dzu(1,k) = -(s(j) * tv1 + drag(1,i,j) * tv2) * rtv(i,j)
             dzu(2,k) = -(drag(2,i,j) * tv4 - sv(j) * tv5) * rtv3(i,j)
             DO l = 1, 2
                IF (k == k1(i,j)) THEN
                   u(l,i,j,k) = 0
                ELSE
                   u(l,i,j,k) = u(l,i,j,k-1) + &
                        & dza(k-1) * (dzu(l,k) + dzu(l,k-1)) * 0.5
                   sum(l) = sum(l) + dz(k) * u(l,i,j,k)
                END IF
             END DO
          END DO
          DO k = k1(i,j), maxk
             ! add barotropic part and relax
             IF (k1(i+1,j) <= k) THEN
                u(1,i,j,k) = u(1,i,j,k) - sum(1) * rh(1,i,j) + ub(1,i,j)
                u(1,i,j,k) = rel * u1(1,i,j,k) + (1.0 - rel) * u(1,i,j,k)
                u1(1,i,j,k) = u(1,i,j,k)
             END IF
             IF (k1(i,j+1) <= k) THEN
                u(2,i,j,k) = u(2,i,j,k) - sum(2) * rh(2,i,j) + ub(2,i,j)
                u(2,i,j,k) = rel * u1(2,i,j,k) + (1.0 - rel) * u(2,i,j,k)
                u1(2,i,j,k) = u(2,i,j,k)
             END IF
          END DO
       END DO
    END DO

    ! set boundary conditions
    DO j = 1, maxj
       DO k = k1(1,j), maxk
          u(1,0,j,k) = u(1,maxi,j,k)
       END DO
    END DO

    ! calculate w
    DO j = 1, maxj
       DO i = 1, maxi
          tv = 0
          DO k = k1(i,j), maxk-1
             tv1 = (u(1,i,j,k) - u(1,i-1,j,k)) * rdphi * rc(j)
             tv2 = (u(2,i,j,k) * cv(j) - u(2,i,j-1,k) * cv(j-1)) * rds(j)
             u(3,i,j,k) = tv           - dz(k) * (tv1 + tv2)
             tv = u(3,i,j,k)
          END DO
       END DO
    END DO
  END SUBROUTINE velc


  ! Sets wind stress forcing for barotropic streamfunction
  ! separated from stream.f 9/2/1
  ! updated for generalised grid (RMA, 10/5/05)
  SUBROUTINE wind
    IMPLICIT NONE

    INTEGER :: i, j, k, n, ip1

    n = maxi

    ! calculate constant wind stress part of forcing
    ! noting that tau not currently defined outside domain
    DO i = 1, maxi
       DO j = 0, maxj
          ip1 = MOD(i, maxi) + 1
          k = i + j * n
          IF (MAX(k1(i,j), k1(i+1,j), k1(i,j+1), k1(i+1,j+1)) <= maxk) THEN
             IF (j == maxj .OR. j == 0) &
                  & STOP 'wind stress not defined outside domain'
             gb(k) = (tau(2,ip1,j) * rh(2,i+1,j) - &
                  &   tau(2,i,j) * rh(2,i,j)) * rdphi * rcv(j) - &
                  &  (tau(1,i,j+1) * c(j+1) * rh(1,i,j+1) - &
                  &   tau(1,i,j) * c(j) * rh(1,i,j)) * rdsv(j)
          ELSE
             gb(k) = 0
          END IF
          gbold(k) = gb(k)
       END DO
    END DO
  END SUBROUTINE wind

END MODULE goldstein

MODULE ents

  USE genie_control, ONLY: dim_GOLDSTEINNLONS, dim_GOLDSTEINNLATS
  USE ents_lib
  USE ents_data
  USE ents_diag
  USE ents_netcdf
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_ents
  PUBLIC :: step_ents

CONTAINS

  SUBROUTINE initialise_ents(dum_lin, dum_rsc, dum_syr, dum_nyear, &
       & dum_ds, dum_dphi, dum_kmax, dum_k1, dum_rmax, dum_rdtdim, &
       & dum_tstar_atm, dum_qstar_atm, dum_ca, dum_co2_out, &
       & gn_daysperyear, dum_lat, landice_slicemask_lic, &
       & albs_lnd, land_albs_snow_lnd, land_albs_nosnow_lnd,  &
       & land_snow_lnd, land_bcap_lnd, land_z0_lnd, land_temp_lnd, &
       & land_moisture_lnd, sfcatm_lnd, sfxatm_lnd)
    IMPLICIT NONE
    CHARACTER(LEN=13), INTENT(IN) :: dum_lin
    REAL, INTENT(IN) :: dum_rsc, dum_syr
    INTEGER, INTENT(IN) :: dum_nyear
    REAL, DIMENSION(:), INTENT(IN) :: dum_ds
    REAL, INTENT(IN) :: dum_dphi
    INTEGER, INTENT(IN) :: dum_kmax
    INTEGER, DIMENSION(:,:), INTENT(IN) :: dum_k1
    REAL, INTENT(IN) :: dum_rmax, dum_rdtdim
    REAL, DIMENSION(:,:), INTENT(IN) :: dum_tstar_atm, dum_qstar_atm
    REAL, DIMENSION(:,:), INTENT(INOUT) :: dum_ca
    REAL, DIMENSION(:,:), INTENT(IN) :: dum_co2_out
    REAL, INTENT(IN) :: gn_daysperyear
    REAL, DIMENSION(:), INTENT(IN) :: dum_lat
    REAL, DIMENSION(:,:), INTENT(IN) :: landice_slicemask_lic
    REAL, DIMENSION(:,:), INTENT(INOUT) :: albs_lnd
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & land_albs_snow_lnd, land_albs_nosnow_lnd
    REAL, DIMENSION(:,:), INTENT(INOUT) :: land_snow_lnd
    REAL, DIMENSION(:,:), INTENT(OUT) :: land_bcap_lnd
    REAL, DIMENSION(:,:), INTENT(OUT) :: land_z0_lnd
    REAL, DIMENSION(:,:), INTENT(INOUT) :: land_temp_lnd
    REAL, DIMENSION(:,:), INTENT(INOUT) :: land_moisture_lnd
    REAL, DIMENSION(:,:,:), INTENT(OUT) :: sfcatm_lnd, sfxatm_lnd

    REAL, DIMENSION(:,:,:), ALLOCATABLE :: dum_tq
    INTEGER :: ios              ! File check flag

    ! Namelist declaration for ENTS goin variables
    NAMELIST /ents_control/ indir_name, outdir_name, condir_name
    NAMELIST /ents_control/ ents_igrid
    NAMELIST /ents_control/ ents_npstp, ents_iwstp
    NAMELIST /ents_control/ ents_itstp, ents_ianav
    NAMELIST /ents_control/ ents_restart, ents_yearlen
    NAMELIST /ents_control/ ents_out_name, ents_netin
    NAMELIST /ents_control/ ents_netout, ents_ascout, ents_filenetin
    NAMELIST /ents_control/ ents_dirnetout, rstdir_name
    NAMELIST /ents_control/ ents_restart_file
    NAMELIST /ents_control/ dosc
    NAMELIST /ents_control/ topt, copt, k7, k8, k9, k10, k11, k11a, k12
    NAMELIST /ents_control/ k13, k14, k16, k17, k18, k20, k24, k26
    NAMELIST /ents_control/ k29, k31, k32, kz0
    NAMELIST /ents_control/ atchem_fert
    NAMELIST /ents_control/ atchem_update
    NAMELIST /ents_control/ start_year
    NAMELIST /ents_control/ opt_timeseries_output
    NAMELIST /ents_control/ opt_append_data
    NAMELIST /ents_control/ par_output_years_file_0d
    NAMELIST /ents_control/ par_output_years_file_2d

    INTEGER, EXTERNAL :: lnsig1

    ! ======================================================================
    ! Setting up ENTS
    ! ======================================================================

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising ENTS land surface module ...'

    maxi = dim_GOLDSTEINNLONS
    maxj = dim_GOLDSTEINNLATS

    OPEN(UNIT=59, FILE='data_ENTS', STATUS='old', IOSTAT=ios)
    IF (ios /= 0) THEN
       PRINT *, 'ERROR: could not open ENTS namelist file'
       STOP
    END IF

    READ(UNIT=59, NML=ENTS_CONTROL, IOSTAT=ios)
    IF (ios /= 0) THEN
       PRINT *, 'ERROR: could not read ENTS namelist'
       STOP
    ELSE
       CLOSE(59)
    END IF

    ALLOCATE(ents_k1(maxi,maxj)) ; ents_k1 = 0
    ALLOCATE(ents_lat(maxj))     ; ents_lat = 0.0

    ALLOCATE(Cveg(maxi,maxj))     ; Cveg = 0.0
    ALLOCATE(Csoil(maxi,maxj))    ; Csoil = 0.0
    ALLOCATE(fv(maxi,maxj))       ; fv = 0.0
    ALLOCATE(epsv(maxi,maxj))     ; epsv = 0.0
    ALLOCATE(leaf(maxi,maxj))     ; leaf = 0.0
    ALLOCATE(respveg(maxi,maxj))  ; respveg = 0.0
    ALLOCATE(respsoil(maxi,maxj)) ; respsoil = 0.0
    ALLOCATE(photo(maxi,maxj))    ; photo = 0.0

    ALLOCATE(sphoto(maxi,maxj))  ; sphoto = 0.0
    ALLOCATE(srveg(maxi,maxj))   ; srveg = 0.0
    ALLOCATE(sleaf(maxi,maxj))   ; sleaf = 0.0
    ALLOCATE(srsoil(maxi,maxj))  ; srsoil = 0.0
    ALLOCATE(sCveg1(maxi,maxj))  ; sCveg1 = 0.0
    ALLOCATE(sCsoil1(maxi,maxj)) ; sCsoil1 = 0.0
    ALLOCATE(sfv1(maxi,maxj))    ; sfv1 = 0.0
    ALLOCATE(sepsv1(maxi,maxj))  ; sepsv1 = 0.0
    ALLOCATE(sfx0a(maxi,maxj))   ; sfx0a = 0.0
    ALLOCATE(sfx0o(maxi,maxj))   ; sfx0o = 0.0
    ALLOCATE(sfxsens(maxi,maxj)) ; sfxsens = 0.0
    ALLOCATE(sfxlw(maxi,maxj))   ; sfxlw = 0.0
    ALLOCATE(sevap(maxi,maxj))   ; sevap = 0.0
    ALLOCATE(spptn(maxi,maxj))   ; spptn = 0.0
    ALLOCATE(srelh(maxi,maxj))   ; srelh = 0.0
    ALLOCATE(sbcap(maxi,maxj))   ; sbcap = 0.0
    ALLOCATE(salbs(maxi,maxj))   ; salbs = 0.0
    ALLOCATE(ssnow(maxi,maxj))   ; ssnow = 0.0
    ALLOCATE(sz0(maxi,maxj))     ; sz0 = 0.0

    ALLOCATE(stqld(2,maxi,maxj))   ; stqld = 0.0
    ALLOCATE(tqld(2,maxi,maxj))    ; tqld = 0.0
    ALLOCATE(tqldavg(2,maxi,maxj)) ; tqldavg = 0.0

    ALLOCATE(bcap(maxi,maxj))    ; bcap = 0.0
    ALLOCATE(bcapavg(maxi,maxj)) ; bcapavg = 0.0
    ALLOCATE(snowavg(maxi,maxj)) ; snowavg = 0.0
    ALLOCATE(z0avg(maxi,maxj))   ; z0avg = 0.0
    ALLOCATE(albsavg(maxi,maxj)) ; albsavg = 0.0
    ALLOCATE(z0(maxi,maxj))      ; z0 = 0.0
    ALLOCATE(evapavg(maxi,maxj)) ; evapavg = 0.0
    ALLOCATE(pptnavg(maxi,maxj)) ; pptnavg = 0.0
    ALLOCATE(runavg(maxi,maxj))  ; runavg = 0.0
    ALLOCATE(fvfv(maxi,maxj))    ; fvfv = 0.0

    ALLOCATE(fxavg(7,maxi,maxj)) ; fxavg = 0.0

    ! Input directory name
    lenin = lnsig1(indir_name)
    IF (indir_name(lenin:lenin) /= '/') THEN
       lenin = lenin + 1
       indir_name(lenin:lenin) = '/'
    END IF
    PRINT *, 'Input dir. name ', indir_name(1:lenin)

    ! Output directory name
    lenout = lnsig1(outdir_name)
    IF (outdir_name(lenout:lenout) /= '/') THEN
       lenout = lenout + 1
       outdir_name(lenout:lenout+1) = '/'
    END IF
    PRINT *, 'Output dir. name ', outdir_name(1:lenout)

    ! Config directory name
    lencon = lnsig1(condir_name)
    IF (condir_name(lencon:lencon) /= '/') THEN
       lencon = lencon + 1
       condir_name(lencon:lencon+1) = '/'
    END IF
    PRINT *, 'Config dir. name ', condir_name(1:lencon)

    PRINT *, 'npstp ', ents_npstp
    PRINT *, 'ents_out_name = ', TRIM(ents_out_name)
    PRINT *, 'ents_restart_file = ', TRIM(ents_restart_file)

    PRINT *,  'lin = ', dum_lin
    PRINT *, dum_rsc, dum_syr, ents_nyear
    PRINT *, 'rmax dphi ', dum_rmax, dum_dphi
    PRINT *,  'Seasonality enabled: ', dosc

    ! Transfer variables to common variables within ENTS
    ents_kmax = dum_kmax
    ents_nyear = dum_nyear
    ents_lat=dum_lat

    ! Convert tstar_atm & qstar_atm arrays into one tq array
    ALLOCATE(dum_tq(2,maxi,maxj))
    dum_tq(1,:,:) = REAL(dum_tstar_atm, KIND(dum_tq))
    dum_tq(2,:,:) = REAL(dum_qstar_atm, KIND(dum_tq))
    ents_k1 = dum_k1

    ! Initialise simple land variables
    call setup_ents(dum_rsc, dum_syr, dum_ds, dum_dphi, dum_ca, dum_tq, &
         & dum_rmax, dum_rdtdim, dum_co2_out, gn_daysperyear, &
         & landice_slicemask_lic, albs_lnd, land_snow_lnd)
    DEALLOCATE(dum_tq)

    ! albedo
    land_albs_nosnow_lnd = (fv * aveg) + ((1.0 - fv) * &
         & MAX(apeat, ((apeat - asand) * k10 * Csoil / (k8 - k9)) + asand))
    albs_lnd = land_albs_nosnow_lnd
    IF (snowswitch == 1) THEN
       land_albs_snow_lnd = ((asnow - asnowv) * EXP(-k7 * Cveg)) + asnowv
       WHERE (land_snow_lnd == 1) albs_lnd = land_albs_snow_lnd
    ELSE
       land_albs_snow_lnd = land_albs_nosnow_lnd
    END IF
    land_bcap_lnd = REAL(bcap, KIND(land_bcap_lnd))
    land_z0_lnd = REAL(z0, KIND(land_z0_lnd))
    land_temp_lnd = REAL(tqld(1,:,:), KIND(land_temp_lnd))
    land_moisture_lnd = REAL(tqld(2,:,:), KIND(land_moisture_lnd))

    ! initialise land-atmosphere fluxes
    sfcatm_lnd = 0.0
    sfxatm_lnd = 0.0

    PRINT *, ' <<< Initialisation complete'
    PRINT *, '======================================================='
  END SUBROUTINE initialise_ents


  ! Sets up initial values for ENTS
  SUBROUTINE setup_ents(dum_rsc, dum_syr, dum_ds, dum_dphi, dum_ca, &
       & dum_tq, dum_rmax, dum_rdtdim, dum_co2_out, gn_daysperyear, &
       & landice_slicemask_lic, albs_lnd, land_snow_lnd)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_rsc, dum_syr, dum_dphi
    REAL, DIMENSION(maxj), INTENT(IN) :: dum_ds
    REAL, DIMENSION(maxi,maxj), INTENT(INOUT) :: dum_ca
    REAL, DIMENSION(2,maxi,maxj), INTENT(IN) :: dum_tq
    REAL, INTENT(IN) :: dum_rmax, dum_rdtdim
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: dum_co2_out
    REAL, INTENT(IN) :: gn_daysperyear
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: landice_slicemask_lic
    REAL, DIMENSION(maxi,maxj), INTENT(INOUT) :: albs_lnd, land_snow_lnd

    REAL :: Cveg_ini, Csoil_ini, fv_ini, photo_ini, fws, fta, fco2, rland_pts
    REAL :: z0_ini
    INTEGER :: isdump
    CHARACTER(LEN=30) :: dumpfl
    CHARACTER(LEN=200) :: filename

    ! pbh k_constants are no longer read in from "k_constants.dat" but
    ! are now namelist paramaters k21 is now hard-wired. This is the
    ! Universal Gas Constant
    rk19 = (copt - k13 + k14) / (copt - k13)
    k21 = 8.314
    k0 = EXP(-k31 / (tzero - k32))
    q10 = EXP((10.0 * k31) / ((tzero - k32)**2))
    rk30 = 1.0 / EXP(-k31 / (topt - k32))
    rk25 = 1.0 / EXP(-k20 / (k21 * topt))

    ! Some conversion factors
    asurfrea = dum_rsc * dum_rsc * dum_ds(1) * dum_dphi ! Grid box area (m2)
    rasurf = 1.0 / asurfrea
    rsyr = 1.0 / dum_syr         ! reciprocal no. of secs/year

    PRINT *, ' '
    PRINT *, 'ENTS (land) option used'
    PRINT *,  'rmax'
    PRINT *, dum_rmax
    PRINT *, 'daysperyear ', gn_daysperyear
    PRINT *, 'ents_nyear ', REAL(ents_nyear)

    ! Open goin file and read in restart if required
    OPEN(66, FILE=condir_name(1:lencon)//'ents_config.par')

    ! Calculate length of timestep (yrs)
    READ (66,*) msimpleland
    PRINT *, 'ENTS called every', msimpleland, 'ocean timesteps'
    dtland = (REAL(msimpleland) / ents_nyear)
    PRINT *, 'dtland =', dtland, 'yr'

    ! Print out initial sizes of global carbon reservoirs
    PRINT *, 'Initial carbon reservoir sizes are...'
    READ (66,*) Cveg_ini
    PRINT *, 'Cveg_ini =', Cveg_ini, '(GtC)'
    READ (66,*) Csoil_ini
    PRINT *, 'Csoil_ini =', Csoil_ini, '(GtC)'

    rland_pts = 1.0 / land_pts_ents

    ! Set up spatial initial carbon boxes (carbon partitioned equally
    ! spatially) as all grid boxes have same area, asurf.  Units
    ! kgC/m2
    WHERE (ents_k1 > ents_kmax .AND. landice_slicemask_lic < 2.0)
       Cveg = Cveg_ini * gtk * rasurf * rland_pts
       Csoil = Csoil_ini * gtk * rasurf * rland_pts
    ELSEWHERE
       Cveg = 0.0
       Csoil = 0.0
    END WHERE

    asnow = 0.8                 ! Land surface albedo paramaters
    asnowv = 0.3
    aveg = 0.1
    apeat = 0.11
    asand = 0.3
    albedol = 0.2               ! Land albedo constant

    ! Prescribed surface albedo
    WHERE (ents_k1 > ents_kmax)
       ! Constant land surface albedo
       WHERE (landice_slicemask_lic > 1.0)
          ! Prescribe surface albedo over ice sheets to be 0.8
          albs_lnd = 0.8
       ELSEWHERE
          albs_lnd = albedol
       END WHERE
    ELSEWHERE
       albs_lnd = 0.0
    END WHERE

    PRINT *, 'dum_syr ', dum_syr

    ! Switch for snow albedo feedback
    PRINT *, 'Use snow scheme? 1=Y 0=N'
    read(66,*)snowswitch
    PRINT *, snowswitch

    ! Initialise roughness length calculation
    z0_ini = MAX(0.001, kz0 * Cveg(1,1))

    ! setup initial land temperature, bucket size and set ca over land
    PRINT *, 'Initial land temp and water'
    PRINT *, dum_tq(1,1,1), MIN(k8, k9 + (k10 * Csoil(1,1)))
    PRINT *, 'Initial total water on land is'
    PRINT *, min(k8, (k9 + (k10 * Csoil(1,1)))) * asurfrea * &
         & land_pts_ents * 1.0E-12, '(*10^12 m^3)'
    PRINT *, 'Initial roughness length is'
    PRINT *, z0_ini, 'm'

    WHERE (ents_k1 > ents_kmax)
       bcap = MIN(k8, k9 + (k10 * Csoil))             ! initial bucket capacity
       tqld(1,:,:) = REAL(dum_tq(1,:,:), KIND(tqld))  ! initial temp
       tqld(2,:,:) = REAL(bcap, KIND(tqld))           ! initial bucket size
       dum_ca = 0.3                                   ! absorp. coeff over land
       z0 = z0_ini                                    ! initial transfer coeffs
    ELSEWHERE
       tqld(1,:,:) = 0.0
       tqld(2,:,:) = 0.0
       z0 = 0.
    END WHERE
    land_snow_lnd = 0           ! snow matrix

    dtdim = 1.0 / dum_rdtdim    ! Length of ocean timestep (s)

    ! Initialize photo and fv arrays for use in surflux (calculation
    ! of transpiration)
    fv_ini = MAX(1.0E-5, 1.0 - EXP(-k17 * Cveg_ini * gtk * rasurf * rland_pts))

    fws = 1.0                   ! New water stress function

    ! New temperature response function
    fta = ((2.0**(0.1 * (dum_tq(1,1,1) - topt))) / &
         &    ((1.0 + EXP(0.3 * (dum_tq(1,1,1) - k11))) * &
         &    (1.0 + EXP(-0.3 * (dum_tq(1,1,1) - k12))))) &
         & +  ((2.0**(0.1 * (dum_tq(1,1,1) - topt))) / &
         &    ((1.0 + EXP(0.6 * (dum_tq(1,1,1) - k11a))) * &
         &    (1.0 + EXP(-0.3 * (dum_tq(1,1,1) - k12)))))

    pco2ld = dum_co2_out(1,1) * rmtp
    IF (pco2ld >= k13) THEN
       fco2 = (pco2ld - k13) / (pco2ld - k13 + k14)
    ELSE
       fco2 = 0.0
    END IF

    photo_ini = k18 * rk19 * fco2 * fws * fta * fv_ini
    WHERE (ents_k1 > ents_kmax .AND. landice_slicemask_lic < 2.0)
       fv = fv_ini
       photo = photo_ini
    ELSEWHERE
       fv = 0.0
       photo = 0.0
    END WHERE

    ! Emissions option
    READ (66,'(a1)') include_emissions
    PRINT *, 'Force model with an emissions timeseries?'
    PRINT *, include_emissions

    ! Continue run
    IF (ents_restart == 'c' .OR. ents_restart == 'C') THEN
       IF (ents_netin == 'y' .OR. ents_netin == 'Y') THEN
          CALL in_ents_netcdf(ents_filenetin, land_snow_lnd)
       ELSE
          filename = indir_name(1:lenin) // TRIM(ents_restart_file)
          OPEN(1,FILE=TRIM(filename))
          CALL in_ents_ascii(1, land_snow_lnd)

          ! Continue ENTS from a different .sland file
          READ (66,*) isdump
          PRINT *, 'Continue run from different .sland file? (0=N 1=Y)', isdump
          READ (66,'(a35)') dumpfl
          IF (isdump == 1) THEN
             PRINT *, 'Different land restart file name is'
             PRINT *, dumpfl
             OPEN(1,FILE=TRIM(dumpfl),STATUS='old')
             PRINT *, 'ENTS restart file is ', dumpfl
             CALL in_ents_ascii(1, land_snow_lnd)
             CLOSE(1)
          ELSE
             PRINT *, 'ENTS restart file is ', filename
          END IF
          CLOSE(66)
          CLOSE(1)
       END IF
    END IF

    ! Initialise sealevel module
    OPEN(66,FILE=condir_name(1:lencon)//'sealevel_config.par')
    PRINT *, ' '
    PRINT *, 'SEALEVEL MODULE INITIALIZATION'

    ! For change in sea level need a reference average ocean density
    PRINT *, 'Reference average ocean density used', &
         & ' for change in sea-level calculation'
    READ (66,*) rhoref
    PRINT *, rhoref

    IF (dosc) THEN
       ! zero annual average arrays
       pco2ld_tot = 0.0 ; tot_mass_ocn_c = 0.0
       sphoto = 0.0 ; srveg = 0.0 ; sleaf = 0.0 ; srsoil = 0.0
       sCveg1 = 0.0 ; sCsoil1 = 0.0 ; sfv1 = 0.0 ; sepsv1 = 0.0
       stqld = 0.0 ; stqld = 0.0
       sfx0a = 0.0 ; sfx0o = 0.0 ; sfxsens = 0.0 ; sfxlw = 0.0
       sevap = 0.0 ; spptn = 0.0 ; srelh = 0.0
       sbcap = 0.0 ; salbs = 0.0 ; ssnow = 0.0 ; sz0 = 0.0
       tqldavg = 0.0 ; tqldavg = 0.0
       snowavg = 0.0 ; albsavg = 0.0 ; pptnavg = 0.0 ; runavg = 0.0
       bcapavg = 0.0 ; evapavg = 0.0 ; z0avg = 0.0
       fxavg = 0.0
       gmairttot=0.0
    END IF
  END SUBROUTINE setup_ents


  SUBROUTINE step_ents(istep, nyear, torog_atm, dum_co2_out, dum_rh0sc, &
       & dum_rhosc, dum_rsc, dum_ds, dum_dphi, dum_dsc, dum_saln0, &
       & dum_dz, dum_ec, dum_rho, dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, &
       & dum_evap, dum_pptn, dum_relh, dum_istep0, dum_el_photo, &
       & dum_el_respveg, dum_el_respsoil, dum_el_leaf, landice_slicemask_lic, &
       & albs_lnd, land_albs_snow_lnd, land_albs_nosnow_lnd, &
       & land_snow_lnd, land_bcap_lnd, land_z0_lnd, land_temp_lnd, &
       & land_moisture_lnd, ntrac_atm, sfcatm_lnd, sfxatm_lnd)
    USE netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep, nyear
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: torog_atm
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: dum_co2_out
    REAL, INTENT(IN) :: dum_rh0sc, dum_rhosc, dum_rsc
    REAL, DIMENSION(maxj), INTENT(IN) :: dum_ds
    REAL, INTENT(IN) :: dum_dphi, dum_dsc, dum_saln0
    REAL, DIMENSION(ents_kmax), INTENT(IN) :: dum_dz
    REAL, DIMENSION(4), INTENT(IN) :: dum_ec
    REAL, DIMENSION(maxi,maxj,ents_kmax), INTENT(IN) :: dum_rho
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: &
         & dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, dum_evap, dum_pptn, dum_relh
    INTEGER, INTENT(INOUT) :: dum_istep0
    REAL, DIMENSION(maxi,maxj), INTENT(INOUT) :: &
         & dum_el_photo, dum_el_respveg, dum_el_respsoil, dum_el_leaf
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: landice_slicemask_lic
    REAL, DIMENSION(maxi,maxj), INTENT(INOUT) :: albs_lnd
    REAL, DIMENSION(maxi,maxj), INTENT(OUT) :: &
         & land_albs_snow_lnd, land_albs_nosnow_lnd
    REAL, DIMENSION(maxi,maxj), INTENT(INOUT) :: &
         & land_snow_lnd, land_bcap_lnd, land_z0_lnd, &
         & land_temp_lnd, land_moisture_lnd
    INTEGER, INTENT(IN) :: ntrac_atm
    REAL, DIMENSION(ntrac_atm,maxi,maxj), INTENT(IN) :: sfcatm_lnd
    REAL, DIMENSION(ntrac_atm,maxi,maxj), INTENT(INOUT) :: sfxatm_lnd

    INTEGER :: itv, iout, i, j, istot
    CHARACTER(LEN=200) :: filename, fname, label

    REAL, DIMENSION(:,:,:), ALLOCATABLE :: var_data
    CHARACTER(LEN=8), DIMENSION(10) :: labels = &
         & (/ 'photo   ', 'respveg ', 'leaf    ', 'respsoil', 'Cveg    ', &
         &    'Csoil   ', 'fv      ', 'tqld1   ', 'tqld2   ', 'snow    ' /)
    INTEGER :: kk, myyear, var_id, vardim_id, ncid
    INTEGER :: mymonth, myday, inistep
    LOGICAL :: fexist

    dum_istep0 = 0
    istot=0

    bcap = REAL(land_bcap_lnd, KIND(bcap))
    z0 = REAL(land_z0_lnd, KIND(z0))
    tqld(1,:,:) = REAL(land_temp_lnd, KIND(tqld))
    tqld(2,:,:) = REAL(land_moisture_lnd, KIND(tqld))

    itv = MOD(istep + nyear - 1, ents_ianav)
    IF (itv < nyear) THEN
       IF (istep >= nyear .AND. itv == nyear-1) THEN
          iout = 1
       ELSE
          iout = 0
       END IF
       CALL entsdiagosc(nyear, istep, iout, albs_lnd, land_snow_lnd)
       call annav_diags(istep, iout, dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, &
            & dum_evap, dum_pptn, dum_relh, albs_lnd, land_snow_lnd)
    END IF

    IF (MOD(istep, ents_npstp) < 1) CALL screen_diags

    IF (MOD(istep + istot, msimpleland) == 0) THEN
       ! Note, the index for the atmospheric pCO2 tracer is 3
       ! TODO: define this index globally
       IF (atchem_fert) THEN
          CALL carbon(torog_atm, sfcatm_lnd(3,:,:), landice_slicemask_lic, &
               & sfxatm_lnd(3,:,:))
       ELSE
          CALL carbon(torog_atm, dum_co2_out, landice_slicemask_lic, &
               & sfxatm_lnd(3,:,:))
       END IF
    END IF

    ! Write ENTS restarts
    IF (MOD(istep, ents_iwstp) == 0) THEN
       IF (ents_ascout == 'y' .OR. ents_ascout == 'Y') THEN
          PRINT *, 'Writing ENTS restart file'
          filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.sland'
          OPEN(3,FILE=TRIM(filename))
          REWIND(3)
          CALL out_ents(3, land_snow_lnd)
          CLOSE(3)
       END IF

       IF (ents_netout == 'y' .OR. ents_netout == 'Y') THEN
          IF (ents_netin == 'y' .OR. ents_netin == 'Y') THEN
             inistep = INT(ents_nyear * iniday / ents_yearlen) + istep
             myyear = INT(inistep / ents_nyear)
             mymonth = INT(12 * MOD(inistep, ents_nyear) / ents_nyear)
             myday = INT(ents_yearlen * inistep / ents_nyear - &
                  & mymonth * (ents_yearlen / 12) - myyear * ents_yearlen)
          ELSE
             myyear = INT(istep / ents_nyear)
             mymonth = INT(12 * MOD(istep, ents_nyear) / ents_nyear)
             myday = INT(ents_yearlen * istep / ents_nyear - &
                  & mymonth * (ents_yearlen / 12) - myyear * ents_yearlen)
          END IF

          IF (MOD(ents_iwstp, ents_nyear) == 0) THEN
             fname = TRIM(outdir_name) // TRIM(ents_out_name) // &
                  & '_restart_' // TRIM(ConvertFunc(myyear - 1, 10)) // &
                  & '_12_30.nc'
          ELSE
             fname = TRIM(outdir_name) // TRIM(ents_out_name) // &
                  & '_restart_' // &
                  & TRIM(ConvertFunc(myyear, 10)) // '_' // &
                  & TRIM(ConvertFunc(mymonth, 2)) // '_' // &
                  & TRIM(ConvertFunc(myday, 2)) // '.nc'
          END IF

          INQUIRE(FILE=fname,EXIST=fexist)
          IF (fexist) THEN
             OPEN(8,FILE=fname,STATUS='old')
             CLOSE(8,STATUS='delete')
          END IF

          DO kk = 1, 10
             ALLOCATE(var_data(1,maxj,maxi))
             label = labels(kk)
             DO j = 1, maxj
                DO i = 1, maxi
                   SELECT CASE (kk)
                   CASE (1)
                      var_data(1,j,i) = photo(i,j)
                   CASE (2)
                      var_data(1,j,i) = respveg(i,j)
                   CASE (3)
                      var_data(1,j,i) = leaf(i,j)
                   CASE (4)
                      var_data(1,j,i) = respsoil(i,j)
                   CASE (5)
                      var_data(1,j,i) = Cveg(i,j)
                   CASE (6)
                      var_data(1,j,i) = Csoil(i,j)
                   CASE (7)
                      var_data(1,j,i) = fv(i,j)
                   CASE (8)
                      var_data(1,j,i) = tqld(1,i,j)
                   CASE (9)
                      var_data(1,j,i) = tqld(2,i,j)
                   CASE (10)
                      var_data(1,j,i) = land_snow_lnd(i,j)
                   END SELECT
                END DO
             END DO
             CALL netcdf_ents(fname, var_data, label, myday)
             DEALLOCATE(var_data)
          END DO

          ! Adding final restart value (single)
          CALL check_err(NF90_OPEN(fname, NF90_WRITE, ncid))
          CALL check_err(NF90_REDEF(ncid))
          CALL check_err(NF90_DEF_DIM(ncid, 'pco2ld', 1, vardim_id))
          CALL check_err(NF90_DEF_VAR(ncid, 'pco2ld', &
               & NF90_FLOAT, (/ vardim_id /), var_id))
          CALL check_err(NF90_PUT_ATT(ncid, var_id, 'long_name', 'pco2ld'))
          CALL check_err(NF90_ENDDEF(ncid))
          CALL check_err(NF90_PUT_VAR(ncid, var_id, pco2ld))
          CALL check_err(NF90_CLOSE(ncid))
       END IF
    END IF

    IF (MOD(istep, ents_itstp) == 0) THEN
       CALL carbt_diags(istep)
       CALL sealevel(istep, dum_rh0sc, dum_rhosc, dum_rsc, dum_ds, &
            & dum_dphi, dum_dsc, dum_saln0, dum_dz, dum_ec, dum_rho)
       CALL physt_diags(istep, dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, &
            & dum_evap, dum_pptn, dum_relh, albs_lnd, land_snow_lnd)
    END IF

    ! copy carbon arrays into genie_global arrays for rokgem
    dum_el_leaf = leaf
    dum_el_photo = photo
    dum_el_respveg = respveg
    dum_el_respsoil = respsoil

    ! albedo
    land_albs_nosnow_lnd = (fv * aveg) + ((1.0 - fv) * &
         & MAX(apeat, ((apeat - asand) * k10 * Csoil / (k8 - k9)) + asand))
    albs_lnd = land_albs_nosnow_lnd
    IF (snowswitch == 1) THEN
       land_albs_snow_lnd = ((asnow - asnowv) * EXP(-k7 * Cveg)) + asnowv
       WHERE (land_snow_lnd == 1) albs_lnd = land_albs_snow_lnd
    ELSE
       land_albs_snow_lnd = land_albs_nosnow_lnd
    END IF
    WHERE (ents_k1 > ents_kmax)
       bcap = MIN(k8, k9 + (k10 * Csoil))    ! field capacity
       z0 = MAX(0.001, kz0 * Cveg)           ! roughness length
    END WHERE
    land_bcap_lnd = REAL(bcap, KIND(land_bcap_lnd))
    land_z0_lnd = REAL(z0, KIND(land_z0_lnd))
    land_temp_lnd = REAL(tqld(1,:,:), KIND(land_temp_lnd))
    land_moisture_lnd = REAL(tqld(2,:,:), KIND(land_moisture_lnd))
  END SUBROUTINE step_ents


  ! Calculate change in sealevel height relative to a reference
  ! average density specified in goin_ents
  SUBROUTINE sealevel(istep, dum_rh0sc, dum_rhosc, dum_rsc, dum_ds, dum_dphi, &
       & dum_dsc, dum_saln0, dum_dz, dum_ec, dum_rho)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep
    REAL, INTENT(IN) :: dum_rh0sc, dum_rhosc, dum_rsc
    REAL, DIMENSION(maxj), INTENT(IN) :: dum_ds
    REAL, INTENT(IN) :: dum_dphi, dum_dsc, dum_saln0
    REAL, DIMENSION(ents_kmax), INTENT(IN) :: dum_dz
    REAL, DIMENSION(4), INTENT(IN) :: dum_ec
    REAL, DIMENSION(maxi,maxj,ents_kmax), INTENT(IN) :: dum_rho

    REAL :: vol, sumrho, avrho, mass, rho1, deltah
    REAL :: diagtime, vol1, areas
    INTEGER :: pts, i, j, k
    CHARACTER(LEN=200) :: filename, fname, label
    REAL :: var_data
    CHARACTER(LEN=6), DIMENSION(2) :: labels = (/ 'deltah', 'avrho ' /)
    INTEGER :: kk, myday

    diagtime = REAL(istep) / REAL(ents_nyear)
    vol = 0.0
    sumrho = 0.0
    pts = 0

    ! Open sealevel file for diagnostics
    filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.sealevel'
    OPEN(44,FILE=TRIM(filename),POSITION='APPEND')

    ! Sealevel rise due to thermal expansion
    DO i = 1, maxi
       DO j = 1, maxj
          DO k = 1, ents_kmax
             IF (k >= ents_k1(i,j)) THEN
                rho1 = (dum_rho(i,j,k) + &
                     & dum_saln0 * dum_ec(2)) * dum_rhosc + dum_rh0sc
                vol1 = dum_rsc * dum_rsc * dum_dphi * &
                     & dum_ds(1) * dum_dz(k) * dum_dsc
                vol = vol + vol1
                sumrho = sumrho + (rho1 * vol1)
             END IF
          END DO
          IF (ents_k1(i,j) <= ents_kmax) pts=pts+1
       END DO
    END DO

    avrho = sumrho / vol
    mass = sumrho
    areas = dum_rsc * dum_rsc * dum_dphi * dum_ds(1) * pts
    deltah = (vol / areas) * ((rhoref / avrho) - 1.0)

    WRITE (44,'(3e24.16)') diagtime, deltah, avrho
    CLOSE (44)

    myday = INT(360 * istep / ents_nyear)
    fname = TRIM(outdir_name) // TRIM(ents_out_name) // '_TS.nc'
    DO kk = 1, 2
       label = labels(kk)
       SELECT CASE (kk)
       CASE (1)
          var_data = deltah
       CASE (2)
          var_data = avrho
       END SELECT
       CALL netcdf_ts_ents(fname, var_data, label, myday)
    END DO
  END SUBROUTINE sealevel


  ! Calculate fluxes then update carbon reservoirs
  SUBROUTINE carbon(torog_atm, dum_co2_out, landice_slicemask_lic, &
       & sfxatm_lnd)
    IMPLICIT NONE
    REAL, DIMENSION(maxi,maxj), INTENT(IN) :: &
         & torog_atm, dum_co2_out, landice_slicemask_lic
    REAL, DIMENSION(maxi,maxj), INTENT(INOUT) :: sfxatm_lnd

    REAL, DIMENSION(maxi,maxj) :: tair, tlnd, qlnd
    REAL :: k1v, k1s, k1a
    INTEGER :: i, j

    ! Work out CO2 conc according to atchem (ppmv)

    ! The following statement assumes spatially uniform pCO2 in the atmosphere
    ! TODO: compute global average pCO2
    pco2ld = dum_co2_out(1,1) * rmtp

    ! Start land_pts loop
    DO i = 1, maxi
       DO j = 1, maxj
          ! Only calculate for non-ice land points
          IF (ABS(landice_slicemask_lic(i,j) - 1.0) < 1.0E-19) THEN
             ! Set up temperature and water arrays in Kelvin
             tair(i,j) = torog_atm(i,j) + tzero
             tlnd(i,j) = tqld(1,i,j) + tzero
             qlnd(i,j) = tqld(2,i,j)

             ! Calculate photosynthesis (kgC/m2/yr)
             CALL photosynthesis(Cveg(i,j), tair(i,j), pco2ld, qlnd(i,j), &
                  & bcap(i,j), fv(i,j), photo(i,j))

             ! Calculate plant respiration (kgC/m2/yr)
             CALL veg_resp(Cveg(i,j), tair(i,j), respveg(i,j))

             ! Calculate leaf turnover (kgC/m2/yr)
             CALL leaf_litter(Cveg(i,j), photo(i,j), respveg(i,j), &
                  & epsv(i,j), leaf(i,j))

             ! Calculate soil respiration (kgC/m2/yr) using land temp.
             CALL soil_resp(Csoil(i,j), tlnd(i,j), respsoil(i,j))

             ! vegetation carbon ODE
             k1v = dtland * (photo(i,j) - respveg(i,j) - leaf(i,j))

             ! soil carbon ODE
             k1s = dtland * (leaf(i,j) - respsoil(i,j))

             ! atmospheric carbon ODE
             k1a = dtland * (-photo(i,j) + respveg(i,j) + respsoil(i,j))

             ! Euler method update (kg/m2)
             Cveg(i,j) = Cveg(i,j) + k1v
             Csoil(i,j) = Csoil(i,j) + k1s

             IF (atchem_update) THEN
                ! Calculate CO2 fluxes for atchem coupler (mol/m2/s)
                sfxatm_lnd(i,j) = (-photo(i,j) + respveg(i,j) + &
                     & respsoil(i,j)) * rmu * rsyr
             ENDIF

             ! If not a land point don't calculate
          END IF
       END DO
    END DO
  END SUBROUTINE carbon


  !***************************************************************************
  ! Carbon flux subroutines
  !***************************************************************************

  ! Photosynthesis (kgC/m2/yr)
  SUBROUTINE photosynthesis(dum_cveg, dum_tair, dum_pco2, dum_qlnd, &
       & dum_bcap, dum_fv, dum_photo)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_cveg, dum_tair, dum_pco2, dum_qlnd, dum_bcap
    REAL, INTENT(OUT) :: dum_fv, dum_photo

    REAL :: fta, fws, fco2

    IF (dum_pco2 >= k13) THEN
       fco2 = (dum_pco2 - k13) / (dum_pco2 - k13 + k14)
    ELSE
       fco2 = 0.0
    END IF

    fws = MAX(0.0, MIN(1.0, ((4 * dum_qlnd / dum_bcap) - 2)))
    dum_fv = MAX(1.0E-5, 1.0 - EXP(-k17 * dum_cveg))

    fta = ((2.0**(0.1 * (dum_tair - topt))) / &
         &    ((1.0 + EXP( 0.3 * (dum_tair - k11))) * &
         &    ( 1.0 + EXP(-0.3 * (dum_tair - k12))))) &
         & +  ((2.0**(0.1 * (dum_tair - topt))) / &
         &    ((1.0 + EXP( 0.6 * (dum_tair - k11a))) * &
         &    ( 1.0 + EXP(-0.3 * (dum_tair - k12)))))

    dum_photo = k18 * rk19 * fco2 * fws * fta * dum_fv
  END SUBROUTINE photosynthesis


  ! vegetation respiration (kgC/m2/yr)
  SUBROUTINE veg_resp(dum_cveg, dum_tair, dum_respveg)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_cveg, dum_tair
    REAL, INTENT(OUT) :: dum_respveg

    REAL :: ftrv

    ftrv = rk25 * EXP(-k20 / (k21 * dum_tair))
    dum_respveg = k24 * ftrv * dum_cveg
  END SUBROUTINE veg_resp


  ! Leaf litter (kgC/m2/yr)
  SUBROUTINE leaf_litter(dum_cveg, dum_photo, dum_respveg, dum_epsv, dum_leaf)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_cveg, dum_photo, dum_respveg
    REAL, INTENT(OUT) :: dum_epsv, dum_leaf

    dum_epsv = 1.0 / (1.0 + EXP(k16 - dum_cveg))
    dum_leaf = (k26 * dum_cveg) + (dum_epsv * (dum_photo - dum_respveg))
  END SUBROUTINE leaf_litter


  ! Soil respiration (kgC/m2/yr)
  SUBROUTINE soil_resp(dum_csoil, dum_tlnd, dum_respsoil)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dum_csoil, dum_tlnd
    REAL, INTENT(OUT) :: dum_respsoil

    REAL :: ftrs

    IF (dum_tlnd >= tzero) THEN
       ftrs = EXP(-k31 / (dum_tlnd - k32))
    ELSE
       ftrs = k0 * q10**(0.1 * (dum_tlnd - tzero))
    END IF
    dum_respsoil = k29 * rk30 * ftrs * dum_csoil
  END SUBROUTINE soil_resp

END MODULE ents

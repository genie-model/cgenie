MODULE ents_lib

  IMPLICIT NONE
  SAVE

! For GOLDSTEIN ocean
#ifndef GOLDSTEINNLONS
#define GOLDSTEINNLONS 36
#endif
#ifndef GOLDSTEINNLATS
#define GOLDSTEINNLATS 36
#endif

  INTEGER, PARAMETER :: maxi=GOLDSTEINNLONS, maxj=GOLDSTEINNLATS
  INTEGER, PARAMETER :: maxnyr=220
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ents_k1
  INTEGER :: ents_kmax, ents_nyear
  REAL, DIMENSION(:), ALLOCATABLE :: ents_lat

  ! The par_output_years_0d and _2d, rstdir_name, start_year,
  ! opt_timeseries_output, opt_append_data vars are defined to be a
  ! similar type here as the parameters with similar names in the
  ! sedgem and rokgem namelists defined in their libs.  They are
  ! defined here because they are in the ents module in
  ! genie-main/src/xml-config/xml/definition.xml
  CHARACTER(LEN=3) :: lout
  CHARACTER(LEN=200) :: indir_name, outdir_name, condir_name, &
       & ents_out_name, ents_restart_file, ents_filenetin, ents_dirnetout
  CHARACTER(LEN=1) :: ents_netin, ents_netout, ents_ascout
  CHARACTER(LEN=255) :: rstdir_name
  CHARACTER(LEN=127) :: par_output_years_file_0d, par_output_years_file_2d

  INTEGER :: lenin, lenout, lencon, iav, ents_npstp
  INTEGER :: ents_igrid, ents_iwstp, ents_itstp, ents_ianav
  REAL :: ents_yearlen, start_year
  CHARACTER(LEN=1) :: ents_restart

  INTEGER :: iniday

  ! Run-time seasonality option
  LOGICAL :: dosc

  LOGICAL :: atchem_fert, atchem_update

  ! These 'opt_' variables are defined here because they appear in
  ! genie-main/src/xml-config/xml/definition.xml
  LOGICAL :: opt_timeseries_output, opt_append_data


  ! Tunable Constants
  REAL :: k0, k7, k8, k9, k10, k11, k11a, k12, k13, k14, k16, k17, k18, &
       & k20, k21, k24, k26, k29, k31, k32, kz0, rk19, rk25, rk30, q10, &
       & topt, copt

  ! 'Proper' constants
  REAL, PARAMETER :: k_a = 1.773e20       ! Moles of molecules in the atm
  REAL, PARAMETER :: gtm = 1.0E15 / 12.0  ! Conversion (GtC to moles C)
  REAL, PARAMETER :: gtk = 1.0E12         ! Conversion (GtC to kgC)
  REAL, PARAMETER :: rgtm = 12.0 / 1.0E15 ! Conversion (moles C to GtC)
  REAL, PARAMETER :: rgtk = 1.0E-12       ! Conversion (kgC to GtC)
  REAL, PARAMETER :: mtp = 1.0E-6         ! ppmv to ppv
  REAL, PARAMETER :: rmtp = 1.0E6         ! ppv to ppmv
  REAL, PARAMETER :: tzero = 273.15
  REAL, PARAMETER :: mu = 0.012           ! molar mass of carbon (kgC/molC)
  REAL, PARAMETER :: rmu = 1.0 / mu
  REAL :: asurfrea                        ! gridbox area (m2)
  REAL :: rasurf                          ! reciprocal of above
  REAL :: rsyr                            ! reciprocal of secs/yr

  ! Model specific constants
  INTEGER, PARAMETER :: land_pts_ents = 362      ! Number of land points
  INTEGER :: msimpleland        ! How often simple land called (units of istep)

  ! Carbon reservoirs
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & Cveg,  &              ! Veg carbon reservoir (kgC/m2)
       & Csoil, &              ! Soil carbon reservoir (kgC/m2)
       & fv,    &              ! Fractional veget. cover (/gridbox)
       & epsv                  ! Veget. shelf shading frac. (/gridbox)
  REAL :: pco2ld                   ! Atmospheric pCO2 (ppmv)

  REAL :: dtland                   ! time (seconds) of each land timestep

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & leaf,     &            ! leaf litter (kgC/m2/yr)
       & respveg,  &            ! veget. respiration (kgC/m2/yr)
       & respsoil, &            ! soil respiration (kgC/m2/yr)
       & photo                  ! photosynthesis (kgC/m2/yr)

  ! Arrays used to calculate .sland.avg files.  Used in annav_diags
  ! (these arrays only used to calculate .avg files)
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & sphoto,  &          ! summed photosynth
       & srveg,   &          ! summed veg resp
       & sleaf,   &          ! summed leaf litter
       & srsoil,  &          ! summed soil resp
       & sCveg1,  &          ! summed veg carbon
       & sCsoil1, &          ! summed soil carbon
       & sfv1,    &          ! summed veg fraction
       & sepsv1,  &          ! summed self shading
       & sfx0a,   &          ! summed flux into atmosphere over land
       & sfx0o,   &          ! summed flux into land
       & sfxsens, &          ! summed sensible heat flux over land
       & sfxlw,   &          ! summed net longwave heat flux over land
       & sevap,   &          ! summed evapotranspiration over land
       & spptn,   &          ! summed pptn over land
       & srelh,   &          ! summed relative humidity over land
       & sbcap,   &          ! summed soil field capacity
       & salbs,   &          ! summed albedo over land
       & ssnow,   &          ! summed snow over land
       & sz0                 ! summed roughness length over land
  REAL :: &
       & pco2ld_tot, &       ! for use in timeseries summing
       & tot_mass_ocn_c      ! for use in timeseries summing
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & stqld               ! summed land temp (1) and water (2)

  ! Land radiation and hydrology arrays
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       &  tqld, &            ! land temp(1) oC and bucket fullness(2) m
       & tqldavg             ! avg version of above
  REAL, DIMENSION(maxi,maxj) :: &
       & bcap                 ! bucket capacity m
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & bcapavg, &           ! avg bucket capacity (m)
       & snowavg, &           ! avg fractional snow cover
       & z0avg,   &           ! avg roughness length (m)
       & albsavg, &           ! average surface albedo
       & z0,      &           ! roughness length (m)
       & evapavg, &           ! average evapotranspiration (m/s)
       & pptnavg, &           ! average pptn (m/s)
       & runavg,  &           ! average runoff (m/s)
       & fvfv                 ! vegetation fraction for fixed vegetation
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & fxavg                ! avg heat fluxes (W/m2)/timescale avg over
  REAL :: albedol, &          ! land albedo
        & dtdim,   &          ! length of ocean timestep (s)
        & lambda,  &          ! decay timescale for new pptn scheme
        & asnow,   &          ! snow albedo
        & asnowv,  &          ! snow with veg present albedo
        & aveg,    &          ! vegetation albedo
        & apeat,   &          ! peat albedo
        & asand,   &          ! sand albedo
        & gmairttot           ! summed global mean temperature

  ! Switches and datasets
  REAL :: rhoref,  &          ! Ref. average ocean density for sea-level change
        & glairts, &          ! summed air temp over Greenland
        & glairt,  &          ! annual average air temp over Greenland
        & isslold, &          ! last year's sealevel change from Greenland melt
        & issl,    &          ! sealevel change from Greenland melt
        & glmelt,  &          ! annual mean runoff from Greenland melt (m/s)
        & isslfwf, &          ! Same as issl if FW added to ocean
        & glairtini           ! initial air temperature over Greenland

  INTEGER :: snowswitch, &    ! turns on/off snow albedo feedbacks on climate
           & icemeltfwfswitch ! turns on/off Greenland FW melt added to ocean

  CHARACTER :: include_emissions  ! y/n for emissions to be included or not

CONTAINS

  ! Convert number to string and pad it with front zeros, used for
  ! forming file names; the output should be trimmed to get rid of
  ! blanks in case of months/days
  FUNCTION ConvertFunc(innumber, switch) RESULT(outname)
    IMPLICIT NONE
    INTEGER :: innumber, switch
    CHARACTER(LEN=10) :: outname

    CHARACTER(LEN=10) :: buf, flag
    INTEGER :: j, bd

    bd = 0
    IF (switch == 10) THEN
       flag = '(I10)'
       bd = 10
       WRITE (buf,10) innumber
10     FORMAT (I10)
    ELSE IF (switch == 2) THEN
       flag = '(I2)'
       bd = 2
       WRITE (buf,20) innumber
20     FORMAT (I2)
    ELSE IF (switch == 4) THEN
       flag = '(I4)'
       bd = 4
       WRITE (buf,30) innumber
30     FORMAT (I4)
    END IF

    DO j = 1, bd
       IF (buf(j:j) == ' ') buf(j:j) = '0'
    END DO
    outname = TRIM(buf)
  END FUNCTION ConvertFunc

END MODULE ents_lib

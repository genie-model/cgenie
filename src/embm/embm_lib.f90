MODULE embm_lib

  IMPLICIT NONE
  SAVE

  INTEGER :: maxi, maxj, maxk, maxnyr
  INTEGER :: ntot, intot, nyear
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: k1, mk
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ku

  REAL :: phi0, dphi, time, diff(2), ec(4)
  REAL, DIMENSION(:), ALLOCATABLE :: dt, ds, dsv, rds2, dz, s, c, sv
  REAL, DIMENSION(:,:), ALLOCATABLE :: dzu
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: tau, drag, dztau
  REAL :: ez0, sda1, sdomg, dzz, t0
  REAL, DIMENSION(:), ALLOCATABLE :: cv, dza, tsa0
  REAL, DIMENSION(:,:), ALLOCATABLE :: tau0, dztav0, tau1, dztav1
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: dztav
  ! Reciprocal and other variables to speed up fortran
  REAL :: rdphi
  REAL, DIMENSION(:), ALLOCATABLE :: rc, rcv, rds, rdsv, cv2, rc2, rdz, rdza
  REAL, DIMENSION(:,:), ALLOCATABLE :: rtv, rtv3

  ! dztau and dztav are used in c-GOLDSTEIN to hold the d(tau)/dz fields
  ! at, respectively, the u and v points of the grid.  Values are read
  ! into these variables from data files and then scaled.  x and y wind
  ! stresses, variable tau, are then calculated.  In the genie.f version
  ! of GOLDSTEIN it appears necessary for GOLDSTEIN to receive dztau and
  ! dztav as well as tau, so unscaled fields of dztau and dztav are
  ! passed between modules.  In the case of the EMBM and surflux, this
  ! means that the unscaled fields need a separate identity to the
  ! scaled fields, hence these new variable names
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: us_dztau, us_dztav

  ! Dimensional scale values
  REAL, PARAMETER :: pi = 4 * ATAN(1.0)
  REAL, PARAMETER :: usc = 0.05, rsc = 6.37E6, dsc = 5.0E3, fsc = 2*7.2921E-5
  REAL, PARAMETER :: gsc = 9.81, rh0sc = 1.0E3
  REAL, PARAMETER :: rhosc = rh0sc * fsc * usc * rsc / gsc / dsc
  REAL, PARAMETER :: tsc = rsc / usc
  REAL, PARAMETER :: cpsc = 3981.1
  REAL, PARAMETER :: rhoair = 1.25, rho0 = 1.0E3, rhoao = rhoair / rho0

  ! EMBM scaling for heat forcing of ocean
  REAL, PARAMETER :: rfluxsc = rsc / (dsc * usc * rh0sc * cpsc)

  ! EMBM reference salinity
  REAL, PARAMETER :: saln0 = 34.9

  ! EMBM scaling for freshwater forcing of ocean
  REAL,PARAMETER :: rpmesco = rsc * saln0 / (dsc * usc)

  REAL, PARAMETER :: cpa = 1004.0
  REAL, PARAMETER :: hlv = 2.501E6    ! Latent heat of vapourization (J/kg)
  REAL, PARAMETER :: hlf = 3.34E5     ! Latent heat of fusion of ice (J/kg)
  REAL, PARAMETER :: hls = hlv + hlf  ! Latent heat of sublimation (J/kg)

  ! Reference greenhouse gas concentrations
  REAL, PARAMETER :: co20 = 278.0E-6, ch40 = 700.0E-9, n2o0 = 275.0E-9

  ! gas equation alpha values (as per IPCC [2001])
  REAL, PARAMETER :: alphach4 = 0.036, alphan2o = 0.12

  ! grid cell area
  REAL, DIMENSION(:), ALLOCATABLE :: asurf

  ! Constants used in OLW parameterization
  REAL, PARAMETER :: b00 = 2.43414E2, b10 = -3.47968E1, b20 = 1.02790E1
  REAL, PARAMETER :: b01 = 2.60065, b11 = -1.62064, b21 = 6.34856E-1
  REAL, PARAMETER :: b02 = 4.40272E-3, b12 = -2.26092E-2, b22 = 1.12265E-2
  REAL, PARAMETER :: b03 = -2.05237E-5, b13 = -9.67000E-5, b23 = 5.62925E-5

  ! Emissivities
  REAL, PARAMETER :: sigma = 5.67E-8
  REAL, PARAMETER :: emo = 0.94 * sigma, eml = 0.94 * sigma, ema = 0.85 * sigma
  REAL, PARAMETER :: tfreez = 0.0

  ! Constants for saturation specific humidity (Bolton 1980)
  REAL, PARAMETER :: const1 = 3.80E-3, const2 = 21.87, const3 = 265.5
  REAL, PARAMETER :: const4 = 17.67, const5 = 243.5

  ! Parameters for extra heat diffusion where pptn high
  REAL, PARAMETER :: diffmod0 = 0.0

  REAL, PARAMETER :: cd = 0.0013


  INTEGER :: igrid, ndta
  REAL :: dtatm, ghs, rdtdim, scf, z1_embm
  REAL, DIMENSION(:,:), ALLOCATABLE :: qsata, qsato, co2, ch4, n2o
  REAL, DIMENSION(:,:), ALLOCATABLE :: solfor
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: tq, tq1, varice, varice1, tqa

  REAL :: rfluxsca, delf2x, &
       & rate_co2, rate_ch4, rate_n2o, &
       & ryear, olr_adj0, olr_adj, t_eqm, aerofac, volfac, solfac

  REAL :: betam(2), betaz(2), hatmbl(2)
  REAL, dimension(:,:), allocatable :: albcl, fxsw, fxplw, fx0a, fx0o, fxsen, &
       & pmeadj, pptn, evap, usurf, fxlata, fxlato, fxlw, ca, qb, qbsic
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: diffa
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & fx0sic, fx0neto, fwfxneto, evapsic, tsfreez

  ! Forcing stuff
  LOGICAL :: useforc
  CHARACTER(LEN=20) :: forcname

  ! Adjustable freshwater forcing parameters
  REAL :: extra1(3)
  ! Constants and parameters
  REAL :: rmax, rpmesca, ppmin, ppmax
  ! Prescribed/diagnosed atmospheric transports and velocities
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: uatm

  ! Constants and parameters for sea ice
  REAL, PARAMETER :: tsic = -1.8
  REAL, PARAMETER :: consic = 2.166  ! constant ice conductivity (W/m/K)
  REAL, PARAMETER :: rhoice = 913.0  ! representative ice density (kg/m**3)
  REAL, PARAMETER :: hmin = 0.01     ! min. ave. sea-ice thickness
  REAL, PARAMETER :: rhmin = 1.0 / hmin
  REAL, PARAMETER :: rhooi = rho0 / rhoice
  REAL, PARAMETER :: rhoio = rhoice/rho0
  REAL, PARAMETER :: rrholf = 1.0 / (rhoice * hlf) ! melting factor
  REAL :: rsictscsf, rho0sea, diffsic

  ! adjustable freshwater forcing parameters
  INTEGER :: nsteps_extra0
  REAL :: extra0, range0, extra1a, extra1b, extra1c

  ! Days per year
  REAL :: yearlen

  ! Seasonal diagnostics
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & fxlatavg, fxsenavg, fxswavg, fxlwavg, fwpptavg, fwevpavg
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: tqavg, fx0avg, fwavg

  INTEGER(KIND=8) :: nsteps
  INTEGER :: npstp, iwstp, itstp, iw, ianav
  INTEGER :: lenin, lenout, lenrst, iav

  ! Input and output directory locations
  CHARACTER(LEN=3) :: lout
  CHARACTER(LEN=100) :: indir_name, outdir_name

  ! Names of data files for error calculation
  CHARACTER(LEN=128) :: tdatafile, qdatafile
  CHARACTER(LEN=25) :: tdata_varname , qdata_varname
  REAL :: tdata_missing, tdata_scaling, tdata_offset
  REAL :: qdata_missing, qdata_scaling, qdata_offset
  LOGICAL :: tqinterp, qdata_rhum
  INTEGER :: lentdata, lenqdata, lentvar, lenqvar
  LOGICAL :: flat

  ! Variables to convert FW fluxes mm/s <--> m/s
  REAL, PARAMETER :: m2mm = 1000.0, mm2m = 1.0 / m2mm

  ! For netcdf restarts
  CHARACTER(LEN=200) :: filenetin, dirnetout, rstdir_name
  INTEGER :: iyear_rest, imonth_rest, ioffset_rest
  REAL :: day_rest

  ! Flags
  CHARACTER(LEN=1) :: atchem_radfor, orbit_radfor

  ! Orbital parameters for albedo calculation
  CHARACTER(LEN=200) :: filenameorbit
  INTEGER :: t_orbit, norbit,orbitsteps
  REAL, DIMENSION(:), ALLOCATABLE :: &
       & orbitecc_vect, orbitobl_vect, orbitpre_vect, orbittau_vect

  ! CO2 time series
  CHARACTER(LEN=200) :: filenameco2
  INTEGER :: t_co2, nco2, co2steps
  REAL, DIMENSION(:), ALLOCATABLE :: co2_vect

  ! Albedo paramters from ENTS
  REAL, DIMENSION(:,:), ALLOCATABLE :: albo, palb, palbavg

  ! Run-time seasonality and debug options
  LOGICAL :: dosc, debug_init, debug_end, debug_loop

  ! Orography
  REAL :: lapse_rate
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: orog_vect
  INTEGER :: orogswitch, t_orog, norog, orogsteps
  CHARACTER(LEN=200) :: filenameorog

  ! Land ice sheet
  CHARACTER(LEN=200) :: filenamelice
  INTEGER :: t_lice, nlice, licesteps
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: lice_vect
  REAL :: lice_k9

  ! d18o derived orography and ice sheet
  CHARACTER(LEN=200) :: filenamed18o, filenamed18oicethresh, &
       & filenamed18oorogmin, filenamed18ooroggrad
  INTEGER :: t_d18o, nd18o, d18osteps
  REAL, DIMENSION(:), ALLOCATABLE :: d18o_vect
  REAL, DIMENSION(:,:), ALLOCATABLE :: &
       & d18o_ice_thresh, d18o_orog_min, d18o_orog_grad
  REAL :: d18o_k, scale_mwfx

  ! Interpolated seasonal fields
  INTEGER :: ents_seasonswitch, ents_offlineswitch
  INTEGER, PARAMETER :: nmth=12
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: uatml  ! u and v wind comp.'s
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
       & usurfl,  &  ! windspeed (m/s)
       & tncep,   &  ! NCEP air temperature (oC)
       & pncep,   &  ! NCEP pptn (m/s)
       & rhncep,  &  ! NCEP RH at 1000mb (%)
       & atm_alb     ! atmospheric albedo

  ! Transfer coefficients for land grid boxes
  REAL, DIMENSION(:,:), ALLOCATABLE :: chl, cel

  ! Precipitation timescale and land radiation
  REAL :: lambdapptn, rhcld

  ! Diagnostics of precipitation-adjusted specific and relative humidity
  ! (i.e., specific and relative humidity after precipitation)
  REAL, DIMENSION(:,:), ALLOCATABLE :: q_pa, rq_pa, q_pa_avg, rq_pa_avg

  ! Integer arrays for runoff scheme
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: iroff, jroff
  ! ENTS runoff scheme
  REAL :: par_runoff_b, par_runoff_tau, runoff_factor_1, runoff_factor_2
  INTEGER :: par_runoff_scheme

  ! Option to get get rid of the conditional zonal averaging of winds
  ! near the poles
  INTEGER :: par_wind_polar_avg, unify_winds
  ! Sea-ice dynamics parameter control: max sea-ice thickness (m)
  REAL :: par_sich_max, par_albsic_min, par_albsic_max

END MODULE embm_lib

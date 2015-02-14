MODULE embm_lib

  IMPLICIT NONE
  SAVE

#ifndef GOLDSTEINNLONS
#define GOLDSTEINNLONS 36
#endif
#ifndef GOLDSTEINNLATS
#define GOLDSTEINNLATS 36
#endif
#ifndef GOLDSTEINNLEVS
#define GOLDSTEINNLEVS 8
#endif

  INTEGER, PARAMETER :: maxi=GOLDSTEINNLONS, maxj=GOLDSTEINNLATS
  INTEGER, PARAMETER :: maxk=GOLDSTEINNLEVS, maxl=2
  INTEGER, PARAMETER :: maxnyr=400
  INTEGER :: imax,jmax,kmax,lmax,ntot,intot,k1(0:maxi+1,0:maxj+1)
  INTEGER :: ku(2,maxi,maxj),mk(maxi+1,maxj),nyear

  REAL :: dt(maxk),phi0,dphi,ds(maxj),dsv(1:maxj-1),rds2(2:maxj-1), &
       & dz(maxk),time,s(0:maxj),c(0:maxj),dzu(2,maxk) &
       & ,tau(2,maxi,maxj),drag(2,maxi+1,maxj),dztau(2,maxi,maxj) &
       & ,diff(2),ec(4),sv(0:maxj)
  REAL :: cv(0:maxj),dza(maxk),dztav(2,maxi,maxj),ez0 &
       & ,sda1,sdomg,dzz,tau0(maxi,maxj),dztav0(maxi,maxj) &
       & ,tau1(maxi,maxj),dztav1(maxi,maxj),tsa0(maxj),t0
  ! reciprocal and other variables to speed up fortran
  REAL ::  rc(0:maxj),rcv(1:maxj-1),rdphi,rds(maxj),rdsv(1:maxj-1) &
       & ,cv2(1:maxj-1) &
       & ,rc2(0:maxj),rtv(maxi,maxj),rtv3(maxi,maxj),rdz(maxk),rdza(maxk)

  ! dztau and dztav are used in c-GOLDSTEIN to hold the d(tau)/dz fields
  ! at, respectively, the u and v points of the grid.  Values are read
  ! into these variables from data files and then scaled.  x and y wind
  ! stresses, variable tau, are then calculated.  In the genie.f version
  ! of GOLDSTEIN it appears necessary for GOLDSTEIN to receive dztau and
  ! dztav as well as tau, so unscaled fields of dztau and dztav are
  ! passed between modules.  In the case of the EMBM and surflux, this
  ! means that the unscaled fields need a separate identity to the
  ! scaled fields, hence these new variable names
  REAL :: us_dztau(2, maxi, maxj), us_dztav(2, maxi, maxj)

  ! dimensional scale values
  REAL :: usc,rsc,dsc,fsc,gsc,rh0sc,rhosc,cpsc,tsc,pi

  ! grid cell area
  REAL :: asurf(maxj)

  INTEGER :: igrid, ndta
  REAL :: cd,tq(2,maxi,maxj),tq1(2,maxi,maxj) &
       & ,qsata(maxi,maxj),qsato(maxi,maxj) &
       & ,co2(maxi,maxj),ch4(maxi,maxj),n2o(maxi,maxj) &
       & ,varice(2,maxi,maxj),varice1(2,maxi,maxj),dtatm &
       & ,tqa(2,maxi,maxj),solfor(maxj,maxnyr),ghs,rdtdim,scf &
       & ,z1_embm

  REAL :: emo, &
       &     eml, &               !< land emissivity
       &     ema &
       & ,tfreez,rfluxsc,rfluxsca &
       & ,b00,b10,b20,b01,b11,b21,b02,b12,b22,b03,b13,b23,delf2x &
       & ,co20,ch40,n2o0 &
       & ,rate_co2,rate_ch4,rate_n2o &
       & ,ryear,olr_adj0,olr_adj,t_eqm,aerofac,volfac,solfac

  real :: albcl(maxi,maxj) &
       & ,fxsw(maxi,maxj),fxplw(maxi,maxj) &
       & ,fx0a(maxi,maxj),fx0o(maxi,maxj) &
       & ,fxsen(maxi,maxj),pmeadj(maxi,maxj) &
       & ,pptn(maxi,maxj),evap(maxi,maxj),usurf(maxi,maxj) &
       & ,fxlata(maxi,maxj),fxlato(maxi,maxj) &
       & ,fxlw(maxi,maxj) &
       & ,diffa(2,2,maxj),betam(2),betaz(2),hatmbl(2) &
       & ,ca(maxi,maxj) &
       & ,qb(maxi,maxj),qbsic(maxi,maxj)
  real :: fx0sic(maxi,maxj) &
       & ,fx0neto(maxi,maxj),fwfxneto(maxi,maxj) &
       & ,evapsic(maxi,maxj),tsfreez(maxi,maxj)
  ! forcing stuff
  logical useforc
  character forcname*20

  ! adjustable freshwater forcing parameters
  real extra1(3)
  ! constants and parameters
  real rhoair,rhoao,cpa &
       & ,rho0,hlv,hls,hlf,const1,const2,const3,const4,const5,rmax &
       & ,saln0,rpmesca,rpmesco &
       & ,diffmod0,ppmin,ppmax
  ! prescribed/diagnosed atmospheric transports and velocities
  real uatm(2,maxi,maxj)
  ! constants and parameters for sea ice
  real rsictscsf,rhoice,rho0sea,consic,diffsic &
       & ,tsic,hmin,rhoio,rhooi,rrholf,rhmin
  ! adjustable freshwater forcing parameters
  integer nsteps_extra0
  real extra0,range0,extra1a,extra1b,extra1c

  ! days per year
  real yearlen

  ! seasonal diagnostics
  real tqavg(2,maxi,maxj),fxlatavg(maxi,maxj),fxsenavg(maxi,maxj), &
       &     fxswavg(maxi,maxj),fxlwavg(maxi,maxj),fwpptavg(maxi,maxj), &
       &     fwevpavg(maxi,maxj)
  real fx0avg(4,maxi,maxj), fwavg(2,maxi,maxj)

  integer(kind=8) :: nsteps
  integer npstp, iwstp, itstp, iw, ianav
  integer lenin, lenout, lenrst, iav


  ! input and output directory locations
  character lout*3, indir_name*100, outdir_name*100

  ! names of data files for error calculation
  character tdatafile*128, qdatafile*128,tdata_varname*25, &
       &       qdata_varname*25
  real tdata_missing,tdata_scaling,tdata_offset
  real qdata_missing,qdata_scaling,qdata_offset
  logical tqinterp,qdata_rhum
  integer   lentdata, lenqdata, lentvar, lenqvar
  logical flat

  ! variables to convert FW fluxes mm/s <--> m/s
  real m2mm, mm2m

  ! For netcdf restarts
  character filenetin*200,dirnetout*200,rstdir_name*200
  logical lnetin,lnetout,lascout
  integer iyear_rest,imonth_rest,ioffset_rest
  real day_rest

  ! Flags
  character atchem_radfor*1
  character orbit_radfor*1

  ! Orbital parameters for albedo calculation
  integer, parameter :: en_ntimes_max=2000
  character*200 filenameorbit
  integer t_orbit,norbit,orbitsteps
  real orbitecc_vect(en_ntimes_max)
  real orbitobl_vect(en_ntimes_max)
  real orbitpre_vect(en_ntimes_max)
  real orbittau_vect(en_ntimes_max)

  ! CO2 times series
  character*200 filenameco2
  integer t_co2,nco2,co2steps
  real co2_vect(en_ntimes_max)

  ! Albedo paramters from ENTS
  REAL :: albo(maxj,maxnyr) &          ! ocean albedo
       & ,palb(maxi,maxj) &            ! planetary albedo
       & ,palbavg(maxi,maxj)         ! average planetary albedo

  ! Run-time seasonality option
  logical dosc,debug_init,debug_end,debug_loop

  ! Radiative forcing by CH4 and N2O
  real alphach4,alphan2o

  ! Orography
  real lapse_rate
  real orog_vect(maxi,maxj,en_ntimes_max)
  integer orogswitch,t_orog,norog,orogsteps
  character*200 filenameorog

  ! Land ice sheet
  character*200 filenamelice
  integer t_lice,nlice,licesteps
  real lice_vect(maxi,maxj,en_ntimes_max),lice_k9

  ! d18o derived orography and ice sheet
  character*200 filenamed18o,filenamed18oicethresh
  character*200 filenamed18oorogmin,filenamed18ooroggrad
  integer t_d18o,nd18o,d18osteps
  real d18o_vect(en_ntimes_max)
  real d18o_ice_thresh(maxi,maxj)
  real d18o_orog_min(maxi,maxj)
  real d18o_orog_grad(maxi,maxj)
  real d18o_k,scale_mwfx

  ! Interpolated seasonal fields
  integer ents_seasonswitch,ents_offlineswitch,nmth
  parameter(nmth=12)
  real uatml(2,maxi,maxj,maxnyr), &  ! u and v wind comp.'s
       & usurfl(maxi,maxj,maxnyr),  &  ! windspeed (m/s)
       & tncep(maxi,maxj,maxnyr),   &  ! NCEP air temperature (oC)
       & pncep(maxi,maxj,maxnyr),   &  ! NCEP pptn (m/s)
       & rhncep(maxi,maxj,maxnyr),  &  ! NCEP RH at 1000mb (%)
       & atm_alb(maxi,maxj,maxnyr)   ! atmospheric albedo

  ! Transfer coefficients for land grid boxes
  real chl(maxi,maxj), &             ! transfer coefficient
       & cel(maxi,maxj)              ! ditto

  ! Precipitation timescale and land radiation
  real lambdapptn, &
       & rhcld ! scale used to calculate r.o.c. land temp.

  ! Diagnostics of precipitation-adjusted specific and relative humidity
  ! (i.e., specific and relative humidity after precipitation)
  real q_pa(maxi,maxj),rq_pa(maxi,maxj),q_pa_avg(maxi,maxj), &
       & rq_pa_avg(maxi,maxj)

  ! Integer arrays for runoff scheme
  integer iroff(maxi,maxj),jroff(maxi,maxj)
  ! ENTS runoff scheme
  real par_runoff_b
  real par_runoff_tau
  real runoff_factor_1
  real runoff_factor_2
  integer par_runoff_scheme

  ! Option to get get rid of the conditional zonal averaging of winds
  ! near the poles
  integer par_wind_polar_avg
  integer unify_winds
  ! Sea-ice dynamics parameter control: max sea-ice thickness (m)
  real par_sich_max
  real par_albsic_min,par_albsic_max

END MODULE embm_lib

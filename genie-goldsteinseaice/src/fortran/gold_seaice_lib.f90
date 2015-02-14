MODULE gold_seaice_lib

  IMPLICIT NONE
  SAVE

! For GOLDSTEIN seaice
#ifndef GOLDSTEINNLONS
#define GOLDSTEINNLONS 36
#endif
#ifndef GOLDSTEINNLATS
#define GOLDSTEINNLATS 36
#endif
#ifndef GOLDSTEINNLEVS
#define GOLDSTEINNLEVS 8
#endif

  INTEGER, PARAMETER :: maxi = GOLDSTEINNLONS
  INTEGER, PARAMETER :: maxj = GOLDSTEINNLATS
  INTEGER, PARAMETER :: maxk = GOLDSTEINNLEVS

  ! common /sic_invars/imax,jmax,kmax,k1,nyear
  INTEGER :: imax, jmax, kmax, k1(0:maxi+1,0:maxj+1), nyear

  ! common /sic_vars/dtsic,phi0,dphi,ds,dsv,rds2,u,s,c,sv,cv,t0
  REAL, DIMENSION(0:maxj) :: s, c, sv, cv
  REAL :: dtsic, phi0, dphi, ds(maxj), dsv(1:maxj-1), rds2(2:maxj-1), &
       & u(2,0:maxi,0:maxj), t0
  ! Reciprocal and other variables to speed up fortran
  ! common /sic_recips/rc,rcv,rdphi,rds,rdsv,cv2,rc2
  REAL, DIMENSION(0:maxj) :: rc, rcv, cv2, rc2
  REAL :: rdphi, rds(maxj), rdsv(1:maxj-1)

  ! Dimensional scale values
  ! common /sic_dimsc/usc,rsc,dsc,fsc,gsc,rh0sc,rhosc,cpsc,tsc,pi
  REAL, PARAMETER :: pi = 4 * ATAN(1.0)
  REAL, PARAMETER :: usc = 0.05, rsc = 6.37E6, dsc = 5.0E3, fsc = 2*7.2921E-5
  REAL, PARAMETER :: gsc = 9.81, rh0sc = 1.0E3
  REAL, PARAMETER :: rhosc = rh0sc * fsc * usc * rsc / gsc / dsc
  REAL, PARAMETER :: tsc = rsc / usc
  REAL, PARAMETER :: cpsc = 3981.1

  ! Grid cell area.
  ! common /sic_area/asurf
  REAL, DIMENSION(maxj) :: asurf

  ! common /sic_ebmvar/varice,varice1,tice,albice,dtatm,ghs,rdtdim, &
  !      & ryear,dtha,rho0,hlf,rhoice,rho0sea,diffsic,hmin,rhoio, &
  !      & rhooi,rhmin,varicedy,variceth
  REAL, DIMENSION(2,maxi,maxj) :: varice, varice1, dtha, varicedy, variceth
  REAL, DIMENSION(maxi,maxj) :: tice, albice
  REAL, PARAMETER :: rho0 = 1.0E3, rhoice = 913.0
  REAL, PARAMETER :: hmin = 0.01, rhmin = 1.0 / hmin
  REAL :: dtatm, ghs, rdtdim, ryear, rho0sea, diffsic, rhoio, rhooi

  ! latent heat of fusion of ice (J/kg)
  REAL, PARAMETER :: hlf = 3.34e5

  ! Days per year (necessary for GENIE).
  ! common /sic_yearlen/yearlen
  REAL :: yearlen

  ! v2 seasonal diagnostics
  ! common /sic_oscavg/haavg,ticeavg,albiceavg,dthaavg,fxdelavg,fwdelavg
  REAL, DIMENSION(2,maxi,maxj) :: haavg, dthaavg
  REAL, DIMENSION(maxi,maxj) :: ticeavg, albiceavg, fxdelavg ,fwdelavg

  ! common /sicgint/nsteps, npstp, iwstp, itstp, iw, ianav, &
  !      & lenin, lenout, lenrst, iav
  INTEGER(KIND=8) :: nsteps
  INTEGER :: npstp, iwstp, itstp, iw, ianav
  INTEGER :: lenin, lenout, lenrst, iav

  ! Input and output directory locations.
  ! common /sicgchar/lout,indir_name,outdir_name
  CHARACTER(LEN=3) :: lout
  CHARACTER(LEN=100) :: indir_name ,outdir_name

  ! Variables to convert FW fluxes mm/s <--> m/s
  ! common /sicfwconv/m2mm,mm2m
  REAL, PARAMETER :: m2mm = 1000.0, mm2m = 1.0 / m2mm

  ! For netcdf restarts
  ! common /sic_restarts/filenetin,dirnetout,lnetin,lnetout,lascout,&
  !      & rstdir_name,iyear_rest,imonth_rest,ioffset_rest,day_rest
  CHARACTER(LEN=200) :: filenetin, dirnetout, rstdir_name
  LOGICAL :: lnetin, lnetout, lascout
  INTEGER :: iyear_rest, imonth_rest, ioffset_rest
  REAL :: day_rest

  ! for goldseaice energy/water check:
  ! common /conservcheck/conserv_per
  INTEGER :: conserv_per

  ! Run-time seasonality option; debugging options
  ! common /sic_ctrl/dosc,impsic,debug_init,debug_end,debug_loop
  LOGICAL :: dosc, impsic, debug_init, debug_end, debug_loop

  ! Sea-ice dynamics parameter control:
  ! fractional area threshold for blocking sea-ice advection
  ! sea-ice thickness threshold for blocking sea-ice advection
  ! common /sic_dyn/par_sica_thresh,par_sich_thresh
  REAL :: par_sica_thresh, par_sich_thresh
END MODULE gold_seaice_lib

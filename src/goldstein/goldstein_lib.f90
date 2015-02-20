MODULE goldstein_lib

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
#ifndef GOLDSTEINNTRACS
#define GOLDSTEINNTRACS 2
#endif
#ifndef GOLDSTEINMAXISLES
#define GOLDSTEINMAXISLES 5
#endif

  INTEGER, PARAMETER :: maxi=GOLDSTEINNLONS, maxj=GOLDSTEINNLATS
  INTEGER, PARAMETER :: maxk=GOLDSTEINNLEVS, maxl=GOLDSTEINNTRACS
  INTEGER, PARAMETER :: maxnyr=720
  INTEGER, PARAMETER :: mpxi=maxi, mpxj=maxj+1
  INTEGER, PARAMETER :: maxisles=GOLDSTEINMAXISLES, mpi=2 * (maxi + maxj)

  INTEGER :: isles
  INTEGER :: imax, jmax, kmax, lmax, ntot, intot
  INTEGER :: k1(0:maxi+1,0:maxj+1), ku(2,maxi,maxj), mk(maxi+1,maxj), nyear
  INTEGER :: ips(maxj), ipf(maxj), ias(maxj), iaf(maxj), jsf
  INTEGER :: lpisl(mpi,maxisles), ipisl(mpi,maxisles), jpisl(mpi,maxisles)
  INTEGER :: npi(maxisles)

  REAL :: dt(maxk), phi0, dphi, ds(maxj), dsv(1:maxj-1), rds2(2:maxj-1)
  REAL :: dz(maxk), u(3,0:maxi,0:maxj,maxk), ts(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
  REAL :: s(0:maxj), c(0:maxj), dzu(2,maxk), tau(2,maxi,maxj)
  REAL :: drag(2,maxi+1,maxj), dztau(2,maxi,maxj), diff(2), ec(5), cn
  REAL :: ratm(mpxi*mpxj,mpxi+1), ub(2,0:maxi+1,0:maxj)
  REAL :: rho(0:maxi+1,0:maxj+1,0:maxk), ts1(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
  REAL :: sv(0:maxj)

  REAL :: cv(0:maxj), dza(maxk), dztav(2,maxi,maxj), gb(mpxi*mpxj)
  REAL :: gap(mpxi*mpxj,2*mpxi+3), ez0, cost(maxi,maxj), rh(3,0:maxi+1,0:maxj+1)
  REAL :: gbold(mpxi*mpxj), sda1, sdomg, dzz, tau0(maxi,maxj), dztav0(maxi,maxj)
  REAL :: tau1(maxi,maxj), dztav1(maxi,maxj), tsa0(maxj), t0
  REAL :: fw_hosing(maxi,maxj), rhosing(maxi,maxj), zro(maxk), zw(0:maxk)
  REAL :: dzg(maxk,maxk), z2dzg(maxk,maxk), rdzg(maxk,maxk)
  REAL :: fw_anom(maxi,maxj), fw_anom_rate(maxi,maxj)

  REAL :: psi(0:maxi,0:maxj)
  REAL :: rel, u1(3,0:maxi,0:maxj,maxk)
  REAL :: rdphi
  REAL, DIMENSION(0:maxj) :: rc, rc2
  REAL, DIMENSION(maxi,maxj) :: rtv, rtv3
  REAL, DIMENSION(1:maxj-1) :: rcv, rdsv, cv2
  REAL :: rds(maxj), rdz(maxk), rdza(maxk)

  CHARACTER(LEN=6) :: gridnam   ! Topography and grid

  REAL :: bp(maxi+1,maxj,maxk), sbp(maxi+1,maxj)   ! Pressure integral arrays

  ! Diagnostics
  REAL :: dmax
  INTEGER :: limps, icosd(maxi,maxj)

  ! Dimensional scale values
  REAL, PARAMETER :: pi = 4 * ATAN(1.0)
  REAL, PARAMETER :: usc = 0.05, rsc = 6.37E6, dsc = 5.0E3, fsc = 2*7.2921E-5
  REAL, PARAMETER :: gsc = 9.81, rh0sc = 1.0E3
  REAL, PARAMETER :: rhosc = rh0sc * fsc * usc * rsc / gsc / dsc
  REAL, PARAMETER :: tsc = rsc / usc
  REAL, PARAMETER :: cpsc = 3981.1
  REAL, PARAMETER :: rhoair = 1.25, rho0 = 1.0E3, rhoao = rhoair / rho0

  ! Variables to convert FW fluxes mm/s <--> m/s
  REAL, PARAMETER :: m2mm = 1000.0, mm2m = 1.0 / m2mm

  ! OPSIT scaling parameter
  REAL, PARAMETER :: opsisc = dsc * usc * rsc * 1.0E-6

  ! EMBM scaling for heat forcing of ocean
  REAL, PARAMETER :: rfluxsc = rsc / (dsc * usc * rh0sc * cpsc)

  ! Specific heat capacity of air
  REAL, PARAMETER :: cpa=1004.0
  ! Consts for saturation specific humidity (Bolton 1980)
  REAL, PARAMETER :: const1=3.80*1E-3, const2=21.87, const3=265.5, &
       & const4=17.67, const5 = 243.5
  ! Stefan-Boltzmann constant (J/K**4/m**2/s)
  REAL, PARAMETER :: sigma=5.67E-8
  ! Shortwave radiation emission constant for ocean
  REAL, PARAMETER :: emo=0.94 * sigma
  ! Upper limit for ice temperature (set to 0C)
  REAL, PARAMETER :: tfreez=0.0
  ! Latent heats of vapourization, fusion and sublimation of ice (J/kg)
  REAL, PARAMETER :: hlv=2.501E6, hlf=3.34E5, hls=hlv + hlf
  ! Constant ice conductivity (W/m/K)
  REAL, PARAMETER :: consic = 2.166
  ! Kelvin temperature constant
  REAL, PARAMETER :: zeroc=273.15
  ! In parameterization of heat flux at base of sea ice: empirical
  ! constant
  REAL, PARAMETER :: ch_ice=0.0058
  ! Skin friction velocity (m/s)
  REAL, PARAMETER :: u_tau_ice=0.02
  ! Specific heat of sea water under ice at constant pressure (J/kg/K)
  REAL, PARAMETER :: cpo_ice = 4044.0
  ! Representative ice density (kg/m**3)
  REAL, PARAMETER :: rhoice = 913.0
  ! Minimum average sea-ice thickness over a grid cell.
  REAL, PARAMETER :: hmin=0.01, rhmin=1.0/hmin
  ! Density ratios.
  REAL, PARAMETER :: rhooi=rho0 / rhoice
  ! Melting factor
  REAL, PARAMETER :: rrholf = 1.0 / (rhoice * hlf)


  REAL :: asurf(maxj)      ! Grid cell area
  REAL :: yearlen          ! Days per year
  INTEGER :: overdep       ! Depth level for OPSIT min/max overturning

  ! Seasonal diagnostics
  REAL :: tsavg(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
  REAL :: uavg(3,0:maxi,0:maxj,maxk), rhoavg(0:maxi+1,0:maxj+1,0:maxk)
  REAL :: fx0avg(5,maxi,maxj), fwavg(4,maxi,maxj), windavg(4,maxi,maxj)

  ! Flux scaling parameters
  REAL :: cd, saln0, rpmesco, scf, rsictscsf

  INTEGER(KIND=8) :: nsteps
  INTEGER :: npstp, iwstp, itstp, iw, ianav, istep0
  INTEGER :: lenin, lenout, iav

  ! Input and output directory locations
  CHARACTER(LEN=3) :: lout
  CHARACTER(LEN=200) :: indir_name, outdir_name

  ! Names of data files for error calculation
  CHARACTER(LEN=128) :: tdatafile, sdatafile
  CHARACTER(LEN=25) :: tdata_varname, sdata_varname
  REAL :: tdata_missing, tdata_scaling, tdata_offset
  REAL :: sdata_missing, sdata_scaling, sdata_offset
  LOGICAL :: tsinterp

  INTEGER :: lentdata, lensdata, lentvar, lensvar
  LOGICAL :: flat

  REAL :: psisl(0:maxi,0:maxj,maxisles), ubisl(2,0:maxi+1,0:maxj,maxisles)
  REAL :: erisl(maxisles,maxisles+1), psibc(maxisles)

  ! Need this variable to store T and S between iterations (otherwise
  ! these values are lost)
  REAL :: ts_store(maxl,maxi,maxj,maxk)

  ! Climatological albedo array
  REAL :: albcl(maxi,maxj)

  REAL :: albocn, gust, ene_tune

  ! Extra parameters for hosing
  REAL :: hosing, hosing_trend
  INTEGER :: nsteps_hosing

  INTEGER :: iconv              ! Optional convection scheme

  ! For netcdf restarts
  CHARACTER(LEN=200) :: filenetin, dirnetout, rstdir_name
  LOGICAL :: lnetin, lnetout, lascout
  INTEGER :: iyear_rest, imonth_rest, ioffset_rest
  REAL :: day_rest

  INTEGER :: conserv_per        ! For energy/water check

  ! For delay in goldstien energy and evap fluxes
  REAL, DIMENSION(maxi,maxj) :: &
       & evap_save1, late_save1, sens_save1, evap_save2, late_save2, sens_save2

  ! For mixed layer calc
  REAL, DIMENSION(maxi,maxj) :: &
       & mldpebuoy, mldpeconv, mldpelayer1, mldketau, mldemix, mld
  REAL, DIMENSION(maxk) :: mlddec, mlddecd
  REAL :: mldpebuoycoeff, mldketaucoeff, mldwindkedec
  INTEGER :: mldk(maxi,maxj), imld

  ! For variable diapycnal diffusivity calc
  REAL :: ediff0, ediffpow1, ediffpow2, ediffvar, &
       & ediff1(maxi,maxj,maxk-1), diffmax(maxk)
  INTEGER :: iediff, ediffpow2i

  INTEGER :: ieos                               ! Equation of state switch
  REAL :: ssmaxsurf, ssmaxdeep, ssmax(maxk-1)   ! For isopycnal mixing

  LOGICAL :: dosc, diso, ctrl_diagend, &
       & debug_init, debug_end, debug_loop, rst_reset_T

  LOGICAL :: getj(maxi,maxj)

CONTAINS

  ! Calculate path integral around island.

  SUBROUTINE island(ubloc, erisl1, isl, indj)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: isl, indj
    REAL, INTENT(IN) :: ubloc(2,0:maxi+1,0:maxj)
    REAL, INTENT(OUT) :: erisl1

    INTEGER :: i, k, lpi, ipi, jpi
    REAL :: cor, tv1, tv2

    erisl1 = 0.0
    DO i = 1, npi(isl)
       lpi = lpisl(i,isl)
       ipi = ipisl(i,isl)
       jpi = jpisl(i,isl)
       IF (ABS(lpi) == 1) THEN
          cor = - s(jpi) * 0.25 * &
               & (ubloc(2,ipi,jpi) + ubloc(2,ipi+1,jpi) + &
               & ubloc(2,ipi,jpi-1) + ubloc(2,ipi+1,jpi-1))
       ELSE
          cor = sv(jpi) * 0.25 * &
               & (ubloc(1,ipi-1,jpi) + ubloc(1,ipi,jpi) + &
               & ubloc(1,ipi-1,jpi+1) + ubloc(1,ipi,jpi+1))
       END IF
       erisl1 = erisl1 + SIGN(1,lpi) * (drag(ABS(lpi),ipi,jpi) * &
            & ubloc(ABS(lpi),ipi,jpi) + cor - &
            & indj * tau(ABS(lpi),ipi,jpi) * rh(ABS(lpi),ipi,jpi)) * &
            & (c(jpi) * dphi * &
            & (2.0 - ABS(lpi)) + rcv(jpi) * dsv(jpi) * (ABS(lpi) - 1.0))

       ! Calc tricky bits and add to source term for path integral round
       ! islands, all sums have at least one element.
       IF (indj == 1) THEN
          IF (ABS(lpi) == 1) THEN
             tv1 = 0.0
             DO k = ku(1,ipi,jpi), mk(ipi+1,jpi)
                tv1 = tv1 + bp(ipi+1,jpi,k) * dz(k)
             END DO
             DO k = ku(1,ipi,jpi), mk(ipi,jpi)
                tv1 = tv1 - bp(ipi,jpi,k) * dz(k)
             END DO
             erisl1 = erisl1 + (sbp(ipi+1,jpi) - sbp(ipi,jpi) + tv1) * &
                  & SIGN(1,lpi) * rh(1,ipi,jpi)
          ELSE
             tv2 = 0.0
             DO k = ku(2,ipi,jpi), mk(ipi,jpi+1)
                tv2 = tv2 + bp(ipi,jpi+1,k) * dz(k)
             END DO
             DO k = ku(2,ipi,jpi), mk(ipi,jpi)
                tv2 = tv2 - bp(ipi,jpi,k) * dz(k)
             END DO
             erisl1 = erisl1 + (sbp(ipi,jpi+1) - sbp(ipi,jpi) + tv2) * &
                  & SIGN(1,lpi) * rh(2,ipi,jpi)
          END IF
       END IF
    END DO
  END SUBROUTINE island

END MODULE goldstein_lib

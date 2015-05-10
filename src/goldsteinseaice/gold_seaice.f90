MODULE gold_seaice

  USE genie_util, ONLY: check_unit, check_iostat
  USE genie_control, ONLY: dim_GOLDSTEINNLONS, dim_GOLDSTEINNLATS, dim_GOLDSTEINNLEVS
  USE gold_seaice_lib
  USE gold_seaice_netcdf
  USE gold_seaice_data
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_seaice
  PUBLIC :: step_seaice
  PUBLIC :: end_seaice

CONTAINS

  SUBROUTINE initialise_seaice(ilon1, ilat1, ilon2, ilat2, ilon3, ilat3, &
       & iboxedge1_lon, iboxedge1_lat, iboxedge2_lon, iboxedge2_lat, &
       & iboxedge3_lon, iboxedge3_lat, ilandmask1, ilandmask2, ilandmask3, &
       & totsteps, hght_sic, frac_sic, temp_sic, albd_sic, test_energy_seaice)
    USE genie_util, ONLY: die
    IMPLICIT NONE
    ! ======================================================================
    ! Declarations
    ! ======================================================================

    REAL, DIMENSION(:), INTENT(OUT) :: ilon1, ilon2, ilon3
    REAL, DIMENSION(:), INTENT(OUT) :: ilat1, ilat2, ilat3
    REAL, DIMENSION(:), INTENT(OUT) :: &
         & iboxedge1_lon, iboxedge2_lon, iboxedge3_lon
    REAL, DIMENSION(:), INTENT(OUT) :: &
         & iboxedge1_lat, iboxedge2_lat, iboxedge3_lat
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: &
         & ilandmask1, ilandmask2, ilandmask3
    INTEGER(KIND=8), INTENT(IN) :: totsteps
    REAL, DIMENSION(:,:), INTENT(OUT) :: &
         & hght_sic, frac_sic, temp_sic, albd_sic
    REAL, INTENT(OUT) :: test_energy_seaice

    ! Parameters for setting up grid
    ! th is latitude, coords are sin(th), longitude phi, and z
    REAL, PARAMETER :: th0 = -pi/2, th1 = pi/2
    REAL, PARAMETER :: s0 = sin(th0), s1 = sin(th1)
    REAL, PARAMETER :: phix = 2*pi, deg_to_rad = pi / 180.0

    ! Local variables
    REAL, PARAMETER :: ch_ice = 0.0058   ! empirical constant
    REAL, PARAMETER :: u_tau_ice = 0.02  ! skin friction velocity (m/s)
    REAL, PARAMETER :: cpo_ice = 4044    ! specific heat of sea water
                                         ! under ice at constant
                                         ! pressure (J/kg/K)

    REAL :: tv, theta, thv, dth, dscon
    INTEGER :: i, j, ios, igrid, status
    CHARACTER(len=6) :: world
    INTEGER :: lenworld
    CHARACTER(LEN=13) :: lin
    CHARACTER :: ans

    INTEGER, EXTERNAL :: lnsig1

    NAMELIST /ini_sic_nml/ indir_name, outdir_name, rstdir_name
    NAMELIST /ini_sic_nml/ igrid, world
    NAMELIST /ini_sic_nml/ npstp, iwstp, itstp, ianav, conserv_per
    NAMELIST /ini_sic_nml/ ans, yearlen, nyear, diffsic, lout
    NAMELIST /ini_sic_nml/ filenetin, dirnetout
    NAMELIST /ini_sic_nml/ dosc,impsic, debug_init, debug_end, debug_loop
    NAMELIST /ini_sic_nml/ par_sica_thresh, par_sich_thresh

    ! ======================================================================
    ! Setting up sea-ice
    ! ======================================================================
    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising sea-ice module ...'
    IF (debug_init) PRINT *

    maxi = dim_GOLDSTEINNLONS
    maxj = dim_GOLDSTEINNLATS
    maxk = dim_GOLDSTEINNLEVS

    ALLOCATE(k1(0:maxi+1,0:maxj+1),STAT=status) ; k1 = 0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(s(0:maxj),STAT=status)             ; s = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(c(0:maxj),STAT=status)             ; c = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(sv(0:maxj),STAT=status)            ; sv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(cv(0:maxj),STAT=status)            ; cv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ds(maxj),STAT=status)              ; ds = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dsv(1:maxj-1),STAT=status)         ; dsv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rds2(2:maxj-1),STAT=status)        ; rds2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(u(2,0:maxi,0:maxj),STAT=status)    ; u = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rc(0:maxj),STAT=status)            ; rc = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rcv(0:maxj),STAT=status)           ; rcv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(cv2(0:maxj),STAT=status)           ; cv2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rc2(0:maxj),STAT=status)           ; rc2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rds(maxj),STAT=status)             ; rds = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(rdsv(1:maxj-1),STAT=status)        ; rdsv = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(asurf(maxj),STAT=status)           ; asurf = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(varice(2,maxi,maxj),STAT=status)   ; varice = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(varice1(2,maxi,maxj),STAT=status)  ; varice1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dtha(2,maxi,maxj),STAT=status)     ; dtha = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(varicedy(2,maxi,maxj),STAT=status) ; varicedy = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(variceth(2,maxi,maxj),STAT=status) ; variceth = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(tice(maxi,maxj),STAT=status)       ; tice = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(albice(maxi,maxj),STAT=status)     ; albice = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(haavg(2,maxi,maxj),STAT=status)    ; haavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(dthaavg(2,maxi,maxj),STAT=status)  ; dthaavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(ticeavg(maxi,maxj),STAT=status)    ; ticeavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(albiceavg(maxi,maxj),STAT=status)  ; albiceavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fxdelavg(maxi,maxj),STAT=status)   ; fxdelavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(fwdelavg(maxi,maxj),STAT=status)   ; fwdelavg = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    ALLOCATE(nclon1(maxi),STAT=status) ; nclon1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclon2(maxi),STAT=status) ; nclon2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclon3(maxi),STAT=status) ; nclon3 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclat1(maxj),STAT=status) ; nclat1 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclat2(maxj),STAT=status) ; nclat2 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")
    ALLOCATE(nclat3(maxj),STAT=status) ; nclat3 = 0.0
    IF (status /= 0) CALL die("Could not allocate memory")

    ! read DATA (i.e. namelist) file
    CALL check_unit(56, __LINE__, __FILE__)
    OPEN(UNIT=56, FILE='data_goldSIC', STATUS='old', IOSTAT=ios)
    IF (ios /= 0) THEN
       PRINT *, 'ERROR: could not open SIC namelist file'
       STOP
    END IF
    READ(UNIT=56, NML=ini_sic_nml, IOSTAT=ios)
    IF (ios /= 0) THEN
       PRINT *, 'ERROR: could not read SIC namelist'
       STOP
    ELSE
       CLOSE(56, IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
    END IF

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
       rstdir_name(lenrst:lenrst) = '/'
    END IF
    IF (debug_init) PRINT *, 'Restart dir. name ', rstdir_name(1:lenrst)

    ! read in topography filename
    lenworld = lnsig1(world)
    IF (debug_init) PRINT *, 'Topography name ', world(1:lenworld)

    nsteps = totsteps
    IF (debug_init) PRINT *, 'npstp iwstp itstp ianav'
    IF (debug_init) PRINT *, npstp,iwstp,itstp,ianav
    IF (debug_init) PRINT *, 'period of water and energy checks:'
    IF (debug_init) PRINT *, conserv_per
    IF (debug_init) PRINT *, 'new or continuing run ?'
    IF (debug_init) PRINT *, ans
    IF (debug_init) PRINT *, 'number of days per GOLDSTEIN sea-ice year'
    IF (debug_init) PRINT *, yearlen
    IF (debug_init) PRINT *, 'seasonality enabled =', dosc

    ! Grid dimensions must be no greater than array dimensions in var.cmn
    ! maxk needed to interpret topography file for masking
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
    IF (debug_init) PRINT *, 'SIC latitudes: velocity; tracers'
    IF (debug_init) PRINT *, 'j, 180/pi*asin(sv(j)), 180/pi*asin(s(j))'
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

    ! v2 seasonality
    IF (debug_init) PRINT *, 'timesteps per year'
    IF (debug_init) PRINT *, nyear
    tv = 86400.0 * yearlen / (nyear * tsc)
    dtsic = tv
    IF (debug_init) PRINT *, 'sea-ice timestep in days', tv * tsc / 86400
    rdtdim = 1.0 / (tsc * dtsic)
    IF (debug_init) PRINT *, 'rdtdim = ', rdtdim

    ! reading in ocean bathymetry/runoff patterns
    ! seabed depth h needed BEFORE forcing if coastlines are
    ! non-trivial note k1(i,j) must be periodic ; k1(0,j) - k1(maxi,j)
    ! = 0 and k1(1,j) - k1(maxi+1,j) = 0
    IF (debug_init) PRINT *, 'Bathymetry being read in'
    CALL check_unit(13, __LINE__, __FILE__)
    OPEN(13, FILE=indir_name(1:lenin)//world//'.k1', IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    DO j = maxj+1, 0, -1
       READ (13,*,IOSTAT=ios) (k1(i,j), i = 0, maxi+1)
       CALL check_iostat(ios, __LINE__, __FILE__)
       k1(0,j) = k1(maxi,j)
       k1(maxi+1,j) = k1(1,j)
       IF(j /= 0 .AND. j /= maxj+1 .AND. debug_init) &
            & WRITE (6,'(i4,32i3)') j, (k1(i,j), i = 1, 32)
    END DO

    ! read ips etc if possible
    CLOSE(13, IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    IF (debug_init) PRINT *, 'Bathymetry successfully read in'

    ! EMBM stuff follows...
    DO j = 1, maxj
       asurf(j) = rsc * rsc * ds(j) * dphi
       IF (debug_init) &
            & PRINT *, 'j = ', j, 'SIC grid cell area is', asurf(j), 'm2'
    END DO

    ryear = 1.0 / (yearlen * 86400)
    IF (debug_init) PRINT *, 'diffsic'
    IF (debug_init) PRINT *, diffsic

    ! non-dimensionalise eddy diffusivity
    diffsic = diffsic / (rsc * usc)
    ! density ratios
    rhooi = rho0 / rhoice
    rhoio = rhoice / rho0

    IF (debug_init) PRINT *, 'sea-ice variables'
    IF (debug_init) PRINT *, 'water density, rho0 =', rho0
    IF (debug_init) PRINT *, 'latent heat of fusion, hlf =', hlf
    IF (debug_init) PRINT *, &
         & '(dimensional) sea-ice diffusion, diffsic =', diffsic * (rsc * usc)
    IF (debug_init) PRINT *, &
         & 'base of sea-ice empirical constant, ch_ice =', ch_ice
    IF (debug_init) PRINT *, &
         & 'skin friction velocity, u_tau_ice =', u_tau_ice
    IF (debug_init) PRINT *, &
         & 'specific heat of seawater under ice, cpo_ice =', cpo_ice
    IF (debug_init) PRINT *, &
         & 'representative ice density, rhoice =', rhoice
    IF (debug_init) PRINT *, &
         & 'minimum average sea-ice thickness, hmin =', hmin
    IF (debug_init) PRINT *, 'density ratios, rhooi =', rhooi
    IF (debug_init) PRINT *, 'm to mm conversion factor, m2mm =', m2mm
    IF (debug_init) PRINT *, 'mm to m conversion factor, mm2m =', mm2m
    IF (debug_init) PRINT *

    ! Initialize sea ice thickness, fractional area and temperature.
    varice = 0.0
    varice1 = 0.0
    varicedy = 0.0
    variceth = 0.0
    tice = 0.0
    albice = 0.0
    IF (dosc) THEN
       ! v2 seasonal. Annual averages
       haavg = 0.0
       ticeavg = 0.0
       albiceavg = 0.0
       dthaavg = 0.0
       fxdelavg = 0.0
       fwdelavg = 0.0
    END IF

    IF (debug_init) PRINT *, 'file extension for output (a3) ?'
    IF (debug_init) PRINT *, lout
    IF (debug_init) PRINT *, 'filename for netCDF restart input ?'
    IF (debug_init) PRINT *, filenetin
    IF (debug_init) &
         & PRINT *, 'directory name for netCDF restart output ?'
    IF (debug_init) PRINT *, dirnetout

    ! Is this a new or continuing run?
    IF (ans == 'n' .OR. ans == 'N') THEN
       IF (debug_init) PRINT *, &
            & 'this is a new run, initial conditions already set up'
       ! But set up initial default time and date....
       iyear_rest = 2000
       imonth_rest = 1
       ioffset_rest = 0
       day_rest = yearlen / nyear
       IF (debug_init) PRINT *, 'day_rest = ', REAL(day_rest)
    ELSE
       ! This is a continuing run, read in end state filename
       IF (debug_init) PRINT *, 'input file extension for input (a6)'
       IF (debug_init) PRINT *, lin
       IF (debug_init) PRINT *, 'Reading sea-ice restart file'
       CALL inm_netcdf_sic
       varice1 = varice
    END IF

    ! ======================================================================
    ! Open output files
    ! ======================================================================

    ! Average, etc. values of sea-ice properties
    CALL check_unit(43, __LINE__, __FILE__)
    OPEN(43, FILE=outdir_name(1:lenout)//lout//'.'//'sic', IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    WRITE (43,*) '% GOLDSTEIN sea-ice, sea-ice properties'
    WRITE (43,'(6a15,a10,a15,a10,a15,a10)',iostat=ios)'% time        ', &
         & 'N hem ice vol', 'S hem ice vol', 'N hem ice area', &
         & 'S hem ice area', 'Max ice thick', ' Location', 'Min ice temp', &
         & ' Location', 'Max ice albedo', ' Location'
    CALL check_iostat(ios, __LINE__, __FILE__)
    WRITE (43,'(6a15,2a5,a15,2a5,a15,2a5)',IOSTAT=ios)'%             ', &
         & 'm^3', 'm^3', 'm^2', 'm^2', 'm', 'i', 'j', &
         & 'degrees C', 'i', 'j', 'dimensionless', 'i', 'j'
    CALL check_iostat(ios, __LINE__, __FILE__)
    CLOSE(43, IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)

    ! count variable for output file numbers
    iw = 1
    iav = 1

    ! ======================================================================
    ! Setting up grid structure
    ! ======================================================================

    ! This code is copied from an earlier incarnation of c-GOLDSTEIN
    ! and IGCM coupling.  It calculates grid structure for use in
    ! conversion between atmosphere and ocean grids.

314 FORMAT(i3, 6f8.2)

    IF (debug_init) PRINT *
    IF (debug_init) PRINT *, 'Sea-ice/GENIE grid interpolation variables :'
    IF (debug_init) PRINT *, &
         & '* Longitude : ilon1, ilon2, ilon3, ibox1, ibox2, ibox3 *'
    DO i = 1, maxi
       ilon1(i) = REAL(360.0 * (i - 0.5) / REAL(maxi) + phi0 / deg_to_rad)
       ilon2(i) = REAL(360.0 * i / REAL(maxi) + phi0 / deg_to_rad)
       ilon3(i) = REAL(360.0 * (i - 0.5) / REAL(maxi) + phi0 / deg_to_rad)
       nclon1(i) = ilon1(i)
       nclon2(i) = REAL(360.0 * (i - 1.0) / REAL(maxi) + phi0 / deg_to_rad)
       nclon3(i) = ilon3(i)
    END DO
    DO i = 1, maxi+1
       iboxedge1_lon(i) = &
            & REAL(360.0 * (i - 1.0) / REAL(maxi) + phi0 / deg_to_rad)
       iboxedge2_lon(i) = &
            & REAL(360.0 * (i - 0.5) / REAL(maxi) + phi0 / deg_to_rad)
       iboxedge3_lon(i) = &
            & REAL(360.0 * (i - 1.0) / REAL(maxi) + phi0 / deg_to_rad)
    END DO

    IF (debug_init) THEN
       DO i = 1, maxi+1
          IF (i < maxi+1) THEN
             WRITE (*,314) i, ilon1(i), ilon2(i), ilon3(i), &
                  & iboxedge1_lon(i), iboxedge2_lon(i), iboxedge3_lon(i)
          ELSE
             WRITE (*,314) i, -999.99, -999.99, -999.99, &
                  & iboxedge1_lon(i), iboxedge2_lon(i), iboxedge3_lon(i)
          END IF
       END DO
    END IF

    IF (debug_init) PRINT *, &
         & '* Latitude : ilat1, ilat2, ilat3, ibox1, ibox2, ibox3 *'
    nclat3(1) = REAL(ASIN(sv(0)) * 180.0 / pi)
    DO j = 1, maxj
       ilat1(j) = REAL(ASIN(s(j)) * 180.0 / pi)
       ilat2(j) = REAL(ASIN(s(j)) * 180.0 / pi)
       ilat3(j) = REAL(ASIN(sv(j)) * 180.0 / pi)
       nclat1(j) = ilat1(j)
       nclat2(j) = ilat2(j)
       IF (j < maxj) nclat3(j+1) = REAL(ASIN(sv(j)) * 180.0 / pi)
    END DO
    DO j = 1, maxj+1
       iboxedge1_lat(j) = REAL(ASIN(sv(j-1)) * 180.0 / pi)
       iboxedge2_lat(j) = REAL(ASIN(sv(j-1)) * 180.0 / pi)
       ! following if statement stops bounds error
       IF (j <= maxj) iboxedge3_lat(j) = REAL(ASIN(s(j)) * 180.0 / pi)
    END DO
    iboxedge3_lat(maxj+1) = REAL(ASIN(sv(maxj)) * 180.0 / pi)
    IF (debug_init) THEN
       DO j = 1, maxj+1
          IF (j < maxj+1) THEN
             WRITE (*,314) j, ilat1(j), ilat2(j), ilat3(j), &
                  & iboxedge1_lat(j), iboxedge2_lat(j), iboxedge3_lat(j)
          ELSE
             WRITE (*,314) j, -999.99, -999.99, -999.99, &
                  & iboxedge1_lat(j), iboxedge2_lat(j), iboxedge3_lat(j)
          END IF
       END DO
    END IF

    ! Make the land-sea mask on the genie grid.  The genie grid is
    ! offset from the goldstein grid by maxi/4 in the longitudinal
    ! direction.
    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) >= 90) THEN
             ilandmask1(i,j) = 1
             ilandmask2(i,j) = 1
             ilandmask3(i,j) = 1
          ELSE
             ilandmask1(i,j) = 0
             ilandmask2(i,j) = 0
             ilandmask3(i,j) = 0
          END IF
       END DO
    END DO
    IF (debug_init) PRINT *

    ! Initialise the seaice energy.
    test_energy_seaice = 0.0

    ! Output arguments
    ! ----------------------------------------------------------------------

    DO j = 1, maxj
       DO i = 1, maxi
          ! Sea-ice height              [-> surface fluxes]
          hght_sic(i,j) = REAL(varice(1,i,j))
          ! Sea-ice fractional area     [-> surface fluxes]
          frac_sic(i,j) = REAL(varice(2,i,j))
          ! Sea-ice surface temperature [-> surface fluxes]
          temp_sic(i,j) = REAL(tice(i,j))
          ! Sea-ice albedo              [-> surface fluxes]
          albd_sic(i,j) = REAL(albice(i,j))
       END DO
    END DO

    print*,' <<< Initialisation complete'
    print*,'======================================================='

  END SUBROUTINE initialise_seaice


  SUBROUTINE step_seaice(istep, dhght_sic, dfrac_sic, ustar_ocn, vstar_ocn, &
       & hght_sic, frac_sic, temp_sic, albd_sic, sic_FW_ocn, sic_FX0_ocn, &
       & test_energy_seaice, test_water_seaice)

    ! ======================================================================
    ! Declarations
    ! ======================================================================

    REAL :: dhght_sic(maxi,maxj),dfrac_sic(maxi,maxj),    &
         & ustar_ocn(maxi,maxj),vstar_ocn(maxi,maxj),  &
         & hght_sic(maxi,maxj),frac_sic(maxi,maxj),    &
         & temp_sic(maxi,maxj),albd_sic(maxi,maxj),    &
         & sic_FW_ocn(maxi,maxj),sic_FX0_ocn(maxi,maxj)

    INTEGER :: istep

    INTEGER :: i, j, itv, iout, ios
    REAL :: fw_delta(maxi,maxj), fx_delta(maxi,maxj)
    REAL, DIMENSION(2) :: sum1, sum2, sum3, sum4
    INTEGER, DIMENSION(2) :: isum1, isum2, isum3

    REAL :: t

    REAL :: work((maxi + 1) * (maxj + 1))

    CHARACTER(LEN=3) :: ext

    ! For the energy calculations.
    REAL :: tot_energy,tot_water
    REAL, SAVE :: ini_energy,ini_water
    REAL :: vsc
    LOGICAL, SAVE :: first = .TRUE.
    REAL :: test_energy_seaice, test_water_seaice
    REAL ::  tv

    ! ======================================================================
    ! Calculate the inital diagnostic of the total seaice thermal energy
    ! and water.  The water part has units of kg; the energy part has
    ! units of J.

    IF (first) THEN
       vsc = dphi * rsc * rsc
       ini_energy = 0.0
       ini_water = 0.0
       DO j = 1, maxj
          DO i = 1, maxi
             ini_water = ini_water + varice(1,i,j) * ds(j)
          END DO
       END DO
       ! The m2mm is because internally, goldseaice uses m, but genie
       ! uses mm.
       ini_water = m2mm * ini_water * vsc * rhoio
       first = .FALSE.
    endif

    ! ======================================================================
    ! Input field modifications
    ! ======================================================================

    DO j = 1, maxj
       DO i = 1, maxi
          tice(i,j) = temp_sic(i,j)
          albice(i,j) = albd_sic(i,j)
          dtha(1,i,j) = dhght_sic(i,j)
          dtha(2,i,j) = dfrac_sic(i,j)
          u(1,i,j) = ustar_ocn(i,j)
          u(2,i,j) = vstar_ocn(i,j)
          fw_delta(i,j) = 0.0
          fx_delta(i,j) = 0.0
       END DO
    END DO

    ! ======================================================================
    ! Sea-ice model timestep
    ! ======================================================================

    ! update ice
    IF (impsic) THEN
       CALL tstipsic
    ELSE
       CALL tstepsic
    END IF

    ! modify heat and salinity fluxes into ocean according to sea-ice
    ! update; prevent <0%, >100% fractional area A and set A=H=0 if
    ! H<Hmin in which case add an amount -H of ice, hence need to add
    ! appropriate heat and freshwater fluxes.
    !
    ! This code altered so that instead of modifying net ocean fluxes
    ! (which aren't really used in genie.f anyway), a new sea-ice flux
    ! is calculated and fed to goldstein.F where it is combined with the
    ! other fluxes to determine the net ocean fluxes.  This is
    ! consistent with the previous calculations below.

    DO j = 1, maxj
       DO i = 1, maxi
          IF (maxk >= k1(i,j)) THEN
             fw_delta(i,j) = -rhoio * dtha(1,i,j)
             varice(2,i,j) = MAX(0.0, MIN(1.0, varice(2,i,j)))
             IF (varice(1,i,j) < hmin) THEN
                fx_delta(i,j) = -varice(1,i,j) * rhoice * hlf * rdtdim
                fw_delta(i,j) = fw_delta(i,j) + varice(1,i,j) * rhoio * rdtdim
                varice(:,i,j) = 0.0
             END IF

             varice1(:,i,j) = varice(:,i,j)

             ! Convert sea-ice FW flux from m/s to mm/s.
             fw_delta(i,j) = fw_delta(i,j) * m2mm
          END IF
       END DO
    END DO

    ! Note that sea-ice albedo (calculated in the new surf_ocn_sic.F
    ! routine) is not affected by the above, despite the sea-ice
    ! melting.

    ! Sea-ice diagnostics and output
    ! ----------------------------------------------------------------------

    CALL outm_netcdf_sic(istep)

    IF (MOD(istep,iwstp) == 0) THEN
       iw = iw + 1
       IF (debug_loop) PRINT *
    END IF

110 FORMAT(6e15.6,2i5,1e15.6,2i5,1e15.6,2i5)

    IF (MOD(istep,itstp) == 0) THEN
       t = REAL(istep) / REAL(nyear)
       IF (debug_loop) PRINT *, 'Writing to sea-ice time-series file'
       CALL check_unit(43, __LINE__, __FILE__)
       OPEN(43, FILE=outdir_name(1:lenout)//lout//'.'//'sic', &
            & STATUS='old', POSITION='append', IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CALL diag4(sum1, sum2, sum3, sum4, isum1, isum2, isum3)
       IF (debug_loop) &
            & WRITE(43,110,IOSTAT=ios) t,sum1(1),sum1(2),sum2(1),sum2(2), &
            & sum3(1),isum1(1),isum1(2),sum3(2),isum2(1),isum2(2), &
            & sum4(1),isum3(1),isum3(2)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(43,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       IF (debug_loop) PRINT *
    END IF

    IF (dosc) THEN
       ! average the last nyear steps in every ianav steps (if ianav>nyear)
       itv = MOD(istep+nyear-1, ianav)
       IF (itv < nyear) THEN
          ext = conv_sic(MOD(iav, 10))
          IF (istep >= nyear .AND. itv == nyear-1) THEN
             iout = 1
          ELSE
             iout = 0
          END IF
          IF (debug_loop) CALL diagosc_sic(istep, iout, ext, fx_delta, fw_delta)
       END IF
    END IF

    IF (MOD(istep,npstp) == 0) THEN
       IF (debug_loop) CALL diagsic(istep)
       IF (debug_loop) PRINT *
    END IF

    IF (MOD(istep,iwstp) == 0) THEN
       IF (debug_loop) PRINT *, 'Writing sea-ice netCDF file at time', istep
       CALL ini_netcdf_sic(istep, 1)
       ! Note that all of the arguments for the write_netCDF function
       ! are in the same pre-cision as the sea-ice module.
       CALL write_netcdf_sic(k1, varice, tice, albice, &
            & dtha, fx_delta, fw_delta, work, maxi, maxj, 1)
       CALL end_netcdf_sic(1)
       IF (debug_loop) PRINT *
    END IF

    ! Output arguments
    ! ----------------------------------------------------------------------

    DO j = 1, maxj
       DO i = 1, maxi
          ! Sea-ice thickness [-> surface fluxes]
          hght_sic(i,j) = REAL(varice(1,i,j))
          ! Sea-ice area      [-> surface fluxes]
          frac_sic(i,j) = REAL(varice(2,i,j))
          ! Freshwater flux   [-> GOLDSTEIN]
          sic_FW_ocn(i,j) = REAL(fw_delta(i,j))
          ! Heat flux         [-> GOLDSTEIN]
          sic_FX0_ocn(i,j) = REAL(fx_delta(i,j))
       END DO
    END DO

    ! ======================================================================
    ! Calculates the diagnostic of the total seaice thermal energy and
    ! water.  The water part has units of kg, relative to an initial
    ! value.  The energy part has units of J, relative to an initial
    ! value.
    ! THE ENERGY PART (HERE AND IN SURF_OCN_SIC) IS WELL-DODGY AND IS A
    ! COMPLETE FIX.  SEE DJL FOR MORE INFO.
    IF (MOD(istep,conserv_per) == 0) THEN
       tv = 86400.0 * yearlen / (nyear * tsc)
       vsc = dphi * rsc * rsc
       tot_energy = 0.0
       tot_water = 0.0
       DO j = 1, maxj
          DO i = 1, maxi
             tot_energy = tot_energy - sic_FX0_ocn(i,j) * ds(j)
             tot_water = tot_water + varice(1,i,j) * ds(j)
          END DO
       END DO
       ! The m2mm is because internally, goldseaice uses m, but genie
       ! uses mm.
       tot_energy = vsc * tsc * tv * tot_energy
       tot_water = m2mm * vsc * rhoio * tot_water
       test_energy_seaice = REAL(test_energy_seaice + tot_energy - ini_energy)
       test_water_seaice = REAL(tot_water - ini_water)
       IF (debug_loop) THEN
          PRINT*, 'GoldSeaice energy diagnostic: ', test_energy_seaice
          PRINT *, 'GoldSeaice water diagnostic: ', test_water_seaice
       END IF
    END IF
  END SUBROUTINE step_seaice


  SUBROUTINE end_seaice
    IMPLICIT NONE

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising sea-ice module shutdown ...'

    IF (debug_end) CALL diagend_seaice

    PRINT *, ' <<< Shutdown complete'
    PRINT *, '======================================================='

  END SUBROUTINE end_seaice


  ! End-of-run diagnostics.
  SUBROUTINE diagend_seaice
    IMPLICIT NONE

    INTEGER :: i, ios

    PRINT *, 'Writing Arctic sea-ice diagnostic file'
    CALL check_unit(28, __LINE__, __FILE__)
    OPEN(28, FILE=outdir_name(1:lenout)//lout//'.arcice', IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
    DO i = 1 ,maxi
       WRITE (28,'(4e14.6)',IOSTAT=ios) varice(1,i,maxj), &
            & varice(2,i,maxj), tice(i,maxj), albice(i,maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
    END DO
    CLOSE(28,IOSTAT=ios)
    CALL check_iostat(ios, __LINE__, __FILE__)
  END SUBROUTINE diagend_seaice


  FUNCTION conv_sic(i)
    IMPLICIT NONE
    CHARACTER(LEN=3) :: conv_sic
    INTEGER :: i
    INTEGER :: i1, i2, itemp, i3
    CHARACTER(LEN=1) ::  a, b, c
    IF (i < 10) THEN
       a = CHAR(i + 48)
       conv_sic = a // '  '
    ELSE IF (i < 100) THEN
       i1 = i / 10
       i2 = i - i1 * 10
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       conv_sic = a // b // ' '
    ELSE
       i1 =i / 100
       itemp = i - 100 * i1
       i2 = itemp / 10
       i3 = itemp - 10 * i2
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       c = CHAR(i3 + 48)
       conv_sic = a // b // c
    END IF
  END FUNCTION conv_sic

  ! Frequent sea-ice diagnostics
  SUBROUTINE diag4(sum1, sum2, sum3, sum4, isum1, isum2, isum3)

    REAL, DIMENSION(2), INTENT(OUT) :: sum1, sum2, sum3, sum4
    INTEGER, DIMENSION(2), INTENT(OUT) :: isum1, isum2, isum3

    INTEGER :: i, j

    ! Reset arrays
    sum1 = 0.0 ; sum2 = 0.0 ; sum3 = 0.0 ; sum4 = 0.0
    isum1 = 0 ; isum2 = 0 ; isum3 = 0

    ! Calculate averages, minima, maxima, etc.
    DO j = 1, maxj
       DO i = 1, maxi
          ! Northern/Southern hemisphere totals
          IF (j > maxj / 2) THEN
             sum1(1) = sum1(1) + (varice(1,i,j)*varice(2,i,j))*asurf(j)
             sum2(1) = sum2(1) + varice(2,i,j)*asurf(j)
          ELSE
             sum1(2) = sum1(2) + (varice(1,i,j)*varice(2,i,j))*asurf(j)
             sum2(2) = sum2(2) + varice(2,i,j)*asurf(j)
          END IF
          ! Maximum sea-ice height
          IF (varice(1,i,j) > sum3(1)) THEN
             sum3(1) = varice(1,i,j)
             isum1(1) = i
             isum1(2) = j
          END IF
          ! Minimum sea-ice temperature
          IF (tice(i,j) < sum3(2)) THEN
             sum3(2) = tice(i,j)
             isum2(1) = i
             isum2(2) = j
          END IF
          ! Maximum sea-ice albedo
          IF (albice(i,j) > sum4(1)) THEN
             sum4(1) = albice(i,j)
             isum3(1) = i
             isum3(2) = j
          END IF
          ! Note : sum4(2) not used at present
       END DO
    END DO
  END SUBROUTINE diag4

  ! Update sea-ice height and area
  SUBROUTINE tstepsic
    IMPLICIT NONE

    INTEGER :: i, j, l
    REAL :: fe(2), fw(2), fn(2), fs(2,maxi), fwsave(2)

    ! 2nd order explicit transport code using upper level ocean
    ! velocities

    ! southern boundary fluxes
    j = 1
    fs = 0

    DO j = 1, maxj
       ! western boundary fluxes
       i = 1
       DO l = 1, 2
          IF (maxk >= MAX(k1(maxi,j), k1(1,j))) THEN
             ! western doorway
             fw(l) = u(1,maxi,j) * rc(j) * (varice1(l,1,j) + &
                  & varice1(l,maxi,j)) * 0.5
             IF (u(1,maxi,j) >= 0.0) THEN
                IF (varice1(2,1,j) > par_sica_thresh) fw(l) = 0
                IF (varice1(1,1,j) > par_sich_thresh) fw(l) = 0
             ELSE
                IF (varice1(2,maxi,j) > par_sica_thresh) fw(l) = 0
                IF (varice1(1,maxi,j) > par_sich_thresh) fw(l) = 0
             END IF
             fw(l) = fw(l) - (varice1(l,1,j) - varice1(l,maxi,j)) * &
                  & rc(j) * rc(j) * rdphi * diffsic
          ELSE
             fw(l) = 0
          END IF
          fwsave(l) = fw(l)
       END DO

       DO i = 1, maxi
          DO l = 1, 2
             ! flux to east
             IF (i == maxi) THEN
                ! eastern edge(doorway or wall)
                fe(l) = fwsave(l)
             ELSE IF (maxk < max(k1(i,j), k1(i+1,j))) THEN
                fe(l) = 0
             ELSE
                fe(l) = u(1,i,j) * rc(j) * (varice1(l,i+1,j) + &
                     & varice1(l,i,j)) * 0.5
                IF (u(1,i,j) >= 0.0) THEN
                   IF (varice1(2,i+1,j) > par_sica_thresh) fe(l) = 0
                   IF (varice1(1,i+1,j) > par_sich_thresh) fe(l) = 0
                ELSE
                   IF (varice1(2,i,j) > par_sica_thresh) fe(l) = 0
                   IF (varice1(1,i,j) > par_sich_thresh) fe(l) = 0
                END IF
                fe(l) = fe(l) - (varice1(l,i+1,j) - varice1(l,i,j)) * &
                     & rc(j) * rc(j) * rdphi * diffsic
             END IF
             ! flux to north
             IF (maxk < MAX(k1(i,j), k1(i,j+1))) THEN
                fn(l) = 0
             ELSE
                fn(l) = cv(j) * u(2,i,j) * (varice1(l,i,j+1) + &
                     & varice1(l,i,j)) * 0.5
                IF (u(2,i,j) >= 0.0) THEN
                   IF (varice1(2,i,j+1) > par_sica_thresh) fn(l) = 0
                   IF (varice1(1,i,j+1) > par_sich_thresh) fn(l) = 0
                ELSE
                   IF (varice1(2,i,j) > par_sica_thresh) fn(l) = 0
                   IF (varice1(1,i,j) > par_sich_thresh) fn(l) = 0
                END IF
                fn(l) = fn(l) - cv(j)*cv(j)*(varice1(l,i,j+1) - &
                     &varice1(l,i,j))*rdsv(j)*diffsic
             END IF

             IF (maxk >= k1(i,j)) THEN
                varice(l,i,j) = varice1(l,i,j) - dtsic * ( &
                     & (fe(l) - fw(l)) * rdphi + &
                     & (fn(l) - fs(l,i)) * rds(j)) + &
                     & tsc * dtsic * dtha(l,i,j)
             END IF
             fw(l) = fe(l)
             fs(l,i) = fn(l)
          END DO
       END DO
    END DO
  END SUBROUTINE tstepsic


  ! Iterative implicit version
  ! cimp=1 fully implicit, cimp=0 explicit
  ! coeffs for iterative implicit scheme are defined at cell faces.
  ! eg flux across east face = cie(i)*T(i+1) + ciw(i)*T(i)
  ! converted from ocean to ice 28/10/04 yka, edited nre
  SUBROUTINE tstipsic
    IMPLICIT NONE

    REAL :: tv, ups, pec, centre
    REAL, DIMENSION(0:maxi,0:maxj) :: cie, ciw, cin, cis
    REAL :: varice2(0:maxi+1,0:maxj+1)
    REAL, PARAMETER :: ups0=0.0, cimp=0.5

    ! iterations to solve timestep
    INTEGER :: iits
    INTEGER, PARAMETER :: nii=4

    INTEGER :: i, j, l
    LOGICAL, PARAMETER :: correct=.TRUE.

    ! set b.c's on local variables
    cin(:,0) = 0.0
    cin(:,maxj) = 0.0
    cis(:,0) = 0.0
    cis(:,maxj) = 0.0
    varice2(:,0) = 0.0
    varice2(:,maxj+1) = 0.0

    DO j = 1, maxj
       DO i = 1, maxi
          ! flux to east
          IF (maxk < MAX(k1(i,j), k1(i+1,j))) THEN
             cie(i,j) = 0
             ciw(i,j) = 0
          ELSE
             cie(i,j) = u(1,i,j) * rc(j) * 0.5 * rdphi
             tv = rc(j) * rc(j) * rdphi * diffsic * rdphi
             pec = u(1,i,j) * dphi / diffsic
             ups = pec / (2.0 + abs(pec))
             ciw(i,j) = cie(i,j) * (1+ups) + tv
             cie(i,j) = cie(i,j) * (1-ups) - tv
          END IF
          ! flux to north
          IF (maxk < MAX(k1(i,j), k1(i,j+1))) THEN
             cin(i,j) = 0
             cis(i,j) = 0
          ELSE
             cin(i,j) = cv(j) * u(2,i,j) * 0.5
             tv = cv(j) * cv(j) * rdsv(j) * diffsic
             pec = u(2,i,j) * dsv(j) / diffsic
             ups = pec / (2.0 + abs(pec))
             cis(i,j) = cin(i,j) * (1+ups) + tv
             cin(i,j) = cin(i,j) * (1-ups) - tv
          END IF
       END DO
    END DO
    cie(0,1:maxj) = cie(maxi,1:maxj)
    ciw(0,1:maxj) = ciw(maxi,1:maxj)

    ! loop for ice tracers (Hice, Aice)
    DO l = 1, 2
       ! iterate to solve timestep
       DO iits = 1, nii
          DO j = 1, maxj
             DO i = 1, maxi
                varice2(i,j) = cimp * varice(l,i,j) + &
                     & (1.0 - cimp) * varice1(l,i,j)
             END DO
          END DO
          varice2(0,1:maxj) = varice2(maxi,1:maxj)
          varice2(maxi+1,1:maxj) = varice2(1,1:maxj)
          DO j = 1, maxj
             DO i = 1, maxi
                IF (maxk >= k1(i,j)) THEN
                   centre = dtsic*(ciw(i,j) - cie(i-1,j) + &
                        & (cis(i,j) - cin(i,j-1)) * rds(j))
                   varice(l,i,j) = (varice1(l,i,j) * &
                        & (1.0 - (1.0-cimp) * centre) - dtsic * ( &
                        & -dtha(l,i,j) * tsc + &
                        & cie(i,j) * varice2(i+1,j) &
                        & -ciw(i-1,j) * varice2(i-1,j) + &
                        & (cin(i,j) * varice2(i,j+1) &
                        & -cis(i,j-1) * varice2(i,j-1)) * rds(j))) / &
                        & (1 + cimp * centre)
                END IF
             END DO
          END DO
       END DO
       IF (correct) THEN
          DO j = 1, maxj
             DO i = 1, maxi
                varice2(i,j) = 0.5 * (varice2(i,j) + cimp * varice(l,i,j) + &
                     & (1.0 - cimp) * varice1(l,i,j))
             END DO
          END DO
          varice2(0,1:maxj) = varice2(maxi,1:maxj)
          varice2(maxi+1,1:maxj) = varice2(1,1:maxj)
          DO j = 1, maxj
             DO i = 1, maxi
                IF (maxk >= k1(i,j)) THEN
                   ! explicit and conservative corrector step
                   varice(l,i,j) =  varice1(l,i,j)- dtsic * ( &
                        & -dtha(l,i,j) * tsc + &
                        & cie(i,j) * varice2(i+1,j) &
                        & -ciw(i-1,j) * varice2(i-1,j) + &
                        & (cin(i,j) * varice2(i,j+1) &
                        & -cis(i,j-1) * varice2(i,j-1)) * rds(j)) &
                        & -dtsic*varice2(i,j) * ( &
                        & ciw(i,j) - cie(i-1,j) + &
                        & (cis(i,j) - cin(i,j-1)) * rds(j))
                END IF
             END DO
          END DO
       END IF
       ! calculate dynamical and thermal component of ice evolution
       ! (this is purely diagnostic)
       DO j = 1, maxj
          DO i = 1, maxi
             IF (maxk >= k1(i,j)) THEN
                variceth(l,i,j) = tsc * dtsic * dtha(l,i,j)
                varicedy(l,i,j) = &
                     & varice(l,i,j) - varice1(l,i,j) - variceth(l,i,j)
             END IF
          END DO
       END DO
    END DO
  END SUBROUTINE tstipsic

END MODULE gold_seaice

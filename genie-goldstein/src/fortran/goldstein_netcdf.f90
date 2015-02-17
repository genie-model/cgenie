MODULE goldstein_netcdf

  USE goldstein_lib
  USE genie_control, ONLY: nall, nfiles, nmaxdims
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: ini_netcdf_ocn
  PUBLIC :: write_netcdf_ocn
  PUBLIC :: end_netcdf_ocn
  PUBLIC :: nclon1, nclon2, nclon3, nclat1, nclat2, nclat3
  PUBLIC :: ncdepth, ncdepth1

  REAL, DIMENSION(maxi) :: nclon1, nclon2, nclon3
  REAL, DIMENSION(maxj) :: nclat1, nclat2, nclat3
  REAL :: ncdepth(maxk), ncdepth1(maxk+1)

  INTEGER :: nco(nfiles), iddimo(nall, nfiles), idvaro(nall, nfiles)

CONTAINS

  ! Begin NetCDF initialisation process
  SUBROUTINE ini_netcdf_ocn(istep, imode)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep, imode

    REAL :: day, rtime
    INTEGER :: iyear, imonth
    day = (istep * dt(1) * tsc / 86400.0)
    iyear = INT((day + 0.001) / yearlen)
    imonth = INT((day - iyear * yearlen) / (yearlen / 12.0)) + 1
    rtime = iyear + ((day - iyear * yearlen)) / yearlen
    IF (debug_loop) &
         & PRINT *, 'istep', istep, 'day', day, 'iyear', iyear, &
         & 'imonth', imonth, 'rtime', rtime
    CALL ini_netcdf_ocn1(outdir_name, lenout, lout, imonth, iyear, rtime, &
         & nclon1, nclat1, nclon2, nclat2, nclon3, nclat3, &
         & ncdepth, ncdepth1, imax, jmax, kmax, imode)
  END SUBROUTINE ini_netcdf_ocn


  ! Continue NetCDF initialisation process
  SUBROUTINE ini_netcdf_ocn1(dir_name, ilen, runid, imonth, iyear, rtime, &
       & alon1, alat1, alon2, alat2, alon3, alat3, depth, depth1, &
       & mg, jgg, nl, imode)
    IMPLICIT NONE
    CHARACTER(LEN=100), INTENT(IN) :: dir_name
    INTEGER, INTENT(IN) :: ilen
    CHARACTER(LEN=3), INTENT(IN) :: runid
    INTEGER, INTENT(IN) :: imonth, iyear, imode
    REAL, INTENT(IN) :: rtime
    REAL, INTENT(IN) :: depth(nl), depth1(nl+1)
    REAL, DIMENSION(mg), INTENT(IN) :: alon1, alon2, alon3
    REAL, DIMENSION(jgg), INTENT(IN) :: alat1, alat2, alat3
    INTEGER, INTENT(IN) :: mg, jgg, nl

    INTEGER, PARAMETER :: nmaxdims=4
    INTEGER :: ndim, nvar
    INTEGER, DIMENSION(nall) :: natts, nattsvar, vdims, ndims
    INTEGER :: vadims(nmaxdims,nall)
    CHARACTER(LEN=200), DIMENSION(nall) :: dimname, varname
    CHARACTER(LEN=200), DIMENSION(2,nmaxdims,nall) :: attdimname, attvarname
    INTEGER, PARAMETER :: imax=100, jmax=100, kmax=100, lmax=10000
    REAL :: xcoord(imax), ycoord(jmax), zcoord(kmax), tcoord(lmax)
    INTEGER :: i, itime, ifname1
    CHARACTER(LEN=200) :: fname1
    CHARACTER(LEN=10) :: cyear
    CHARACTER(LEN=2), PARAMETER, DIMENSION(13) :: cmon = &
         & (/ '00', '01', '02', '03', '04', '05', '06', &
         &    '07', '08', '09', '10', '11', '12' /)
    INTEGER, EXTERNAL :: lnsig

    itime = 1

    CALL setup_nc_ocn(mg, jgg, nl, itime, nmaxdims, nall, ndim, nvar, natts, &
         & nattsvar, vdims, vadims, ndims, dimname, varname, &
         & attdimname, attvarname)

    WRITE (cyear,'(i10.10)')iyear
    IF (imode.eq.1) THEN
       fname1 = dir_name(1:ilen)//'gold_'//runid(1:3)//'_rs_'// &
            & cyear//'_'//cmon(imonth)//'.nc'
    ELSE IF (imode.eq.2) THEN
       fname1 = dir_name(1:ilen)//'gold_'//runid(1:3)//'_av_'// &
            & cyear//'_'//cmon(imonth)//'.nc'
    END IF

    ifname1 = lnsig(fname1)

    CALL ininc(fname1(1:ifname1), nmaxdims, ndim, nvar, natts, nattsvar, &
         & vdims, vadims, ndims, dimname, varname, attdimname, attvarname, &
         & nco(imode), iddimo(1,imode), idvaro(1,imode))

    ! Longitude coordinates (tracer, u-point, v-point)
    xcoord(1:mg) = alon1
    CALL writedim(nco(imode), iddimo(1,imode), xcoord)
    xcoord(1:mg) = alon2
    CALL writedim(nco(imode), iddimo(2,imode), xcoord)
    xcoord(1:mg) = alon3(i)
    CALL writedim(nco(imode), iddimo(3,imode), xcoord)

    ! Latitude coordinates (tracer, u-point, v-point)
    ycoord(1:jgg) = alat1
    CALL writedim(nco(imode), iddimo(4,imode), ycoord)
    ycoord(1:jgg) = alat2
    CALL writedim(nco(imode), iddimo(5,imode), ycoord)
    ! In the following lines, ycoord is extended (as was perhaps
    ! originally intended by Paul) by one point so that OPSI, etc. data
    ! can be correctly plotted.  note : this doesn't affect the integrity
    ! of alat2
    ycoord(1:jgg) = alat3
    ycoord(jgg+1) = 90.0
    CALL writedim(nco(imode), iddimo(6,imode), ycoord)

    ! Depth coordinates (midpoint, box edges)
    zcoord(1:nl) = depth
    CALL writedim(nco(imode), iddimo(7,imode), zcoord)
    zcoord(1:nl+1) = depth1
    CALL writedim(nco(imode), iddimo(8,imode), zcoord)

    ! Time
    tcoord(1) = REAL(rtime)
    CALL writedim(nco(imode), iddimo(9,imode), tcoord)
  END SUBROUTINE ini_netcdf_ocn1

  ! Set up NetCDF file's array names, units, descriptions, etc.
  SUBROUTINE setup_nc_ocn(nlon, nlat, nl, ntime, nmaxdims, nall, &
       & ndim, nvar, natts, nattsvar, vdims, vadims, ndims, dimname, &
       & varname, attdimname, attvarname)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nlon, nlat, nl, ntime, nmaxdims, nall
    INTEGER, INTENT(OUT) :: ndim, nvar
    INTEGER, DIMENSION(nall), INTENT(OUT) :: natts, nattsvar, vdims, ndims
    INTEGER, INTENT(OUT) :: vadims(nmaxdims,nall)
    CHARACTER(LEN=200), DIMENSION(nall), INTENT(OUT) :: dimname, varname
    CHARACTER(LEN=200), DIMENSION(2,nmaxdims,nall), INTENT(OUT) :: &
         & attdimname, attvarname

    INTEGER, EXTERNAL :: loc_dim

    ndim = 0
    nvar = 0

    ndim = ndim + 1
    dimname(ndim) = 'longitude'
    ndims(ndim) = nlon
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'longitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_east'

    ndim = ndim + 1
    dimname(ndim) = 'longitude_1'
    ndims(ndim) = nlon
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'longitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_east'

    ndim = ndim + 1
    dimname(ndim) = 'longitude_2'
    ndims(ndim) = nlon
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'longitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_east'

    ndim = ndim + 1
    dimname(ndim) = 'latitude'
    ndims(ndim) = nlat
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'latitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_north'

    ndim = ndim + 1
    dimname(ndim) = 'latitude_1'
    ndims(ndim) = nlat
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'latitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_north'

    ndim = ndim + 1
    dimname(ndim) = 'latitude_2'
    ndims(ndim) = nlat + 1
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'latitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_north'

    ndim = ndim + 1
    dimname(ndim) = 'depth'
    ndims(ndim) = nl
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'units'
    attdimname(2,1,ndim) = 'm'
    attdimname(1,2,ndim) = 'positive'
    attdimname(2,2,ndim) = 'down'

    ndim = ndim + 1
    dimname(ndim) = 'depth_1'
    ndims(ndim) = nl + 1
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'units'
    attdimname(2,1,ndim) = 'm'
    attdimname(1,2,ndim) = 'positive'
    attdimname(2,2,ndim) = 'down'

    ndim = ndim + 1
    dimname(ndim) = 'time'
    ndims(ndim) = ntime
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'time'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'years'

    nvar = nvar + 1
    varname(nvar) = 'opsi'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('latitude_2',dimname,nall)
    vadims(2,nvar) = loc_dim('depth_1',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Global Meridional Streamfucntion'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'Sv'

    nvar = nvar + 1
    varname(nvar) = 'opsi_a'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('latitude_2',dimname,nall)
    vadims(2,nvar) = loc_dim('depth_1',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Atlantic Meridional Streamfucntion'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'Sv'

    nvar = nvar + 1
    varname(nvar) = 'opsi_p'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('latitude_2',dimname,nall)
    vadims(2,nvar) = loc_dim('depth_1',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Pacific Meridional Streamfucntion'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'Sv'

    nvar = nvar + 1
    varname(nvar) = 'temp'
    vdims(nvar) = 4
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('depth',dimname,nall)
    vadims(4,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Temperature'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'C'

    nvar = nvar + 1
    varname(nvar) = 'salinity'
    vdims(nvar) = 4
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('depth',dimname,nall)
    vadims(4,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Salinity'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'PSU'

    nvar = nvar + 1
    varname(nvar) = 'density'
    vdims(nvar) = 4
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('depth',dimname,nall)
    vadims(4,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Density'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'ks/m3'

    nvar = nvar + 1
    varname(nvar) = 'uvel'
    vdims(nvar) = 4
    vadims(1,nvar) = loc_dim('longitude_1',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude_1',dimname,nall)
    vadims(3,nvar) = loc_dim('depth',dimname,nall)
    vadims(4,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Ocean Eastward Current'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'm/s'

    nvar = nvar + 1
    varname(nvar) = 'vvel'
    vdims(nvar) = 4
    vadims(1,nvar) = loc_dim('longitude_2',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude_2',dimname,nall)
    vadims(3,nvar) = loc_dim('depth',dimname,nall)
    vadims(4,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Ocean Northward Current'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'm/s'

    nvar = nvar + 1
    varname(nvar) = 'wvel'
    vdims(nvar) = 4
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('depth_1',dimname,nall)
    vadims(4,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Vertical Current'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'm/s'

    nvar = nvar + 1
    varname(nvar) = 'latent'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Latent heat flux'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'W/m2'

    nvar = nvar + 1
    varname(nvar) = 'sensible'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sensible heat flux'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'W/m2'

    nvar = nvar + 1
    varname(nvar) = 'netsolar'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Net solar heat flux'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'W/m2'

    nvar = nvar + 1
    varname(nvar) = 'netlong'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Net longwave heat flux'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'W/m2'

    nvar = nvar + 1
    varname(nvar) = 'sic_heat'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sea-ice heat flux'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'W/m2'

    nvar = nvar + 1
    varname(nvar) = 'evap'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Evaporation'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'mm/s'

    nvar = nvar + 1
    varname(nvar) = 'pptn'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Precipitation'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'mm/s'

    nvar = nvar + 1
    varname(nvar) = 'runoff'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Runoff'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'mm/s'

    nvar = nvar + 1
    varname(nvar) = 'sic_fw'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sea-ice freshwater flux'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'mm/s'

    nvar = nvar + 1
    varname(nvar) = 'bathymetry'
    vdims(nvar) = 2
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Ocean depth'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'm'
  END SUBROUTINE setup_nc_ocn

  ! Write data to NetCDF file
  SUBROUTINE write_netcdf_ocn(imax, jmax, kmax, k1, depth1, &
       & opsi, opsia, opsip, ts, u, rho, fx0flux, fwflux, &
       & work, dsc, usc, rsc, saln0, maxi, maxj, maxk, maxl, imode)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: imax, jmax, kmax
    INTEGER, INTENT(IN) :: k1(0:maxi+1,0:maxj+1)
    REAL, INTENT(IN) :: depth1(maxk+1)
    REAL, DIMENSION(0:maxj,0:maxk), INTENT(IN) :: opsi, opsia, opsip
    REAL, INTENT(IN) :: ts(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
    REAL, INTENT(IN) :: rho(0:maxi+1,0:maxj+1,0:maxk)
    REAL, INTENT(IN) :: fx0flux(5,maxi,maxj), fwflux(4,maxi,maxj)
    REAL, INTENT(IN) :: dsc, usc, rsc
    REAL, INTENT(IN) :: u(3,0:maxi,0:maxj,maxk)
    REAL, INTENT(IN) :: saln0
    REAL, INTENT(INOUT) :: work((maxi+1)*(maxj+1)*(maxk+1))
    INTEGER, INTENT(IN) :: maxi, maxj, maxk, maxl
    INTEGER, INTENT(IN) :: imode

    INTEGER :: i

    work = 0.0

    ! Global streamfunction
    CALL prep_netcdf_ocn(opsi, 0, jmax, kmax, work, k1, depth1, &
         & dsc * usc * rsc * 1e-6, 1, 1, 1)
    CALL writevar(nco(imode), idvaro(1, imode), work)

    ! Atlantic streamfunction
    CALL prep_netcdf_ocn(opsia, 0, jmax, kmax, work, k1, depth1, &
         & dsc * usc * rsc * 1e-6, 1, 1, 1)
    CALL writevar(nco(imode), idvaro(2,imode), work)

    ! Pacific streamfunction
    CALL prep_netcdf_ocn(opsip, 0, jmax, kmax, work, k1, depth1, &
         & dsc * usc * rsc * 1e-6, 1, 1, 1)
    CALL writevar(nco(imode), idvaro(3,imode), work)

    ! Temperature (i.e. final argument = 1)
    CALL prep_netcdf_ocn(ts, imax, jmax, kmax, work, k1, depth1, &
         & 1.0, 2, maxl, 1)
    CALL writevar(nco(imode), idvaro(4,imode), work)

    ! Salinity (i.e. final argument = 2)
    CALL prep_netcdf_ocn(ts, imax, jmax, kmax, work, k1, depth1, &
         & 1.0, 2, maxl, 2)
    ! Correct salinity to PSU
    DO i = 1, ((maxi+1)*(maxj+1)*(maxk+1))
       IF (work(i) > -99999.0) work(i) = work(i) + REAL(saln0)
    END DO
    CALL writevar(nco(imode), idvaro(5,imode), work)

    ! Density
    CALL prep_netcdf_ocn(rho, imax, jmax, kmax, work, k1, depth1, 1.0, 2, 1, 1)
    CALL writevar(nco(imode), idvaro(6,imode), work)

    ! Ocean velocity (component 1)
    CALL prep_netcdf_ocn(u, imax, jmax, kmax, work, k1, depth1, usc, 3, 3, 1)
    CALL writevar(nco(imode), idvaro(7,imode), work)

    ! Ocean velocity (component 2)
    CALL prep_netcdf_ocn(u, imax, jmax, kmax, work, k1, depth1, usc, 4, 3, 2)
    CALL writevar(nco(imode), idvaro(8,imode), work)

    ! Ocean velocity (component 3)
    CALL prep_netcdf_ocn_w(imax, jmax, kmax, u, work, k1, usc * dsc / rsc)
    CALL writevar(nco(imode), idvaro(9,imode), work)

    ! Latent heat flux
    CALL prep_netcdf_ocn(fx0flux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 5, 4)
    CALL writevar(nco(imode), idvaro(10,imode), work)

    ! Sensible heat flux
    CALL prep_netcdf_ocn(fx0flux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 5, 2)
    CALL writevar(nco(imode), idvaro(11,imode), work)

    ! Net solar heat flux
    CALL prep_netcdf_ocn(fx0flux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 5, 1)
    CALL writevar(nco(imode), idvaro(12,imode), work)

    ! Net longwave heat flux
    CALL prep_netcdf_ocn(fx0flux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 5, 3)
    CALL writevar(nco(imode), idvaro(13,imode), work)

    ! Sea-ice heat flux
    CALL prep_netcdf_ocn(fx0flux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 5, 5)
    CALL writevar(nco(imode), idvaro(14,imode), work)

    ! Evaporation
    CALL prep_netcdf_ocn(fwflux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 4, 2)
    CALL writevar(nco(imode), idvaro(15,imode), work)

    ! Precipitation
    CALL prep_netcdf_ocn(fwflux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 4, 1)
    CALL writevar(nco(imode), idvaro(16,imode), work)

    ! Runoff
    CALL prep_netcdf_ocn(fwflux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 4, 3)
    CALL writevar(nco(imode), idvaro(17,imode), work)

    ! Sea-ice freshwater flux
    CALL prep_netcdf_ocn(fwflux, imax, jmax, 0, work, k1, depth1, 1.0, 8, 4, 4)
    CALL writevar(nco(imode), idvaro(18,imode), work)

    ! Ocean bathymetry
    CALL prep_netcdf_ocn(fwflux, imax, jmax, kmax, work, k1, depth1, &
         & 1.0, 7, 4, 1)
    CALL writevar(nco(imode), idvaro(19,imode), work)
  END SUBROUTINE write_netcdf_ocn


  ! Reorganise data for NetCDF file (e.g. re-orientation)
  SUBROUTINE prep_netcdf_ocn(data_i, mg, jgg, nl, data_o, iland, depth1, &
       & scale, itype, ilev, it)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: mg, jgg, nl, itype, it, ilev, iland(*)
    REAL, INTENT(IN) :: scale, data_i(*), depth1(nl+1)
    REAL, INTENT(OUT) :: data_o(*)

    SELECT CASE (itype)
    CASE (1)
       CALL flip_vert(data_i, data_o, jgg, nl, scale)
    CASE (2)
       CALL flip_both1(data_i, data_o, mg, jgg, nl, iland, scale, ilev, it)
    CASE (3)
       CALL flip_both2(data_i, data_o, mg, jgg, nl, mg, jgg, nl, iland, &
            & scale, ilev, it, 1)
    CASE (4)
       CALL flip_both2(data_i, data_o, mg, jgg, nl, mg, jgg + 1, nl, iland, &
            & scale, ilev, it, 1)
    CASE (5)
       CALL flip_both2(data_i, data_o, mg, jgg, nl, mg, jgg, nl, iland, &
            & scale, ilev, it, 1)
    CASE (6)
       CALL twodee_tracer(data_i, data_o, mg, jgg, mg, jgg, iland, scale)
    CASE (7)
       CALL twodee_bathy(data_o, depth1, mg, jgg, nl, mg, jgg,   iland)
    CASE (8)
       CALL twodee_tracer2(data_i, data_o, mg, jgg, mg, jgg, iland, &
            & scale, ilev, it)
    END SELECT
  END SUBROUTINE prep_netcdf_ocn


  SUBROUTINE prep_netcdf_ocn_w(imax, jmax, kmax, data_i, data_o, iland, scale)
    IMPLICIT NONE
    INTEGER, INTENT(IN) ::imax, jmax, kmax
    REAL, INTENT(IN) :: data_i(3,0:imax,0:jmax,1:kmax)
    REAL, INTENT(OUT) :: data_o((imax+1)*(jmax+1)*(kmax+1))
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale

    INTEGER :: i, j, k

    DO k = 1, kmax
       DO j = 1, jmax
          DO i = 1, imax
             IF (iland(i,j) >= 90 .OR. iland(i,j) > (kmax+1-k)) THEN
                data_o(i + (j-1)*imax + (k-1)*jmax*imax) = -99999.0
             ELSE
                data_o(i + (j-1)*imax + (k-1)*jmax*imax) = &
                     & REAL(scale * data_i(3,i,j,kmax+1-k))
             END IF
          END DO
       END DO
    END DO
    k = kmax + 1
    DO j = 1, jmax
       DO i = 1, imax
          data_o(i + (j-1)*imax + (k-1)*jmax*imax) = -99999.0
       END DO
    END DO
  END SUBROUTINE prep_netcdf_ocn_w


  ! Flip 2D arrays so that low k values are shallower
  SUBROUTINE flip_vert(data_in, data_out, iy, iz, scale)
    IMPLICIT NONE
    REAL, INTENT(IN) :: data_in(0:iy,0:iz)
    REAL, INTENT(OUT) :: data_out(0:iy,0:iz)
    INTEGER, INTENT(IN) :: iy, iz
    REAL, INTENT(IN) :: scale

    INTEGER :: j, k

    DO k = 0, iz
       DO j = 0, iy
          data_out(j,iz-k) = REAL(scale * data_in(j,k))
       END DO
    END DO
  END SUBROUTINE flip_vert


  ! Flip 3D arrays so that low k values are shallower (tracer arrays)
  SUBROUTINE flip_both1(temper, temp1, imax, jmax, kmax, iland, scale, iter, it)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(iter,0:imax+1,0:jmax+1,0:kmax+1)
    REAL, INTENT(OUT) :: temp1(imax,jmax,kmax)
    INTEGER, INTENT(IN) :: imax, jmax, kmax
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale
    INTEGER, INTENT(IN) :: it, iter

    INTEGER :: i, j, k

    DO k = 1, kmax
       DO j = 1, jmax
          DO i = 1, imax
             IF (iland(i,j) >= 90 .OR. iland(i,j) > k) THEN
                temp1(i,j,kmax+1-k) = -99999.0
             ELSE
                temp1(i,j,kmax+1-k) = REAL(scale * temper(it,i,j,k))
             END IF
          END DO
       END DO
    END DO
  END SUBROUTINE flip_both1


  ! Flip 3D arrays so that low k values are shallower (velocity arrays)
  SUBROUTINE flip_both2(temper, temp1, imax, jmax, kmax, ix, iy, iz, &
       & iland, scale, iter, it, idom)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(iter,0:imax,0:jmax,kmax)
    REAL, INTENT(OUT) :: temp1(ix,iy,iz)
    INTEGER, INTENT(IN) ::  imax, jmax, kmax, ix, iy, iz
    INTEGER, INTENT(IN) ::  iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) ::  scale
    INTEGER, INTENT(IN) :: iter, it, idom

    INTEGER :: i, j, k, jj

    DO k = 1, iz
       jj = 0
       DO j = idom, iy
          jj = jj + 1
          DO i = 1, imax
             IF (iland(i,j) >= 90 .OR. iland(i,j) > k) THEN
                temp1(i,jj,kmax+1-k) = -99999.0
             ELSE
                temp1(i,jj,kmax+1-k) = REAL(scale * temper(it,i,j,k))
             END IF
          END DO
       END DO
    END DO

    IF (it == 3) temp1(1:imax,1:jmax,iz) = 0.0
  END SUBROUTINE flip_both2


  ! Organise a two-dimensional array that's on the tracer grid
  SUBROUTINE twodee_tracer(temper,  temp1, imax, jmax, ix, iy, iland, scale)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(imax,jmax)
    REAL, INTENT(OUT) :: temp1(ix,iy)
    INTEGER, INTENT(IN) :: imax, jmax, ix, iy
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale

    WHERE (iland(1:imax,1:jmax) >= 90)
       temp1 = -99999.0
    ELSEWHERE
       temp1 = REAL(scale * temper)
    END WHERE
  END SUBROUTINE twodee_tracer

  ! Organise a two-dimensional array that's on the tracer grid
  SUBROUTINE twodee_tracer2(temper, temp1, imax, jmax, ix, iy, &
       & iland, scale, iter, it)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(iter,imax, jmax)
    REAL, INTENT(OUT) :: temp1(ix,iy)
    INTEGER, INTENT(IN) :: imax, jmax, ix, iy
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale
    INTEGER, INTENT(IN) :: iter, it

    WHERE (iland(1:imax,1:jmax) >= 90)
       temp1 = -99999.0
    ELSEWHERE
       temp1 = REAL(scale * temper(it,:,:))
    END WHERE
  END SUBROUTINE twodee_tracer2


  ! Organise a two-dimensional bathymetry array on the tracer grid
  SUBROUTINE twodee_bathy(temp1, depth1, imax, jmax, kmax, ix, iy, iland)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: temp1(ix,iy)
    REAL, INTENT(IN) :: depth1(kmax+1)
    INTEGER, INTENT(IN) :: imax, jmax, kmax, ix, iy
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)

    INTEGER :: i, j, here_dep

    DO j = 1, jmax
       DO i = 1, imax
          IF (iland(i,j) >= 90) THEN
             temp1(i,j) = -99999.0
          ELSE
             here_dep = kmax - iland(i,j) + 2
             IF (here_dep < 1) PRINT *, 'bathymetry too shallow'
             IF (here_dep > kmax+1) PRINT *, 'bathymetry too deep'
             temp1(i,j) = depth1(here_dep)
          END IF
       END DO
    END DO
  END SUBROUTINE twodee_bathy


  ! End NetCDF-writing process and closes netCDF file
  SUBROUTINE end_netcdf_ocn(imode)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: imode

    PRINT *, ' calling end_netcdf_ocn for file = ', imode
    CALL closenc(nco(imode))
    PRINT *, ' called end_netcdf_ocn for file = ', imode
  END SUBROUTINE end_netcdf_ocn

END MODULE goldstein_netcdf

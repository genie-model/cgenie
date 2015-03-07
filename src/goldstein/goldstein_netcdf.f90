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
    USE writenc6
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
    REAL :: ycoord(jmax)
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
    CALL writedim(nco(imode), iddimo(1,imode), alon1)
    CALL writedim(nco(imode), iddimo(2,imode), alon2)
    CALL writedim(nco(imode), iddimo(3,imode), alon3)

    ! Latitude coordinates (tracer, u-point, v-point)
    CALL writedim(nco(imode), iddimo(4,imode), alat1)
    CALL writedim(nco(imode), iddimo(5,imode), alat2)
    ! In the following lines, ycoord is extended (as was perhaps
    ! originally intended by Paul) by one point so that OPSI, etc. data
    ! can be correctly plotted.  note : this doesn't affect the integrity
    ! of alat2
    ycoord(1:jgg) = alat3
    ycoord(jgg+1) = 90.0
    CALL writedim(nco(imode), iddimo(6,imode), ycoord(1:jgg+1))

    ! Depth coordinates (midpoint, box edges)
    CALL writedim(nco(imode), iddimo(7,imode), depth)
    CALL writedim(nco(imode), iddimo(8,imode), depth1)

    ! Time
    CALL writedim(nco(imode), iddimo(9,imode), (/ REAL(rtime) /))
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
    USE writenc6
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
    REAL, INTENT(INOUT) :: work(0:maxi, 0:maxj, 0:maxk)
    INTEGER, INTENT(IN) :: maxi, maxj, maxk, maxl
    INTEGER, INTENT(IN) :: imode

    INTEGER :: i

    work = 0.0

    ! Global streamfunction
    CALL flip_vert(opsi, work(1,:,:), jmax, kmax, dsc * usc * rsc * 1e-6)
    CALL writevar(nco(imode), idvaro(1, imode), work(1,:,:))

    ! Atlantic streamfunction
    CALL flip_vert(opsia, work(1,:,:), jmax, kmax, dsc * usc * rsc * 1e-6)
    CALL writevar(nco(imode), idvaro(2, imode), work(1,:,:))

    ! Pacific streamfunction
    CALL flip_vert(opsip, work(1,:,:), jmax, kmax, dsc * usc * rsc * 1e-6)
    CALL writevar(nco(imode), idvaro(3, imode), work(1,:,:))

    ! Temperature (i.e. final argument = 1)
    CALL flip_both1(ts(1,:,:,:), work(1:imax,1:jmax,1:kmax), &
         & imax, jmax, kmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(4,imode), work(1:imax,1:jmax,1:kmax))

    ! Salinity (i.e. final argument = 2)
    CALL flip_both1(ts(2,:,:,:), work(1:imax,1:jmax,1:kmax), &
         & imax, jmax, kmax, k1, 1.0)
    ! Correct salinity to PSU
    WHERE (work > -99999.0)
       work = work + REAL(saln0)
    END WHERE
    CALL writevar(nco(imode), idvaro(5,imode), work(1:imax,1:jmax,1:kmax))

    ! Density
    CALL flip_both1(rho, work(1:imax,1:jmax,1:kmax), &
         & imax, jmax, kmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(6,imode), work(1:imax,1:jmax,1:kmax))

    ! Ocean velocity (component 1)
    CALL flip_both2(u(1,:,:,:), work(1:imax,1:jmax,1:kmax), &
         & imax, jmax, kmax, imax, jmax, kmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(7,imode), work(1:imax,1:jmax,1:kmax))

    ! Ocean velocity (component 2)
    CALL flip_both2(u(2,:,:,:), work(1:imax,1:jmax+1,1:kmax), &
         & imax, jmax, kmax, imax, jmax+1, kmax, k1, usc)
    CALL writevar(nco(imode), idvaro(8,imode), work(1:imax,1:jmax+1,1:kmax))

    ! Ocean velocity (component 3)
    CALL prep_netcdf_ocn_w(imax, jmax, kmax, u, work, k1, usc * dsc / rsc)
    CALL writevar(nco(imode), idvaro(9,imode), work(1:imax,0:jmax-1,0:kmax))

    ! Latent heat flux
    CALL twodee_tracer2(fx0flux(4,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(10,imode), work(1:imax,1:jmax,1))

    ! Sensible heat flux
    CALL twodee_tracer2(fx0flux(2,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(11,imode), work(1:imax,1:jmax,1))

    ! Net solar heat flux
    CALL twodee_tracer2(fx0flux(1,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(12,imode), work(1:imax,1:jmax,1))

    ! Net longwave heat flux
    CALL twodee_tracer2(fx0flux(3,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(13,imode), work(1:imax,1:jmax,1))

    ! Sea-ice heat flux
    CALL twodee_tracer2(fx0flux(5,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(14,imode), work(1:imax,1:jmax,1))

    ! Evaporation
    CALL twodee_tracer2(fwflux(2,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(15,imode), work(1:imax,1:jmax,1))

    ! Precipitation
    CALL twodee_tracer2(fwflux(1,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(16,imode), work(1:imax,1:jmax,1))

    ! Runoff
    CALL twodee_tracer2(fwflux(3,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(17,imode), work(1:imax,1:jmax,1))

    ! Sea-ice freshwater flux
    CALL twodee_tracer2(fwflux(4,:,:), work(1:imax,1:jmax,1), &
         & imax, jmax, k1, 1.0)
    CALL writevar(nco(imode), idvaro(18,imode), work(1:imax,1:jmax,1))

    ! Ocean bathymetry
    CALL twodee_bathy(work(1:imax,1:jmax,1), depth1, imax, jmax, kmax, &
         & imax, jmax, k1)
    CALL writevar(nco(imode), idvaro(19,imode), work(1:imax,1:jmax,1))
  END SUBROUTINE write_netcdf_ocn


  SUBROUTINE prep_netcdf_ocn_w(imax, jmax, kmax, data_i, data_o, iland, scale)
    IMPLICIT NONE
    INTEGER, INTENT(IN) ::imax, jmax, kmax
    REAL, INTENT(IN) :: data_i(3,0:imax,0:jmax,1:kmax)
    REAL, INTENT(OUT) :: data_o(imax+1, jmax+1, kmax+1)
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale

    INTEGER :: i, j, k

    DO k = 1, kmax
       DO j = 1, jmax
          DO i = 1, imax
             IF (iland(i,j) >= 90 .OR. iland(i,j) > (kmax+1-k)) THEN
                data_o(i,j-1,k-1) = -99999.0
             ELSE
                data_o(i,j-1,k-1) = REAL(scale * data_i(3,i,j,kmax+1-k))
             END IF
          END DO
       END DO
    END DO
    data_o(1:imax,0:jmax-1,kmax) = -99999.0
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
  SUBROUTINE flip_both1(temper, temp1, imax, jmax, kmax, iland, scale)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(0:imax+1,0:jmax+1,0:kmax+1)
    REAL, INTENT(OUT) :: temp1(imax,jmax,kmax)
    INTEGER, INTENT(IN) :: imax, jmax, kmax
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale

    INTEGER :: k

    DO k = 1, kmax
       WHERE (iland >= 90 .OR. iland > k)
          temp1(:,:,kmax+1-k) = -99999.0
       ELSEWHERE
          temp1(:,:,kmax+1-k) = REAL(scale * temper(1:imax,1:jmax,k))
       END WHERE
    END DO
  END SUBROUTINE flip_both1


  ! Flip 3D arrays so that low k values are shallower (velocity arrays)
  SUBROUTINE flip_both2(temper, temp1, imax, jmax, kmax, ix, iy, iz, &
       & iland, scale)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(0:imax,0:jmax,kmax)
    REAL, INTENT(OUT) :: temp1(ix,iy,iz)
    INTEGER, INTENT(IN) ::  imax, jmax, kmax, ix, iy, iz
    INTEGER, INTENT(IN) ::  iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) ::  scale

    INTEGER :: i, j, k, jj

    DO k = 1, iz
       jj = 0
       DO j = 1, iy
          jj = jj + 1
          DO i = 1, imax
             IF (iland(i,j) >= 90 .OR. iland(i,j) > k) THEN
                temp1(i,jj,kmax+1-k) = -99999.0
             ELSE
                temp1(i,jj,kmax+1-k) = REAL(scale * temper(i,j,k))
             END IF
          END DO
       END DO
    END DO
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
  SUBROUTINE twodee_tracer2(temper, temp1, imax, jmax, iland, scale)
    IMPLICIT NONE
    REAL, INTENT(IN) :: temper(imax, jmax)
    REAL, INTENT(OUT) :: temp1(imax,jmax)
    INTEGER, INTENT(IN) :: imax, jmax
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL, INTENT(IN) :: scale

    WHERE (iland(1:imax,1:jmax) >= 90)
       temp1 = -99999.0
    ELSEWHERE
       temp1 = REAL(scale * temper)
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
    USE writenc6
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: imode

    PRINT *, ' calling end_netcdf_ocn for file = ', imode
    CALL closenc(nco(imode))
    PRINT *, ' called end_netcdf_ocn for file = ', imode
  END SUBROUTINE end_netcdf_ocn

END MODULE goldstein_netcdf

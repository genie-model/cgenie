MODULE embm_netcdf

  USE embm_lib
  USE genie_control, ONLY: nall, nfiles, nmaxdims
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: ini_netcdf_embm
  PUBLIC :: write_netcdf_embm
  PUBLIC :: end_netcdf_embm
  PUBLIC :: nclon1, nclon2, nclon3, nclat1, nclat2, nclat3

  REAL, DIMENSION(maxi) :: nclon1, nclon2, nclon3
  REAL, DIMENSION(maxj) :: nclat1, nclat2, nclat3

  INTEGER :: nco(nfiles), iddimo(nall, nfiles), idvaro(nall, nfiles)

CONTAINS

  ! Begin netCDF initialisation process
  SUBROUTINE ini_netcdf_embm(istep, imode)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep, imode

    REAL :: day, rtime
    INTEGER :: iyear, imonth

    day = istep * dtatm * tsc / 86400.0
    ! small constant added to day calculation for round-off reasons
    ! (i.e. 365.2499 vs. 365.25)
    iyear = INT((day + 0.001) / yearlen)
    imonth = INT((day - iyear * yearlen) / (yearlen / 12.0)) + 1
    rtime = iyear + ((day - iyear * yearlen)) / yearlen
    IF (debug_loop) &
         & PRINT *, 'istep', istep, 'day', day, 'iyear', iyear, &
         & 'imonth', imonth, 'rtime',rtime
    CALL ini_netcdf_embm1(outdir_name, lenout, lout, imonth, rtime, &
         & nclon1, nclat1, nclon2, nclat2, nclon3, nclat3, maxi, maxj, imode)
  END SUBROUTINE ini_netcdf_embm


  ! Continue netCDF initialisation process
  SUBROUTINE ini_netcdf_embm1(dir_name, ilen, runid, imonth, rtime, &
       & alon1, alat1, alon2, alat2, alon3, alat3, mg, jgg, imode)
    USE writenc6
    IMPLICIT NONE

    CHARACTER(LEN=100), INTENT(IN) :: dir_name
    INTEGER, INTENT(IN) :: ilen
    CHARACTER(LEN=3), INTENT(IN) :: runid
    INTEGER, INTENT(IN) :: mg, jgg
    REAL, DIMENSION(mg), INTENT(IN) :: alon1, alon2, alon3
    REAL, DIMENSION(jgg), INTENT(IN) :: alat1, alat2, alat3
    REAL, INTENT(IN) :: rtime
    INTEGER, INTENT(IN) :: imonth, imode

    INTEGER, PARAMETER :: nmaxdims=4
    INTEGER :: ndim, nvar
    INTEGER, DIMENSION(nall) :: natts, nattsvar, vdims, ndims
    INTEGER, DIMENSION(nmaxdims,nall) :: vadims
    CHARACTER(LEN=200), DIMENSION(nall) :: dimname, varname
    CHARACTER(LEN=200), DIMENSION(2,nmaxdims,nall) :: attdimname, attvarname

    INTEGER :: itime, ifname1
    CHARACTER(LEN=200) :: fname1
    CHARACTER(LEN=10) :: cyear
    CHARACTER(LEN=2), DIMENSION(13) :: cmon = &
         & (/ '00','01','02','03','04','05','06', &
         &    '07','08','09','10','11','12' /)

    INTEGER, EXTERNAL :: lnsig

    itime = 1

    CALL setup_nc_embm(mg, jgg, itime, nmaxdims, ndim, nvar, natts, &
         & nattsvar, vdims, vadims, ndims, dimname, varname, &
         & attdimname, attvarname)
    IF (imode == 1) THEN
       fname1 = dir_name(1:ilen) // 'embm_' // runid(1:3) // '_rs_' // &
            & cyear // '_' // cmon(imonth) // '.nc'
    else IF (imode == 2) THEN
       fname1 = dir_name(1:ilen) // 'embm_' // runid(1:3) // '_av_' // &
            & cyear // '_' // cmon(imonth) // '.nc'
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
    CALL writedim(nco(imode), iddimo(6,imode), alat3)

    ! Time
    CALL writedim(nco(imode), iddimo(7,imode), (/ REAL(rtime) /))
  END SUBROUTINE ini_netcdf_embm1


  ! Set up NetCDF file's array names, units, descriptions, etc.
  SUBROUTINE setup_nc_embm(nlon, nlat, ntime, nmaxdims, &
       & ndim, nvar, natts, nattsvar, vdims, vadims, ndims, &
       & dimname, varname, attdimname, attvarname)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nlon, nlat, ntime, nmaxdims
    INTEGER, INTENT(OUT) :: ndim, nvar
    INTEGER, DIMENSION(nall), INTENT(OUT) :: natts, nattsvar, vdims, ndims
    INTEGER, INTENT(OUT) :: vadims(nmaxdims,nall)
    CHARACTER(LEN=200), DIMENSION(nall), INTENT(OUT) :: dimname, varname
    CHARACTER(LEN=200), DIMENSION(2,nmaxdims,nall), INTENT(OUT) :: &
         & attdimname, attvarname
    INTEGER, EXTERNAL :: loc_dim

    ! This sets up a file similar to .pc files
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
    ndims(ndim) = nlat
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'latitude'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'degrees_north'

    ndim = ndim + 1
    dimname(ndim) = 'time'
    ndims(ndim) = ntime
    natts(ndim) = 2
    attdimname(1,1,ndim) = 'long_name'
    attdimname(2,1,ndim) = 'time'
    attdimname(1,2,ndim) = 'units'
    attdimname(2,2,ndim) = 'years'

    nvar = nvar + 1
    varname(nvar) = 'air_temp'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Air temperature'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'C'

    nvar = nvar + 1
    varname(nvar) = 'humidity'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Specific humidity'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'g/kg'

    nvar = nvar + 1
    varname(nvar) = 'dry_air_humidity'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Specific humidity after precipitation'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'g/kg'

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
    varname(nvar) = 'landmask'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Land mask'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'dimensionless'

    nvar = nvar + 1
    varname(nvar) = 'dry_air_relative_humidity'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Relative humidity after precipitation'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = '1'
  END SUBROUTINE setup_nc_embm


  ! Write data to netCDF file
  SUBROUTINE write_netcdf_embm(k1, tq, q_pa, rq_pa, fx0flux, fwflux, &
       & work, maxi, maxj, imode)
    USE writenc6
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: maxi, maxj, imode, k1(0:maxi+1,0:maxj+1)
    REAL, INTENT(IN) :: tq(2,maxi,maxj), q_pa(maxi,maxj), rq_pa(maxi,maxj), &
         & fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj)

    REAL :: work(maxi, maxj)

    ! Temperature (i.e. final argument = 1)
    CALL prep_netcdf_embm(tq(1,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(1,imode), work)

    ! Specific humidity (i.e. final argument = 2) (note : the 1000.0 below
    ! converts kg/kg to g/kg)
    CALL prep_netcdf_embm(tq(2,:,:), work, 1000.0)
    CALL writevar(nco(imode), idvaro(2,imode), work)

    ! Specific humidity after precipitation (note : the 1000.0 below
    ! converts kg/kg to g/kg) (note : last two arguments are not used for
    ! itype=6)
    CALL prep_netcdf_embm(q_pa, work, 1000.0)
    CALL writevar(nco(imode), idvaro(3,imode), work)

    ! Latent heat flux
    CALL prep_netcdf_embm(fx0flux(4,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(4,imode), work)

    ! Sensible heat flux
    CALL prep_netcdf_embm(fx0flux(2,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(5,imode), work)

    ! Net solar heat flux
    CALL prep_netcdf_embm(fx0flux(1,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(6,imode), work)

    ! Net longwave heat flux
    CALL prep_netcdf_embm(fx0flux(3,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(7,imode), work)

    ! Evaporation
    CALL prep_netcdf_embm(fwflux(2,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(8,imode), work)

    ! Precipitation
    CALL prep_netcdf_embm(fwflux(1,:,:), work, 1.0)
    CALL writevar(nco(imode), idvaro(9,imode), work)

    ! Land mask
    CALL prep_netcdf_embm(fwflux(1,:,:), work, 1.0, k1)
    CALL writevar(nco(imode), idvaro(10,imode), work)

    ! Precipitation-adjusted relative humidity
    CALL prep_netcdf_embm(rq_pa, work, 1.0)
    CALL writevar(nco(imode), idvaro(11,imode), work)
  END SUBROUTINE write_netcdf_embm


  ! Reorganise data for netCDF file (e.g. re-orientation)
  SUBROUTINE prep_netcdf_embm(data_i, data_o, scale, iland)
    IMPLICIT NONE
    REAL, INTENT(IN)  :: data_i(maxi,maxj)
    REAL, INTENT(OUT) :: data_o(maxi,maxj)
    REAL, INTENT(IN) :: scale
    INTEGER, INTENT(IN), OPTIONAL :: iland(0:maxi+1,0:maxj+1)

    IF (PRESENT(iland)) THEN
       WHERE (iland(1:maxi,1:maxj) >= 90)
          data_o = -99999.0
       ELSEWHERE
          data_o = data_i * scale
       END WHERE
    ELSE
       data_o = data_i * scale
    END IF
  END SUBROUTINE prep_netcdf_embm


  ! End netCDF-writing process and closes netCDF file
  SUBROUTINE end_netcdf_embm(imode)
    USE writenc6
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: imode

    CALL closenc(nco(imode))
  END SUBROUTINE end_netcdf_embm

END MODULE embm_netcdf

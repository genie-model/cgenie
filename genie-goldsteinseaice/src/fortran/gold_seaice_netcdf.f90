MODULE gold_seaice_netcdf

  USE gold_seaice_lib
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: ini_netcdf_sic
  PUBLIC :: write_netcdf_sic
  PUBLIC :: end_netcdf_sic
  PUBLIC :: nclon1, nclon2, nclon3, nclat1, nclat2, nclat3

  REAL, DIMENSION(maxi) :: nclon1, nclon2, nclon3
  REAL, DIMENSION(maxj) :: nclat1, nclat2, nclat3

  INTEGER, PARAMETER :: nall=100, nfiles=2
  INTEGER :: nco(nfiles), iddimo(nall, nfiles), idvaro(nall, nfiles)

CONTAINS

  ! Begin netCDF initialisation process
  SUBROUTINE ini_netcdf_sic(istep, imode)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep, imode

    REAL :: day, rtime
    INTEGER :: iyear, imonth

    day = istep * dtsic * tsc / 86400.0
    ! Small constant added to day calculation for round-off reasons
    ! (i.e. 365.2499 vs. 365.25)
    iyear = INT((day + 0.001) / yearlen)
    imonth = INT((day - iyear * yearlen) / (yearlen / 12.0)) + 1
    rtime = iyear + ((day - iyear * yearlen)) / yearlen
    IF (debug_loop) &
         & PRINT *, 'istep', istep, 'day', day, 'iyear', iyear, &
         & 'imonth', imonth, 'rtime', rtime

    CALL ini_netcdf_sic1(outdir_name, lenout, lout, imonth, rtime, &
         & nclon1, nclat1, nclon2, nclat2, nclon3, nclat3, imax, jmax, imode)
  END SUBROUTINE ini_netcdf_sic


  ! Continue netCDF initialisation process
  SUBROUTINE ini_netcdf_sic1(dir_name, ilen, runid, imonth, rtime, &
       & alon1, alat1, alon2, alat2, alon3, alat3, mg, jgg, imode)

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

    INTEGER, PARAMETER :: imax=100, jmax=100, kmax=100, lmax=10000
    REAL :: xcoord(imax), ycoord(jmax), tcoord(lmax)
    INTEGER :: itime, ifname1
    CHARACTER(LEN=200) :: fname1
    CHARACTER(LEN=10) :: cyear
    CHARACTER(LEN=2), DIMENSION(13) :: cmon = &
         & (/ '00','01','02','03','04','05','06', &
         &    '07','08','09','10','11','12' /)

    INTEGER, EXTERNAL :: lnsig

    itime = 1

    CALL setup_nc_sic(mg, jgg, itime, nmaxdims, ndim, nvar, natts, &
         & nattsvar, vdims, vadims, ndims, dimname, varname, &
         & attdimname, attvarname)
    IF (imode == 1) THEN
       fname1 = dir_name(1:ilen)//'gsic_'//runid(1:3)//'_rs_'// &
            & cyear//'_'//cmon(imonth)//'.nc'
    ELSE IF (imode == 2) THEN
       fname1 = dir_name(1:ilen)//'gsic_'//runid(1:3)//'_av_'// &
            & cyear//'_'//cmon(imonth)//'.nc'
    END IF
    ifname1 = lnsig(fname1)

    CALL ininc(fname1(1:ifname1), nmaxdims, ndim, nvar, natts, nattsvar, &
         & vdims, vadims, ndims, dimname, varname, attdimname, attvarname, &
         & nco(imode), iddimo(1,imode), idvaro(1,imode))

    ! Longitude coordinates (tracer, u-point, v-point)
    xcoord(1:mg) = alon1(1:mg)
    CALL writedim(nco(imode), iddimo(1,imode), xcoord)
    xcoord(1:mg) = alon2(1:mg)
    CALL writedim(nco(imode), iddimo(2,imode), xcoord)
    xcoord(1:mg) = alon3(1:mg)
    CALL writedim(nco(imode), iddimo(3,imode), xcoord)

    ! Latitude coordinates (tracer, u-point, v-point)
    ycoord(1:jgg) = alat1(1:jgg)
    CALL writedim(nco(imode), iddimo(4,imode), ycoord)
    ycoord(1:jgg) = alat2(1:jgg)
    CALL writedim(nco(imode), iddimo(5,imode), ycoord)
    ycoord(1:jgg) = alat3(1:jgg)
    CALL writedim(nco(imode), iddimo(6,imode), ycoord)

    ! Time
    tcoord(1) = REAL(rtime)
    CALL writedim(nco(imode), iddimo(7,imode), tcoord)
  END SUBROUTINE ini_netcdf_sic1


  ! Set up netCDF file's array names, units, descriptions, etc.
  SUBROUTINE setup_nc_sic(nlon, nlat, ntime, nmaxdims, &
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
    varname(nvar) = 'sic_height'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sea-ice height'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'm'

    nvar = nvar + 1
    varname(nvar) = 'sic_cover'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sea-ice fractional cover'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'dimensionless'

    nvar = nvar + 1
    varname(nvar) = 'sic_temp'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sea-ice temperature'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'C'

    nvar = nvar + 1
    varname(nvar) = 'sic_albedo'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Sea-ice albedo'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'dimensionless'

    nvar = nvar + 1
    varname(nvar) = 'delta_height'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Change in sea-ice height'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'm/s'

    nvar = nvar + 1
    varname(nvar) = 'delta_cover'
    vdims(nvar) = 3
    vadims(1,nvar) = loc_dim('longitude',dimname,nall)
    vadims(2,nvar) = loc_dim('latitude',dimname,nall)
    vadims(3,nvar) = loc_dim('time',dimname,nall)
    nattsvar(nvar) = 2
    attvarname(1,1,nvar) = 'long_name'
    attvarname(2,1,nvar) = 'Change in sea-ice fractional cover'
    attvarname(1,2,nvar) = 'units'
    attvarname(2,2,nvar) = 'dimensionless'

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
  END SUBROUTINE setup_nc_sic


  ! Writes data to netCDF file
  SUBROUTINE write_netcdf_sic(k1, varice, tice, albice, dtha, &
       & fx0_sic, fw_sic, work, maxi, maxj, imode)
    IMPLICIT NONE

    integer maxi,maxj,imode,k1(0:maxi+1,0:maxj+1)
    real varice(2,maxi,maxj),tice(maxi,maxj),albice(maxi,maxj), &
         & dtha(2,maxi,maxj),fx0_sic(maxi,maxj),fw_sic(maxi,maxj)
    real work((maxi+1)*(maxj+1))

    ! Sea-ice height (i.e. final argument = 1)
    call prep_netcdf_sic(varice(1,:,:), work, k1)
    call writevar(nco(imode), idvaro(1,imode), work)

    ! Sea-ice area (i.e. final argument = 2)
    call prep_netcdf_sic(varice(2,:,:), work, k1)
    call writevar(nco(imode), idvaro(2,imode), work)

    ! Sea-ice temperature
    call prep_netcdf_sic(tice, work, k1)
    call writevar(nco(imode), idvaro(3,imode), work)

    ! Sea-ice albedo
    call prep_netcdf_sic(albice, work, k1)
    call writevar(nco(imode), idvaro(4,imode), work)

    ! Change in sea-ice height
    call prep_netcdf_sic(dtha(1,:,:), work, k1)
    call writevar(nco(imode), idvaro(5,imode), work)

    ! Change in sea-ice fractional cover
    call prep_netcdf_sic(dtha(2,:,:), work, k1)
    call writevar(nco(imode), idvaro(6,imode), work)

    ! Sea-ice heat flux
    call prep_netcdf_sic(fx0_sic, work, k1)
    call writevar(nco(imode), idvaro(7,imode), work)

    ! Sea-ice freshwater flux
    call prep_netcdf_sic(fw_sic, work, k1)
    call writevar(nco(imode), idvaro(8,imode), work)
  END SUBROUTINE write_netcdf_sic


  ! Organises a two-dimensional array that's on the tracer grid
  SUBROUTINE prep_netcdf_sic(temper, temp1, iland)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: iland(0:imax+1,0:jmax+1)
    REAL,    INTENT(IN) :: temper(imax,jmax)
    REAL,    INTENT(OUT) :: temp1(imax,jmax)

    WHERE (iland >= 90)
       temp1 = -99999.0
    ELSEWHERE
       temp1 = temper
    END WHERE
  END SUBROUTINE prep_netcdf_sic


  ! Ends netCDF-writing process and closes netCDF file
  SUBROUTINE end_netcdf_sic(imode)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: imode

    CALL closenc(nco(imode))
  END SUBROUTINE END_NETCDF_SIC

END MODULE gold_seaice_netcdf

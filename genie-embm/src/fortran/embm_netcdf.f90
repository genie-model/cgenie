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

  ! These netCDF routines were originally written for c-GOLDSTEIN by
  ! Paul Valdes, and have been altered to work with the componentised
  ! versions of GOLDSTEIN, the EMBM and GOLDSTEIN sea-ice.  The
  ! main changes are :
  !
  ! - array bounds issues addressed in flip_both2 routine
  ! - GOLDSTEIN's -260E to +100E longitude grid restored
  ! - ancillary longitude, latitude and depth arrays calculated in
  !   module initialisation routines, not netCDF routines
  ! - new preparation routines for two dimensional fields
  ! - ordering of the routines in this file altered to reflect
  !   call order
  !
  ! Note : missing data flag should be -99999, but appears as another
  ! number in netCDF files.  As yet this discrepancy has not been
  ! tracked down and corrected

  ! Begin netCDF initialisation process
  SUBROUTINE ini_netcdf_embm(istep,imode)
    implicit none

    integer istep,imode
    real day,rtime
    integer iyear, imonth

    day=istep*dtatm*tsc/86400.0
    ! small constant added to day calculation for round-off reasons
    ! (i.e. 365.2499 vs. 365.25)
    iyear=int((day + 0.001)/yearlen)
    imonth=int((day-iyear*yearlen)/(yearlen/12.))+1
    rtime=iyear + ((day-iyear*yearlen))/yearlen
    if (debug_loop) &
         & print*,'istep',istep,'day',day,'iyear',iyear,'imonth',imonth, &
         &     'rtime',rtime
    call ini_netcdf_embm1(outdir_name,lenout,lout,imonth,rtime, &
         & nclon1,nclat1,nclon2,nclat2,nclon3,nclat3,imax,jmax,imode)

  END SUBROUTINE ini_netcdf_embm


  ! Continue netCDF initialisation process
  SUBROUTINE ini_netcdf_embm1(dir_name,ilen,runid,imonth,rtime, &
       & alon1,alat1,alon2,alat2,alon3,alat3,mg,jgg,imode)
    implicit none

    integer nvar,ndim
    integer natts(nall), nattsvar(nall)
    integer vdims(nall), ndims(nall)
    integer vadims(nmaxdims,nall)
    character dimname(nall)*200
    character attdimname(2,nmaxdims,nall)*200
    character attvarname(2,nmaxdims,nall)*200
    character varname(nall)*200

    integer mg,jgg
    real :: alon1(mg),alon2(mg),alon3(mg),alat1(jgg),alat2(jgg),alat3(jgg)
    real rtime
    integer imonth,imode

    integer, parameter :: imax=100, jmax=100, kmax=100, lmax=10000
    real xcoord(imax),ycoord(jmax),tcoord(lmax)
    integer i,j,itime,ifname1,lnsig
    character dir_name*100
    integer ilen
    character runid*3
    character fname1*200
    character cyear*10
    character cmon(13)*2
    data cmon/'00','01','02','03','04','05','06', &
         & '07','08','09','10','11','12'/

    itime=1

    call setup_nc_embm(mg,jgg,itime,nmaxdims,nall,ndim,nvar,natts,nattsvar, &
         & vdims,vadims,ndims,dimname,varname,attdimname,attvarname)
    if (imode.eq.1) then
       fname1=dir_name(1:ilen)//'embm_'//runid(1:3)//'_rs_'// &
            & cyear//'_'//cmon(imonth)//'.nc'
    else if (imode.eq.2) then
       fname1=dir_name(1:ilen)//'embm_'//runid(1:3)//'_av_'// &
            & cyear//'_'//cmon(imonth)//'.nc'
    end if

    ifname1=lnsig(fname1)

    call ininc(fname1(1:ifname1),nmaxdims,ndim,nvar,natts,nattsvar, &
         & vdims,vadims,ndims,dimname,varname,attdimname,attvarname, &
         & nco(imode),iddimo(1,imode),idvaro(1,imode))

    ! Longitude coordinates (tracer, u-point, v-point)
    do i=1,mg
       xcoord(i)=alon1(i)
    end do
    call writedim(nco(imode),iddimo(1,imode),xcoord)
    do i=1,mg
       xcoord(i)=alon2(i)
    end do
    call writedim(nco(imode),iddimo(2,imode),xcoord)
    do i=1,mg
       xcoord(i)=alon3(i)
    end do
    call writedim(nco(imode),iddimo(3,imode),xcoord)

    ! Latitude coordinates (tracer, u-point, v-point)
    do j=1,jgg
       ycoord(j)=alat1(j)
    end do
    call writedim(nco(imode),iddimo(4,imode),ycoord)
    do j=1,jgg
       ycoord(j)=alat2(j)
    end do
    call writedim(nco(imode),iddimo(5,imode),ycoord)
    do j=1,jgg
       ycoord(j)=alat3(j)
    end do
    call writedim(nco(imode),iddimo(6,imode),ycoord)

    ! Time
    do i=1,1
       tcoord(i)=real(rtime)
    end do
    call writedim(nco(imode),iddimo(7,imode),tcoord)

  END SUBROUTINE ini_netcdf_embm1


  ! Set up NetCDF file's array names, units, descriptions, etc.
  subroutine setup_nc_embm(nlon,nlat,ntime,nmaxdims,nall,ndim,nvar, &
       & natts,nattsvar,vdims,vadims,ndims,dimname, &
       & varname,attdimname,attvarname)
    implicit none
    integer nlon,nlat,ntime,nmaxdims,nall
    integer ndim,nvar,natts(nall),nattsvar(nall), &
         & vdims(nall),vadims(nmaxdims,nall),ndims(nall)
    character dimname(nall)*200,varname(nall)*200, &
         & attdimname(2,nmaxdims,nall)*200, &
         & attvarname(2,nmaxdims,nall)*200

    integer loc_dim

    ! This sets up a file similar to .pc files
    ndim=0
    nvar=0

    ndim=ndim+1
    dimname(ndim)='longitude'
    ndims(ndim)=nlon
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='longitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_east'

    ndim=ndim+1
    dimname(ndim)='longitude_1'
    ndims(ndim)=nlon
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='longitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_east'

    ndim=ndim+1
    dimname(ndim)='longitude_2'
    ndims(ndim)=nlon
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='longitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_east'

    ndim=ndim+1
    dimname(ndim)='latitude'
    ndims(ndim)=nlat
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='latitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_north'

    ndim=ndim+1
    dimname(ndim)='latitude_1'
    ndims(ndim)=nlat
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='latitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_north'

    ndim=ndim+1
    dimname(ndim)='latitude_2'
    ndims(ndim)=nlat
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='latitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_north'

    ndim=ndim+1
    dimname(ndim)='time'
    ndims(ndim)=ntime
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='time'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='years'

    nvar=nvar+1
    varname(nvar)='air_temp'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Air temperature'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='C'

    nvar=nvar+1
    varname(nvar)='humidity'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Specific humidity'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='g/kg'

    ! AP (03/08/06) : addition of specific humidity after precipitation

    nvar=nvar+1
    varname(nvar)='dry_air_humidity'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Specific humidity after precipitation'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='g/kg'

    nvar=nvar+1
    varname(nvar)='latent'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Latent heat flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W/m2'

    nvar=nvar+1
    varname(nvar)='sensible'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Sensible heat flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W/m2'

    nvar=nvar+1
    varname(nvar)='netsolar'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Net solar heat flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W/m2'

    nvar=nvar+1
    varname(nvar)='netlong'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Net longwave heat flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W/m2'

    nvar=nvar+1
    varname(nvar)='evap'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Evaporation'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='mm/s'

    nvar=nvar+1
    varname(nvar)='pptn'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Precipitation'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='mm/s'

    nvar=nvar+1
    varname(nvar)='landmask'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Land mask'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='dimensionless'

    nvar=nvar+1
    varname(nvar)='dry_air_relative_humidity'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='Relative humidity after precipitation'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='1'

  end subroutine setup_nc_embm


  ! Write data to netCDF file
  subroutine write_netcdf_embm(imax,jmax,k1,tq,q_pa, rq_pa,fx0flux,fwflux, &
       & work,maxi,maxj,imode)
    implicit none

    integer imax,jmax,maxi,maxj,imode,k1(0:maxi+1,0:maxj+1)
    real tq(2,maxi,maxj), q_pa(maxi,maxj), rq_pa(maxi,maxj), &
         & fx0flux(4,maxi,maxj),fwflux(2,maxi,maxj)
    real work((maxi+1)*(maxj+1))

    ! Temperature (i.e. final argument = 1)
    call prep_netcdf_embm(tq,imax,jmax,work,k1,1.0,7,2,1)
    call writevar(nco(imode),idvaro(1,imode),work)

    ! Specific humidity (i.e. final argument = 2) (note : the 1000.0 below
    ! converts kg/kg to g/kg)
    call prep_netcdf_embm(tq,imax,jmax,work,k1, 1000.0,7,2,2)
    call writevar(nco(imode),idvaro(2,imode),work)

    ! Specific humidity after precipitation (note : the 1000.0 below
    ! converts kg/kg to g/kg) (note : last two arguments are not used for
    ! itype=6)
    call prep_netcdf_embm(q_pa,imax,jmax,work,k1,1000.0,6,99,99)
    call writevar(nco(imode),idvaro(3,imode),work)

    ! Latent heat flux
    call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,1.0,7,4,4)
    call writevar(nco(imode),idvaro(4,imode),work)

    ! Sensible heat flux
    call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,1.0,7,4,2)
    call writevar(nco(imode),idvaro(5,imode),work)

    ! Net solar heat flux
    call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,1.0,7,4,1)
    call writevar(nco(imode),idvaro(6,imode),work)

    ! Net longwave heat flux
    call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,1.0,7,4,3)
    call writevar(nco(imode),idvaro(7,imode),work)

    ! Evaporation
    call prep_netcdf_embm(fwflux,imax,jmax,work,k1,1.0,7,2,2)
    call writevar(nco(imode),idvaro(8,imode),work)

    ! Precipitation
    call prep_netcdf_embm(fwflux,imax,jmax,work,k1,1.0,7,2,1)
    call writevar(nco(imode),idvaro(9,imode),work)

    ! Land mask
    call prep_netcdf_embm(fwflux,imax,jmax,work,k1,1.0,8,2,1)
    call writevar(nco(imode),idvaro(10,imode),work)

    ! Precipitation-adjusted relative humidity
    call prep_netcdf_embm(rq_pa,imax,jmax,work,k1,1.0,6,99,99)
    call writevar(nco(imode),idvaro(11,imode),work)

  end subroutine write_netcdf_embm


  ! Reorganise data for netCDF file (e.g. re-orientation)
  subroutine prep_netcdf_embm(data_i,mg,jgg,data_o,iland, &
       & scale,itype,ilev,it)
    implicit none

    integer mg,jgg,itype,it,ilev,iland(*)
    real scale
    real data_i(*)
    real data_o(*)

    if (itype.eq.6) then
       call twodee_embm(data_i,data_o,mg,jgg,mg,jgg,scale)
    else if (itype.eq.7) then
       call twodee_embm2(data_i,data_o,mg,jgg,mg,jgg,scale,ilev,it)
    else if (itype.eq.8) then
       call twodee_embm3(data_o,mg,jgg,mg,jgg,iland)
    end if

  end subroutine prep_netcdf_embm


  ! Organise a two-dimensional array that's on the tracer grid
  subroutine twodee_embm(temper, temp1,imax,jmax,ix,iy,scale)
    implicit none

    integer imax,jmax,ix,iy
    real    temper(imax,jmax),scale
    real temp1(ix,iy)

    integer i,j

    do j=1,jmax
       do i=1,imax
          temp1(i,j)=real(scale*temper(i,j))
       enddo
    enddo

  end subroutine twodee_embm


  ! Organise a two-dimensional array that's on the tracer grid
  subroutine twodee_embm2(temper, temp1, imax,jmax, ix,iy, scale,iter,it)
    implicit none

    integer imax,jmax,ix,iy,iter,it
    real    temper(iter,imax,jmax),scale
    real temp1(ix,iy)

    integer i,j

    do j=1,jmax
       do i=1,imax
          temp1(i,j)=real(scale*temper(it,i,j))
       enddo
    enddo

  end subroutine twodee_embm2


  ! Organise a two-dimensional land mask array that's on the tracer grid
  subroutine twodee_embm3(temp1, imax,jmax, ix,iy, iland)
    implicit none

    integer imax,jmax,ix,iy
    integer iland(0:imax+1,0:jmax+1)
    real temp1(ix,iy)

    integer i,j

    do j=1,jmax
       do i=1,imax
          if (iland(i,j).ge.90) then
             temp1(i,j)=-99999.0
          else
             temp1(i,j)=0.0
          endif
       enddo
    enddo

  end subroutine twodee_embm3


  ! End netCDF-writing process and closes netCDF file
  SUBROUTINE end_netcdf_embm(imode)
    implicit none
    integer imode

    call closenc(nco(imode))

  end SUBROUTINE end_netcdf_embm

END MODULE embm_netcdf

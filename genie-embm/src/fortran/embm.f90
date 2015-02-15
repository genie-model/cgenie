MODULE embm

  USE genie_util, ONLY: check_unit, check_iostat
  USE embm_lib
  USE embm_netcdf
  USE embm_data
  USE embm_diag
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: initialise_embm
  PUBLIC :: step_embm
  PUBLIC :: end_embm
  PUBLIC :: radfor
  PUBLIC :: surflux

CONTAINS

  subroutine step_embm(istep,latent_atm,sensible_atm,netsolar_atm,netlong_atm, &
       & evap_atm,pptn_atm,stressxu_atm,stressyu_atm,stressxv_atm, &
       & stressyv_atm,tstar_atm,qstar_atm,koverall,torog_atm, &
       & surf_orog_atm,flag_ents,lowestlu2_atm,lowestlv3_atm)
    use genie_util, ONLY: check_unit, check_iostat
    implicit none
    real, dimension(imax,jmax) :: latent_atm, sensible_atm, netsolar_atm, &
         & netlong_atm, pptn_atm, evap_atm, stressxu_atm, stressyu_atm, &
         & stressxv_atm, stressyv_atm, tstar_atm, qstar_atm

    integer istep
    integer(kind=8) :: koverall
    integer i, j, itv, iout, ios
    real fx0neta, fwfxneta
    real sum1(4), sum2(4)
    integer isum1(4), isum2(4)
    real    t
    real    fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj)
    real    qdrydum(maxi,maxj)
    real work((maxi+1)*(maxj+1))
    character ext*3
    real wateratm
    real::tatm
    real,intent(out)::torog_atm(imax,jmax)
    real,intent(in)::surf_orog_atm(imax,jmax)
    logical flag_ents
    real deltq
    real qsat
    real,dimension(maxi,maxj),intent(inout)::lowestlu2_atm, &
         & lowestlv3_atm

    do j=1,jmax
       do i=1,imax
          ! reset internal atmospheric wind fields
          uatm(1,i,j) = lowestlu2_atm(i,j)/usc
          uatm(2,i,j) = lowestlv3_atm(i,j)/usc

          ! *** Heat flux ***

          ! net heat flux into atmosphere
          fx0neta = netsolar_atm(i,j) + latent_atm(i,j) + &
               & sensible_atm(i,j) + netlong_atm(i,j)

          ! fluxes copied for output routines
          fx0flux(1,i,j) = netsolar_atm(i,j)
          fx0flux(2,i,j) = sensible_atm(i,j)
          fx0flux(3,i,j) = netlong_atm(i,j)
          fx0flux(4,i,j) = latent_atm(i,j)

          ! *** Freshwater flux ***

          fwfxneta = evap_atm(i,j)
          fwfxneta = fwfxneta * mm2m

          ! fluxes copied for output routines (note : leaving in
          ! precip for now, even though it's not used here)
          fwflux(1,i,j) = pptn_atm(i,j)
          fwflux(2,i,j) = evap_atm(i,j)

          ! non-dimensionalize surface fluxes for use in tstepa:
          tqa(1,i,j) = (fx0neta * rfluxsca)
          tqa(2,i,j) = (fwfxneta * rpmesca)
       enddo
    enddo

    ! EMBM model timestep

    ! EMBM update 1-layer atmosphere
    call tstipa

    ! Diagnostic fields of precipitation-adjusted humidity (i.e.,
    ! humidity after precipitation)
    ! calculate saturation humidity in line with 'surflux.F' for
    ! diagnostics ('surflux.F' will update it again) and calculate
    ! specific and relative humidity after adjustment for
    ! precipitation has been made
    do j=1,jmax
       do i=1,imax
          if(orogswitch.ge.1)then
             tatm=tq(1,i,j)+(lapse_rate*surf_orog_atm(i,j))
          else
             tatm=tq(1,i,j)
          endif
          if((orogswitch.lt.2).and.(flag_ents)) then
             qsat = const1*exp(const4*tatm /(tatm+const5))
          else
             qsat = const1*exp(const4*tq(1,i,j) /(tq(1,i,j)+const5))
          endif
          if (flag_ents) then
             deltq = lambdapptn*(tq(2,i,j)-(rmax*qsat))
             q_pa(i,j) = min(tq(2,i,j), tq(2,i,j)-deltq)
          else
             q_pa(i,j) = min(tq(2,i,j), rmax*qsat)
          endif
          rq_pa(i,j) = q_pa(i,j)/qsat
       enddo
    enddo

    if(mod(istep,(npstp*ndta)).lt.1) then
       if (debug_loop) call diaga
       if (debug_loop) print*
    endif

    ! Atmosphere diagnostics and output
    if (lnetout) call outm_netcdf_embm(istep)

    ! write EMBM restart file
    if(mod(istep,(iwstp*ndta)).eq.0)then
       if (lascout) then
          ext=conv_num(mod(iw,10))
          if (debug_loop) print*,'Writing EMBM restart file at time', &
               & istep/real(ndta),'(koverall',koverall,')'
          call check_unit(2,__LINE__,__FILE__)
          open(2,file=outdir_name(1:lenout)//lout//'.'//ext, iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          rewind 2
          call outm_embm(2)
          close(2,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif

       iw = iw + 1
       if (debug_loop) print*
    endif

110 format(4e15.6,2i5,1e15.6,2i5)

    if(mod(istep,(itstp*ndta)).eq.0)then
       t = real(istep)/real(nyear*ndta)
       if (debug_loop) print*,'Writing to EMBM time-series files'

       call check_unit(41,__LINE__,__FILE__)
       open(41,file=outdir_name(1:lenout)//lout//'.'//'airt', &
            & status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       call check_unit(42,__LINE__,__FILE__)
       open(42,file=outdir_name(1:lenout)//lout//'.'//'q', &
            & status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       call diag3(sum1,sum2,isum1,isum2)

       if (debug_loop) &
            & write(41,110,iostat=ios)t,sum1(1),sum1(2),sum1(3),isum1(1), &
            & isum1(2),sum1(4),isum1(3),isum1(4)
       call check_iostat(ios,__LINE__,__FILE__)
       if (debug_loop) &
            & write(42,110,iostat=ios)t,sum2(1),sum2(2),sum2(3),isum2(1), &
            & isum2(2),sum2(4),isum2(3),isum2(4)
       call check_iostat(ios,__LINE__,__FILE__)

       close(41,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       close(42,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       if (debug_loop) print*
    endif

    if (dosc) then
       ! average the last nyear steps in every ianav steps (if ianav>nyear)
       itv = mod(istep+(nyear*ndta)-1,(ianav*ndta))
       if(itv.lt.(nyear*ndta) .and. (mod(istep,ndta).lt.1))then
          ext=conv_num(mod(iav,10))
          if(istep.ge.(nyear*ndta).and.itv.eq.(nyear*ndta)-1)then
             iout = 1
          else
             iout = 0
          endif
          if (debug_loop) &
               & call diagosc_embm(istep, iout, ext, fx0flux, fwflux, wateratm)
       endif
    endif

    if(mod(istep,(iwstp*ndta)).eq.0) then
       if (debug_loop) print*,'Writing EMBM netCDF file at time',istep
       call ini_netcdf_embm(istep,1)
       qdrydum(:,:)=0.
       call write_netcdf_embm(k1, tq, qdrydum, qdrydum,&
            & fx0flux, fwflux, work, maxi, maxj, 1)
       call end_netcdf_embm(1)
       if (debug_loop) print*
    endif

    ! Output arguments
    do j=1,jmax
       do i=1,imax
          ! Surface air temperature [-> surface fluxes]
          tstar_atm(i,j) = real(tq(1,i,j))
          ! Orography-adjusted surface air temperature
          if(orogswitch.ge.1)then
             tatm=tq(1,i,j)+(lapse_rate*surf_orog_atm(i,j))
          else
             tatm=tq(1,i,j)
          endif
          torog_atm(i,j) = real(tatm)
          ! Surface specific humidity [-> surface fluxes]
          qstar_atm(i,j) = real(tq(2,i,j))
          ! Wind stress x components [-> ocean, surface fluxes]
          stressxu_atm(i,j) = real(us_dztau(1,i,j))
          stressxv_atm(i,j) = real(us_dztav(1,i,j))
          ! Wind stress y components [-> ocean, surface fluxes]
          stressyu_atm(i,j) = real(us_dztau(2,i,j))
          stressyv_atm(i,j) = real(us_dztav(2,i,j))
       enddo
    enddo

  end subroutine step_embm


  subroutine initialise_embm(alon1,alat1,alon2,alat2,alon3,alat3, &
       & aboxedge1_lon,aboxedge1_lat,aboxedge2_lon,aboxedge2_lat, &
       & aboxedge3_lon,aboxedge3_lat,ilandmask1,ilandmask2,ilandmask3, &
       & ias,iaf,ips,ipf,jsf,tstar_ocn,totsteps,co2_out,ch4_out,n2o_out, &
       & stressxu_atm,stressyu_atm,stressxv_atm,stressyv_atm, &
       & tstar_atm,qstar_atm,atmos_dt_tim,solconst, &
       & eb_rmax,eb_dphi,eb_rdtdim,eb_ca,gn_daysperyear,torog_atm, &
       & surf_orog_atm,landice_slicemask_lic,syr,flag_ents, &
       & lowestlu2_atm,lowestlv3_atm,flag_wind)
    use genie_util, ONLY: check_unit, check_iostat
    implicit none

    REAL,INTENT(out) :: atmos_dt_tim ! EMBM timestep (s)
    real :: alon1(maxi),alat1(maxj),alon2(maxi),alat2(maxj), &
         & alon3(maxi),alat3(maxj), &
         & aboxedge1_lon(maxi+1),aboxedge1_lat(maxj+1), &
         & aboxedge2_lon(maxi+1),aboxedge2_lat(maxj+1), &
         & aboxedge3_lon(maxi+1),aboxedge3_lat(maxj+1)
    real :: tstar_ocn(maxi,maxj), &
         & co2_out(maxi,maxj),ch4_out(maxi,maxj),n2o_out(maxi,maxj)
    real, dimension(maxi,maxj) :: stressxu_atm, stressyu_atm, &
         & stressxv_atm, stressyv_atm, tstar_atm, qstar_atm

    integer, dimension(maxi,maxj) :: ilandmask1, ilandmask2, ilandmask3
    integer(kind=8) :: totsteps

    integer ias(maxj),iaf(maxj),ips(maxj),ipf(maxj),jsf
    integer bmask(maxi,maxj)

    real z1, tv, tv1, tv2, tv3, tv4, tv5, tatm, relh0_ocean, &
         & relh0_land, diffamp(2), diffwid, difflin, &
         & diffend, zro(maxk), zw(0:maxk)
    real radfor_scl_co2,radfor_pc_co2_rise
    real radfor_scl_ch4,radfor_pc_ch4_rise
    real radfor_scl_n2o,radfor_pc_n2o_rise
    real u_tau_ice,ch_ice,cpo_ice

    real th0, th1, s0, s1, phix, scl_fwf

    real theta,thv,dth,dscon
    real deg_to_rad
    real area_atl1a, area_atl1b, area_atl1c
    real area_pac1a, area_pac1b, area_pac1c

    integer i, j, k, l, m, natl1a, npac1a, natl1b, npac1b
    integer natl1c, npac1c
    integer n

    integer lnsig1

    character(len=6) world
    integer          lenworld

    character xu_wstress*127, yu_wstress*127
    character xv_wstress*127, yv_wstress*127
    integer   len_xu, len_yu, len_xv, len_yv

    character u_wspeed*20, v_wspeed*20
    integer   len_uws, len_vws

    character ans, lin*13

    character netin*1,netout*1,ascout*1

    ! Value of solar constant to be passed to radfor
    real solconst

    real eb_rmax, eb_dphi, eb_rdtdim, eb_ca(maxi,maxj), gn_daysperyear

    real,intent(out)::torog_atm(maxi,maxj)
    real,intent(out)::surf_orog_atm(maxi,maxj)

    ! land ice sheet mask
    real,dimension(maxi,maxj),intent(out)::landice_slicemask_lic

    real::syr

    ! ENTS functionality
    logical flag_ents
    real deltq

    real qsat

    !     file access checks
    integer :: ios
    logical :: ioex

    !AJR Antarctic atmospheric diffusivity fudge
    !AJR diffusivity scaling factor
    real diffa_scl
    !AJR grid point distance over which scalar is applied (j direction)
    integer diffa_len
    !AJR planetary albedo modifications
    real albedop_offs
    real albedop_amp
    real albedop_skew
    integer albedop_skewp
    real albedop_mod2
    real albedop_mod4
    real albedop_mod6
    !AJR planetary albedo modifications: local
    real albedop_scl

    ! seasonal fields
    real uatml1(2,maxi,maxj,nmth+1)
    real usurfl1(maxi,maxj,nmth+1)
    real tncep1(maxi,maxj,nmth+1)
    real pncep1(maxi,maxj,nmth+1)
    real rhncep1(maxi,maxj,nmth+1)
    real atm_alb1(maxi,maxj,nmth+1)

    ! precipitation timescale and land radiation
    real timepptn
    real hld,cld

    ! southern boundaries of regions for FW flux adjustment
    integer j1as,j1bs,j1cs

    ! zonal and meridional wind speed components
    real,dimension(maxi,maxj),intent(inout)::lowestlu2_atm,lowestlv3_atm

    ! this flag indicates if external wind forcing from 'genie-wind'
    ! module is active
    logical flag_wind

    real orbitall_vect(en_ntimes_max,5)

    ! declare a namelist to store EMBM's initialisation vars
    NAMELIST /ini_embm_nml/ indir_name,outdir_name
    NAMELIST /ini_embm_nml/ rstdir_name
    NAMELIST /ini_embm_nml/ igrid,world
    NAMELIST /ini_embm_nml/ xu_wstress,yu_wstress,xv_wstress
    NAMELIST /ini_embm_nml/ yv_wstress,u_wspeed,v_wspeed
    NAMELIST /ini_embm_nml/ npstp,iwstp,itstp,ianav,ans
    NAMELIST /ini_embm_nml/ yearlen,nyear,ndta,scf
    NAMELIST /ini_embm_nml/ diffamp,diffwid,difflin
    NAMELIST /ini_embm_nml/ betaz,betam
    NAMELIST /ini_embm_nml/ radfor_scl_co2,radfor_pc_co2_rise
    NAMELIST /ini_embm_nml/ radfor_scl_ch4,radfor_pc_ch4_rise
    NAMELIST /ini_embm_nml/ radfor_scl_n2o,radfor_pc_n2o_rise
    NAMELIST /ini_embm_nml/ tatm,relh0_ocean,relh0_land
    NAMELIST /ini_embm_nml/ extra1a,extra1b,extra1c,scl_fwf
    NAMELIST /ini_embm_nml/ z1_embm,tdatafile,qdatafile
    NAMELIST /ini_embm_nml/ tdata_varname,qdata_varname
    NAMELIST /ini_embm_nml/ tdata_missing,qdata_missing
    NAMELIST /ini_embm_nml/ tdata_scaling,qdata_scaling
    NAMELIST /ini_embm_nml/ tdata_offset,qdata_offset
    NAMELIST /ini_embm_nml/ qdata_rhum
    NAMELIST /ini_embm_nml/ tqinterp
    NAMELIST /ini_embm_nml/ lout,netin,netout,ascout
    NAMELIST /ini_embm_nml/ rmax
    NAMELIST /ini_embm_nml/ filenetin,dirnetout
    NAMELIST /ini_embm_nml/ lin,atchem_radfor
    NAMELIST /ini_embm_nml/ orbit_radfor
    NAMELIST /ini_embm_nml/ t_orbit,norbit,orbitsteps,filenameorbit
    NAMELIST /ini_embm_nml/ t_co2,nco2,co2steps,filenameco2
    NAMELIST /ini_embm_nml/ diffa_scl,diffa_len
    NAMELIST /ini_embm_nml/ dosc,delf2x
    NAMELIST /ini_embm_nml/ olr_adj0,olr_adj,t_eqm
    NAMELIST /ini_embm_nml/ aerofac,volfac,solfac
    NAMELIST /ini_embm_nml/ useforc,forcname
    NAMELIST /ini_embm_nml/ albedop_offs,albedop_amp
    NAMELIST /ini_embm_nml/ albedop_skew,albedop_skewp
    NAMELIST /ini_embm_nml/ albedop_mod2,albedop_mod4,albedop_mod6
    NAMELIST /ini_embm_nml/ lapse_rate
    NAMELIST /ini_embm_nml/ orogswitch
    NAMELIST /ini_embm_nml/ t_orog
    NAMELIST /ini_embm_nml/ filenameorog
    NAMELIST /ini_embm_nml/ norog
    NAMELIST /ini_embm_nml/ orogsteps
    NAMELIST /ini_embm_nml/ t_lice
    NAMELIST /ini_embm_nml/ filenamelice
    NAMELIST /ini_embm_nml/ nlice
    NAMELIST /ini_embm_nml/ licesteps
    NAMELIST /ini_embm_nml/ lice_k9
    NAMELIST /ini_embm_nml/ t_d18o
    NAMELIST /ini_embm_nml/ nd18o
    NAMELIST /ini_embm_nml/ d18osteps
    NAMELIST /ini_embm_nml/ d18o_k,scale_mwfx
    NAMELIST /ini_embm_nml/ filenamed18o
    NAMELIST /ini_embm_nml/ filenamed18oicethresh
    NAMELIST /ini_embm_nml/ filenamed18oorogmin
    NAMELIST /ini_embm_nml/ filenamed18ooroggrad
    NAMELIST /ini_embm_nml/ ents_seasonswitch
    NAMELIST /ini_embm_nml/ ents_offlineswitch
    NAMELIST /ini_embm_nml/ timepptn
    NAMELIST /ini_embm_nml/ par_runoff_scheme
    NAMELIST /ini_embm_nml/ par_runoff_b
    NAMELIST /ini_embm_nml/ par_runoff_tau
    NAMELIST /ini_embm_nml/ debug_init,debug_end,debug_loop
    NAMELIST /ini_embm_nml/ par_wind_polar_avg
    NAMELIST /ini_embm_nml/ unify_winds
    NAMELIST /ini_embm_nml/ par_sich_max
    NAMELIST /ini_embm_nml/ par_albsic_min,par_albsic_max

    ! Initialize variables
    area_atl1a = 0.0
    area_atl1b = 0.0
    area_atl1c = 0.0
    area_pac1a = 0.0
    area_pac1b = 0.0
    area_pac1c = 0.0
    j1as = 0
    j1bs = 0
    j1cs = 0
    len_xu = 0
    len_yu = 0
    len_xv = 0
    len_yv = 0
    len_uws = 0
    len_vws = 0

    ! Setting up EMBM
    print*,'======================================================='
    print*,' >>> Initialising EMBM atmosphere module ...'

    if (debug_init) print*

    call check_unit(56,__LINE__,__FILE__)
    open(unit=56,file='data_EMBM',status='old',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open EMBM namelist file'
       print*,"ERROR on line ", __LINE__, " in file ", __FILE__
       stop
    end if

    read(UNIT=56,NML=ini_embm_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read EMBM namelist'
       print*,"ERROR on line ", __LINE__, " in file ", __FILE__
       stop
    else
       close(56,iostat=ios)
       if (ios /= 0) then
          print*,'ERROR: could not close EMBM namelist file'
          print*,"ERROR on line ", __LINE__, " in file ", __FILE__
          stop
       end if
    end if

    ! Input directory name
    lenin = lnsig1(indir_name)
    if (indir_name(lenin:lenin).ne.'/') then
       lenin = lenin + 1
       indir_name(lenin:lenin) = '/'
    end if
    if (debug_init) print*,'Input dir. name ',indir_name(1:lenin)

    ! Output directory name
    lenout = lnsig1(outdir_name)
    if (outdir_name(lenout:lenout).ne.'/') then
       lenout = lenout + 1
       outdir_name(lenout:lenout+1) = '/'
    end if
    if (debug_init) print*,'Output dir. name ',outdir_name(1:lenout)

    ! Restart (input) directory name
    lenrst = lnsig1(rstdir_name)
    if (rstdir_name(lenrst:lenrst).ne.'/') then
       lenrst = lenrst + 1
       rstdir_name(lenrst:lenrst+1) = '/'
    end if
    if (debug_init) print*,'Restart dir. name ',rstdir_name(1:lenrst)

    ! Read in topography filename
    lenworld = lnsig1(world)
    if (debug_init) print*,'Topography name ',world(1:lenworld)

    ! Read in wind stress filenames
    if (.not.flag_wind) then
       len_xu = lnsig1(xu_wstress)
       len_yu = lnsig1(yu_wstress)
       len_xv = lnsig1(xv_wstress)
       len_yv = lnsig1(yv_wstress)
       if (debug_init) then
          print*, 'x windstress at u point ',xu_wstress(1:len_xu)
          print*, 'y windstress at u point ',yu_wstress(1:len_yu)
          print*, 'x windstress at v point ',xv_wstress(1:len_xv)
          print*, 'y windstress at v point ',yv_wstress(1:len_yv)
       end if
    endif

    ! Read in wind speed filenames
    if (.not.flag_wind) then
       len_uws = lnsig1(u_wspeed)
       len_vws = lnsig1(v_wspeed)
       if (debug_init) then
          print*,'u wind speed ',u_wspeed(1:len_uws)
          print*,'v wind speed ',v_wspeed(1:len_vws)
       end if
    endif

    ! Time-steps information
    nsteps = totsteps
    if (debug_init) then
       print*,'npstp iwstp itstp ianav'
       print*,npstp,iwstp,itstp,ianav
       print*,'new or continuing run ?'
       print*,ans
       print*,'number of days per EMBM year'
       print*,yearlen
       print*,'seasonality enabled =',dosc
    end if

    ! parameters for setting up grid
    ! th is latitude, coords are sin(th), longitude phi, and z
    th0 = -pi/2
    th1 = pi/2
    s0 = sin(th0)
    s1 = sin(th1)
    phix = 2*pi
    deg_to_rad = pi/180.

    ! grid dimensions must be no greater than array dimensions in var.cmn
    imax = maxi
    jmax = maxj
    kmax = maxk
    lmax = 2

    dphi = phix/imax
    if (igrid.lt.2) then
       phi0 = -260.0*deg_to_rad
    endif
    rdphi = 1.0/dphi

    ! set up horizontal grid: sin and cos factors at rho and v points
    ! (c grid) fix for global domain although only cv and cv2 are
    ! referred to at or beyond limits 24/6/2 if no flow out of N + S
    ! boundaries.
    sv(0) = s0
    cv(0) = cos(th0)
    if(igrid.eq.1)then
       dth = (th1 - th0)/jmax
       do j=1,jmax
          thv = th0 + j*dth
          theta = thv - 0.5*dth
          sv(j) = sin(thv)
          s(j) = sin(theta)
          cv(j) = cos(thv)
       enddo
    elseif(igrid.eq.0)then
       dscon = (s1-s0)/jmax
       do j=1,jmax
          sv(j) = s0 + j*dscon
          cv(j) = sqrt(1 - sv(j)*sv(j))
          s(j) = sv(j) - 0.5*dscon
       enddo
    endif
    if (debug_init) then
       print*,'EMBM latitudes: velocity; tracers'
       print*,'j, 180/pi*asin(sv(j)), 180/pi*asin(s(j))'
    end if
    do j=1,jmax
       ds(j) = sv(j) - sv(j-1)
       rds(j) = 1.0/ds(j)
       c(j) = sqrt(1 - s(j)*s(j))
       rc(j) = 1.0/c(j)
       rc2(j) = rc(j)*rc(j)*rdphi
       if(j.lt.jmax)then
          dsv(j) = s(j+1) - s(j)
          rdsv(j) = 1.0/dsv(j)
          rcv(j) = 1.0/cv(j)
          cv2(j) = cv(j)*cv(j)*rdsv(j)
          if(j.gt.1)rds2(j) = 2.0/(dsv(j)+dsv(j-1))
       endif
       if (debug_init) print*,j,180/pi*asin(sv(j)),180/pi*asin(s(j))
    enddo

    ! area of grid cell (assumes sine(lat) grid)
    do j=1,jmax
       asurf(j) = rsc*rsc*ds(j)*dphi
       if (debug_init) &
            &print*, 'j = ',j,'EMBM grid cell area is',asurf(j),'m2'
    enddo

    ! seasonality
    if (debug_init) print*,'timesteps per year and A/O dt ratio'
    if (debug_init) print*,nyear,ndta
    if(nyear.gt.maxnyr)stop 'embm : nyear > maxnyr'
    tv = 86400.0*yearlen/(nyear*tsc)
    ryear = 1.0/(yearlen*86400)

    dtatm = tv/ndta
    if (debug_init) print*, 'embm timestep (s) =',dtatm*tsc
    atmos_dt_tim = REAL(dtatm*tsc)

    do k=1,kmax
       dt(k) = tv
    enddo
    if (debug_init) print*,'dimensional ocean timestep',tv*tsc/86400
    if (debug_init) print*,'dimensionless O/A timesteps',tv,dtatm

    rdtdim = 1.0/(tsc*dt(kmax))
    if (debug_init) print*,'rdtdim = ',rdtdim

    ! set up grid
    ! For variable (exponential) dz use ez0 > 0, else use ez0 < 0
    ez0 = 0.1
    z1 = ez0*((1.0 + 1/ez0)**(1.0/kmax) - 1.0)
    if (debug_init)print*,'z1',z1
    tv4 = ez0*((z1/ez0+1)**0.5-1)
    tv2 = 0
    tv1 = 0
    zro(kmax) = -tv4
    zw(kmax) = tv2
    do k=1,kmax
       if(ez0.gt.0)then
          tv3 = ez0*((z1/ez0+1)**k-1)
          dz(kmax-k+1) = tv3 - tv2
          tv2 = tv3
          tv5 = ez0*((z1/ez0+1)**(k+0.5)-1)
          if(k.lt.kmax)dza(kmax-k) = tv5 - tv4
          tv4 = tv5
          tv1 = tv1 + dz(kmax-k+1)
          ! tv3 is the depth of the kth w level from the top
          ! tv5 is the depth of the k+1th density level from the top
       else
          dz(k) = real(1d0/kmax)
          dza(k) = real(1d0/kmax)
       endif
    enddo

    do k=kmax,1,-1
       if(k.gt.1)zro(k-1) = zro(k) - dza(k-1)
       zw(k-1) = zw(k) - dz(k)
    enddo
    if (debug_init) &
         & write(6,'(i4,3e12.4)')(k,dsc*zw(k),dsc*zro(k),dsc*dz(k) &
         & ,k=kmax,1,-1)

    if (debug_init) print*,'dzz'
    dzz = dz(kmax)*dza(kmax-1)/2
    if (debug_init) print*,dzz

    ! efficiency array
    do k=1,kmax-1
       rdz(k) = 1.0/dz(k)
       rdza(k) = 1.0/dza(k)
    enddo
    rdz(kmax) = 1.0/dz(kmax)

    ! dza(kmax) never referenced, set to 0 for Andy's biogeo-code
    dza(kmax) = 0.

    ! set up sin and cos factors at rho and v points (c grid) fix for
    ! global domain although only cv and cv2 are referred to at or
    ! beyond limits 24/6/2 if no flow out of N + S boundaries.

    if (debug_init) print*,'scf'
    if (debug_init) print*,scf

    ! define forcing

    ! read wind data
    if (.not.flag_wind) then
       ! taux,tauy at u-points
       call check_unit(96,__LINE__,__FILE__)
       open(96,file=indir_name(1:lenin)//xu_wstress(1:len_xu),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       call check_unit(97,__LINE__,__FILE__)
       open(97,file=indir_name(1:lenin)//yu_wstress(1:len_yu),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       ! taux,tauy at v-points
       call check_unit(98,__LINE__,__FILE__)
       open(98,file=indir_name(1:lenin)//xv_wstress(1:len_xv),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       call check_unit(99,__LINE__,__FILE__)
       open(99,file=indir_name(1:lenin)//yv_wstress(1:len_yv),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       do j=1,jmax
          do i=1,imax
             ! Variables dztau and dztav are renamed here to separate
             ! the unscaled (hence 'us') versions read in from files,
             ! from those scaled versions used in surflux and
             ! GOLDSTEIN.
             read(96,*,iostat=ios)us_dztau(1,i,j)
             call check_iostat(ios,__LINE__,__FILE__)
             read(97,*,iostat=ios)us_dztau(2,i,j)
             call check_iostat(ios,__LINE__,__FILE__)
             read(98,*,iostat=ios)us_dztav(1,i,j)
             call check_iostat(ios,__LINE__,__FILE__)
             read(99,*,iostat=ios)us_dztav(2,i,j)
             call check_iostat(ios,__LINE__,__FILE__)
          enddo
       enddo

       close(96,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       close(97,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       close(98,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       close(99,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    else
       do j=1,jmax
          do i=1,imax
             us_dztau(1,i,j) = 0.0
             us_dztau(2,i,j) = 0.0
             us_dztav(1,i,j) = 0.0
             us_dztav(2,i,j) = 0.0
          enddo
       enddo
    endif

    ntot = 0
    intot = 0

    if (debug_init) print*,'Land runoff being read in'
    call check_unit(13,__LINE__,__FILE__)
    open(13,file=indir_name(1:lenin)//world//'.k1',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    ! note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
    ! k1(1,j) - k1(imax+1,j) = 0, as enforced below;
    do j=jmax+1,0,-1
       read(13,*,iostat=ios)(k1(i,j),i=0,imax+1)
       call check_iostat(ios,__LINE__,__FILE__)

       ! rotate grid to check b.c.s
       k1(0,j) = k1(imax,j)
       k1(imax+1,j) = k1(1,j)
       if (debug_init .and.(j.ne.0.and.j.ne.jmax+1)) &
            & write(6,'(i4,32i3)')j,(k1(i,j),i=1,32)
    enddo

    ! read ips etc if possible
    close(13,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (debug_init) print*,'Land runoff successfully read in'

    ! read parameters
    if (debug_init) print*,'diffamp(1),diffamp(2), diffwid, difflin'
    if (debug_init) print*,diffamp(1),diffamp(2), diffwid, difflin
    ! parameter beta relates vertically-averaged advective transport
    ! to surface advective transport
    if (debug_init) print*,'betaz(1),betam(1),betaz(2),betam(2)'
    if (debug_init) print*,betaz(1),betam(1),betaz(2),betam(2)

    ! climatological albedo (similar to Weaver et al. 2001)
    if (debug_init) print*,'climatological albedo, by latitude'
    do j=1,jmax
       albedop_scl = ((albedop_skew - s(j))/2.0)**albedop_skewp
       tv = asin(s(j))
       tv2 = albedop_offs + albedop_amp*0.5*(1.0 - &
            & cos(2.0*tv) + &
            & albedop_scl*albedop_mod2*cos(2.0*tv) + &
            & albedop_scl*albedop_mod4*cos(4.0*tv) + &
            & albedop_scl*albedop_mod6*cos(6.0*tv) &
            & )
       do i=1,imax
          albcl(i,j) = tv2
       enddo
       if (debug_init) print*,j,albcl(1,j)
    enddo

    ! atmospheric SSW absorption coefficient, value over land purely
    ! diagnostic
    do j=1,jmax
       do i=1,imax
          if(k1(i,j).le.kmax)then
             ca(i,j)=0.3
          else
             ca(i,j)=1.0
          endif
       enddo
    enddo

    ! read some scalings
    if (debug_init) then
       print*,'radfor_scl_co2 =',radfor_scl_co2
       print*,'radfor_pc_co2_rise =',radfor_pc_co2_rise
       print*,'radfor_scl_ch4 =',radfor_scl_ch4
       print*,'radfor_pc_ch4_rise =',radfor_pc_ch4_rise
       print*,'radfor_scl_n2o =',radfor_scl_n2o
       print*,'radfor_pc_n2o_rise =',radfor_pc_n2o_rise
    end if

    ! factor corresponding to radiative forcing of 4 W/m**2 per
    ! doubling of atmospheric CO2 NOTE: value now set via a namelist
    if (debug_init) print*,'delf2x =',delf2x
    if (debug_init) &
         & print*, '=> climate sensitivity =',delf2x*log(2.0),' Wm-2'

    ! initialize greenhouse gas concentrations
    do j = 1,jmax
       do i = 1,imax
          co2(i,j) = radfor_scl_co2*co20
          ch4(i,j) = radfor_scl_ch4*ch40
          n2o(i,j) = radfor_scl_n2o*n2o0
          co2_out(i,j) = co2(i,j)
          ch4_out(i,j) = ch4(i,j)
          n2o_out(i,j) = n2o(i,j)
       enddo
    enddo
    ! rate of increase of greenhouse gas concentrations
    rate_co2 = radfor_pc_co2_rise*0.01*tsc*dtatm*ndta*ryear
    rate_ch4 = radfor_pc_ch4_rise*0.01*tsc*dtatm*ndta*ryear
    rate_n2o = radfor_pc_n2o_rise*0.01*tsc*dtatm*ndta*ryear

    ! alternative method for defining time varying co2 from an input
    ! time series read in time varying co2
    if(t_co2.gt.0) then
       if (debug_init) print*, 'co2 defined from ',trim(filenameco2)
       if (debug_init) print*,'t_co2,nco2,co2steps',t_co2,nco2,co2steps
       open(729,file=trim(filenameco2))
       do i=1,nco2
          read(729,*) co2_vect(i)
       enddo
       close(729)
       co2(:,:)=co2_vect(1)
       co2_out(:,:)=co2_vect(1)
    endif

    ! depth scale for atmospheric thermal b.l. used by Weaver et al. (2001)
    hatmbl(1) = 8400.

    ! scaling for heat forcing of atmosphere
    rfluxsca = rsc/(hatmbl(1)*usc*rhoair*cpa)

    ! atmospheric winds
    if (.not.flag_wind) then
       call check_unit(35,__LINE__,__FILE__)
       open(35,file=indir_name(1:lenin)//u_wspeed(1:len_uws), iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       read(35,*,iostat=ios)((uatm(1,i,j),i=1,imax),j=1,jmax)
       call check_iostat(ios,__LINE__,__FILE__)
       close(35,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       open(35,file=indir_name(1:lenin)//v_wspeed(1:len_vws),iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       read(35,*,iostat=ios)((uatm(2,i,j),i=1,imax),j=1,jmax)
       call check_iostat(ios,__LINE__,__FILE__)
       close(35,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       ! conditional zonal average
       ! only when par_wind_polar_avg is 0 (default)
       ! 1 makes the code consistent with genie-winds
       ! 2 removes all conditional zonal averages
       if (par_wind_polar_avg.ne.1.and.par_wind_polar_avg.ne.2)  then
          if (debug_init) print*,"Averaging advective winds near poles"
          do j=1,jmax
             if(j.le.2.or.j.ge.jmax-1)then
                do l=1,2
                   tv = 0.
                   do i=1,imax
                      tv = tv + uatm(l,i,j)
                   enddo
                   tv = tv / imax
                   do i=1,imax
                      uatm(l,i,j) = tv
                   enddo
                enddo
             endif
          enddo
       endif

       ! remove zonal average of v else fail mass conservation (may not be
       ! disastrous).
       do i=1,imax
          do j=1,jmax
             uatm(1,i,j) = uatm(1,i,j)/usc
             uatm(2,i,j) = uatm(2,i,j)/usc
          enddo
          if (par_wind_polar_avg.ne.1.and.par_wind_polar_avg.ne.2) then
             uatm(2,i,jmax) = 0.
          endif
       enddo
    else
       do j=1,jmax
          do i=1,imax
             do l=1,2
                uatm(l,i,j) = 0.0
             enddo
          enddo
       enddo
    endif

    ! parameters for extra heat diffusion where pptn high
    ppmin = 2.0 / (yearlen * 86400.0)
    ppmax = 4.0 / (yearlen * 86400.0)

    ! nre simpler diffusivity
    diffend = exp(-(0.5*pi/diffwid)**2)
    if (debug_init) print*, &
         & 'atm stability numbers, need to be small for explicit dt'
    do j=1,jmax
       tv = asin(s(j))
       tv2 = asin(sv(j))
       diffa(2,1,j) = diffamp(2)
       diffa(2,2,j) = diffamp(2)
       ! Variable-width and asymmetry (or slope) thermal diffusivity
       diffa(1,1,j) = diffamp(1)*(difflin*2.0*(tv+0.5*pi)/pi &
            & + (1.0-difflin)*(exp(-(tv/diffwid)**2) - diffend) &
            & /(1.0 - diffend))
       diffa(1,2,j) = diffamp(1)*(difflin*2.0*(tv2+0.5*pi)/pi &
            & + (1.0-difflin)*(exp(-(tv2/diffwid)**2) - diffend) &
            & /(1.0 - diffend))

       ! Adjust atmospheric diffusivity over S. Ocean and Antarctica
       ! Scale meridional heat diffusion by a factor of diffa_scl as
       ! far as diffa_len in the j direction
       if (diffa_len.lt.0) then
          if(sin(pi*real(diffa_len)/180.).gt.sv(j)) then
             diffa(1,2,j) = diffa_scl*diffa(1,2,j)
          endif
       else
          if(j.le.diffa_len) then
             diffa(1,2,j) = diffa_scl*diffa(1,2,j)
          endif
       endif

       ! non-dimensionalise diffusivities
       diffa(1,1,j) = diffa(1,1,j)/(rsc*usc)
       diffa(1,2,j) = diffa(1,2,j)/(rsc*usc)
       diffa(2,1,j) = diffa(2,1,j)/(rsc*usc)
       diffa(2,2,j) = diffa(2,2,j)/(rsc*usc)

       ! Test for const lat, very unstable diffusivities so control T
       ! diffusion by input params and make sure q diff is no bigger
       ! only if constant lat grid (i.e. allowing compatibility with
       ! 36x36 version)
       if(igrid.eq.1.or.igrid.eq.2)then
          diffa(2,1,j) = min(diffa(2,1,j),diffa(1,1,j))
       endif
       if (debug_init .and.(j.gt.1))write(6,'(i4,4e15.5)')j, &
            & diffa(1,1,j)*dtatm*rc(j)*rc(j)*rdphi*rdphi &
            & ,diffa(2,1,j)*dtatm*rc(j)*rc(j)*rdphi*rdphi &
            & ,diffa(1,2,j-1)*dtatm*cv(j-1)*cv(j-1)*rdsv(j-1)*rds(j) &
            & ,diffa(2,2,j-1)*dtatm*cv(j-1)*cv(j-1)*rdsv(j-1)*rds(j)
    enddo

    ! scale height for specific humidity (Peixoto and Oort 1992)
    hatmbl(2) = 1800.

    ! scaling for P-E forcing of atmosphere
    rpmesca = rsc*rho0/(hatmbl(2)*usc*rhoair)

    ! reconstruct surface wind field for bulk turbulent transfer and
    ! zonally average near poles as for uatm for stability

    do j=1,jmax
       tv3 = 0.
       do i=1,imax
          if(i.eq.1) then
             tv = (tau(1,i,j)+tau(1,imax,j))/2
          else
             tv = (tau(1,i,j)+tau(1,i-1,j))/2
          endif
          if(j.eq.1) then
             tv2 = tau(2,i,j)/2
          else
             tv2 = (tau(2,i,j)+tau(2,i,j-1))/2
          endif
          usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2)) &
               & *rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
          tv3 = tv3 + usurf(i,j)
       enddo
       if (par_wind_polar_avg.ne.2) then
          do i=1,imax
             if(j.le.2.or.j.ge.jmax-1)usurf(i,j) = tv3/imax
          enddo
       endif
    enddo

    ! sea-ice parameter definitions
    if (debug_init) print*, 'constant ice conductivity, consic =',consic
    ! in parameterization of heat flux at base of sea ice:
    ! empirical constant
    ch_ice = 0.0058
    if (debug_init) print*, &
         & 'base of sea-ice empirical constant, ch_ice =',ch_ice
    ! skin friction velocity (m/s)
    u_tau_ice = 0.02
    if (debug_init) print*, &
         & 'skin friction velocity, u_tau_ice =',u_tau_ice
    ! specific heat of sea water under ice at constant pressure (J/kg/K)
    cpo_ice = 4044
    if (debug_init) print*, &
         & 'specific heat of seawater under ice, cpo_ice =',cpo_ice
    if (debug_init) print*, &
         & 'representative ice density, rhoice =',rhoice
    ! useful constant proportional to inverse timscale for surface freezing
    rsictscsf = ch_ice*u_tau_ice*rho0*cpo_ice
    if (debug_init) print*,'rsictscsf = ',rsictscsf
    rsictscsf = dsc*dz(kmax)*rho0*cpo_ice/(17.5*86400.0)
    if (debug_init) print*,'rsictscsf = ',rsictscsf
    if (debug_init) print*, &
         & 'minimum average sea-ice thickness, hmin =',hmin
    ! density ratios
    if (debug_init) print*,'density ratios, rhooi =',rhooi
    if (debug_init) print*,'rrholf = ',rrholf
    ! FW flux conversion parameters
    if (debug_init) print*,'m to mm conversion factor, m2mm =',m2mm
    if (debug_init) print*,'mm to m conversion factor, mm2m =',mm2m
    ! read initial atmos state
    if (debug_init) print*,'tatm relh0_ocean relh0_land'
    if (debug_init) print*,tatm,relh0_ocean,relh0_land
    ! read freshwater flux perturbation data
    if (debug_init) print*,'extra1a range1b nsteps_extra1c'
    if (debug_init) print*,extra1a,extra1b,extra1c
    ! read scaling factor for extra1a, extra1b, extra1c
    if (debug_init) print*,'scl_fwf'
    if (debug_init) print*,scl_fwf
    ! Read the EMBM reference height
    if (debug_init) print*,'EMBM reference height, z1 (m)'
    if (debug_init) print*,z1_embm
    ! apply scl_fwf
    extra1a = scl_fwf*extra1a
    extra1b = scl_fwf*extra1b
    extra1c = scl_fwf*extra1c

    ! find total no. of Pac/Atl gridboxes

    ! find southern boundaries of regions for FW flux adjustment
    ! region 1a: 'jsf+1' to 20S
    ! region 1b: 20S to 24N
    ! region 1c: 24N to 90N

    ! southern boundary of region 1a
    j1as = jsf+1
    tv=sin(-20.0*pi/180.0)
    tv2=sin(24.0*pi/180.0)
    do j=1,jmax
       ! southern boundary of region 1b
       if ((tv.ge.sv(j-1)).and.(tv.le.sv(j))) then
          ! at least half of box area has to be in region
          if (((sv(j)-tv)/ds(j)).ge.0.5) then
             j1bs = j
          else
             j1bs = j+1
          endif
       endif
       ! southern boundary of region 1c
       if ((tv2.ge.sv(j-1)).and.(tv2.le.sv(j))) then
          ! at least half of box area has to be in region
          if (((sv(j)-tv2)/ds(j)).ge.0.5) then
             j1cs = j
          else
             j1cs = j+1
          endif
       endif
    enddo

    if(igrid.eq.0) then
       ! in south Atlantic (to 20 deg S)
       npac1a = 0
       natl1a = 0
       do j=j1as,(j1bs-1)
          npac1a = npac1a + ipf(j) - ips(j) + 1
          natl1a = natl1a + iaf(j) - ias(j) + 1
       enddo

       ! in tropical Atlantic (20 deg S to 24 deg N)
       npac1b = 0
       natl1b = 0
       do j=j1bs,(j1cs-1)
          npac1b = npac1b + ipf(j) - ips(j) + 1
          natl1b = natl1b + iaf(j) - ias(j) + 1
       enddo

       ! in north Atlantic (north of 24 deg N) NB INCLUDES DRY POINTS
       npac1c = 0
       natl1c = 0
       do j=j1cs,jmax
          do i=ips(j),ipf(j)
             if(k1(i,j).le.kmax)npac1c = npac1c + 1
          enddo
          do i=ias(j),iaf(j)
             if(k1(i,j).le.kmax)natl1c = natl1c + 1
          enddo
       enddo
    endif

    if(igrid.eq.1.or.igrid.eq.2)then
       area_pac1a = 0.
       area_atl1a = 0.
       do j=j1as,(j1bs-1)
          do i=ips(j),ipf(j)
             area_pac1a = area_pac1a + asurf(j)
          enddo
          ! conditionality to take care of "split Atlantic" (rma, 5/10/05)
          if(ias(j).gt.iaf(j)) then
             do i=ias(j),imax
                area_atl1a = area_atl1a + asurf(j)
             enddo
             do i=1,iaf(j)
                area_atl1a = area_atl1a + asurf(j)
             enddo
          else
             do i=ias(j),iaf(j)
                area_atl1a = area_atl1a + asurf(j)
             enddo
          endif
       enddo

       area_pac1b = 0.
       area_atl1b = 0.
       do j=j1bs,(j1cs-1)
          do i=ips(j),ipf(j)
             area_pac1b = area_pac1b + asurf(j)
          enddo
          ! conditionality to take care of "split Atlantic" (rma, 5/10/05)
          if(ias(j).gt.iaf(j)) then
             do i=ias(j),imax
                area_atl1b = area_atl1b + asurf(j)
             enddo
             do i=1,iaf(j)
                area_atl1b = area_atl1b + asurf(j)
             enddo
          else
             do i=ias(j),iaf(j)
                area_atl1b = area_atl1b + asurf(j)
             enddo
          endif
       enddo

       area_pac1c = 0.
       area_atl1c = 0.
       do j=j1cs,jmax
          do i=ips(j),ipf(j)
             if(k1(i,j).le.kmax) area_pac1c = area_pac1c + asurf(j)
          enddo
          ! conditionality to take care of "split Atlantic" (rma, 5/10/05)
          if(ias(j).gt.iaf(j)) then
             do i=ias(j),imax
                if(k1(i,j).le.kmax) area_atl1c = area_atl1c + asurf(j)
             enddo
             do i=1,iaf(j)
                if(k1(i,j).le.kmax) area_atl1c = area_atl1c + asurf(j)
             enddo
          else
             do i=ias(j),iaf(j)
                if(k1(i,j).le.kmax) area_atl1c = area_atl1c + asurf(j)
             enddo
          endif
       enddo
    endif

    if (debug_init) print*, &
         & natl1a, npac1a, natl1b, npac1b, natl1c, npac1c
    if (debug_init) print*, &
         & 'natl1a, npac1a, natl1b, npac1b, natl1c, npac1c '

    ! increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
    ! [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
    ! here add/remove total extra1a, extra1b, extra1c Sv of freshwater
    ! equally by area in Pac/Atl resp.
    do j=1,jmax
       do i=1,imax
          pmeadj(i,j) = 0.
       enddo
    enddo

    ! If 36x36s ...
    if(igrid.eq.0)then
       do j=j1as,(j1bs-1)
          do i=ips(j),ipf(j)
             pmeadj(i,j) = 1e6*extra1a/(npac1a*asurf(j))
          enddo
          do i=ias(j),iaf(j)
             pmeadj(i,j) = -1e6*extra1a/(natl1a*asurf(j))
          enddo
       enddo

       do j=j1bs,(j1cs-1)
          do i=ips(j),ipf(j)
             pmeadj(i,j) = 1e6*extra1b/(npac1b*asurf(j))
          enddo
          do i=ias(j),iaf(j)
             pmeadj(i,j) = -1e6*extra1b/(natl1b*asurf(j))
          enddo
       enddo

       do j=j1cs,jmax
          do i=ips(j),ipf(j)
             if(k1(i,j).le.kmax) pmeadj(i,j) = 1e6*extra1c/(npac1c*asurf(j))
          enddo
          do i=ias(j),iaf(j)
             if(k1(i,j).le.kmax) pmeadj(i,j) = -1e6*extra1c/(natl1c*asurf(j))
          enddo
       enddo
    endif

    ! If 64x32 or 64x32l ...
    if(igrid.eq.1.or.igrid.eq.2)then
       do j=j1as,(j1bs-1)
          do i=ips(j),ipf(j)
             pmeadj(i,j) = 1e6*extra1a/area_pac1a
          enddo
          ! conditionality to take care of "split Atlantic" (rma, 5/10/05)
          if(ias(j).gt.iaf(j)) then
             do i=ias(j),imax
                pmeadj(i,j) = -1e6*extra1a/area_atl1a
             enddo
             do i=1,iaf(j)
                pmeadj(i,j) = -1e6*extra1a/area_atl1a
             enddo
          else
             do i=ias(j),iaf(j)
                pmeadj(i,j) = -1e6*extra1a/area_atl1a
             enddo
          endif
       enddo

       do j=j1bs,(j1cs-1)
          do i=ips(j),ipf(j)
             pmeadj(i,j) = 1e6*extra1b/area_pac1b
          enddo
          ! conditionality to take care of "split Atlantic" (rma, 5/10/05)
          if(ias(j).gt.iaf(j)) then
             do i=ias(j),imax
                pmeadj(i,j) = -1e6*extra1b/area_atl1b
             enddo
             do i=1,iaf(j)
                pmeadj(i,j) = -1e6*extra1b/area_atl1b
             enddo
          else
             do i=ias(j),iaf(j)
                pmeadj(i,j) = -1e6*extra1b/area_atl1b
             enddo
          endif
       enddo

       do j=j1cs,jmax
          do i=ips(j),ipf(j)
             if(k1(i,j).le.kmax) pmeadj(i,j) = 1e6*extra1c/area_pac1c
          enddo
          ! conditionality to take care of "split Atlantic" (rma, 5/10/05)
          if(ias(j).gt.iaf(j)) then
             do i=ias(j),imax
                if(k1(i,j).le.kmax) pmeadj(i,j) = -1e6*extra1c/area_atl1c
             enddo
             do i=1,iaf(j)
                if(k1(i,j).le.kmax) pmeadj(i,j) = -1e6*extra1c/area_atl1c
             enddo
          else
             do i=ias(j),iaf(j)
                if(k1(i,j).le.kmax) pmeadj(i,j) = -1e6*extra1c/area_atl1c
             enddo
          endif
       enddo
    endif

    ! *********************************************************************
    ! *** alternaive mask-based methodology *******************************
    ! *********************************************************************

    ! read in mask
    ! basin mask: 3: Atlantic
    !             2: Pacific
    !             1: remaining ocean grid cells
    !             0: land
    ! sub-region 1 (a): 'jsf+1' to 20S
    ! sub-region 2 (b): 20S to 24N
    ! sub-region 3 (c): 24N to 90N

    inquire(file=indir_name(1:lenin)//world//'.bmask',exist=ioex)
    if (ioex) then
       if (debug_init) print *, 'reading in basin mask file'
       call check_unit(13,__LINE__,__FILE__)
       open(13,file=indir_name(1:lenin)//world//'.bmask',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       do j=jmax,1,-1
          read(13,*,iostat=ios)(bmask(i,j),i=1,imax)
          call check_iostat(ios,__LINE__,__FILE__)
          if (debug_init) write(6,'(i4,66i3)')j,(bmask(i,j),i=1,imax)
       enddo
       close(13,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)

       ! find total no. of Pac/Atl gridboxes
       natl1a = 0
       npac1a = 0
       natl1b = 0
       npac1b = 0
       natl1c = 0
       npac1c = 0
       do j=1,jmax
          do i=1,imax
             ! south Pacific, Atlantic (to 20 deg S)
             if (bmask(i,j).eq.31) natl1a = natl1a + 1
             if (bmask(i,j).eq.21) npac1a = npac1a + 1
             ! tropical Pacific, Atlantic (20 deg S to 24 deg N)
             if (bmask(i,j).eq.32) natl1b = natl1b + 1
             if (bmask(i,j).eq.22) npac1b = npac1b + 1
             ! north Pacific, Atlantic (north of 24 deg N)
             if (bmask(i,j).eq.33) natl1c = natl1c + 1
             if (bmask(i,j).eq.23) npac1c = npac1c + 1
          enddo
       enddo

       ! increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
       ! [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
       ! here add/remove total extra1a, extra1b, extra1c Sv of freshwater
       ! equally by area in Pac/Atl resp.
       do j=1,jmax
          do i=1,imax
             pmeadj(i,j) = 0.
          enddo
       enddo
       do j=1,jmax
          do i=1,imax
             if (bmask(i,j).eq.31) pmeadj(i,j) = -1e6*extra1a/(natl1a*asurf(j))
             if (bmask(i,j).eq.21) pmeadj(i,j) = 1e6*extra1a/(npac1a*asurf(j))
             if (bmask(i,j).eq.32) pmeadj(i,j) = -1e6*extra1b/(natl1b*asurf(j))
             if (bmask(i,j).eq.22) pmeadj(i,j) = 1e6*extra1b/(npac1b*asurf(j))
             if (bmask(i,j).eq.33) pmeadj(i,j) = -1e6*extra1c/(natl1c*asurf(j))
             if (bmask(i,j).eq.23) pmeadj(i,j) = 1e6*extra1c/(npac1c*asurf(j))
          enddo
       enddo

    endif

    ! initialize atmosphere
    do j=1,jmax
       do i=1,imax
          ! initial air temperatures
          tq(1,i,j) = tatm
          tq1(1,i,j) = tq(1,i,j)

          ! initial specific humidities
          ! set to relh0_ocean*qsat_ocean over ocean and
          ! relh0_land*qsat_atmos over land
          if(k1(i,j).le.kmax)then
             if(tstar_ocn(i,j).gt.tsic) then
                tq(2,i,j) = relh0_ocean*const1* &
                     & exp(const2*tstar_ocn(i,j)/(tstar_ocn(i,j)+const3))
             else
                tq(2,i,j) = relh0_ocean*const1* &
                     & exp(const4*tstar_ocn(i,j)/(tstar_ocn(i,j)+const5))
             endif
          else
             if(tq1(1,i,j).gt.0.0) then
                tq(2,i,j) = relh0_land*const1*exp(const2 &
                     & *tq1(1,i,j)/(tq1(1,i,j)+const3))
             else
                tq(2,i,j) = relh0_land*const1*exp(const4 &
                     & *tq1(1,i,j)/(tq1(1,i,j)+const5))
             endif
          endif

          tq1(2,i,j) = tq(2,i,j)

          if (dosc) then
             do l=1,2
                tqavg(l,i,j) = 0.
             enddo
             ! Annual average fields for diagnostic of precipitation-adjusted
             ! humidity (i.e., humidity after precipitation)
             q_pa_avg(i,j) = 0.
             rq_pa_avg(i,j) = 0.
             do l=1,2
                fx0avg(l,i,j)  = 0.
                fwavg(l,i,j)   = 0.
             enddo
             do l=3,4
                fx0avg(l,i,j)  = 0.
             enddo
             palbavg(i,j)=0.
          endif
       enddo
    enddo

    call readroff

    ! read in observational data filenames
    lentdata = lnsig1(tdatafile)
    if (debug_init) print*,'Temperature observations filename, ', &
         & tdatafile(1:lentdata)
    lenqdata = lnsig1(qdatafile)
    if (debug_init) print*,'Humidity observations filename, ', &
         & qdatafile(1:lenqdata)
    if (tqinterp) then
       print*,'Interpoate observational dataset'
       lentvar = lnsig1(tdata_varname)
       print*,'Temperature observations variable name, ', &
            & tdata_varname(1:lentvar)
       print*,'Temperature observations scaling factor, ' &
            & ,tdata_scaling
       print*,'Temperature observations offset, ' ,tdata_offset
       print*,'Temperature observations missing value, ' &
            & ,tdata_missing
       lenqvar = lnsig1(qdata_varname)
       print*,'Humidity observations variable name, ', &
            & qdata_varname(1:lenqvar)
       print*,'Humidity observations scaling factor, ' &
            & ,qdata_scaling
       print*,'Humidity observations offset, ' ,qdata_offset
       print*,'Humidity observations missing value, ' &
            & ,qdata_missing
    endif
    if (debug_init) print*

    ! read in time varying orbital forcing
    if(orbit_radfor.eq."y".or.orbit_radfor.eq."Y") then
       open(unit=729,file=indir_name(1:lenin)// &
            & trim(filenameorbit),iostat=ios)
       do i=1,norbit
          read(729,*,iostat=ios)(orbitall_vect(i,n),n=1,5)
       enddo
       close(729)
       do i=1,norbit
          orbitecc_vect(norbit-i+1) = orbitall_vect(i,2)
          orbitobl_vect(norbit-i+1) = orbitall_vect(i,3)
          orbitpre_vect(norbit-i+1) = orbitall_vect(i,4)
          orbittau_vect(norbit-i+1) = orbitall_vect(i,5)
       enddo
    endif

    ! v2 seasonal. Calculate radiative forcing
    ! NOTE: this is where the solar constant is set
    if (debug_init) print *,'going to radfor'
    call radfor (0,gn_daysperyear,solconst,flag_ents)
    if (debug_init) print*,'file extension for output (a3) ?'
    if (debug_init) print*,lout

    if (debug_init) print*,'netCDF restart input ?'
    if (debug_init) print*,netin
    if ((netin.eq.'n').or.(netin.eq.'N')) then
       lnetin=.false.
    else
       lnetin=.true.
    endif

    if (debug_init) print*,'netCDF restart output ?'
    if (debug_init) print*,netout
    if ((netout.eq.'n').or.(netout.eq.'N')) then
       lnetout=.false.
    else
       lnetout=.true.
    endif

    if (debug_init) print*,'ASCII restart output ?'
    if (debug_init) print*,ascout
    if ((ascout.eq.'n').or.(ascout.eq.'N')) then
       lascout=.false.
    else
       lascout=.true.
    endif

    if (debug_init) print*,'filename for netCDF restart input ?'
    if (debug_init) print*,filenetin

    if (debug_init) print*, &
         & 'directory name for netCDF restart output ?'
    if (debug_init) print*,dirnetout

    if (debug_init) print*, &
         & 'Force EMBM from ATCHEM atmostpheric tracer cons?'
    if (debug_init) print*,atchem_radfor

    ! Is this a new or continuing run?
    if(ans.eq.'n'.or.ans.eq.'N')then
       if (debug_init) print*, &
            & 'this is a new run, initial conditions already set up'
       ! But set up initial default time and date....
       iyear_rest=2000
       imonth_rest=1
       ioffset_rest=0
       day_rest=yearlen / (nyear*ndta)
       if (debug_init) print*,'day_rest = ',day_rest
    else
       ! This is a continuing run, read in end state filename
       if (debug_init) print*,'input file extension for input (a6)'
       if (debug_init) print*,lin

       if (debug_init) print*,'Reading EMBM restart file'
       if(lnetin) then
          call inm_netcdf_embm
       else
          call check_unit(1,__LINE__,__FILE__)
          open(1,file=rstdir_name(1:lenrst)//lin,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          call inm_embm(1)
          close(1,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)

          !     But set up initial default time and date....
          iyear_rest=2000
          imonth_rest=1
          ioffset_rest=0
          day_rest=yearlen / (nyear*ndta)
          if (debug_init) print*,'day_rest = ',day_rest
       endif

       ! EMBM atm
       do j=1,jmax
          do i=1,imax
             tq1(1,i,j) = tq(1,i,j)
             tq1(2,i,j) = tq(2,i,j)
          enddo
       enddo
    endif


    ! Open output files

    ! Average, etc. values of surface air temperature
    call check_unit(41,__LINE__,__FILE__)
    open(41,file=outdir_name(1:lenout)//lout//'.'//'airt',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    write(41,*) '% EMBM atmosphere model, air temperature'
    write(41,'(4a15,a10,a15,a10)') '% time        ','N hem air temp', &
         & 'S hem air temp','Max air temp',' Location','Min air temp', &
         & ' Location'
    write(41,'(4a15,2a5,a15,2a5)') '%             ','degrees C', &
         & 'degrees C','degrees C','i','j','degrees C','i','j'
    close(41,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    ! Average, etc. values of surface specific humidity
    call check_unit(42,__LINE__,__FILE__)
    open(42,file=outdir_name(1:lenout)//lout//'.'//'q', iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    write(42,*) '% EMBM atmosphere model, specific humidity'
    write(42,'(4a15,a10,a15,a10)') '% time        ','N hem spec hum', &
         & 'S hem spec hum','Max spec hum',' Location','Min spec hum', &
         & ' Location'
    write(42,'(4a15,2a5,a15,2a5)') '%             ','g / kg','g / kg', &
         & 'g / kg','i','j','g / kg','i','j'
    close(42,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    if (dosc) then
    endif

    ! count variable for output file numbers
    iw  = 1
    iav = 1

    ! Setting up grid structure (D. Lunt code)


313 format(i3,6f8.2)

    if (debug_init) print*
    if (debug_init) print*,'EMBM/GENIE grid interpolation variables :'

    if (debug_init) print*, &
         & '* Longitude : alon1, alon2, alon3, abox1, abox2, abox3 *'
    do i=1,imax
       alon1(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
       alon2(i)=real(360.0*i/real(imax)+phi0/deg_to_rad)
       alon3(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
       nclon1(i) = alon1(i)
       nclon2(i) = real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
       nclon3(i) = alon3(i)
    end do
    do i=1,imax+1
       aboxedge1_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
       aboxedge2_lon(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
       aboxedge3_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
    end do

    do i=1,imax+1
       if(i.lt.imax+1) then
          if (debug_init) write(*,313) i,alon1(i),alon2(i),alon3(i), &
               & aboxedge1_lon(i),aboxedge2_lon(i),aboxedge3_lon(i)
       else
          if (debug_init) write(*,313) i,-999.99,-999.99,-999.99, &
               & aboxedge1_lon(i),aboxedge2_lon(i),aboxedge3_lon(i)
       endif
    enddo

    if (debug_init) print*, &
         & '* Latitude : alat1, alat2, alat3, abox1, abox2, abox3 *'
    nclat3(1)=real(asin(sv(0))*180.0/pi)
    do j=1,jmax
       alat1(j)=real(asin(s(j))*180.0/pi)
       alat2(j)=real(asin(s(j))*180.0/pi)
       alat3(j)=real(asin(sv(j))*180.0/pi)
       nclat1(j) = alat1(j)
       nclat2(j) = alat2(j)
       if (j.lt.jmax) nclat3(j+1) =real(asin(sv(j))*180.0/pi)
    end do
    do j=1,jmax+1
       aboxedge1_lat(j)=real(asin(sv(j-1))*180.0/pi)
       aboxedge2_lat(j)=real(asin(sv(j-1))*180.0/pi)
       ! following if statement stops bounds error
       if (j.le.jmax) aboxedge3_lat(j)=real(asin(s(j))*180.0/pi)
    end do
    aboxedge3_lat(jmax+1)=real(asin(sv(jmax))*180.0/pi)

    do j=1,jmax+1
       if(j.lt.jmax+1) then
          if (debug_init) write(*,313) j,alat1(j),alat2(j),alat3(j), &
               & aboxedge1_lat(j),aboxedge2_lat(j),aboxedge3_lat(j)
       else
          if (debug_init) write(*,313) j,-999.99,-999.99,-999.99, &
               & aboxedge1_lat(j),aboxedge2_lat(j),aboxedge3_lat(j)
       endif
    enddo

    ! This bit is to make the land-sea mask on the genie grid.  The genie
    ! grid is offset from the goldstein grid by imax/4 in the longitudinal
    ! direction.
    do j=1,jmax
       do i=1,imax
          if (k1(i,j).ge.90) then
             ilandmask1(i,j)=1
             ilandmask2(i,j)=1
             ilandmask3(i,j)=1
          else
             ilandmask1(i,j)=0
             ilandmask2(i,j)=0
             ilandmask3(i,j)=0
          end if
       end do
    end do
    if (debug_init) print*

    ! Output arguments
    do j=1,jmax
       do i=1,imax
          ! Surface air temperature [-> surface fluxes]
          tstar_atm(i,j) = real(tq1(1,i,j))
          ! Surface specific humidity [-> surface fluxes]
          qstar_atm(i,j) = real(tq1(2,i,j))
          ! Variables dztau and dztav are renamed here to separate the unscaled
          ! (hence 'us') versions read in from files, from those scaled versions
          ! used in surflux and GOLDSTEIN.
          ! Wind stress x components [-> ocean, surface fluxes]
          stressxu_atm(i,j) = real(us_dztau(1,i,j))
          stressxv_atm(i,j) = real(us_dztav(1,i,j))
          ! Wind stress y components [-> ocean, surface fluxes]
          stressyu_atm(i,j) = real(us_dztau(2,i,j))
          stressyv_atm(i,j) = real(us_dztav(2,i,j))
          eb_ca(i,j) = real(ca(i,j))
          lowestlu2_atm(i,j)=real(uatm(1,i,j)*usc)
          lowestlv3_atm(i,j)=real(uatm(2,i,j)*usc)
       enddo
    enddo

    ! Set values of dummy variables destined for BIOGEM
    eb_rmax = real(rmax)
    eb_dphi = real(dphi)
    eb_rdtdim = real(rdtdim)

    ! read in orography even if orogswitch is off (so orography can be
    ! used for ice sheet melwater as well as climatology)
    if (t_orog.eq.1) then
       if(t_d18o.eq.1) then
          print*,"ERROR: Both transient orography switches are on"
          stop
       endif
       open(77,file=trim(filenameorog))
       do l=1,norog
          do i=1,imax
             do j=1,jmax
                read(77,*) orog_vect(i,j,l)
             enddo
          enddo
       enddo
       close(77)
       surf_orog_atm(:,:)=orog_vect(:,:,1)
    else if (t_d18o.ge.1) then
       ! read in benthic d18o time series and fields to construct transient
       ! ice sheets
       ! see holden et al 2009 Clim Past Disc
       ! d18o time series
       open(77,file=trim(filenamed18o))
       do l=1,nd18o
          read(77,*) d18o_vect(l)
       enddo
       close(77)
       ! threshold value of d18o at which cell is ice covered
       open(77,file=trim(filenamed18oicethresh))
       do i=1,imax
          do j=1,jmax
             read(77,*) d18o_ice_thresh(i,j)
          enddo
       enddo
       close(77)
       ! minimum (modern) orography
       open(77,file=trim(filenamed18oorogmin))
       do i=1,imax
          do j=1,jmax
             read(77,*) d18o_orog_min(i,j)
          enddo
       enddo
       ! "gradient" of orography wrt d180
       open(77,file=trim(filenamed18ooroggrad))
       do i=1,imax
          do j=1,jmax
             read(77,*) d18o_orog_grad(i,j)
          enddo
       enddo
       close(77)
       ! initialise orography
       ! LOOK need to initialise this correctly for restarts
       ! (when ENTS restarts are enabled which they are not as of 2/2/10)
       do i=1,imax
          do j=1,jmax
             if(d18o_ice_thresh(i,j).lt.1.0e-5) then
                ! ocean (not used but initialised for completeness)
                surf_orog_atm(i,j)=0.0
             else if(d18o_vect(1).gt.d18o_ice_thresh(i,j)) then
                ! ice sheet
                surf_orog_atm(i,j)=d18o_orog_min(i,j)+ &
                     & d18o_orog_grad(i,j)* &
                     & (d18o_vect(1)-d18o_ice_thresh(i,j))/ &
                     & (d18o_vect(1)-d18o_ice_thresh(i,j)+d18o_k)
             else
                ! land (no ice)
                surf_orog_atm(i,j)=d18o_orog_min(i,j)
             endif
          enddo
       enddo
    else
       if (orogswitch.ge.1) then
          open(77,file=trim(filenameorog))
          do i=1,imax
             do j=1,jmax
                read(77,*) surf_orog_atm(i,j)
             enddo
          enddo
          close(77)
       else
          do i=1,imax
             do j=1,jmax
                surf_orog_atm(i,j)=0.0
             enddo
          enddo
       endif
       if (debug_init) print*,'orog:', &
            & sum(surf_orog_atm)/size(surf_orog_atm)
    endif

    do i=1,imax
       do j=1,jmax
          torog_atm(i,j) = tstar_atm(i,j)
          if (orogswitch.eq.1) then
             torog_atm(i,j) = torog_atm(i,j) + lapse_rate*surf_orog_atm(i,j)
          endif
       enddo
    enddo

    ! Read in where ice sheets are (0=ocean, 1=land, 2=ice sheet)
    ! If t_lice is on (i.e. time-varying ice-sheets) then file is big.
    if (t_lice.eq.1) then
       if(t_d18o.eq.1) then
          print*,"ERROR: Both transient icemask switches are on"
          stop
       endif
       open(77,file=trim(filenamelice))
       do l=1,nlice
          do i=1,imax
             do j=1,jmax
                read(77,*) lice_vect(i,j,l)
             enddo
          enddo
       enddo
       close(77)
       landice_slicemask_lic(:,:)=nint(lice_vect(:,:,1))
    else if (t_d18o.ge.1) then
       ! calculate ice mask from benthic d18o
       ! LOOK need to initialise this correctly for restarts
       ! (when ENTS restarts are enabled which they are not as of 2/2/10)
       do i=1,imax
          do j=1,jmax
             if(d18o_ice_thresh(i,j).lt.1.0e-5) then
                ! ocean
                landice_slicemask_lic(i,j)=0.0
             else if (d18o_vect(1).gt.d18o_ice_thresh(i,j)) then
                ! ice sheets
                landice_slicemask_lic(i,j)=2.0
             else
                ! land
                landice_slicemask_lic(i,j)=1.0
             endif
          enddo
       enddo
    else
       if (flag_ents) then
          open(77,file=trim(filenamelice))
          do i=1,imax
             do j=1,jmax
                read(77,*) landice_slicemask_lic(i,j)
             enddo
          enddo
          close(77)
       else
          do i=1,imax
             do j=1,jmax
                if (k1(i,j).le.kmax) then
                   landice_slicemask_lic(i,j)=0.
                else
                   landice_slicemask_lic(i,j)=1.
                endif
             enddo
          enddo
       endif
       if (debug_init) print*,'lice:', &
            & sum(landice_slicemask_lic)/size(landice_slicemask_lic)
    endif

    ! Read in offline NCEP fields if offline version selected
    if (flag_ents) then
       if(ents_offlineswitch.eq.1)then
          open(77,file=indir_name(1:lenin)//'NCEP_airt_monthly.dat')
          open(78,file=indir_name(1:lenin)//'NCEP_pptn_monthly.dat')
          open(79,file=indir_name(1:lenin)//'NCEP_RH_monthly.dat')
          do l=1,nmth+1
             do i=1,imax
                do j=1,jmax
                   read(77,*) tncep1(i,j,l)
                   read(78,*) pncep1(i,j,l)
                   read(79,*) rhncep1(i,j,l)
                enddo
             enddo
          enddo
          close(77)
          close(78)
          close(79)
       endif

       ! Create atmospheric albedo fields
       open(77,file=indir_name(1:lenin)//'atm_albedo_monthly.dat')
       do l=1,nmth+1
          do i=1,imax
             do j=1,jmax
                read(77,*) atm_alb1(i,j,l)
             enddo
          enddo
       enddo
       close(77)

       ! Read in and initialise monthly winds
       open(35,file=indir_name(1:lenin)//'uvic_windx.silo')
       read(35,*)(((uatml1(1,i,j,l),i=1,imax),j=1,jmax),l=1,nmth+1)
       close(35)

       open(35,file=indir_name(1:lenin)//'uvic_windy.silo')
       read(35,*)(((uatml1(2,i,j,l),i=1,imax),j=1,jmax),l=1,nmth+1)
       close(35)

       ! read in wind speeds for use in radiation/evap calc.
       open(35,file=indir_name(1:lenin)//'monthly_windspd.silo')
       read(35,*)(((usurfl1(i,j,l),i=1,imax),j=1,jmax),l=1,nmth+1)
       close(35)

       ! preprocess for use in tsetpa/tstipa
       ! conditional zonal average
       ! Step avoided if par_wind_polar_avg is 2
       if (par_wind_polar_avg.ne.2)  then
          do m=1,nmth+1
             do j=1,jmax
                if(j.le.2.or.j.ge.jmax-1)then
                   do l=1,2
                      tv = 0.
                      tv1= 0.
                      do i=1,imax
                         tv = tv + uatml1(l,i,j,m)
                         tv1= tv1+ usurfl1(i,j,m)
                      enddo
                      tv = tv / imax
                      tv1= tv1/ imax
                      do i=1,imax
                         uatml1(l,i,j,m) = tv
                         usurfl1(i,j,m) = tv1
                      enddo
                   enddo
                endif
             enddo
          enddo
       endif

       ! remove zonal average of v else fail mass conservation (may not be
       ! disastrous).
       ! Step avoided if par_wind_polar_avg is 2
       do l=1,nmth+1
          do i=1,imax
             do j=1,jmax
                uatml1(1,i,j,l) = uatml1(1,i,j,l)/usc
                uatml1(2,i,j,l) = uatml1(2,i,j,l)/usc
             enddo
             if (par_wind_polar_avg.ne.2)  then
                uatml1(2,i,jmax,l) = 0.
             endif
          enddo
       enddo
       ! Interpolate prescribed model fields
       call field_interp(uatml1,usurfl1,tncep1,pncep1,rhncep1, &
            & atm_alb1)
       ! I removed this part so that we keep ents winds separate from embm
       ! winds even with unify_winds
       if (unify_winds .ge. 1) then
          ! Replace ents uatm with embm fields
          do l=1,nyear
             do i=1,imax
                do j=1,jmax
                   uatml(1,i,j,l)=uatm(1,i,j)
                   uatml(2,i,j,l)=uatm(2,i,j)
                enddo
             enddo
          enddo
       endif
       if (unify_winds .eq. 1) then
          ! Replace ents usurf by embm fields
          do l=1,nyear
             do i=1,imax
                do j=1,jmax
                   usurfl(i,j,l)=usurf(i,j)
                enddo
             enddo
          enddo
       endif
    else
       do l=1,nyear
          do i=1,imax
             do j=1,jmax
                uatml(1,i,j,l)=0.
                uatml(2,i,j,l)=0.
                usurfl(i,j,l)=0.
                tncep(i,j,l)=0.
                pncep(i,j,l)=0.
                rhncep(i,j,l)=0.
                atm_alb(i,j,l)=0.
             enddo
          enddo
       enddo
    endif

    ! precipitation timescale and land radiation

    ! Define lambda i.e. atm timestep/pptn timescale converted to secs
    if (debug_init) print*,'timepptn=',timepptn
    lambdapptn = min(1.,gn_daysperyear/(real(nyear)*timepptn))
    if (debug_init) print*,'lambdapptn=',lambdapptn
    ! typical land depth scale
    hld = 1.
    ! typical land heat capacity (J/m3/K)
    cld = 3.3e5
    rhcld = syr/(nyear*hld*cld)
    if (debug_init) print*,'rhcld ',rhcld,nyear,hld,cld

    ! Diagnostic fields of precipitation-adjusted humidity (i.e., humidity
    ! after precipitation)
    ! calculate saturation humidity in line with 'surflux.F' for
    ! diagnostics ('surflux.F' will update it again) and calculate
    ! specific and relative humidity after adjustment for precipitation
    ! has been made
    do j=1,jmax
       do i=1,imax
          if((orogswitch.lt.2).and.(flag_ents)) then
             qsat = const1*exp(const4*torog_atm(i,j)/(torog_atm(i,j)+const5))
          else
             qsat = const1*exp(const4*tstar_atm(i,j)/(tstar_atm(i,j)+const5))
          endif
          if (flag_ents) then
             deltq = lambdapptn*(qstar_atm(i,j)-(rmax*qsat))
             q_pa(i,j) = min(qstar_atm(i,j), real(qstar_atm(i,j)-deltq))
          else
             q_pa(i,j) = min(qstar_atm(i,j), real(rmax*qsat))
          endif
          rq_pa(i,j) = q_pa(i,j)/qsat
       enddo
    enddo

    ! scheme to allocate land runoff: 0 - bucket overflow (default); 1 -
    ! Leaky bucket (Meissner et al '03); 2 - soil moisture 'half-life'
    if (debug_init) print*,'runoff scheme ',par_runoff_scheme
    if (debug_init) print*, &
         & 'Clapp-Hornberger exponent  (for runoff scheme = 1) = ', &
         & par_runoff_b
    if (debug_init) print*, &
         & 'soil moisture half-life (for runoff scheme = 2) = ', &
         & par_runoff_tau

    if (debug_init) print*,'syr = ',syr

    runoff_factor_1 = 2*par_runoff_b+3
    runoff_factor_2 = 1-exp(-0.693*dtatm*ndta*tsc/ &
         & (par_runoff_tau*syr/12.0))
    if (debug_init) print*,'runoff factor 1 = ',runoff_factor_1
    if (debug_init) print*,'runoff factor 2 = ',runoff_factor_2

    print*,' <<< Initialisation complete'
    print*,'======================================================='
  end subroutine initialise_embm


  SUBROUTINE end_embm
    IMPLICIT NONE

    PRINT *, '======================================================='
    PRINT *, ' >>> Initialising EMBM module shutdown ...'

    IF (debug_end) CALL diagend_embm

    PRINT *, ' <<< Shutdown complete'
    PRINT *, '======================================================='
  END SUBROUTINE end_embm


  ! Atmospheric timestep: iterative implicit version
  ! Coefficient of implicitness cimp: cimp=1 fully implicit, cimp=0
  ! explicit.
  ! Coeffs for iterative implicit scheme are defined at cell faces.
  ! eg flux across east face = cie(i)*T(i+1) + ciw(i)*T(i)
  subroutine tstipa
    implicit none

    real tv, ups, ups0, pec, diffpp, cimp, centre, dtloc
    real cie(0:maxi,0:maxj),ciw(0:maxi,0:maxj), &
         & cin(0:maxi,0:maxj),cis(0:maxi,0:maxj)
    real tq2(0:maxi+1,0:maxj+1)
    integer iits, nii         ! iterations to solve timestep
    parameter (nii=4, ups0=999, cimp=0.5)
    integer i, j, l
    logical correct
    parameter(correct=.true. )
    dtloc = dtatm

    ! set b.c's on local variables
    do i=0,imax
       cin(i,0) = 0.
       cis(i,0) = 0.
       tq2(i,0) = 0.
       cin(i,jmax) = 0.
       cis(i,jmax) = 0.
       tq2(i,jmax+1) = 0.
    enddo

    do l=1,2
       do j=1,jmax
          do i=1,imax
             ! flux to east
             cie(i,j) = betaz(l)*uatm(1,i,j)*rc(j)*0.5*rdphi
             diffpp = diffa(l,1,j) + &
                  & (2-l)*diffmod0*max(0.0,min(1.0, &
                  & (pptn(i,j)-ppmin)/(ppmax-ppmin)))

             tv = rc(j)*rc(j)*rdphi*diffpp*rdphi
             pec = betaz(l)*uatm(1,i,j)*dphi/diffpp
             ups = pec / (2.0 + abs(pec))
             ciw(i,j) = cie(i,j)*(1+ups) + tv
             cie(i,j) = cie(i,j)*(1-ups) - tv
             ! flux to north
             cin(i,j) = cv(j)*betam(l)*uatm(2,i,j)*0.5
             diffpp = diffa(l,2,j) + &
                  & (2-l)*diffmod0*max(0.0,min(1.0, &
                  & (pptn(i,j)-ppmin)/(ppmax-ppmin)))
             ! cv(jmax) = 0 but dsv not defined so mask needed
             if(j.lt.jmax)then
                tv = cv(j)*cv(j)*rdsv(j)*diffa(l,2,j)
                pec = betam(l)*uatm(2,i,j)*dsv(j)/diffpp
                ups = pec / (2.0 + abs(pec))
             else
                tv = 0.
                ups = 0.
             endif
             cis(i,j) = cin(i,j)*(1+ups) + tv
             cin(i,j) = cin(i,j)*(1-ups) - tv
          enddo
       enddo
       do j=1,jmax
          cie(0,j) = cie(imax,j)
          ciw(0,j) = ciw(imax,j)
       enddo

       ! iterate to solve timestep
       do iits=1,nii
          do j=1,jmax
             do i=1,imax
                tq2(i,j) = cimp*tq(l,i,j) + (1.0 - cimp)*tq1(l,i,j)
             enddo
          enddo
          do j=1,jmax
             tq2(0,j) = tq2(imax,j)
             tq2(imax+1,j) = tq2(1,j)
          enddo
          do j=1,jmax
             do i=1,imax
                centre = dtloc*(ciw(i,j) - cie(i-1,j) &
                     & + (cis(i,j) - cin(i,j-1))*rds(j))
                tq(l,i,j) = (tq1(l,i,j)*(1.0 - (1.0-cimp) &
                     & *centre) - dtloc*(-tqa(l,i,j) &
                     & + cie(i,j)  *tq2(i+1,j) &
                     & - ciw(i-1,j)*tq2(i-1,j) &
                     & + (cin(i,j)  *tq2(i,j+1) &
                     & - cis(i,j-1)*tq2(i,j-1))*rds(j)))/ &
                     & (1 + cimp*centre)
             enddo
          enddo
       enddo
       if(correct)then
          do j=1,jmax
             do i=1,imax
                tq2(i,j) = 0.5*(tq2(i,j) + cimp*tq(l,i,j) &
                     & + (1.0 - cimp)*tq1(l,i,j))
             enddo
          enddo
          do j=1,jmax
             tq2(0,j) = tq2(imax,j)
             tq2(imax+1,j) = tq2(1,j)
          enddo
          do j=1,jmax
             do i=1,imax

                ! explicit and conservative corrector step
                tq(l,i,j) =  tq1(l,i,j) - dtloc*(-tqa(l,i,j) &
                     & + cie(i,j)  *tq2(i+1,j) &
                     & - ciw(i-1,j)*tq2(i-1,j) &
                     & + (cin(i,j)  *tq2(i,j+1) &
                     & - cis(i,j-1)*tq2(i,j-1))*rds(j)) &
                     & - dtloc*tq2(i,j)*( &
                     & ciw(i,j) - cie(i-1,j) &
                     & + (cis(i,j) - cin(i,j-1))*rds(j) )
             enddo
          enddo
       endif
    enddo

    ! update tq1
    do j=1,jmax
       do i=1,imax
          do l=1,2
             tq1(l,i,j) = tq(l,i,j)
          enddo
       enddo
    enddo

  end subroutine tstipa


  ! Subroutine to interpolate between monthly mean
  ! fields. The interpolation method is calculated as
  ! in Killworth (1995)
  ! Ref: P. D. Killworth (1995). Time interpolation of
  !      forcing fields in ocean models. J. Phys. Ocn.
  !      26, 136-143.
  ! This method preserves the monthly mean of the fields
  ! whereas linear interpolation does not.
  ! MSW 8/2/5
  subroutine field_interp(uatml1,usurfl1,tncep1,pncep1,rhncep1,atm_alb1)
    implicit none

    real invmat(nmth,nmth)

    real uatml1(2,imax,jmax,nmth+1)
    real usurfl1(imax,jmax,nmth+1)
    real tncep1(imax,jmax,nmth+1)
    real pncep1(imax,jmax,nmth+1)
    real rhncep1(imax,jmax,nmth+1)
    real atm_alb1(imax,jmax,nmth+1)

    real puatml(2,imax,jmax,nmth)
    real pusurfl(imax,jmax,nmth)
    real ptncep(imax,jmax,nmth)
    real ppncep(imax,jmax,nmth)
    real prhncep(imax,jmax,nmth)
    real patm_alb(imax,jmax,nmth)

    real midpoint(0:nmth+1)

    real xint,x1int,x2int,y1int(8),y2int(8),gradint(8)

    integer i,j,m,istep,l

    ! Read in inverse of linear interpolation matrix. Calculation of this
    ! matrix and its inverse is assuming equal month length
    open(1,file=indir_name(1:lenin)//'inv_linterp_matrix.dat')
    do j=1,nmth
       do i=1,nmth
          read(1,*)invmat(i,j)
       enddo
    enddo
    close(1)

    ! Calculate pseudo data
    do j=1,jmax
       do i=1,imax
          puatml(1,i,j,:)=matmul(invmat,uatml1(1,i,j,1:12))
          puatml(2,i,j,:)=matmul(invmat,uatml1(2,i,j,1:12))
          pusurfl(i,j,:)=matmul(invmat,usurfl1(i,j,1:12))
          ptncep(i,j,:)=matmul(invmat,tncep1(i,j,1:12))
          ppncep(i,j,:)=matmul(invmat,pncep1(i,j,1:12))
          prhncep(i,j,:)=matmul(invmat,rhncep1(i,j,1:12))
          patm_alb(i,j,:)=matmul(invmat,atm_alb1(i,j,1:12))
       enddo
    enddo

    ! Linearly interpolate based on the pseudo data
    ! First find exact istep for midpoint of each month
    do m=0,nmth+1
       midpoint(m)=0.5*((2.*m)-1)*real(nyear)/real(nmth)
    enddo

    do j=1,jmax
       do i=1,imax
          m=0
          do istep=1,nyear
             if(real(istep).ge.midpoint(m))then
                m=m+1
             endif
             ! x terms (i.e. time)
             xint=istep
             x1int=midpoint(m-1)
             x2int=midpoint(m)
             ! y terms (i.e. field values)
             if(m.eq.1)then
                y1int(1)=puatml(1,i,j,nmth)
                y1int(2)=puatml(2,i,j,nmth)
                y1int(3)=pusurfl(i,j,nmth)
                y1int(4)=ptncep(i,j,nmth)
                y1int(5)=ppncep(i,j,nmth)
                y1int(6)=prhncep(i,j,nmth)
                y1int(7)=patm_alb(i,j,nmth)

                y2int(1)=puatml(1,i,j,m)
                y2int(2)=puatml(2,i,j,m)
                y2int(3)=pusurfl(i,j,m)
                y2int(4)=ptncep(i,j,m)
                y2int(5)=ppncep(i,j,m)
                y2int(6)=prhncep(i,j,m)
                y2int(7)=patm_alb(i,j,m)
             else if(m.gt.nmth)then
                y1int(1)=puatml(1,i,j,m-1)
                y1int(2)=puatml(2,i,j,m-1)
                y1int(3)=pusurfl(i,j,m-1)
                y1int(4)=ptncep(i,j,m-1)
                y1int(5)=ppncep(i,j,m-1)
                y1int(6)=prhncep(i,j,m-1)
                y1int(7)=patm_alb(i,j,m-1)

                y2int(1)=puatml(1,i,j,1)
                y2int(2)=puatml(2,i,j,1)
                y2int(3)=pusurfl(i,j,1)
                y2int(4)=ptncep(i,j,1)
                y2int(5)=ppncep(i,j,1)
                y2int(6)=prhncep(i,j,1)
                y2int(7)=patm_alb(i,j,1)
             else
                y1int(1)=puatml(1,i,j,m-1)
                y1int(2)=puatml(2,i,j,m-1)
                y1int(3)=pusurfl(i,j,m-1)
                y1int(4)=ptncep(i,j,m-1)
                y1int(5)=ppncep(i,j,m-1)
                y1int(6)=prhncep(i,j,m-1)
                y1int(7)=patm_alb(i,j,m-1)

                y2int(1)=puatml(1,i,j,m)
                y2int(2)=puatml(2,i,j,m)
                y2int(3)=pusurfl(i,j,m)
                y2int(4)=ptncep(i,j,m)
                y2int(5)=ppncep(i,j,m)
                y2int(6)=prhncep(i,j,m)
                y2int(7)=patm_alb(i,j,m)
             endif
             do l=1,8
                gradint(l)=(y2int(l)-y1int(l))/(x2int-x1int)
             enddo
             uatml(1,i,j,istep)=(gradint(1)*(xint-x1int))+y1int(1)
             uatml(2,i,j,istep)=(gradint(2)*(xint-x1int))+y1int(2)
             usurfl(i,j,istep)=(gradint(3)*(xint-x1int))+y1int(3)
             tncep(i,j,istep)=(gradint(4)*(xint-x1int))+y1int(4)
             pncep(i,j,istep)=(gradint(5)*(xint-x1int))+y1int(5)
             rhncep(i,j,istep)=(gradint(6)*(xint-x1int))+y1int(6)
             atm_alb(i,j,istep)=(gradint(7)*(xint-x1int))+y1int(7)

             ! If just want annual average forcing then make all elements in array
             ! the same in the time direction
             if(ents_seasonswitch.eq.0)then
                uatml(1,i,j,istep)=uatml1(1,i,j,nmth+1)
                uatml(2,i,j,istep)=uatml1(2,i,j,nmth+1)
                usurfl(i,j,istep)=usurfl1(i,j,nmth+1)
                tncep(i,j,istep)=tncep1(i,j,nmth+1)
                pncep(i,j,istep)=pncep1(i,j,nmth+1)
                rhncep(i,j,istep)=rhncep1(i,j,nmth+1)
                atm_alb(i,j,istep)=atm_alb1(i,j,nmth+1)
             endif

          enddo
       enddo
    enddo

  end subroutine field_interp


  ! Used to calculate mean daily ocean albedo zenith angle dependence
  ! Calculation performed at initialisation stage for every latitude and
  ! istep.
  subroutine ocean_alb(oscss,osccc,oscday,j,istep)
    implicit none

    real oscss,osccc,oscday
    real h
    real a,b,sum,tol

    real old,int,radout
    real xp

    integer n,p,j,x,istep

    parameter(tol=1.e-5)

    sum = 0.0
    p = 0

    ! Integration loop (adaptive extended trapezium rule) returns the
    ! integrated value when converges to a specified tolerance. See
    ! Numerical Recipes Ch. 4

    ! Initial values
    old=-1.e30
    int=1.e30
    ! Integration limits
    a=0.
    b=oscday

    if(b.le.0.)then
       albo(j,istep)=1.
    else
       do n=1,15
          ! Initial guess based on end points
          if(n.eq.1)then
             p=1
             call rad_out(radout,a,oscss,osccc)
             sum=0.5*radout
             call rad_out(radout,b,oscss,osccc)
             sum=sum+(0.5*radout)
             old=(b-a)*sum
          else
             old=int
          endif
          ! Interval size doubles with each iteration
          h=(b-a)/(2*p)
          ! Calculate for new points only then add to the running sum
          do x=1,p
             xp=a+h+(2*h*(x-1))
             call rad_out(radout,xp,oscss,osccc)
             sum=sum+radout
          enddo
          ! Calculate new value of integral
          int=h*sum
          ! Check tolerance
          if(abs(int-old).lt.tol*abs(old)) exit
          ! Double number of points to evaluate
          p=2*p
       enddo

       ! Work out ocean albedo (outgoing radiation/incoming radiation)
       albo(j,istep)=int/(oscss*(oscday-tan(oscday)))
    endif

  end subroutine ocean_alb


  ! Function to work out instantaneous outgoing radiation   *
  subroutine rad_out(radout,h,oscss,osccc)
    implicit none

    real oscss,osccc
    real czsol,h,rspec,rtot,radout
    real rdiff

    parameter(rdiff=0.06)

    ! Cosine of zenith angle
    czsol=oscss+(osccc*cos(h))
    if(czsol.gt.1.)then
       czsol=1.
    endif
    if(czsol.lt.0.)then
       czsol=0.
    endif

    ! specular reflectance according to Briegleb et al. (1986)
    rspec=(0.026/((czsol**1.7)+0.065))+ &
         & (0.15*(czsol-0.1)*(czsol-0.5)*(czsol-1.0))

    ! total reflectance
    rtot=rspec+rdiff

    ! Instantaneous outgoing radiation (but without the constants)
    radout=rtot*czsol

  end subroutine rad_out


  ! Routine to calculate radiative forcing for c-goldstein
  ! started 30/5/3 Neil R. Edwards
  ! loosely based on Peter Cox's SUNNY.f or see Peixoto and Oort 1992 p.99
  !
  ! nyear = no. dt per year
  ! osct  = angular time of year (0,..,2pi)
  ! oscsind = sin of declination
  ! oscsob = sin of obliquity
  ! osce = eccentricity
  ! oscgam = ??
  ! oscday = 0.5 * sunlit (angular) fraction of day, ie 1/2 length of day
  ! solfor = solar forcing = scaling factor * integral of cosine of solar
  !          elevation during daylight
  subroutine radfor (iistep,gn_daysperyear,solconst,flag_ents)
    implicit none

    real solconst

    integer istep, j
    real rpi, osce, oscsob, oscgam, tv, osce1, osce2, osce3, osce4
    real oscryr, osct, oscv, oscsolf, oscsind, oscss, osccc, osctt
    real oscday

    ! orbital variables
    integer iistep
    integer time_1,time_2
    real time_frac,osctau0,osctau1
    real gn_daysperyear

    real solavg(maxj)
    real alboavg(maxj)

    logical flag_ents

    osce=0.0167
    oscsob=0.397789
    oscgam=1.352631
    osctau0=-0.5

    if ((orbit_radfor.eq.'y').or.(orbit_radfor.eq.'Y')) then

       if (t_orbit.eq.2) then
          osce=orbitecc_vect(1)
          oscsob=orbitobl_vect(1)
          oscgam=orbitpre_vect(1)
          osctau0=orbittau_vect(1)
          print*,'orbitvars:',iistep
          print*,'orbitosce,oscsob:',osce,oscsob
          print*,'orbitoscgam,orbitosctau0',oscgam,osctau0
       endif

       if (t_orbit.eq.1) then
          time_1=int(iistep/real(orbitsteps))+1
          time_2=time_1+1
          time_frac=(mod(iistep,orbitsteps))/real(orbitsteps)
          if (time_2.le.norbit) then
             osce=(1-time_frac)*orbitecc_vect(time_1)+ &
                  & time_frac*orbitecc_vect(time_2)
             oscsob=(1-time_frac)*orbitobl_vect(time_1)+ &
                  & time_frac*orbitobl_vect(time_2)

             if (abs(orbitpre_vect(time_1)-orbitpre_vect(time_2)).gt.pi) then

                if (orbitpre_vect(time_1).gt.orbitpre_vect(time_2)) then
                   oscgam=mod((1-time_frac)*orbitpre_vect(time_1)+ &
                        & time_frac*(orbitpre_vect(time_2)+2*pi),2*pi)
                else
                   oscgam=mod((1-time_frac)*(orbitpre_vect(time_1)+2*pi)+ &
                        & time_frac*(orbitpre_vect(time_2)),2*pi)
                endif

             else
                oscgam=(1-time_frac)*orbitpre_vect(time_1)+ &
                     & time_frac*orbitpre_vect(time_2)
             endif

             if (abs(orbittau_vect(time_1)- &
                  & orbittau_vect(time_2)).gt.gn_daysperyear/2.0) then

                if (orbittau_vect(time_1).gt.orbittau_vect(time_2)) then
                   osctau0=mod((1-time_frac)*orbittau_vect(time_1)+ &
                        & time_frac*(orbittau_vect(time_2)+ &
                        & gn_daysperyear),gn_daysperyear)
                else
                   osctau0=mod((1-time_frac)* &
                        & (orbittau_vect(time_1)+gn_daysperyear)+ &
                        & time_frac*(orbittau_vect(time_2)),gn_daysperyear)
                endif


             else
                osctau0=(1-time_frac)*orbittau_vect(time_1)+ &
                     & time_frac*orbittau_vect(time_2)
             endif

          else
             if (time_frac.ne.0) print*,'Time out of bounds for orbit'
             osce=orbitecc_vect(norbit)
             oscsob=orbitobl_vect(norbit)
             oscgam=orbitpre_vect(norbit)
             osctau0=orbittau_vect(norbit)
          endif

          if (mod(iistep-1,10000).eq.0) then
             if (debug_loop) then
                print*,'orbitvars:',iistep,time_1,time_frac
                print*,'orbitosce,oscsob:',osce,oscsob
                print*,'orbitoscgam,orbitosctau0',oscgam,osctau0
             endif
          endif

       endif
    endif

    rpi = 1.0/pi

    tv = osce*osce
    osce1 = osce * (2.0 - 0.25*tv)
    osce2 = 1.25 * tv
    osce3 = osce*tv * 13./12.
    osce4 = ((1.0 + 0.5*tv)/(1.0 - tv))**2
    oscryr = 2.0*pi/float(nyear)

    osctau1 = osctau0 + 0.5

    do istep=1,nyear

       ! Dan's offset for angular time of year
       osct = (float(mod(istep-1,nyear)+1) - &
            & (nyear*osctau1/gn_daysperyear))*oscryr

       do j=1,jmax
          oscv = osct + osce1*sin(osct) + osce2*sin(2.0*osct) &
               & + osce3*sin(3.0*osct)
          oscsolf = osce4*(1.0 + osce*cos(oscv))**2
          oscsind = oscsob*sin(oscv-oscgam)

          oscss = oscsind * s(j)
          osccc = sqrt(1.0 - oscsind**2) * c(j)
          osctt = min(1.0,max(-1.0,oscss/osccc))

          oscday = acos(- osctt)

          solfor(j,istep) = solconst*oscsolf*rpi* &
               & (oscss*oscday + osccc*sin(oscday))
          if (flag_ents) then
             call ocean_alb(oscss,osccc,oscday,j,istep)
          endif
       enddo
    enddo

    if (dosc) then
    else

       ! replace variable forcing by its average
       do j=1,jmax
          solavg(j) = 0.
          do istep=1,nyear
             solavg(j) = solavg(j) + solfor(j,istep)
             if (flag_ents) then
                alboavg(j) = alboavg(j) + albo(j,istep)
             endif
          enddo
       enddo
       do j=1,jmax
          do istep=1,nyear
             solfor(j,istep) = solavg(j)/nyear
             if (flag_ents) then
                albo(j,istep) = alboavg(j)/nyear
             endif
          enddo
       enddo
    endif

  end subroutine radfor


  ! surflux
  !
  ! Notation; fx = heat flux, lh = latent, sh = sensible, sw = shortwave/solar,
  !           lw = long wave, o = ocean, a = atm., sic = sea ice
  !           thus eg fxsho = sensible heat flux into (???) ocean
  !           dthsic,dtareasic are rates of change (thermodynamic only) of
  !           sea-ice height and area
  !
  ! Note evap array passed to the ocean is the NET evaporation, NOT the
  ! evaporation from the ocean alone, hence renamed
  ! AY (10/12/03) : output fluxes now dimensional (ie SI)
  !
  ! PPH (01/09/04) : Partially removed use of Dland precompiler options.
  ! Under genie.F structure the Dland option is unsupported and you are
  ! strongly advised to not use it.  Instead set flag_land=.true. in the
  ! job script.
  !
  ! Notes for coupling; for IGCM pptn and atm state variables atemp, ashum
  ! should become unalterable input fields. Ouput heat flux to atm includes
  ! both surface fluxes like surface LW (same for IGCM) and interior fluxes
  ! like planetary OLWR (not needed for IGCM. Net fluxes are only diagnostic,
  ! however in componentised, GENIE version.
  !
  subroutine surflux(istep,otemp, osaln,atemp, ashum,sich, sica, &
       & tice, albice, stressxu_ocn,stressyu_ocn, &
       & stressxv_ocn,stressyv_ocn, albedo,fxlho,fxsho, fxswo,fxlwo, &
       & evap_ocn,pptn_ocn,runoff_ocn,runoff_land,fxlha,fxsha, &
       & fxswa,fxlwa,evap_atm,pptn_atm,dthsic,dtareasic, &
       & atmos_lowestlh_atm,go_solfor,go_fxsw,dum_n_atm,dum_sfcatm, &
       & eb_ca,gn_daysperyear,eb_fx0a,eb_fx0o,eb_fxsen,eb_fxlw, &
       & eb_evap,eb_pptn,eb_relh,eb_uv,eb_usurf,solconst, &
       & co2_out,ch4_out,n2o_out,surf_orog_atm,landice_slicemask_lic, &
       & albs_atm,land_albs_snow_lnd,land_albs_nosnow_lnd, &
       & land_snow_lnd,land_bcap_lnd,land_z0_lnd,land_temp_lnd, &
       & land_moisture_lnd,flag_ents,lowestlu2_atm,lowestlv3_atm)

    use genie_util, ONLY: check_unit, check_iostat
    implicit none

    integer istep

    real, dimension(imax,jmax) :: otemp,osaln,atemp,ashum,sich,sica,tice, &
         & albice,stressxu_ocn,stressyu_ocn,stressxv_ocn,stressyv_ocn, &
         & albedo,fxlho,fxsho,fxswo,fxlwo,evap_ocn,pptn_ocn, &
         & runoff_ocn,runoff_land,fxlha, fxsha,fxswa,fxlwa, &
         & evap_atm,pptn_atm,dthsic,dtareasic,co2_out,ch4_out,n2o_out

    REAL,INTENT(out),DIMENSION(imax,jmax) :: atmos_lowestlh_atm

    ! Dummy variables to be passed to/from BIOGEM via genie.F
    real go_solfor(jmax)
    real go_fxsw(imax,jmax)
    integer,intent(in)::dum_n_atm
    real,intent(in),dimension(dum_n_atm,imax,jmax)::dum_sfcatm

    real,intent(inout)::surf_orog_atm(imax,jmax)

    ! land ice sheet mask
    real,dimension(maxi,maxj),intent(out)::landice_slicemask_lic

    ! surface albedo
    real,dimension(maxi,maxj),intent(inout)::albs_atm
    real,dimension(maxi,maxj),intent(in)::land_albs_snow_lnd
    real,dimension(maxi,maxj),intent(in)::land_albs_nosnow_lnd
    ! land snow cover
    real,dimension(maxi,maxj),intent(inout)::land_snow_lnd
    ! bucket capacity
    real,dimension(maxi,maxj),intent(inout)::land_bcap_lnd
    ! roughness length
    real,dimension(maxi,maxj),intent(inout)::land_z0_lnd
    ! land temperature
    real,dimension(maxi,maxj),intent(inout)::land_temp_lnd
    ! land moisture content
    real,dimension(maxi,maxj),intent(inout)::land_moisture_lnd

    real orog_exact(imax,jmax)
    real lice_exact(imax,jmax)

    ! for interpolation between time series d18o input
    real d18o_exact
    real surf_orog_atm_previous(imax,jmax)

    ! for ice sheet melt runoff
    real ice_runoff(imax,jmax)

    ! Variables required for/from ENTS
    real beta,tolld
    real tldlast,dtld,dtld1
    real dfxsens,dfxlw,devap,dqsato
    real dum_talt,dum_hum,dum_pptn
    real qsalt
    real fxswsica
    real,intent(in)::eb_ca(imax,jmax)

    integer itld,devapcheck,itldmax
    parameter( itldmax = 21, tolld = 1e-3)

    real deltq,talt,lmd

    ! for orography
    real surf_tq2,surf_qsata

    real eb_fx0a(imax,jmax)
    real eb_fx0o(imax,jmax)
    real eb_fxsen(imax,jmax)
    real eb_fxlw(imax,jmax)
    real eb_evap(imax,jmax)
    real eb_pptn(imax,jmax)
    real eb_relh(imax,jmax)
    real eb_uv(2,imax,jmax)
    real eb_usurf(imax,jmax)

    ! Existing declarations

    real runoff(imax,jmax)
    real ce,ch,cesic,chsic,rq,tv0,tv,tv1,tv2,tv3,tol
    real albsic, fxswsic , fxlwsic, fxsensic , fx0oa, fx0sica
    real qsatsic, zeroc, alw, ticold, cfxsensic, salt, dho, dhsic
    real tieqn, dtieq

    real atm_latent, atm_sensible, atm_netsol, atm_netlong
    real atm_latenti, atm_sensiblei, atm_netsoli, atm_netlongi

    real gn_daysperyear
    real solconst

    parameter(zeroc = 273.15)

    integer i, j, iter, itice, istot, imth, ios

    real :: meantemp

    ! variables for surface flux output routine
    character ext*3

    ! edit to sea-ice convergence tolerance
    parameter(itice = 21)

    integer my_year,io
    real co2lev,solar,vol,aero
    character header*40
    !     radiative forcing terms
    real ch4_term,n2o_term

    ! for time series co2
    integer time_1,time_2
    real time_frac
    real co2_exact

    ! dimensionalised surflux timestep
    real dtsurfdim,rdtsurfdim

    ! ENTS functionality
    logical flag_ents

    ! zonal and meridional wind speed components
    real,dimension(maxi,maxj),intent(inout)::lowestlu2_atm,lowestlv3_atm

    talt  = 0.0
    lmd = 0.0
    surf_tq2 = 0.0
    surf_qsata = 0.0
    fxswsica = 0.0
    dum_talt = 0.0
    dum_hum = 0.0
    dum_pptn = 0.0
    beta = 0.0
    imth = 1

    ! dimensionalised surflux timestep
    dtsurfdim = dtatm*ndta*tsc
    rdtsurfdim = 1./dtsurfdim

    istot = istep

    ! If running on real*4, need to use a lower tolerance than usual
    tol = 1e-10

    ! initialise ice sheet meltwater
    ice_runoff(:,:)=0.0

    if (flag_ents) then
       ! istot defined currently as zero
       ! N.B. RESTARTS WILL NOT WORK!!!
       imth=mod(istep+0-1,nyear)+1

       if (t_orog.eq.1) then
          time_1=int(istot/real(orogsteps))+1
          time_2=time_1+1
          time_frac=(mod(istot,orogsteps))/real(orogsteps)
          if (time_2.le.norog) then
             orog_exact(:,:)=(1-time_frac)*orog_vect(:,:,time_1)+ &
                  & time_frac*orog_vect(:,:,time_2)
          else
             if (time_frac.ne.0) print*,'Time out of bounds for orog'
             orog_exact(:,:)=orog_vect(:,:,norog)
          endif
          if (mod(istot,10000).eq.0) then
             print*,'orog:',istot,time_1,time_frac, &
                  & sum(orog_exact)/size(orog_exact)
          endif
       endif

       if (t_lice.eq.1) then
          time_1=int(istot/real(licesteps))+1
          time_2=time_1+1
          time_frac=(mod(istot,licesteps))/real(licesteps)
          if (time_2.le.nlice) then
             lice_exact(:,:)=(1-time_frac)*lice_vect(:,:,time_1)+ &
                  & time_frac*lice_vect(:,:,time_2)
          else
             if (time_frac.ne.0) print*,'Time out of bounds for lice'
             lice_exact(:,:)=lice_vect(:,:,nlice)
          endif
          if (mod(istot,10000).eq.0) then
             print*,'lice:',istot,time_1,time_frac, &
                  & sum(lice_exact)/size(lice_exact)
          endif
       endif

       ! interpolate d18o for icesheets
       if (t_d18o.eq.1) then
          time_1=int(istot/real(d18osteps))+1
          time_2=time_1+1
          time_frac=(mod(istot,d18osteps))/real(d18osteps)
          if (time_2.le.nd18o) then
             d18o_exact=(1-time_frac)*d18o_vect(time_1)+ &
                  & time_frac*d18o_vect(time_2)
          else
             if (time_frac.ne.0) print*,'Time out of bounds for d18o'
             d18o_exact=d18o_vect(nlice)
          endif
          ! d18o orography and icemask.
          ! store orography at previous timestep to derive meltwater
          surf_orog_atm_previous(:,:)=surf_orog_atm(:,:)
          do i=1,imax
             do j=1,jmax
                if(d18o_ice_thresh(i,j).lt.1.0e-5) then
                   ! ocean
                   surf_orog_atm(i,j)=0.0
                   landice_slicemask_lic(i,j)=0.0
                else if (d18o_exact.gt.d18o_ice_thresh(i,j)) then
                   ! ice sheet
                   surf_orog_atm(i,j)=d18o_orog_min(i,j)+ &
                        & d18o_orog_grad(i,j)* &
                        & (d18o_exact-d18o_ice_thresh(i,j))/ &
                        & (d18o_exact-d18o_ice_thresh(i,j)+d18o_k)
                   landice_slicemask_lic(i,j)=2.0
                else
                   ! land (no ice)
                   surf_orog_atm(i,j)=d18o_orog_min(i,j)
                   landice_slicemask_lic(i,j)=1.0
                endif
             enddo
          enddo

          ! ice runoff
          do i=1,imax
             do j=1,jmax
                ice_runoff(iroff(i,j),jroff(i,j))= &
                     & ice_runoff(iroff(i,j),jroff(i,j))- &
                     & (surf_orog_atm(i,j)-surf_orog_atm_previous(i,j))* &
                     & (rhoice/1000.0)*rdtsurfdim* &
                     & (asurf(j)/asurf(jroff(i,j)))* &
                     & scale_mwfx
             enddo
          enddo

          if (mod(istot,10000).eq.0) then
             if (debug_loop) &
                  & print*, 'd18o:',istot,time_1,time_frac,d18o_exact
          endif
       endif

    endif

    ! Input field modifications

    ! This next section executes routines that calculate usurf from wind
    ! stresses.  If the winds are fixed, as they are with the EMBM, this
    ! only needs be run just prior to the first iteration.  If the winds
    ! are time-variant, as they are with the IGCM, this will need to be
    ! run every time-step.
    do j=1,jmax
       do i=1,imax
          dztau(1,i,j) = scf*stressxu_ocn(i,j)/(rh0sc*dsc*usc*fsc)/dzz
          dztau(2,i,j) = scf*stressyu_ocn(i,j)/(rh0sc*dsc*usc*fsc)/dzz
          dztav(1,i,j) = scf*stressxv_ocn(i,j)/(rh0sc*dsc*usc*fsc)/dzz
          dztav(2,i,j) = scf*stressyv_ocn(i,j)/(rh0sc*dsc*usc*fsc)/dzz
          tau(1,i,j) = dztau(1,i,j)*dzz
          tau(2,i,j) = dztav(2,i,j)*dzz

          ! Transfer modified coefficients from ENTS
          if (flag_ents) then
             ca(i,j) = real(eb_ca(i,j),kind(ca))
             ! don't use ents winds when winds are unified
             if (unify_winds .eq. 0) then
                lowestlu2_atm(i,j)=real(uatml(1,i,j,imth)*usc, &
                     & kind(lowestlu2_atm))
                lowestlv3_atm(i,j)=real(uatml(2,i,j,imth)*usc, &
                     & kind(lowestlv3_atm))
             endif
             if (unify_winds .eq. 0 .or. unify_winds .eq. 2 ) then
                usurf(i,j)=usurfl(i,j,imth)
             endif
          endif
       enddo
    enddo

    if (unify_winds .eq. 1 .or..not.flag_ents) then
       do j=1,jmax
          tv3 = 0.
          do i=1,imax
             if(i.eq.1) then
                tv = (tau(1,i,j)+tau(1,imax,j))/2
             else
                tv = (tau(1,i,j)+tau(1,i-1,j))/2
             endif
             if(j.eq.1) then
                tv2 = tau(2,i,j)/2
             else
                tv2 = (tau(2,i,j)+tau(2,i,j-1))/2
             endif
             ! use embm winds even with ents (unify_winds=1
             usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2)) &
                  & *rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
             tv3 = tv3 + usurf(i,j)
          enddo
          ! added wind_polar_avg that undoes the zonal average of usurf near
          ! poles when eq 2
          if (par_wind_polar_avg.ne.2) then
             do i=1,imax
                if(j.le.2.or.j.ge.jmax-1)usurf(i,j) = tv3/imax
             enddo
          endif
       enddo
       ! and now replace usufl with usurf for the rest of the calculations
       do i=1,imax
          do j=1,jmax
             usurfl(i,j,imth) = usurf(i,j)
          enddo
       enddo
    endif

    do j=1,jmax
       do i=1,imax
          atmos_lowestlh_atm(i,j) = REAL(z1_embm)
       enddo
    enddo

    ! Set up local atmospheric greenhouse gas arrays
    !
    !     Options:
    !     (1) Test for climate feedback with ATCHEM (BIOGEM) is selected
    !         NOTE: current atmospheric tracer assignment is:
    !               CO2 == #3
    !               CH4 == #10
    !               N2O == #14
    !         NOTE: catch situation where tracers are not selected in ATCHEM
    !               (=> gas concentrations will be zero)
    !     (2) Test for JDA forcings
    !     (3) Time series co2 input (from Dan Lunt's goldstein option)
    !     (4) Else, utilize original compound increase in CO2 (if specified)
    ! *** 1 ***
    if ((atchem_radfor.eq.'y').or.(atchem_radfor.eq.'Y')) then
       do j=1,jmax
          do i=1,imax
             if (dum_sfcatm(3,i,j).lt.(co20/1000.0)) then
                co2(i,j) = co20
             else
                co2(i,j) = dum_sfcatm(3,i,j)
             endif
             if (dum_sfcatm(10,i,j).lt.(ch40/1000.0)) then
                ch4(i,j) = ch40
             else
                ch4(i,j) = dum_sfcatm(10,i,j)
             endif
             if (dum_sfcatm(14,i,j).lt.(n2o0/1000.0)) then
                n2o(i,j) = n2o0
             else
                n2o(i,j) = dum_sfcatm(14,i,j)
             endif
          enddo
       enddo
       ! *** 2 ***
       ! JDA reading forcings (if appropriate)
    elseif (useforc) then
       if(mod(istep,nyear).eq.1)then
          my_year=istep/nyear+1
          co2lev=co2(1,1)*1.e6
          aero=0.
          vol=0.
          open(25,file=rstdir_name(1:lenrst)//forcname)
          read(25,*)header
          ! file exists, will read contents and use
          j=0
          do while (j.lt.my_year)
             read(25,*,iostat=io)i,co2lev,vol,aero,solar
             if(io.lt.0) then
                ! reached end of file
                print *,'Warning: reached end of forcing file'
                j=my_year
             endif
             j=j+1
          enddo
          ! now set forcings
          print *,i,co2lev,vol,aero,solar
          co2(:,:)=co2lev*1.E-6
          ! update global values
          co2_out(:,:)=co2(:,:)

          call radfor(0,real(nyear),real((solar-1368.)*solfac+1368.0 &
               & +(aero*aerofac+vol*volfac)*4./0.7),flag_ents)
          close(25)
       endif
       ! *** 3 ***
       ! time series input for co2
    elseif (t_co2.gt.0) then
       if (t_co2.eq.1) then
          time_1=int(istot/real(co2steps))+1
          time_2=time_1+1
          time_frac=(mod(istot,co2steps))/real(co2steps)
          if (time_2.le.nco2) then
             co2_exact=(1-time_frac)*co2_vect(time_1)+ &
                  & time_frac*co2_vect(time_2)
          else
             if (time_frac.ne.0) print*,'Time out of bounds for co2'
             co2_exact=co2_vect(nco2)
          endif
          if(mod(istot,10000).eq.0) then
             print*,'co2',istot,time_1,time_frac,co2_exact
          endif
          co2(:,:)=co2_exact
          ! update global values
          co2_out(:,:)=co2(:,:)
       endif
    else
       ! *** 4 ***
       ! original simple compound increase
       do j=1,jmax
          do i=1,imax
             co2(i,j) = (1.0 + rate_co2)*co2(i,j)
             ch4(i,j) = (1.0 + rate_ch4)*ch4(i,j)
             n2o(i,j) = (1.0 + rate_n2o)*n2o(i,j)
          enddo
       enddo
       ! update global values
       co2_out(:,:)=co2(:,:)
       ch4_out(:,:)=ch4(:,:)
       n2o_out(:,:)=n2o(:,:)
    endif


    ! SURFLUX model timestep

    ! initialize integrated runoff (and other arrays)
    do j=1,jmax
       do i=1,imax
          evap(i,j) = 0.
          runoff(i,j) = 0.
          runoff_land(i,j) = 0.
          fxlho(i,j) = 0.
          fxsho(i,j) = 0.
          fxswo(i,j) = 0.
          fxlwo(i,j) = 0.
          if (flag_ents) then
             pptn(i,j) = 0.
          endif
          pptn_ocn(i,j) = 0.
          runoff_ocn(i,j) = 0.
          fxlha(i,j) = 0.
          fxsha(i,j) = 0.
          fxswa(i,j) = 0.
          fxlwa(i,j) = 0.
          dthsic(i,j) = 0.
          dtareasic(i,j) = 0.
          if (.not.flag_ents) then
             albedo(i,j) = real(albcl(i,j))
          endif
          albice(i,j) = 0.
       enddo
    enddo

    ! make global mean temp for use in sensitivity adjustment
    meantemp=0.
    do j = 1,jmax
       do i = 1,imax
          meantemp=meantemp+atemp(i,j)
       enddo
    enddo
    meantemp=meantemp/real(jmax*imax)

    ! main i,j loop to compute surface flux terms
    ! note for IGCM ; the first section defines P, OLWR, rel hum. CO2, all
    ! irrelevant to IGCM
    do i=1,imax
       do j=1,jmax

          if (flag_ents) then
             ! Optional effect of altitude in ENTS.
             ! Air temp calculated at altitude using lapse
             ! rate. This air temp used in calc.s of SVP and
             ! longwave radiation.
             if(orogswitch.ge.1)then
                talt=atemp(i,j)+(lapse_rate*surf_orog_atm(i,j))
             else
                talt=atemp(i,j)
             endif
             lmd = lambdapptn
          endif
          ! pptn (over land and ocean)
          ! - need saturation vapour pressure, relative humidity

          ! no orography for precipitation (orography only affects surface
          ! processes if orogswitch=2)
          if((orogswitch.lt.2).and.(flag_ents)) then
             qsata(i,j) = const1*exp(const4*talt/(talt+const5))
          else
             qsata(i,j) = const1*exp(const4*atemp(i,j)/(atemp(i,j)+const5))
          endif

          if (flag_ents) then
             ! surface saturation humidity for evaporation (used if orogswitch=2)
             surf_qsata = const1*exp(const4*talt/(talt+const5))

             deltq = lmd*(ashum(i,j)-(rmax*qsata(i,j)))

             pptn(i,j) = max(0.0,deltq*rhoao*hatmbl(2)*rdtsurfdim)
          else
             pptn(i,j) = max(0.0,(ashum(i,j) - rmax*qsata(i,j))* &
                  & rhoao*hatmbl(2)*rdtdim)
          endif

          ! instantaneous precipitation
          ! calc relative humidity rq after P not before

          ! the next two lines cause problems for surflux as they alter
          ! atmosphere properties independently of embm.F and the fluxes (but
          ! for good reasons)
          if (flag_ents) then
             ashum(i,j) = min(ashum(i,j),real(ashum(i,j)-deltq))
          else
             ashum(i,j) = min(ashum(i,j),real(rmax*qsata(i,j)))
          endif
          tq1(2,i,j) = ashum(i,j)
          tq(2,i,j) = tq1(2,i,j)

          rq = ashum(i,j)/qsata(i,j)
          if (flag_ents) then
             eb_relh(i,j) = real(rq)

             ! surface specific humidity, derived at talt assuming vertically
             ! constant relh (orogswitch=2)
             surf_tq2=rq*surf_qsata
          endif

          ! use climatological albedo in calculating incoming shortwave radiation
          ! shortwave radiation NB modified later over ice
          if (flag_ents) then
             albedo(i,j) = atm_alb(i,j,imth)
          endif
          ! cgv2 seasonal
          if (flag_ents) then
             fxsw(i,j) = solfor(j,mod(istot-1,nyear)+1)*(1.- albedo(i,j))
          else
             fxsw(i,j) = solfor(j,mod(istot-1,nyear)+1)*(1.- albcl(i,j))
          endif
          if (flag_ents) then
             if (t_orog.eq.1) then
                ! store orography at previous timestep to derive meltwater
                surf_orog_atm_previous(i,j)=surf_orog_atm(i,j)
                surf_orog_atm(i,j) = orog_exact(i,j)
             endif
             if (t_lice.eq.1) then
                landice_slicemask_lic(i,j) = ceiling(lice_exact(i,j))
             else
                lice_exact(i,j) = landice_slicemask_lic(i,j)
             endif
          endif

          ! Outgoing planetary longwave

          !     NOTE: JDA added in olr_adj term (equivalent to olr_1 in holden
          !           (clim dyn) but with opposite sign
          !           pbh added in olr_adj0 term - globally uniform perturbation
          !           (reduction) to clear skies OLR

          ! calculate coefficients
          tv0 = b00 + rq*(b10 + b20*rq)
          tv1 = b01 + rq*(b11 + b21*rq)
          tv2 = b02 + rq*(b12 + b22*rq)
          tv3 = b03 + rq*(b13 + b23*rq)
          ! calculate CH4 and N2O contributions
          ! NOTE units are ppb for CH4 and N2O calculation
          ch4_term = &
               & alphach4*(sqrt(1e9*ch4(i,j)) - sqrt(1e9*ch40)) &
               & - ch4_func(1e9*ch4(i,j),1e9*n2o0) &
               & + ch4_func(1e9*ch40,1e9*n2o0)
          n2o_term = &
               & alphan2o*(sqrt(1e9*n2o(i,j)) - sqrt(1e9*n2o0)) &
               & - ch4_func(1e9*ch40,1e9*n2o(i,j)) &
               & + ch4_func(1e9*ch40,1e9*n2o0)
          ! calculate outgoign longwave
          ! talt dependence removed for olr (orography only affects surface
          ! proceses now if orogswitch=2)
          if((orogswitch.lt.2).and.(flag_ents)) then
             fxplw(i,j) = tv0 + talt*(tv1 &
                  & + talt*(tv2 &
                  & + talt*tv3)) &
                  & - delf2x*log(co2(i,j)/co20) &
                  & - ch4_term - n2o_term &
                  & + olr_adj*(meantemp-t_eqm) &
                  & - olr_adj0
          else
             fxplw(i,j) = tv0 + atemp(i,j)*(tv1 &
                  & + atemp(i,j)*(tv2 &
                  & + atemp(i,j)*tv3)) &
                  & - delf2x*log(co2(i,j)/co20) &
                  & - ch4_term - n2o_term &
                  & + olr_adj*(meantemp-t_eqm) &
                  & - olr_adj0
          endif

          ! Latent heat flux

          ! latent heat flux into atmos associated with condensation no account
          ! taken of snow melting, must assume all pptn is rain
          fxlata(i,j) = rho0*pptn(i,j)*hlv

          ! calculate terms over ocean or ice
          if(k1(i,j).le.kmax) then

             ! longwave radiation
             alw = atemp(i,j)+zeroc
             alw = alw * alw
             alw = alw * alw
             alw = ema * alw

             ! surface salinity-dependent freezing point:
             salt = saln0+osaln(i,j)
             tsfreez(i,j) = salt*(-0.0575 + 0.0017*sqrt(salt) - 0.0002*salt)

             ! maximum amount of heat available in first layer
             ! nre rsictscsf must be changed if dt>17.5 days, see gseta
             qb(i,j) = rsictscsf*(tsfreez(i,j)-otemp(i,j))
             qbsic(i,j) = qb(i,j)

             ! calculate terms over ice
             if(sica(i,j).gt.0.0)then
                ! * Sea-ice present *

                ! let albedo over sea ice vary as a function of tair (Holland et al. 1993)
                albsic = max(par_albsic_min, &
                     & min(par_albsic_max,0.40 - 0.04*atemp(i,j)))
                ! cgv2 seasonal
                ! nre note for IGCM needs changing
                if (flag_ents) then
                   fxswsic = solfor(j,mod(istot-1,nyear)+1) &
                        & *(1. - albsic)*(1.-albedo(i,j))

                   fxswsica = solfor(j,mod(istot-1,nyear)+1) &
                        & *(1.-albedo(i,j))
                else
                   fxswsic = solfor(j,mod(istot-1,nyear)+1) &
                        & *(1. - albsic)
                endif

                ! first need to calculate T_ice
                do iter=1,itice
                   ticold = tice(i,j)
                   ! Dalton number
                   cesic = 1.0e-3*(1.0022 - 0.0822*(atemp(i,j) &
                        & - ticold) + 0.0266*usurf(i,j))
                   cesic = max(6.0e-5,min(2.19e-3,cesic))

                   chsic = 0.94*cesic

                   ! sensible heat flux
                   cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                   qsatsic = const1*exp(const2*ticold/(ticold + const3))

                   evapsic(i,j) = max(0.0,(qsatsic - ashum(i,j)) &
                        & *rhoao*cesic*usurf(i,j))

                   tieqn = sich(i,j)*((1-ca(i,j))*fxswsic + alw &
                        & - emo*(ticold+zeroc)**4  - cfxsensic*(ticold &
                        & - atemp(i,j)) - rho0*hls*evapsic(i,j) ) &
                        & + consic*(tsfreez(i,j)-ticold)

                   dtieq = sich(i,j)*( &
                        & - 4.0*emo*(ticold+zeroc)**3 - cfxsensic &
                        & - hls*rhoair*cesic*usurf(i,j)*qsatsic*const2 &
                        & *const3/((ticold + const3)**2) &
                        & *0.5*(1.0 + sign(1.0,qsatsic - ashum(i,j))) ) &
                        & - consic

                   tice(i,j) = real(ticold - tieqn/dtieq)

                   if(abs(tice(i,j) - ticold).lt.tol .or. &
                        & ticold.gt.tfreez.and.tieqn.gt.0.0)goto 10
                enddo

                print*,'warning sea-ice iteration failed at',istot,i,j &
                     & ,tice(i,j),ticold,tice(i,j)-ticold,tieqn,dtieq
                print*,'cesic ',cesic,'cfxsensic ',cfxsensic,'qsatsic ',qsatsic &
                     & ,'evapsic ',evapsic(i,j),'tieqn ',tieqn,'dtieq ',dtieq
                print*,'sich ',sich(i,j),'sica ',sica(i,j)
                if (debug_loop) stop
10              tice(i,j) = min(real(tfreez),tice(i,j))

                ! recalc everything in case of resetting of tice
                fxlwsic = emo*(tice(i,j)+zeroc )**4 - alw

                cesic = 1.0e-3*(1.0022 - 0.0822*(atemp(i,j) &
                     & - tice(i,j)) + 0.0266*usurf(i,j))
                cesic = max(6.0e-5,min(2.19e-3,cesic))

                chsic = 0.94*cesic

                cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                fxsensic = cfxsensic*(tice(i,j) - atemp(i,j))

                qsatsic = const1*exp(const2*tice(i,j) /(tice(i,j) + const3))

                evapsic(i,j) = max(0.0,(qsatsic - ashum(i,j)) &
                     & *rhoao*cesic*usurf(i,j))

                fx0sic(i,j) = (1-ca(i,j))*fxswsic -fxsensic &
                     & - fxlwsic - rho0*hls*evapsic(i,j)

                if (flag_ents) then
                   fx0sica = ca(i,j)*fxswsica + fxlata(i,j) &
                        & + fxsensic + fxlwsic - fxplw(i,j)
                else
                   fx0sica = ca(i,j)*fxswsic + fxlata(i,j) &
                        & + fxsensic + fxlwsic - fxplw(i,j)
                endif
                if (flag_ents) then
                   fx0sica = ca(i,j)*fxswsica + fxlata(i,j) &
                        & + fxsensic + fxlwsic - fxplw(i,j)
                endif

                ! components of heat flux into atmosphere over sea-ice
                atm_latenti   = + fxlata(i,j)
                atm_sensiblei = + fxsensic
                if (flag_ents) then
                   atm_netsoli = + ca(i,j)*fxswsica
                else
                   atm_netsoli = + ca(i,j)*fxswsic
                endif
                atm_netlongi  = + fxlwsic - fxplw(i,j)

                dhsic = rrholf*(qb(i,j) - fx0sic(i,j)) - rhooi*evapsic(i,j)

                ! AR (19/12/12): assign a dummy value of qb(i,j) calculated to give
                !                no further sea-ice growth if existing thickness
                !                exceeds a given  threshold (par_sich_max)
                if (sich(i,j) >= par_sich_max) then
                   if (dhsic > 0.0) then
                      qbsic(i,j) = (0.0 + rhooi*evapsic(i,j))/rrholf &
                           & + fx0sic(i,j)
                      dhsic = rrholf*(qbsic(i,j) - fx0sic(i,j)) &
                           & - rhooi*evapsic(i,j)
                   endif
                endif

             else
                !              * Sea-ice absent *
                albsic        = 0.
                fx0sica       = 0.
                dhsic         = 0.
                evapsic(i,j)  = 0.
                tice(i,j)     = 0.
                atm_latenti   = 0.
                atm_sensiblei = 0.
                atm_netsoli   = 0.
                atm_netlongi  = 0.
             endif

             ! over open ocean
             fxlw(i,j) = emo*(otemp(i,j)+zeroc)**4 - alw

             ! Dalton number
             ce = 1.0e-3*(1.0022 - 0.0822*(atemp(i,j)- &
                  & otemp(i,j)) + 0.0266*usurf(i,j))
             ce = max(6.0e-5,min(2.19e-3,ce))
             ch = 0.94*ce

             ! sensible heat flux from ocean to atmosphere
             fxsen(i,j) = rhoair*ch*cpa*usurf(i,j)*(otemp(i,j)-atemp(i,j))

             ! evaporation/sublimation rate
             qsato(i,j) = const1*exp(const4*otemp(i,j) &
                  & /(otemp(i,j)+const5))
             evap(i,j) = max(0.0,(qsato(i,j) - ashum(i,j)) &
                  & *rhoao*ce*usurf(i,j))

             ! net heat flux into atmosphere
             fx0oa = ca(i,j)*fxsw(i,j) + fxlata(i,j) + fxsen(i,j) &
                  & + fxlw(i,j) - fxplw(i,j)

             ! set up fluxes -> atmosphere
             atm_latent   = + fxlata(i,j)
             atm_sensible = + fxsen(i,j)
             atm_netsol = + ca(i,j)*fxsw(i,j)
             atm_netlong  = + fxlw(i,j) - fxplw(i,j)

             ! add proportions over open ocean and sea ice
             fx0a(i,j) = (1-sica(i,j))*fx0oa + sica(i,j)*fx0sica

             ! add fluxes to include sea-ice and ocean components

             fxlha(i,j)   = real((sica(i,j)*atm_latenti) + &
                  & ((1-sica(i,j))*atm_latent))
             fxsha(i,j) = real((sica(i,j)*atm_sensiblei) + &
                  & ((1-sica(i,j))*atm_sensible))
             fxswa(i,j) = real((sica(i,j)*atm_netsoli) + &
                  & ((1-sica(i,j))*atm_netsol))
             fxlwa(i,j)  = real((sica(i,j)*atm_netlongi) + &
                  & ((1-sica(i,j))*atm_netlong))

             ! heat flux from atmosphere into open ocean
             if (flag_ents) then
                fx0o(i,j) = ((1-ca(i,j))*fxsw(i,j)*(1.-albo(j,imth))) &
                     & - fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j)
             else
                fx0o(i,j) = (1-ca(i,j))*fxsw(i,j) - fxsen(i,j) &
                     & - fxlw(i,j) - rho0*hlv*evap(i,j)
             endif

             ! net heat flux into ocean from atmosphere and sea ice
             ! including possible ice growth over open ocean
             fx0neto(i,j) = sica(i,j)*qbsic(i,j) &
                  & + (1-sica(i,j))*max(qb(i,j),fx0o(i,j))

             ! set up fluxes -> ocean
             fxlho(i,j) = real((1-sica(i,j))*( - rho0*hlv*evap(i,j) &
                  & + max(0.0,qb(i,j) - fx0o(i,j))) &
                  & + sica(i,j)*qbsic(i,j))
             fxsho(i,j) = - real((1-sica(i,j))*fxsen(i,j))
             if (flag_ents) then
                fxswo(i,j) = real((1-sica(i,j))*(1-ca(i,j))*fxsw(i,j) &
                     & *(1.-albo(j,imth)))
             else
                fxswo(i,j) = real((1-sica(i,j))*(1-ca(i,j))*fxsw(i,j))
             endif
             fxlwo(i,j)  = - real((1-sica(i,j))*fxlw(i,j))

             dho = max(0.0,rrholf*(qb(i,j) - fx0o(i,j)))

             dthsic(i,j) = real(sica(i,j)*dhsic + (1-sica(i,j))*dho)

             dtareasic(i,j) = real(max(0.0,rhmin*dho*(1-sica(i,j))))
             if(sich(i,j).gt.1e-12) then
                dtareasic(i,j) = real(dtareasic(i,j) &
                     & + min(0.0,0.5*sica(i,j)*sica(i,j) * dhsic/sich(i,j)))
             endif

             if (flag_ents) then
                ! Calculate planetary albedo over ocean boxes
                albs_atm(i,j)=(albo(j,imth) &
                     & - (sica(i,j)*(albo(j,imth)-albsic)))
                palb(i,j)=(1.-albedo(i,j))*(albo(j,imth) &
                     & - (sica(i,j)*(albo(j,imth)-albsic))) &
                     & + albedo(i,j)
             endif

             ! albedo array, purely diagnostic variable
             if (.not.flag_ents) then
                albedo(i,j)=real(sica(i,j)*albsic+(1-sica(i,j))*albcl(i,j))
             endif

             ! sea-ice albedo now output
             albice(i,j) = real(albsic)
          else
             ! calculate terms over land

             if (.not.flag_ents) then
                fx0a(i,j) = fxsw(i,j) + fxlata(i,j) - fxplw(i,j)
             endif

             ! set up fluxes -> atmosphere
             if (.not.flag_ents) then
                fxlha(i,j)   = + real(fxlata(i,j))
                fxsha(i,j) = + 0.
                fxswa(i,j) = + real(fxsw(i,j))
                fxlwa(i,j)  = - real(fxplw(i,j))
             endif

             ! runoff; add pptn to appropriate coastal grid cell
             if (.not.flag_ents) then
                if(igrid.ne.0)then
                   runoff(iroff(i,j),jroff(i,j)) = runoff(iroff(i,j) &
                        & ,jroff(i,j)) + pptn(i,j)*ds(j)*rds(jroff(i,j))
                else
                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j)) + pptn(i,j)
                endif
             endif

             if (.not.flag_ents) then
                runoff_land(i,j) = real(pptn(i,j))
             endif

             if (flag_ents) then

                ! ENTS radiation and hydrology

                ! Offline model: switch variables

                if(ents_offlineswitch.eq.1)then
                   dum_talt=talt
                   dum_hum=ashum(i,j)
                   dum_pptn=pptn(i,j)
                   ! Replace with offline data
                   if(orogswitch.eq.1)then
                      talt=tncep(i,j,imth)+(lapse_rate*surf_orog_atm(i,j))
                   else
                      talt=tncep(i,j,imth)
                   endif
                   qsalt=const1*exp(const4*talt/(talt+const5))
                   ashum(i,j)=rhncep(i,j,imth)*qsalt
                   pptn(i,j)=pncep(i,j,imth)
                endif

                ! Snow
                ! also update land surface albedo values according to changes here

                if(land_temp_lnd(i,j).lt.-5.and.talt.lt.-5 &
                     & .and.pptn(i,j).gt.0.)then
                   land_snow_lnd(i,j)=1
                   albs_atm(i,j)=land_albs_snow_lnd(i,j)
                endif

                ! Snow melt
                ! also update land surface albedo values according to changes here
                if(land_temp_lnd(i,j).ge.-5.and.talt.ge.-5 &
                     & .and.land_snow_lnd(i,j).eq.1)then
                   land_snow_lnd(i,j)=0
                   albs_atm(i,j)=land_albs_nosnow_lnd(i,j)
                endif

                ! transfer coefficients
                ! should this be moved after affects of slicemask calculated?
                chl(i,j)=1./(((1./0.41)*log(10./land_z0_lnd(i,j)))**2)
                cel(i,j)=chl(i,j)

                ! Albedo functionality
                if(abs(landice_slicemask_lic(i,j)-2.0).lt.1e-19) then
                   albs_atm(i,j)=albs_atm(i,j) &
                        & *(2-lice_exact(i,j)) + &
                        & 0.8*(lice_exact(i,j)-1)
                   land_z0_lnd(i,j)=land_z0_lnd(i,j)*(2-lice_exact(i,j))+ &
                        & 0.001*(lice_exact(i,j)-1)
                   land_bcap_lnd(i,j)=land_bcap_lnd(i,j) &
                        & *(2-lice_exact(i,j)) + &
                        & lice_k9*(lice_exact(i,j)-1)
                endif

                ! Newton-Raphson iteration loop to solve for eqm land
                ! temperature. Justified for 1 ocean timestep.
                do itld=1,itldmax

                   ! sensible heat flux from land to atmosphere

                   fxsen(i,j) = rhoair*chl(i,j)*cpa*usurfl(i,j,imth)* &
                        & (land_temp_lnd(i,j)-talt)

                   ! longwave radiation
                   if(ents_offlineswitch.eq.1)then
                      alw = tncep(i,j,imth)+zeroc
                   else
                      alw = tq1(1,i,j)+zeroc
                   endif
                   alw = alw * alw
                   alw = alw * alw
                   alw = ema * alw
                   ! Net longwave over land (positive upward)
                   fxlw(i,j) = eml*((land_temp_lnd(i,j)+zeroc)**4) - alw

                   ! Evaporation
                   if(land_temp_lnd(i,j).le.0.)then
                      qsato(i,j) = const1*exp(const2*land_temp_lnd(i,j) &
                           & /(land_temp_lnd(i,j)+const3))
                   else
                      qsato(i,j) = const1*exp(const4*land_temp_lnd(i,j) &
                           & /(land_temp_lnd(i,j)+const5))
                   endif

                   ! devapcheck used to find out if evap has a differential (0=N, 1=Y)
                   if(land_moisture_lnd(i,j).gt.0.)then
                      beta=min(1.,(land_moisture_lnd(i,j) &
                           & /land_bcap_lnd(i,j))**4)

                      ! evap calculated with surface humidity if orogswitch=2
                      if(orogswitch.lt.2) then
                         evap(i,j) = max(0.0,(qsato(i,j) - ashum(i,j)) &
                              & *rhoao*cel(i,j)*usurfl(i,j,imth)*beta)
                      else
                         evap(i,j) = max(0.0,(qsato(i,j) - surf_tq2) &
                              & *rhoao*cel(i,j)*usurfl(i,j,imth)*beta)
                      endif

                      devapcheck=1
                   else
                      evap(i,j)=0.
                      devapcheck=0
                   endif

                   if(evap(i,j)*dtsurfdim.gt.land_moisture_lnd(i,j))then
                      evap(i,j)=land_moisture_lnd(i,j)*rdtsurfdim
                      devapcheck=0
                   endif

                   ! ODE for land temp wrt to time
                   dtld = rhcld*((1-ca(i,j))*fxsw(i,j) &
                        & *(1-albs_atm(i,j)) &
                        & - fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j))
                   ! Sensible heat derivative
                   dfxsens=rhoair*chl(i,j)*cpa*usurfl(i,j,imth)

                   ! net longwave derivative
                   dfxlw=4.*eml*((land_temp_lnd(i,j)+zeroc)**3)

                   ! evap derivative
                   if(devapcheck.eq.0)then
                      devap=0.
                   else
                      if(land_temp_lnd(i,j).le.0.)then
                         dqsato = ((const1*const2*const3)/ &
                              & (land_temp_lnd(i,j)+const3)**2) &
                              & *exp(const2*land_temp_lnd(i,j) &
                              & /(land_temp_lnd(i,j)+const3))
                      else
                         dqsato = ((const1*const4*const5)/ &
                              & (land_temp_lnd(i,j)+const5)**2) &
                              & *exp(const4*land_temp_lnd(i,j) &
                              & /(land_temp_lnd(i,j)+const5))
                      endif

                      ! Calculate evaporation diff. (bare soil) or evapotranspiration
                      ! diff. (veg) depending on whether carbon feedbacks on climate chosen
                      ! (carbonswitch)
                      devap=rhoao*cel(i,j)*usurfl(i,j,imth)*beta*dqsato
                   endif

                   ! total derivative
                   dtld1 = -rhcld*(dfxsens+dfxlw+(rho0*hlv*devap))

                   ! update last value
                   tldlast=land_temp_lnd(i,j)

                   ! calculate new value of tqld(1,i,j)
                   land_temp_lnd(i,j)=land_temp_lnd(i,j)-(dtld/dtld1)

                   if(abs(tldlast-land_temp_lnd(i,j)).lt.tolld) exit

                   if(itld.eq.itldmax)then

                      print*,'Land rad. calc. did not converge to specified tol'
                      print*,'at point',i,j
                      print*,'final value was ',land_temp_lnd(i,j)
                      print*,'last iteration value was ',tldlast
                      print*,'istep ',istep
                      print*,' '


                   endif

                enddo

                ! Radiation fluxes
                fx0a(i,j) = ca(i,j)*fxsw(i,j) + fxlata(i,j) + &
                     & fxlw(i,j) + fxsen(i,j) - fxplw(i,j)
                fx0o(i,j) = (1-ca(i,j))*fxsw(i,j)*(1-albs_atm(i,j)) - &
                     & fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j)

                ! Bucket land hydrology: update bucket sizes
                land_moisture_lnd(i,j)=land_moisture_lnd(i,j) &
                     & +((pptn(i,j)-evap(i,j))*dtsurfdim)

                ! runoff scheme: in the case of land water bucket gt bucket capacity, bcap,
                ! find nearest ocean
                ! gridbox/gridboxes and add the surplus as runoff there
                if(land_moisture_lnd(i,j).gt.land_bcap_lnd(i,j)) then
                   if (igrid.ne.0) then
                      runoff(iroff(i,j),jroff(i,j)) = runoff(iroff(i,j) &
                           & ,jroff(i,j)) +((land_moisture_lnd(i,j) &
                           & -land_bcap_lnd(i,j))*rdtsurfdim)*ds(j) &
                           & *rds(jroff(i,j))
                      ! need the excess surface water skimmed off anyway before trying other schemes
                      if (par_runoff_scheme.gt.0) then
                         runoff_land(i,j) = runoff_land(i,j)+ &
                              & ((land_moisture_lnd(i,j)-land_bcap_lnd(i,j))* &
                              & rdtsurfdim)*ds(j) *rds(jroff(i,j))
                      endif
                   else
                      runoff(iroff(i,j),jroff(i,j)) = runoff(iroff(i,j) &
                           & ,jroff(i,j)) +((land_moisture_lnd(i,j) &
                           & -land_bcap_lnd(i,j))*rdtsurfdim)
                      ! need the excess surface water skimmed off anyway before trying other schemes
                      if (par_runoff_scheme.gt.0) then
                         runoff_land(i,j) = runoff_land(i,j)+ &
                              & ((land_moisture_lnd(i,j)-land_bcap_lnd(i,j))* &
                              & rdtsurfdim)
                      endif
                   endif
                   land_moisture_lnd(i,j)=land_bcap_lnd(i,j)
                endif

                ! Meissner et al (2003) leaky bucket scheme.
                ! 6.63 = b = Clapp-Hornberger exponent (=4.50 for coarse grain soil
                ! and 11.20 for fien grain)
                ! 0.0047 kg/m2/s = K_s = 0.0047 mm s-1 = 0.0000047 m s-1 =
                ! Saturated hydraulic conductivity
                ! Y = runoff; M = moisture; M_sat = saturated soil moisture
                ! Y = K_s(M/M_Sat)^(2b+3) = 0.0047*(M/M_Sat)^15.2 [2b+3 = {12.00,
                ! 16.26, 25.40} = runoff_factor_1]
                ! kg m-2 is mm for rainfall, which rokgem uses.
                ! need to divide rather than multiply by rdtdim (= 1/ocean timestep in
                ! sec = 3.1688087814028956E-006 s-1),
                ! or just leave out altogether(!) as have a s-1 in K_s? Yes, am
                ! multiplying by a factor of 1000 below (m2mm) so all
                ! in all getting the right magnitude for the wrong reason (should be
                ! ~300x more!)
                ! could adjust K_s, or even have a 2d field for it...
                ! /(11.0-min(10,istep)) is used to ramp up runoff to full steam so as
                ! to avoid crashing the model
                if (par_runoff_scheme.eq.2) then
                   runoff_land(i,j)=runoff_land(i,j)+ &
                        & ((0.0000047/(11.0-min(10,istep)))* &
                        & (land_moisture_lnd(i,j)/land_bcap_lnd(i,j))**runoff_factor_1)

                   ! balance water budget
                   land_moisture_lnd(i,j)=land_moisture_lnd(i,j)- &
                        & runoff_land(i,j)*dtsurfdim/(11.0-min(10,istep))

                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j))+runoff_land(i,j)
                endif

                ! Try Phil Holden's idea of soil moiture half-life (residence time) of
                ! 1-2 months (call it 1.5) from
                ! Pidwirny, M. (2006). "The Hydrologic Cycle". Fundamentals of
                ! Physical Geography, 2nd Edition. Date
                ! Viewed. http://www.physicalgeography.net/fundamentals/8b.html
                ! residence time = tau = 1.5 months = 1/8 year = 3944700sec. dtdim =
                ! timestep = 315576sec
                ! runoff_factor_2 = [1-exp(-tln2/tau)] =0.054 with 100 tsteps/year
                if (par_runoff_scheme.eq.3) then
                   runoff_land(i,j)=runoff_land(i,j)+ &
                        & land_moisture_lnd(i,j)* &
                        & runoff_factor_2*rdtsurfdim

                   ! balance water budget
                   land_moisture_lnd(i,j)=land_moisture_lnd(i,j)-runoff_land(i,j)*dtsurfdim

                   runoff(iroff(i,j),jroff(i,j)) = &
                        & runoff(iroff(i,j),jroff(i,j))+runoff_land(i,j)
                endif

                if (par_runoff_scheme.eq.0) then
                   runoff_land(i,j) = real(pptn(i,j))
                endif

                ! Calculate planetary albedo over land
                palb(i,j)=((1.-albedo(i,j))*albs_atm(i,j))+albedo(i,j)

                ! add fluxes to include sea-ice and ocean components
                fxlha(i,j) = real(fxlata(i,j))
                fxsha(i,j) = real(fxsen(i,j))
                fxswa(i,j) = real((ca(i,j)*fxsw(i,j)))
                fxlwa(i,j) = real((fxlw(i,j)-fxplw(i,j)))

                ! Offline model: switch variables back
                if(ents_offlineswitch.eq.1)then
                   talt=dum_talt
                   ashum(i,j)=dum_hum
                   pptn(i,j)=dum_pptn
                endif

                ! Update global heat souce over land
                ghs = ghs + fx0a(i,j) + fx0o(i,j)

             endif
          endif
       enddo
    enddo

    ! add in effects of runoff (non-local in i,j) and set up fluxes
    ! freshwater forcing in m/s open ocean P-E over ocean gridboxes
    ! evap zero over land,
    ! nre P goes straight through as no snow allowed, but E is E total
    ! for atm model and solely E_ocean for the fwflux
    do j=1,jmax
       do i=1,imax
          if(k1(i,j).le.kmax) then

             ! wet points;
             pptn_ocn(i,j) = real(pptn(i,j))
             runoff_ocn(i,j) = real(runoff(i,j))+ real(ice_runoff(i,j))
             evap_atm(i,j) = real(evap(i,j)*(1-sica(i,j)) + &
                  & evapsic(i,j)*sica(i,j))

             ! note that the FW anomaly normally applied to ocean FW fluxes has an
             ! odd status in a split-apart model.  As present it is added here via
             ! the precipitation term.  It may make sense to add it differently or
             ! not at all in the future.
             pptn_ocn(i,j) = real(pptn_ocn(i,j) + pmeadj(i,j))
          else
             ! set "ocean" fluxes to zero over land
             if (flag_ents) then
                pptn_ocn(i,j) = real(pptn(i,j))
                runoff_ocn(i,j) = real(runoff(i,j))
                evap_atm(i,j) = real(evap(i,j))
             else
                pptn_ocn(i,j) = 0.
                runoff_ocn(i,j) = 0.
                evap_atm(i,j) = 0.
             endif
          endif

          ! All cells (no evaporation over land - in this scheme)
          pptn_atm(i,j) = real(pptn(i,j))
          evap_ocn(i,j) = -evap_atm(i,j)
          pptn_ocn(i,j) = real(pptn_ocn(i,j) * m2mm)
          evap_ocn(i,j) = real(evap_ocn(i,j) * m2mm)
          runoff_ocn(i,j) = real(runoff_ocn(i,j) * m2mm)
          runoff_land(i,j) = real(runoff_land(i,j) * m2mm)
          pptn_atm(i,j) = real(pptn_atm(i,j) * m2mm)
          evap_atm(i,j) = real(evap_atm(i,j) * m2mm)
       enddo
    enddo


    ! call routine to output surface flux fields (in the same way as a
    ! restart file)
    if(mod(istep,iwstp).eq.0)then
       ext=conv_num(mod(iw,10))
       if (debug_loop) print*, &
            & 'Writing SURFLUX output file at time',istep
       call check_unit(2,__LINE__,__FILE__)
       open(2,file=outdir_name(1:lenout)//lout//'.sfx.'//ext,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       rewind 2
       call outm_surf(2,co2,albedo,usurf,fxlho,fxsho,fxswo,fxlwo, &
            & evap_atm,pptn_ocn,runoff_ocn,fxlha,fxsha,fxswa,fxlwa, &
            & evap_atm,pptn_atm,dthsic,dtareasic)
       close(2,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       if (debug_loop) print*
    endif

    ! Set values of dummy variables destined for BIOGEM & ENTS
    ! NOTE: convert precision between GOLDSTEIn (real*8) and GENIE
    !       (which can be real*4 or real* depending on the IGCM)
    do i=1,imax
       do j=1,jmax
          go_fxsw(i,j) = real(fxsw(i,j))
          eb_fx0a(i,j)  = real(fx0a(i,j))
          eb_fx0o(i,j)  = real(fx0o(i,j))
          eb_fxsen(i,j) = real(fxsen(i,j))
          eb_fxlw(i,j)  = real(fxlw(i,j))
          eb_evap(i,j)  = real(evap(i,j))
          eb_pptn(i,j)  = real(pptn(i,j))
          eb_uv(1,i,j)  = usc*uatm(1,i,j)
          eb_uv(2,i,j)  = usc*uatm(2,i,j)
          eb_usurf(i,j) = usurf(i,j)
       enddo
    enddo
    do j=1,jmax
       go_solfor(j) = real(solfor(j,mod(istot-1,nyear)+1))
    enddo

    ! annual call to radfor if transient orbit is applied
    if(t_orbit.eq.1) then
       if(mod(istep-1,nyear).eq.0) then
          call radfor(istep+0,real(gn_daysperyear),solconst,flag_ents)
       endif
    endif

  end subroutine surflux


  FUNCTION conv_num(i)
    IMPLICIT NONE
    CHARACTER(LEN=3) :: conv_num
    INTEGER :: i

    INTEGER :: i1, i2, itemp, i3
    CHARACTER(LEN=1) :: a, b, c
    IF (i < 10) THEN
       a = CHAR(i + 48)
       conv_num = a // '  '
    ELSE IF (i < 100) THEN
       i1 = i / 10
       i2 = i - i1 * 10
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       conv_num = a // b // ' '
    ELSE
       i1 = i / 100
       itemp = i - 100 * i1
       i2 = itemp / 10
       i3 = itemp - 10 * i2
       a = CHAR(i1 + 48)
       b = CHAR(i2 + 48)
       c = CHAR(i3 + 48)
       conv_num = a // b // c
    END IF
  END FUNCTION conv_num


  ! see: IPCC [1990, 2001]
  FUNCTION ch4_func(ch4, n2o)
    IMPLICIT NONE
    REAL :: ch4_func
    REAL :: ch4, n2o
    ch4_func = 0.47 * LOG(1.0 + 2.01E-5 * (ch4 * n2o)**0.75 + &
         & 5.31E-15 * ch4 * (ch4 * n2o)**1.52)
  END FUNCTION ch4_func


  ! Define runoff matrix for EMBM
  !
  ! The dry points in the k1 file now define a compass direction
  ! for the runoff from that point/cell. By following this direction
  ! to the sea (where k1.le.kmax) we build up the
  ! matrix (iroff(i,j),jroff(i,j))
  ! which defines where to put the runoff from point (i,j)
  subroutine readroff
    implicit none

    integer i, j, loop, iroe, iros, irow, iron

    parameter (iroe=91, iros=92, irow=93, iron=94)

    if (debug_init) print*,'Calculating runoff routing'

    do j=1,jmax
       do i=1,imax
          iroff(i,j) = i
          jroff(i,j) = j
          loop = 0
          do while(k1(iroff(i,j),jroff(i,j)).gt.kmax)
             if(k1(iroff(i,j),jroff(i,j)).eq.iroe)then
                iroff(i,j) = iroff(i,j) + 1
             else if(k1(iroff(i,j),jroff(i,j)).eq.iros)then
                jroff(i,j) = jroff(i,j) - 1
             else if(k1(iroff(i,j),jroff(i,j)).eq.irow)then
                iroff(i,j) = iroff(i,j) - 1
             else if(k1(iroff(i,j),jroff(i,j)).eq.iron)then
                jroff(i,j) = jroff(i,j) + 1
             endif
             ! periodic b.c.
             if(iroff(i,j).eq.imax+1)then
                iroff(i,j) = 1
             elseif(iroff(i,j).eq.0)then
                iroff(i,j) = imax
             endif
             ! avoid inf. loops
             loop = loop + 1
             if(loop.gt.100000)then
                print*,'There is a problem calculating runoff'
                print*,'Located at k1(',i,',',j,')'
                print*,'k1(',i,',',j,') = ',k1(i,j)
                print*,'iroff(i,j) = ',iroff(i,j)
                print*,'jroff(i,j) = ',jroff(i,j)
                print*,'k1(iroff(i,j),jroff(i,j)) = ', &
                     & k1(iroff(i,j),jroff(i,j)),' (kmax = ',kmax,')'
                stop 'problem calculating runoff'
             endif
          enddo
       enddo
    enddo

    if (debug_init) print*,'Runoff routing successfully calculated'
  end subroutine readroff

END MODULE embm

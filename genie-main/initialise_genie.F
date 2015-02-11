      subroutine initialise_genie()
c
      use genie_control
      use genie_global
      use genie_util, ONLY: check_unit, check_iostat, message, die
c
      implicit none
c
c     no args
c
c     locals
c
      integer ios
      integer :: unitNum = 8
c
c     Define members of the top-level control namelist
c
      namelist/genie_control_nml/koverall_total,
     :      katm_loop,
     :      ksic_loop,
     :      kocn_loop,
     :      klnd_loop,
     :      conv_kocn_katchem,
     :      conv_kocn_kbiogem,
     :      conv_kocn_ksedgem,
     :      conv_kocn_krokgem,
     :      kgemlite,
     :      flag_ebatmos,flag_igcmatmos,
     :      flag_fixedocean,flag_slabocean,flag_goldsteinocean,
     :      flag_fixedseaice,flag_slabseaice,flag_goldsteinseaice,
     :      flag_icesheet,flag_fixedatmos,flag_fakeatmos,
     :      flag_land,flag_ents,flag_fixedland,
     :      flag_fixedicesheet,
     :      flag_fixedchem,
     :      flag_biogem,
     :      flag_atchem,
     :      flag_sedgem,
     :      flag_rokgem,
     :      flag_ichem,
     :      flag_wind,
     :      flag_gemlite,
     :      write_flag_atm,write_flag_ocn,write_flag_sic,
     :      outputdir_name,dt_write,
     :      fname_restart_main,
     :      fname_fluxrestart,
     :      flag_checkfluxes_sic,
     :      flag_checkfluxes_ocn,
     :      flag_checkfluxes_surf,
     :      flag_checkfluxes_atlantic,
     :      genie_timestep,
     :      lrestart_genie,
     :      flag_glim_t2m_force,
     :      flag_glim_pforce,
     :      genie_solar_constant,
     :      fname_topo,
     :      verbosity,
     :      debug_init,debug_end,debug_loop,
     :      gem_yr,gem_yr_min,gem_yr_max,
     :      gem_notyr,gem_notyr_min,gem_notyr_max,
     :      gem_dyr,
     :      gem_adapt_auto,gem_adapt_dpCO2dt,gem_adapt_DpCO2,
     :      gem_adapt_auto_unlimitedGEM,
     :      gem_adapt_diag_biogem_full
c
c     assign default values to members of the namelist
c
      koverall_total=0
      conv_kocn_katchem=-1
      conv_kocn_ksedgem=-1
      conv_kocn_kbiogem=-1
      conv_kocn_krokgem=-1
      kgemlite=1
c
      flag_igcmatmos=.FALSE.
      flag_ebatmos=.FALSE.
      flag_fixedatmos=.FALSE.
      flag_fakeatmos=.FALSE.
c
      flag_fixedocean=.FALSE.
      flag_slabocean=.FALSE.
      flag_goldsteinocean=.TRUE.
c
      flag_fixedseaice=.FALSE.
      flag_slabseaice=.FALSE.
      flag_goldsteinseaice=.TRUE.
c
      flag_icesheet=.FALSE.
      flag_fixedicesheet=.TRUE.
c
      flag_land=.FALSE.
      flag_ents=.FALSE.
      flag_fixedland=.FALSE.
c
      flag_fixedchem=.FALSE.
      flag_biogem=.false.
      flag_atchem=.false.
      flag_sedgem=.false.
      flag_rokgem=.false.
      flag_gemlite=.false.
c
      write_flag_atm=.true.
      write_flag_ocn=.true.
      write_flag_sic=.true.
      outputdir_name='~/genie_output/main'
      fname_restart_main='xxx'
      fname_fluxrestart='xxx'
      dt_write=1
c
      flag_checkfluxes_sic=.false.
      flag_checkfluxes_ocn=.false.
      flag_checkfluxes_surf=.false.
      flag_checkfluxes_atlantic=.false.
c
      genie_timestep = 3600.0
      genie_clock    = 0
c
      lrestart_genie=.false.
c
      genie_solar_constant=1368.0
c
      verbosity = 0
c
      debug_init = 1
c
      fname_topo='worbe2'
c
c open the file containing the namelist
c
      call check_unit(unitNum,__LINE__,__FILE__)
      open(unit=unitNum,file='data_genie',status='old',iostat=ios)
      if (ios /= 0) then
         call die("could not open GENIE control namelist file",
     &  __LINE__, __FILE__)
      end if
c
c read in namelist
c
      read(UNIT=unitNum,NML=genie_control_nml,IOSTAT=ios)
      if (ios /= 0) then
         call die('could not read GENIE control namelist',
     & __LINE__, __FILE__)
      else
         close(unitNum,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
      end if
c
      if (debug_init > 1)
     & call message("read values for genie_control_nml:",3)
      if(verbosity.ge.3) then
         write(UNIT=6,NML=genie_control_nml)
      end if
c
      if (debug_init > 1)
     & call message("read namelist: genie_control_nml",1)
c
      return
      end

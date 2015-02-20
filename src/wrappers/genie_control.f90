!--------------------------------------------------------
!> Variables which control the overall GENIE configuration.
!!
!> This Fortran90 module contains variables which pertain
!> to the overall GENIE configuration.  For example, the
!> various grid sizes and LOGICAL flags specifying which
!> components are used.
!--------------------------------------------------------

MODULE genie_control

  ! ======================================================================
  ! Assigning grid sizes (add your own [commented] entries below)

  ! For IGCM
#ifndef GENIENL
#define GENIENL 7
#endif
#ifndef GENIENX
#define GENIENX 64
#endif
#ifndef GENIENY
#define GENIENY 32
#endif

  ! For GOLDSTEIN ocean
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

  ! For sediments
#ifndef SEDGEMNLONS
#define SEDGEMNLONS 36
#endif
#ifndef SEDGEMNLATS
#define SEDGEMNLATS 36
#endif

  ! For weathering
#ifndef ROKGEMNLONS
#define ROKGEMNLONS 36
#endif
#ifndef ROKGEMNLATS
#define ROKGEMNLATS 36
#endif


  ! ======================================================================
  ! Grid dimension variables for tracer (1), u (2) and v (3) points
  ! Atmosphere
  INTEGER, PARAMETER :: ilon1_atm=GENIENX, ilat1_atm=GENIENY
  INTEGER, PARAMETER :: ilon2_atm=GENIENX, ilat2_atm=GENIENY
  INTEGER, PARAMETER :: ilon3_atm=GENIENX, ilat3_atm=GENIENY
  INTEGER, PARAMETER :: inl1_atm=GENIENL

  ! Ocean
  INTEGER, PARAMETER :: ilon1_ocn=GOLDSTEINNLONS, ilat1_ocn=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon2_ocn=GOLDSTEINNLONS, ilat2_ocn=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon3_ocn=GOLDSTEINNLONS, ilat3_ocn=GOLDSTEINNLATS
  INTEGER, PARAMETER :: inl1_ocn=GOLDSTEINNLEVS, inl2_ocn=inl1_ocn+1
  INTEGER, PARAMETER :: intrac_ocn=GOLDSTEINNTRACS

  ! For c-GOLDSTEIN sea-ice
  INTEGER, PARAMETER :: ilon1_sic=GOLDSTEINNLONS, ilat1_sic=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon2_sic=GOLDSTEINNLONS, ilat2_sic=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon3_sic=GOLDSTEINNLONS, ilat3_sic=GOLDSTEINNLATS

  ! For ice sheets in EMBM and ENTS
  INTEGER, PARAMETER :: ilon1_lic=GOLDSTEINNLONS, ilat1_lic=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon2_lic=GOLDSTEINNLONS, ilat2_lic=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon3_lic=GOLDSTEINNLONS, ilat3_lic=GOLDSTEINNLATS

  ! For ENTS
  INTEGER, PARAMETER :: ilon1_lnd=GOLDSTEINNLONS, ilat1_lnd=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon2_lnd=GOLDSTEINNLONS, ilat2_lnd=GOLDSTEINNLATS
  INTEGER, PARAMETER :: ilon3_lnd=GOLDSTEINNLONS, ilat3_lnd=GOLDSTEINNLATS

  ! For sediments
  INTEGER, PARAMETER :: ilon1_sed=SEDGEMNLONS, ilat1_sed=SEDGEMNLATS

  ! For weathering
  INTEGER, PARAMETER :: ilon1_rok=ROKGEMNLONS, ilat1_rok=ROKGEMNLATS

  ! ======================================================================
  ! Miscellaneous control variables

  ! Used for NetCDF output in various places
  INTEGER, PARAMETER :: BUFSIZ = 1024  !< to hold CHARACTER strings
  INTEGER, PARAMETER :: nfiles=4, nmaxdims=4, nall=100

  ! Biogeochemistry time stepping ratios -- loop modifiers (relative
  ! to ocean loop)
  INTEGER(KIND=8) :: conv_kocn_katchem  !< atchem
  INTEGER(KIND=8) :: conv_kocn_ksedgem  !< sedgem
  INTEGER(KIND=8) :: conv_kocn_kbiogem  !< biogem
  INTEGER(KIND=8) :: conv_kocn_krokgem  !< rokgem
  INTEGER :: kgemlite

  ! Total defined tracer numbers
  ! WARNING: parameter information duplicated in gem_cmn.f90
  INTEGER, PARAMETER :: intrac_atm_max=19, intrac_ocn_max=95, intrac_sed_max=79

  ! Others
  INTEGER(KIND=8) :: koverall_total
  INTEGER(KIND=8) :: katm_loop, ksic_loop, kocn_loop, klnd_loop

  ! .TRUE. indicates that component is included in model 'recipe'
  LOGICAL :: flag_ebatmos          !< EMBM
  LOGICAL :: flag_igcmatmos        !< IGCM
  LOGICAL :: flag_fixedocean       !< prescribed ocean
  LOGICAL :: flag_slabocean        !< slab ocean
  LOGICAL :: flag_goldsteinocean   !< GOLDSTEIN ocean

  LOGICAL :: flag_fixedseaice, flag_slabseaice, flag_goldsteinseaice
  LOGICAL :: flag_icesheet, flag_fixedicesheet
  LOGICAL :: flag_fixedatmos, flag_fakeatmos
  LOGICAL :: flag_land, flag_ents, flag_fixedland
  LOGICAL :: flag_fixedchem, flag_atchem
  LOGICAL :: flag_biogem, flag_sedgem, flag_rokgem, flag_gemlite
  LOGICAL :: flag_checkfluxes_sic, flag_checkfluxes_ocn, flag_checkfluxes_surf
  LOGICAL :: flag_checkfluxes_atlantic, flag_ichem, flag_wind
  INTEGER :: debug_init, debug_end, debug_loop

  LOGICAL :: write_flag_atm, write_flag_ocn, write_flag_sic
  CHARACTER(LEN=BUFSIZ) :: outputdir_name, fname_restart_main, fname_fluxrestart
  CHARACTER(LEN=6) :: fname_topo
  INTEGER(KIND=8) :: dt_write

  ! Days per year.
  REAL, PARAMETER :: global_daysperyear = 365.25

END MODULE genie_control

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
  ! Grid dimension variables for tracer (1), u (2) and v (3) points
  ! Atmosphere
  INTEGER :: ilon1_atm, ilat1_atm
  INTEGER :: ilon2_atm, ilat2_atm
  INTEGER :: ilon3_atm, ilat3_atm
  INTEGER :: inl1_atm

  ! Ocean
  INTEGER :: ilon1_ocn, ilat1_ocn
  INTEGER :: ilon2_ocn, ilat2_ocn
  INTEGER :: ilon3_ocn, ilat3_ocn
  INTEGER :: inl1_ocn, inl2_ocn
  INTEGER :: intrac_ocn

  ! For c-GOLDSTEIN sea-ice
  INTEGER :: ilon1_sic, ilat1_sic
  INTEGER :: ilon2_sic, ilat2_sic
  INTEGER :: ilon3_sic, ilat3_sic

  ! For ice sheets in EMBM and ENTS
  INTEGER :: ilon1_lic, ilat1_lic

  ! For ENTS
  INTEGER :: ilon1_lnd, ilat1_lnd

  ! For sediments
  INTEGER :: ilon1_sed, ilat1_sed

  ! For weathering
  INTEGER :: ilon1_rok, ilat1_rok

  ! Coordinate definitions.
  INTEGER :: dim_GENIENL, dim_GENIENX, dim_GENIENY
  INTEGER :: dim_GOLDSTEINNLONS, dim_GOLDSTEINNLATS
  INTEGER :: dim_GOLDSTEINNLEVS, dim_GOLDSTEINNTRACS
  INTEGER :: dim_SEDGEMNLONS, dim_SEDGEMNLATS
  INTEGER :: dim_ROKGEMNLONS, dim_ROKGEMNLATS

  ! ======================================================================
  ! Miscellaneous control variables

  ! Used for NetCDF output in various places
  INTEGER, PARAMETER :: BUFSIZ = 1024  !< to hold CHARACTER strings
  INTEGER, PARAMETER :: nfiles=4, nmaxdims=4, nall=100

  ! Biogeochemistry time stepping ratios -- loop modifiers (relative
  ! to ocean loop)
  INTEGER :: conv_kocn_katchem  !< atchem
  INTEGER :: conv_kocn_ksedgem  !< sedgem
  INTEGER :: conv_kocn_kbiogem  !< biogem
  INTEGER :: conv_kocn_krokgem  !< rokgem
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

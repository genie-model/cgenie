! ******************************************************************************************************************************** !
! atchem_data.f90
! Atmospheric Chemistry
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE atchem_data


  USE atchem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! LOAD AtCheM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_atchem()
    ! local variables
    integer::ia,ias                                              ! tracer counter
    integer::ios                                                 !
    ! read data_ATCHEM file
    open(unit=in,file='data_ATCHEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open ATCHEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_ATCHEM file
    read(UNIT=in,NML=ini_atchem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read ATCHEM namelist'
       stop
    else
       close(unit=in)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    if (ctrl_debug_init > 0) then
       ! --- TRACER INITIALIZATION ----------------------------------------------------------------------------------------------- !
       print*,'--- TRACER INITIALIZATION --------------------------'
       DO ia = 1, n_atm
          ias = ia_ias(ia)
          print*,'atm tracer initial value: ',trim(string_atm(ias)),' = ',atm_init(ias)
       end do
       ! --- COSMOGENIC & RADIOGENIC PRODUCTION ---------------------------------------------------------------------------------- !
       print*,'--- COSMOGENIC & RADIOGENIC PRODUCTION -------------'
       print*,'Global cosmogenic production rate of 14C (mol yr-1) : ',par_atm_F14C
       ! --- EMISSIONS-TO-ATMOSPHERE --------------------------------------------------------------------------------------------- !
       print*,'--- EMISSIONS-TO-ATMOSPHERE ------------------------'
       print*,'Wetlands CH4 flux (mol yr-1)                        : ',par_atm_wetlands_FCH4
       print*,'Wetlands CH4 d13C (o/oo)                            : ',par_atm_wetlands_FCH4_d13C
       ! --- SLAB BIOSPHERE ------------------------------------------------------------------------------------------------------ !
       print*,'--- SLAB BIOSPHERE ---------------------------------'
       print*,': ',par_atm_slabbiosphere_C
       print*,': ',par_atm_slabbiosphere_C_d13C
       print*,': ',par_atm_FterrCO2exchange
       ! --- RUN CONTROL --------------------------------------------------------------------------------------------------------- !
       print*,'--- RUN CONTROL ------------------------------------'
       print*,'Continuing run?                                     : ',ctrl_continuing
       ! --- I/O DIRECTORY DEFINITIONS ------------------------------------------------------------------------------------------- !
       print*,'--- I/O DIRECTORY DEFINITIONS ----------------------'
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       ! --- DATA SAVING: MISC --------------------------------------------------------------------------------------------------- !
       print*,'--- DATA SAVING: MISC ------------------------------'
       print*,'Restart in netCDF format?                           : ',ctrl_ncrst
       print*,'netCDF restart file name                            : ',trim(par_ncrst_name)
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ########################################################################## !
       !
       ! ######################################################################################################################### !
    end if
  END SUBROUTINE sub_load_goin_atchem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** LOAD Atchem RESTART DATA ************************************************************************************************* !
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_load_rst()
    USE atchem_lib
    use gem_netcdf
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,ia,iv,ias                                       ! local counting variables
    integer::ios                                               !
    integer::loc_ncid                                          !
    CHARACTER(len=255)::loc_filename                           ! filename string
    integer::loc_n_l_atm                                       ! number of selected tracers in the re-start file
    integer,DIMENSION(n_atm_all)::loc_conv_iselected_ia            ! number of selected atmospheric tracers in restart
    real,dimension(n_i,n_j)::loc_atm                           !
    integer::loc_ndims,loc_nvars
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_atm = 0.0
    ! -------------------------------------------------------- ! set filename
    IF (ctrl_ncrst) THEN
       loc_filename = TRIM(par_rstdir_name)//par_ncrst_name
    else
       loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    endif
    ! -------------------------------------------------------- ! check file status
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
    close(unit=in)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'atchem_data','sub_data_load_restart', &
            & 'You have requested a CONTINUING run, but restart file <'//trim(loc_filename)//'> does not exist', &
            & 'SKIPPING - using default initial values (FILE: gem_config_atm.par)', &
            & (/const_real_null/),.false. &
            & )
    else
       ! -------------------------------------------------------- !
       ! LOAD RESTART
       ! -------------------------------------------------------- !
       IF (ctrl_ncrst) THEN
          call sub_openfile(loc_filename,loc_ncid)
          ! -------------------------------------------------------- ! determine number of variables
          call sub_inqdims (loc_filename,loc_ncid,loc_ndims,loc_nvars)
          ! -------------------------------------------------------- ! allocate arrays
          ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! get variable names
          call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
          ! -------------------------------------------------------- ! load and apply only tracers that are selected
          IF (ctrl_debug_init == 1) print*,' * Loading restart tracers: '
          DO iv = 1, loc_nvars
             DO ia = 1, n_atm
                ias = ia_ias(ia)
                IF ('atm_' // TRIM(string_atm(ias)) == TRIM(loc_varname(iv))) THEN
                   IF (ctrl_debug_init == 1) PRINT *, '   ', TRIM(loc_varname(iv))
                   loc_atm = 0.0
                   CALL sub_getvarij(loc_ncid, 'atm_' // TRIM(string_atm(ias)), n_i, n_j, loc_atm)
                   atm(ias, :, :) = loc_atm(:,:)
                endif
             end do
          end DO
          ! -------------------------------------------------------- ! deallocate arrays
          deALLOCATE(loc_dimlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_vdims,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varname,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! close file
          call sub_closefile(loc_ncid)
       else
          OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
          read(unit=in) &
               & loc_n_l_atm,                                          &
               & (loc_conv_iselected_ia(l),l=1,loc_n_l_atm),           &
               & (atm(loc_conv_iselected_ia(l),:,:),l=1,loc_n_l_atm)
          close(unit=in,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif
    endif
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_data_load_rst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! initialize atmosphere grid
  SUBROUTINE sub_init_phys_atm()
    ! local variables
    INTEGER::i,j
    real::loc_th0,loc_th1,loc_s0,loc_s1,loc_ds
    real,dimension(0:n_j)::loc_s,loc_sv
    ! zero array
    phys_atm(:,:,:) = 0.0
    ! calculate local constants
    loc_th0 = -const_pi/2
    loc_th1 = const_pi/2
    loc_s0 = sin(loc_th0)
    loc_s1 = sin(loc_th1)
    loc_ds = (loc_s1-loc_s0)/real(n_j)
    DO j=0,n_j
       loc_sv(j) = loc_s0 + real(j)*loc_ds
       loc_s(j) = loc_sv(j) - 0.5*loc_ds
    end do
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          phys_atm(ipa_lat,i,j)  = (180.0/const_pi)*ASIN(loc_s(j))
          phys_atm(ipa_lon,i,j)  = (360.0/real(n_i))*(real(i)-0.5) + par_grid_lon_offset
          phys_atm(ipa_dlat,i,j) = (180.0/const_pi)*(ASIN(loc_sv(j)) - ASIN(loc_sv(j-1)))
          phys_atm(ipa_dlon,i,j) = (360.0/real(n_i))
          phys_atm(ipa_latn,i,j) = (180.0/const_pi)*ASIN(loc_sv(j))
          phys_atm(ipa_lone,i,j) = (360.0/n_i)*real(i) + par_grid_lon_offset
          phys_atm(ipa_dh,i,j)   = par_atm_th
          phys_atm(ipa_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))*(loc_sv(j) - loc_sv(j-1))
          phys_atm(ipa_rA,i,j)   = 1.0/phys_atm(ipa_A,i,j)
          phys_atm(ipa_V,i,j)    = phys_atm(ipa_dh,i,j)*phys_atm(ipa_A,i,j)
          phys_atm(ipa_rV,i,j)   = 1.0/phys_atm(ipa_V,i,j)
       END DO
    END DO
  END SUBROUTINE sub_init_phys_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER COMPOSITION - ATMOSPHERE
  SUBROUTINE sub_init_tracer_atm_comp()
    IMPLICIT NONE
    INTEGER :: ias
    REAL :: loc_frac, loc_standard

    atm = 0.0
    ! set <atm> array
    ! NOTE: need to seed ias_T as temperature is required in order to convert between mole (total) and partial pressure
    DO ias = 1, n_atm_all
       IF (atm_select(ias)) THEN
          SELECT CASE (atm_type(ias))
          CASE (0)
             IF (ias == ias_T) atm(ias,:,:) = const_zeroC
          CASE (1)
             atm(ias,:,:) = atm_init(ias)
          CASE (11, 12, 13, 14)
             loc_standard = const_standards(atm_type(ias))
             loc_frac = fun_calc_isotope_fraction(atm_init(ias),loc_standard)
             atm(ias,:,:) = loc_frac * atm_init(atm_dep(ias))
          END SELECT
       end if
    END DO
  END SUBROUTINE sub_init_tracer_atm_comp
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE SLAB BIOSPHERE
  SUBROUTINE sub_init_slabbiosphere()
    ! local variables
    INTEGER::i,j
    real::loc_tot,loc_frac,loc_standard
    ! initialize global arrays
    atm_slabbiosphere(:,:,:)  = 0.0
    ! set <atm> array
    ! NOTE: units of (mol)
    DO i=1,n_i
       DO j=1,n_j
          IF (atm_select(ias_pCO2)) THEN
             atm_slabbiosphere(ias_pCO2,i,j) = par_atm_slabbiosphere_C/real(n_i*n_j)
          end if
          IF (atm_select(ias_pCO2_13C)) THEN
             loc_tot  = atm_slabbiosphere(ias_pCO2,i,j)
             loc_standard = const_standards(atm_type(ias_pCO2_13C))
             loc_frac = fun_calc_isotope_fraction(par_atm_slabbiosphere_C_d13C,loc_standard)
             atm_slabbiosphere(ias_pCO2_13C,i,j) = loc_frac*loc_tot
          end if
       END DO
    END DO
  END SUBROUTINE sub_init_slabbiosphere
  ! ****************************************************************************************************************************** !


END MODULE atchem_data

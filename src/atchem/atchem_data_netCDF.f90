! ******************************************************************************************************************************** !
! atchem_data_netCDF.f90
! ATCHEM netCDF ROUTINES
! ******************************************************************************************************************************** !


MODULE atchem_data_netCDF


  USE gem_netcdf
  USE atchem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! SAVE NETCDF RESTART DATA
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_netCDF_ncrstsave(dum_name,dum_yr,dum_iou)
    ! -------------------------------------------------------- !
    ! DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    character(LEN=*),INTENT(IN)::dum_name                      !
    REAL,INTENT(in)::dum_yr                                    !
    INTEGER,INTENT(OUT)::dum_iou                               !
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::ia,ias
    integer::loc_ntrec,loc_iou
    integer::loc_id_lonm,loc_id_latm,loc_id_lon_e,loc_id_lat_e
    integer,dimension(1:2)::loc_it_1
    integer,dimension(1:3)::loc_it_2
    character(127)::loc_title,loc_timunit
    character(7)::loc_string_year
    real::loc_c0,loc_c1
    real,dimension(0:n_i)::loc_lon_e
    real,dimension(0:n_j)::loc_lat_e
    real,dimension(n_i,n_j)::loc_ij,loc_ij_mask
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_c0 = 0.0
    loc_c1 = 1.0
    loc_lon_e = 0.0 ; loc_lat_e = 0.0
    loc_ij = 0.0 ; loc_ij_mask = 0.0
    ! -------------------------------------------------------- !
    ! WRITE TO FILE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! open file
    call sub_opennew(dum_name,loc_iou)
    ! -------------------------------------------------------- ! start definitions
    call sub_redef(loc_iou)
    ! -------------------------------------------------------- ! set global attributes
    loc_string_year = fun_conv_num_char_n(8,int(dum_yr))
    loc_title = 'ATCHEM restart @ year '//loc_string_year
    call sub_putglobal(loc_iou,dum_name,loc_title,string_ncrunid,loc_timunit)
    ! -------------------------------------------------------- ! define dimensions
    call sub_defdim ('lon', loc_iou, n_i, loc_id_lonm)
    call sub_defdim ('lat', loc_iou, n_j, loc_id_latm)
    call sub_defdim ('lon_edges', loc_iou, n_i+1, loc_id_lon_e)
    call sub_defdim ('lat_edges', loc_iou, n_j+1, loc_id_lat_e)
    ! -------------------------------------------------------- ! define 1d data (t)
    loc_it_1(1) = loc_id_lonm
    call sub_defvar ('lon',loc_iou,1,loc_it_1,loc_c0,loc_c0,'X','D','longitude of the t grid','longitude','degrees_east')
    loc_it_1(1) = loc_id_latm
    call sub_defvar ('lat',loc_iou,1,loc_it_1,loc_c0,loc_c0,'Y','D','latitude of the t grid','latitude','degrees_north')
    loc_it_1(1) = loc_id_lon_e
    call sub_defvar ('lon_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D' ,'longitude of t grid edges',' ','degrees')
    loc_it_1(1) = loc_id_lat_e
    call sub_defvar ('lat_edges',loc_iou,1,loc_it_1,loc_c0,loc_c0,' ','D','latitude of t grid edges',' ','degrees')
    loc_it_2(1) = loc_id_lonm
    loc_it_2(2) = loc_id_latm
    ! -------------------------------------------------------- ! define (2D) tracer variables
    DO ia = 1, n_atm
       ias = ia_ias(ia)
       CALL sub_defvar('atm_' // TRIM(string_atm(ias)), &
            & loc_iou, 2, loc_it_2, loc_c0, loc_c0, ' ', 'F', &
            & string_longname_atm(ias), &
            & 'Atmosphere tracer - ' // TRIM(string_atm(ias)), ' ')
    END DO
    ! -------------------------------------------------------- ! end definitions
    call sub_enddef (loc_iou)
    call sub_sync(loc_iou)
    ! -------------------------------------------------------- !
    loc_ntrec = 1
    ! -------------------------------------------------------- ! write 1D variables
    call sub_putvar1d ('lon',loc_iou,n_i,loc_ntrec,n_i,phys_atm(ipa_lon,:,1),loc_c1,loc_c0)
    call edge_maker (1,loc_lon_e,phys_atm(ipa_lon,:,1),phys_atm(ipa_lone,:,1),phys_atm(ipa_dlon,:,1),n_i)
    call sub_putvar1d ('lon_edges',loc_iou,n_i+1,loc_ntrec,n_i+1,loc_lon_e,loc_c1,loc_c0)
    call sub_putvar1d ('lat',loc_iou,n_j,loc_ntrec,n_j,phys_atm(ipa_lat,1,:),loc_c1,loc_c0)
    call edge_maker (1,loc_lat_e, phys_atm(ipa_lat,1,:),phys_atm(ipa_latn,1,:),phys_atm(ipa_dlat,1,:), n_j)
    call sub_putvar1d ('lat_edges',loc_iou,n_j+1,loc_ntrec,n_j+1,loc_lat_e,loc_c1,loc_c0)
    ! -------------------------------------------------------- ! write (2D) tracer variables
    loc_ij_mask(:,:) = 1.0
    loc_ij(:,:)= 0.0
    DO ia = 1, n_atm
       loc_ij(:,:) = atm(ia,:,:)
       CALL sub_putvar2d('atm_' // TRIM(string_atm(ia_ias(ia))), loc_iou, n_i, n_j, &
            & loc_ntrec, loc_ij(:,:), loc_ij_mask(:,:))
    end DO
    ! -------------------------------------------------------- ! close file and return IOU
    call sub_closefile(loc_iou)
    dum_iou = loc_iou
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_netCDF_ncrstsave
  ! ****************************************************************************************************************************** !

END MODULE atchem_data_netCDF

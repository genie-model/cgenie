
! ******************************************************************************************************************************** !
! END BioGeM
SUBROUTINE end_biogem()
  USE biogem_lib
  USE biogem_data
  USE genie_util, ONLY: check_iostat
  
  print*,'======================================================='
  print*,' >>> Initialising BIOGEM module shutdown ...'

  ! *** output audit diagnostics ***
  ! reporting of initial/final tracer inventories
  IF (ctrl_audit) THEN
     PRINT*,' '
     PRINT*,'*** BioGeM tracer audit diagnostics ***'
     CALL sub_data_audit_diagnostics()
  END IF

  IF (ctrl_debug_lvl2) PRINT*,'saving netcdf record numbers',ncout2d_ntrec,ncout3d_ntrec
  OPEN(unit=out,status='replace',file=TRIM(par_outdir_name)//'netcdf_record_numbers',form='formatted',action='write')
  WRITE(unit=out,fmt='(i6)') ncout2d_ntrec,ncout3d_ntrec                             
  close(unit=out)

  ! ---------------------------------------------------------- !
  !  DEALLOCATE ARRAYS
  !- --------------------------------------------------------- !
  !- --------------------------------------------------------- ! explicit grid: restoring forcing
  DEALLOCATE(force_restore_locn,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(force_restore_locn_I,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(force_restore_locn_II,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !- --------------------------------------------------------- ! explicit grid: flux forcing
  DEALLOCATE(force_flux_locn,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(force_flux_locn_I,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(force_flux_locn_II,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !- --------------------------------------------------------- ! data saving
  if (ctrl_data_save_3d_sig) then
     DEALLOCATE(int_misc_3D_sig,STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
  end if
  !- --------------------------------------------------------- ! vectorized: 3D
  DEALLOCATE(vocn,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(vdocn,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(vbio_remin,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(vbio_part,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(vdbio_part,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(vphys_ocn,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !- --------------------------------------------------------- ! vectorized: 2D

  print*,' <<< Shutdown complete'
  print*,'======================================================='

END SUBROUTINE end_biogem
! ******************************************************************************************************************************** !

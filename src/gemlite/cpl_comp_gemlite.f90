! ******************************************************************************************************************************** !
! cpl_comp_gemlite.f90
! GEMlite tracer field transfers
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: ATM,OCN->GEMLITE
SUBROUTINE cpl_comp_gemglt(dum_genie_atm1, dum_genie_ocn)
  use genie_global
  use gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  real,dimension(:,:,:),intent(inout)::dum_genie_atm1 !
  real,dimension(:,:,:,:),intent(inout)::dum_genie_ocn !
  ! local variables
  integer::l,io
  ! copy tracer array
  atm = dum_genie_atm1
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     ocn(l,:,:,:) = dum_genie_ocn(io,:,:,:)
  end do
  ! reset composition arrays
  dum_genie_atm1 = 0.0
  dum_genie_ocn = 0.0
end SUBROUTINE cpl_comp_gemglt
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEMLITE->OCN,ATM
SUBROUTINE cpl_comp_gltgem_d(dum_genie_datm1, dum_genie_docn)
  use genie_global
  use gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  real,dimension(:,:,:),intent(out)::dum_genie_datm1 !
  real,dimension(:,:,:,:),intent(out)::dum_genie_docn !
  ! local variables
  integer::l,io
  ! initialize arrays
  dum_genie_datm1 = 0.0
  dum_genie_docn = 0.0
  ! copy tracer anomaly arrays
  dum_genie_datm1 = datm
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     dum_genie_docn(io,:,:,:) = docn(l,:,:,:)
  end do
  ! reset composition anomaly arrays
  datm = 0.0
  docn = 0.0
end SUBROUTINE cpl_comp_gltgem_d
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE TRACER FIELDS: GEMLITE->OCN,ATM (SUMMED)
SUBROUTINE cpl_comp_gltgem_dsum(dum_genie_datm1, dum_genie_docn)
  use genie_global
  use gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  real,dimension(:,:,:),intent(out)::dum_genie_datm1 !
  real,dimension(:,:,:,:),intent(out)::dum_genie_docn !
  ! local variables
  integer::l,io
  ! initialize receiving arrays
  dum_genie_datm1 = 0.0
  dum_genie_docn = 0.0
  ! copy tracer anomaly arrays
  ! NOTE: do not re-set integrated composition anomaly arrays yet ...
  dum_genie_datm1 = datm_sum
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     dum_genie_docn(io,:,:,:) = docn_sum(l,:,:,:)
  end do
  ! reset composition anomaly arrays
  datm_sum = 0.0
  docn_sum = 0.0
end SUBROUTINE cpl_comp_gltgem_dsum
! ******************************************************************************************************************************** !

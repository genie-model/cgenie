

! ******************************************************************************************************************************** !
! cpl_flux_rokatm.f90
! RokGeM interface flux integrator
! ******************************************************************************************************************************** !

! ******************************************************************************************************************************** !
! COUPLE FLUXES: rok->atm
 SUBROUTINE cpl_flux_rokatm(    &
     & dum_dts,                 &
     & dum_n_maxatm,            &
     & dum_nr_maxi,dum_nr_maxj, &
     & dum_n_maxi,dum_n_maxj,   &
     & dum_sfxatm1,             &
     & dum_sfxsumatm,           &
     & dum_gem                  &
     & )
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts
  integer,intent(in)::dum_n_maxatm
  integer,intent(in)::dum_nr_maxi,dum_nr_maxj
  integer,intent(in)::dum_n_maxi,dum_n_maxj
  real,dimension(dum_n_maxatm,dum_n_maxi,dum_n_maxj),intent(inout)::dum_sfxatm1
  real,dimension(dum_n_maxatm,dum_n_maxi,dum_n_maxj),intent(out)::dum_sfxsumatm
  logical,intent(in)::dum_gem
  ! local variables
  integer::loc_scalei,loc_scalej
  real::loc_scale
  ! initialize local variables!
  loc_scalei = dum_nr_maxi/dum_n_maxi
  loc_scalej = dum_nr_maxj/dum_n_maxj
  loc_scale = 1.0/real(loc_scalei*loc_scalej)
  ! set return (dissolution) flux to ocean
  ! NOTE: rok->atm flux (rok grid) <dum_sfxatm> in units of (mol m-2 s-1)
  ! NOTE: rok->atm flux (atm grid) <dum_sfxatm1> in units of (mol m-2 s-1)
!!! *** KLUDGE (works for matching grids) ***
   dum_sfxsumatm(:,:,:) = dum_sfxsumatm(:,:,:) + dum_dts*dum_sfxatm1(:,:,:)
   if (.NOT. dum_gem) then
      dum_sfxatm1(:,:,:) = 0.0
   end if
!!! *****************************************
end SUBROUTINE cpl_flux_rokatm
! ******************************************************************************************************************************** !

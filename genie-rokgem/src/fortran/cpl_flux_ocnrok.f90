

! ******************************************************************************************************************************** !
! cpl_flux_ocnrok.f90
! rokgem interface flux integrator
! ******************************************************************************************************************************** !

! Edited from original atchem/cpl_flux_ocnatm.f90
!  -> changed instances of atm->rok (for rocks on land)
! Only want runoff flux from atm to ocean (no flux from land to atm)
! maybe don't need this as should be dumping solute fluxes in coastal cells rather than doing
! a cell to cell flux matching 2 grids!
! ******************************************************************************************************************************** !
! COUPLE fluxes surface (embm) -> rokgem
SUBROUTINE cpl_flux_ocnrok()
END SUBROUTINE cpl_flux_ocnrok
! ******************************************************************************************************************************** !

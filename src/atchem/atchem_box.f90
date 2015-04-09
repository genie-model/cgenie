! ******************************************************************************************************************************** !
! atchem_box.f90
! Atmosphere Chemistry
! MISCELLANEOUS ROUTINES
! ******************************************************************************************************************************** !

MODULE atchem_box
  USE atchem_lib
  IMPLICIT NONE
  SAVE

CONTAINS

  ! ****************************************************************************************************************************** !
  ! EXCHANGE CARBON WITH A VIRTUAL TERRESTRIAL RESERVOIR
  SUBROUTINE sub_calc_terrCO2exchange(i, j, dtyr, fatm)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i, j
    REAL, INTENT(IN) :: dtyr
    REAL, DIMENSION(n_atm), INTENT(INOUT) :: fatm          ! net flux to atmosphere (mol)

    REAL :: loc_Fatm, loc_Fterr                                ! flux to atm, flux to terrestrial biosphere
    REAL :: loc_Ratm, loc_Rterr                                ! local isotopic variables

    ! *** INITIALIZE LOCAL VARIABLES ***
    loc_Fatm  = dtyr * par_atm_FterrCO2exchange / REAL(n_i*n_j)
    loc_Fterr = dtyr * par_atm_FterrCO2exchange / REAL(n_i*n_j)
    loc_Ratm = atm(ia_pCO2_13C,i,j) / atm(ia_pCO2,i,j)
    loc_Rterr = atm_slabbiosphere(ia_pCO2_13C,i,j) / atm_slabbiosphere(ia_pCO2,i,j)

    ! *** EXCHANGE CO2 ***
    ! NOTE: atm_slabbiosphere in units of mol
    ! bulk CO2
    fatm(ia_pCO2) = fatm(ia_pCO2) + loc_Fatm - loc_Fterr
    atm_slabbiosphere(ia_pCO2,i,j) = atm_slabbiosphere(ia_pCO2,i,j) - loc_Fatm + loc_Fterr
    ! d13C
    fatm(ia_pCO2_13C) = fatm(ia_pCO2_13C) + loc_Rterr*loc_Fatm - loc_Ratm*loc_Fterr
    atm_slabbiosphere(ia_pCO2_13C,i,j) = atm_slabbiosphere(ia_pCO2_13C,i,j) - loc_Rterr*loc_Fatm + loc_Ratm*loc_Fterr
  END SUBROUTINE sub_calc_terrCO2exchange

  ! ****************************************************************************************************************************** !
  ! OXIDIZE CH4
  SUBROUTINE sub_calc_oxidize_CH4(i, j, dtyr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i, j
    REAL, INTENT(IN) :: dtyr

    REAL :: loc_tau, loc_fracdecay, loc_CH4

    ! *** CALCULATE LOCAL CONSTANTS ***
    ! atmospheric lifetime from Osborn and Wigley [1994]
    ! NOTE1: restrict lifetime calculation to 0.5*CH4(0) at the lower end (limit of calibration curve)
    ! NOTE2: strictly, calibratio curve ends at 4.0*CH4(0) in Osborn and Wigley [1994]
    ! NOTE3: omitting [OH], [NOx] etc etc
    loc_CH4 = atm(ia_pCH4,i,j)
    if (loc_CH4 < 0.5 * const_pCH4_oxidation_C0) loc_CH4 = 0.5 * const_pCH4_oxidation_C0
    loc_tau = const_pCH4_oxidation_tau0 * (loc_CH4 / const_pCH4_oxidation_C0)**const_pCH4_oxidation_N
    loc_fracdecay = dtyr / loc_tau

    ! *** ATMOSPHERIC CH4->CO2 ***
    atm(ia_pO2,i,j)      = atm(ia_pO2,i,j)      - 2.0*loc_fracdecay * atm(ia_pCH4,i,j)
    atm(ia_pCO2,i,j)     = atm(ia_pCO2,i,j)     + loc_fracdecay * atm(ia_pCH4,i,j)
    atm(ia_pCH4,i,j)     = (1.0 - loc_fracdecay) * atm(ia_pCH4,i,j)
    IF (atm_select(ias_pCO2_13C) .AND. atm_select(ias_pCH4_13C)) &
         & atm(ia_pCO2_13C,i,j) = atm(ia_pCO2_13C,i,j) + loc_fracdecay * atm(ia_pCH4_13C,i,j)
    IF (atm_select(ias_pCO2_14C) .AND. atm_select(ias_pCH4_14C)) &
         & atm(ia_pCO2_14C,i,j) = atm(ia_pCO2_14C,i,j) + loc_fracdecay * atm(ia_pCH4_14C,i,j)
    IF (atm_select(ias_pCH4_13C)) atm(ia_pCH4_13C,i,j) = (1.0 - loc_fracdecay) * atm(ia_pCH4_13C,i,j)
    IF (atm_select(ias_pCH4_14C)) atm(ia_pCH4_14C,i,j) = (1.0 - loc_fracdecay) * atm(ia_pCH4_14C,i,j)
END SUBROUTINE sub_calc_oxidize_CH4

  ! ****************************************************************************************************************************** !
  ! WETLANDS CH4 FLUX
  SUBROUTINE sub_calc_wetlands_CH4(dtyr, fatm)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dtyr
    REAL, DIMENSION(:), INTENT(INOUT) :: fatm                  ! flux to atmosphere (mol)

    REAL :: loc_flux_CH4, loc_flux_CH4_13C                     ! local CH4 flux
    REAL :: loc_tot, loc_standard                              ! local isotopic variables

    ! *** INITIALIZE LOCAL VARIABLES ***
    fatm = 0.0

    ! *** CALCULATE 'WETLANDS' CH4 FLUX TO ATMOSPHERE ***
    ! NOTE: multiply by 1/(imax x jmax) to divide up total emissions equally across all grid cells
    ! NOTE: loc_fatm in units of (mol), par_atm_wetlands_FCH4 in units of (mol yr-1)
    loc_flux_CH4 = dtyr * (1.0 / REAL(n_i*n_j)) * par_atm_wetlands_FCH4

    ! *** ADD 'WETLAND' CH4 EMISSIONS SOURCE TO ATMOSPHERE ***
    fatm(ia_pCH4) = fatm(ia_pCH4) + loc_flux_CH4
    IF (atm_select(ias_pCH4_13C)) THEN
       loc_tot = loc_flux_CH4
       loc_standard = const_standards(atm_type(ias_pCH4_13C))
       loc_flux_CH4_13C = fun_calc_isotope_fraction(par_atm_wetlands_FCH4_d13C, loc_standard) * loc_tot
       fatm(ia_pCH4_13C) = fatm(ia_pCH4_13C) + loc_flux_CH4_13C
       IF (atm_select(ias_pCH4_14C)) fatm(ia_pCH4_14C) = fatm(ia_pCH4_14C)
    END IF

    ! *** BALANCE CO2 and O2 BUDGETS ***
    ! remove CO2 from atmosphere, assuming CO2 has at some point been removed to form wetland Corg and hence CH4
    ! add (2x) O2 to the atmopshere, again implicitly accounting for net O2 release upon Corg deposition in wetlands
    fatm(ia_pCO2) = fatm(ia_pCO2) - loc_flux_CH4
    IF (atm_select(ias_pCH4_13C) .AND. atm_select(ias_pCH4_13C)) THEN
       fatm(ia_pCO2_13C) = fatm(ia_pCO2_13C) - loc_flux_CH4_13C
       IF (atm_select(ias_pCO2_14C)) fatm(ia_pCO2_14C) = fatm(ia_pCO2_14C)
    END IF
    fatm(ia_pO2) = fatm(ia_pO2) + 2.0 * loc_flux_CH4
  END SUBROUTINE sub_calc_wetlands_CH4

  ! ****************************************************************************************************************************** !
  ! PRODUCE 14C
  SUBROUTINE sub_calc_generate_14C(dtyr, fatm)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dtyr
    REAL, DIMENSION(:), INTENT(INOUT) :: fatm              ! flux to atmosphere (mol)

    ! *** CALCULATE COSMOGENIC 14C FLUX ***
    fatm(ia_pCO2_14C) = fatm(ia_pCO2_14C) + dtyr * (1.0 / REAL(n_i*n_j)) * par_atm_F14C
  END SUBROUTINE sub_calc_generate_14C

END MODULE atchem_box

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
  SUBROUTINE sub_calc_terrCO2exchange(dum_i,dum_j,dum_dtyr,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm),intent(inout)::dum_fatm              ! net flux to atmosphere (mol)
    ! local variables
    real::loc_Fatm,loc_Fterr                                   ! flux to atm, flux to terrestrial biosphere
    real::loc_Ratm,loc_Rterr                                   ! local isotopic variables

    ! *** INITIALIZE LOCAL VARIABLES ***
    loc_Fatm  = dum_dtyr*par_atm_FterrCO2exchange/real(n_i*n_j)
    loc_Fterr = dum_dtyr*par_atm_FterrCO2exchange/real(n_i*n_j)
    loc_Ratm = atm(ia_pCO2_13C,dum_i,dum_j)/atm(ia_pCO2,dum_i,dum_j)
    loc_Rterr = atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j)/atm_slabbiosphere(ia_pCO2,dum_i,dum_j)
        
    ! *** EXCHANGE CO2 ***
    ! NOTE: atm_slabbiosphere in units of mol
    ! bulk CO2
    dum_fatm(ia_pCO2) = dum_fatm(ia_pCO2) + loc_Fatm - loc_Fterr
    atm_slabbiosphere(ia_pCO2,dum_i,dum_j) = &
         & atm_slabbiosphere(ia_pCO2,dum_i,dum_j) - loc_Fatm + loc_Fterr
    ! d13C
    dum_fatm(ia_pCO2_13C) = dum_fatm(ia_pCO2_13C) + loc_Rterr*loc_Fatm - loc_Ratm*loc_Fterr
    atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j) = &
         & atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j) - loc_Rterr*loc_Fatm + loc_Ratm*loc_Fterr

  END SUBROUTINE sub_calc_terrCO2exchange
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! OXIDIZE CH4
  SUBROUTINE sub_calc_oxidize_CH4(dum_i,dum_j,dum_dtyr)
    IMPLICIT NONE
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    real,intent(in)::dum_dtyr
    ! local variables
    real::loc_tau
    real::loc_fracdecay
    real::loc_CH4
    
    ! *** CALCULATE LOCAL CONSTANTS ***
    ! atmospheric lifetime from Osborn and Wigley [1994]
    ! NOTE1: restrict lifetime calculation to 0.5*CH4(0) at the lower end (limit of calibration curve)
    ! NOTE2: strictly, calibratio curve ends at 4.0*CH4(0) in Osborn and Wigley [1994]
    ! NOTE3: omitting [OH], [NOx] etc etc
    loc_CH4 = atm(ia_pCH4,dum_i,dum_j)
    if (loc_CH4 < 0.5*const_pCH4_oxidation_C0) loc_CH4 = 0.5*const_pCH4_oxidation_C0
    loc_tau = const_pCH4_oxidation_tau0*(loc_CH4/const_pCH4_oxidation_C0)**const_pCH4_oxidation_N
    loc_fracdecay = dum_dtyr/loc_tau

    ! *** ATMOSPHERIC CH4->CO2 ***
    atm(ia_pO2,dum_i,dum_j)      = atm(ia_pO2,dum_i,dum_j)      - 2.0*loc_fracdecay*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCO2,dum_i,dum_j)     = atm(ia_pCO2,dum_i,dum_j)     + loc_fracdecay*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCO2_13C,dum_i,dum_j) = atm(ia_pCO2_13C,dum_i,dum_j) + loc_fracdecay*atm(ia_pCH4_13C,dum_i,dum_j)
    atm(ia_pCO2_14C,dum_i,dum_j) = atm(ia_pCO2_14C,dum_i,dum_j) + loc_fracdecay*atm(ia_pCH4_14C,dum_i,dum_j)
    atm(ia_pCH4,dum_i,dum_j)     = (1.0 - loc_fracdecay)*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCH4_13C,dum_i,dum_j) = (1.0 - loc_fracdecay)*atm(ia_pCH4_13C,dum_i,dum_j)
    atm(ia_pCH4_14C,dum_i,dum_j) = (1.0 - loc_fracdecay)*atm(ia_pCH4_14C,dum_i,dum_j)
    
  END SUBROUTINE sub_calc_oxidize_CH4
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! WETLANDS CH4 FLUX
  SUBROUTINE sub_calc_wetlands_CH4(dum_dtyr,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm),intent(inout)::dum_fatm              ! flux to atmosphere (mol)
    ! local variables
    REAL::loc_flux_CH4,loc_flux_CH4_13C                        ! local CH4 flux
    real::loc_tot,loc_standard                                 ! local isotopic variables

    ! *** INITIALIZE LOCAL VARIABLES ***
    dum_fatm(:) = 0.0
        
    ! *** CALCULATE 'WETLANDS' CH4 FLUX TO ATMOSPHERE ***
    ! NOTE: multiply by 1/(imax x jmax) to divide up total emissions equally across all grid cells
    ! NOTE: loc_fatm in units of (mol), par_atm_wetlands_FCH4 in units of (mol yr-1)
    loc_flux_CH4 = dum_dtyr*(1.0/real(n_i*n_j))*par_atm_wetlands_FCH4
        
    ! *** ADD 'WETLAND' CH4 EMISSIONS SOURCE TO ATMOSPHERE ***
    dum_fatm(ia_pCH4) = dum_fatm(ia_pCH4) + loc_flux_CH4
    IF (atm_select(ia_pCH4_13C)) THEN
       loc_tot = loc_flux_CH4
       loc_standard = const_standards(atm_type(ia_pCH4_13C))
       loc_flux_CH4_13C = fun_calc_isotope_fraction(par_atm_wetlands_FCH4_d13C,loc_standard)*loc_tot
       dum_fatm(ia_pCH4_13C) = dum_fatm(ia_pCH4_13C) + loc_flux_CH4_13C
       IF (atm_select(ia_pCH4_14C)) THEN
          dum_fatm(ia_pCH4_14C) = dum_fatm(ia_pCH4_14C)
       end IF
    end IF
        
    ! *** BALANCE CO2 and O2 BUDGETS ***
    ! remove CO2 from atmosphere, assuming CO2 has at some point been removed to form wetland Corg and hence CH4
    ! add (2x) O2 to the atmopshere, again implicitly accounting for net O2 release upon Corg deposition in wetlands
    dum_fatm(ia_pCO2) = dum_fatm(ia_pCO2) - loc_flux_CH4
    IF (atm_select(ia_pCH4_13C) .AND. atm_select(ia_pCH4_13C)) THEN
       dum_fatm(ia_pCO2_13C) = dum_fatm(ia_pCO2_13C) - loc_flux_CH4_13C
       IF (atm_select(ia_pCO2_14C)) THEN
          dum_fatm(ia_pCO2_14C) = dum_fatm(ia_pCO2_14C)
       end IF
    end IF
    dum_fatm(ia_pO2) = dum_fatm(ia_pO2) + 2.0*loc_flux_CH4
    
  END SUBROUTINE sub_calc_wetlands_CH4
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! PRODUCE 14C
  SUBROUTINE sub_calc_generate_14C(dum_dtyr,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm),intent(inout)::dum_fatm              ! flux to atmosphere (mol)
    
    ! *** CALCULATE COSMOGENIC 14C FLUX ***
    dum_fatm(ia_pCO2_14C) = dum_fatm(ia_pCO2_14C) + dum_dtyr*(1.0/real(n_i*n_j))*par_atm_F14C
    
  END SUBROUTINE sub_calc_generate_14C
  ! ****************************************************************************************************************************** !
  

END MODULE atchem_box

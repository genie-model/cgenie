! ===========================================================
! This module contains wrapper subroutines to hide arg lists
!             __End Routines Only__
! ===========================================================

MODULE genie_end_wrappers

  USE genie_global

CONTAINS

  SUBROUTINE end_gem_wrapper
    IMPLICIT NONE
    CALL end_gem()
  END SUBROUTINE end_gem_wrapper

  SUBROUTINE end_biogem_wrapper
    IMPLICIT NONE
    CALL end_biogem()
  END SUBROUTINE end_biogem_wrapper

  SUBROUTINE end_atchem_wrapper
    USE atchem
    IMPLICIT NONE
    CALL end_atchem()
  END SUBROUTINE end_atchem_wrapper

  SUBROUTINE end_goldstein_wrapper
    use goldstein
    IMPLICIT NONE
    CALL end_goldstein()
  END SUBROUTINE end_goldstein_wrapper

  SUBROUTINE end_seaice_wrapper
    use gold_seaice
    IMPLICIT NONE
    CALL end_seaice()
  END SUBROUTINE end_seaice_wrapper

  SUBROUTINE end_embm_wrapper
    use embm
    IMPLICIT NONE
    CALL end_embm()
  END SUBROUTINE end_embm_wrapper

  SUBROUTINE end_sedgem_wrapper
    IMPLICIT NONE
    CALL end_sedgem(REAL(conv_kocn_ksedgem * kocn_loop) * genie_timestep, &
                  & genie_sfcsumocn)
  END SUBROUTINE end_sedgem_wrapper

  SUBROUTINE end_rokgem_wrapper
    IMPLICIT NONE
    CALL end_rokgem()
  END SUBROUTINE end_rokgem_wrapper

  SUBROUTINE end_gemlite_wrapper
    IMPLICIT NONE
    CALL end_gemlite()
  END SUBROUTINE end_gemlite_wrapper

END MODULE genie_end_wrappers

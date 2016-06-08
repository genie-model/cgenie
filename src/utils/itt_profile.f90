module itt_profile
    USE, INTRINSIC :: ISO_C_BINDING
    implicit none
#ifdef INTEL_PROFILE
    type(c_ptr) itt_domain

    type(c_ptr) task_timestep
    type(c_ptr) task_status
    type(c_ptr) task_gemlite
    type(c_ptr) task_normal
    type(c_ptr) task_surflux_wrapper
    type(c_ptr) task_gemlite2

    type(c_ptr) task_biogem
    type(c_ptr) task_biogem_adapt

    type(c_ptr) task_biogem_wrapper

    type(c_ptr) task_biogem_climate_wrapper
    type(c_ptr) task_biogem_tracercoupling_wrapper
    type(c_ptr) task_diag_biogem_timeslice_wrapper
    type(c_ptr) task_diag_biogem_timeseries_wrapper

    type(c_ptr) task_cpl_flux_ocnatm_wrapper
    type(c_ptr) task_cpl_flux_ocnsed_wrapper
    type(c_ptr) task_cpl_comp_ocnsed_wrapper
    type(c_ptr) task_reinit_flux_rokocn_wrapper
    type(c_ptr) task_biogem_step

    type(c_ptr) task_surfux_wrapper


    type(c_ptr) task_goldstein_tstepo_flux
#endif

contains
    subroutine itt_profile_init()
#ifdef INTEL_PROFILE
        use itt_fortran
        implicit none

        itt_domain = FITT_DOMAIN_CREATE(C_CHAR_"dom"//C_NULL_CHAR)
        task_timestep = FITT_STRING_HANDLE_CREATE(C_CHAR_"timestep"//C_NULL_CHAR)
        task_status = FITT_STRING_HANDLE_CREATE(C_CHAR_"status"//C_NULL_CHAR)

        task_gemlite = FITT_STRING_HANDLE_CREATE(C_CHAR_"gemlite"//C_NULL_CHAR)
        task_normal = FITT_STRING_HANDLE_CREATE(C_CHAR_"normal"//C_NULL_CHAR)
        task_surflux_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"surflux_wrapper"//C_NULL_CHAR)
        task_gemlite2 = FITT_STRING_HANDLE_CREATE(C_CHAR_"gemlite2"//C_NULL_CHAR)

        task_biogem = FITT_STRING_HANDLE_CREATE(C_CHAR_"biogem"//C_NULL_CHAR)
        task_biogem_adapt = FITT_STRING_HANDLE_CREATE(C_CHAR_"biogem_adapt"//C_NULL_CHAR)

        ! from genie main
        task_biogem_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"biogem_wrapper"//C_NULL_CHAR)
        !within biogem in genie main

        task_biogem_climate_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"biogem_climate"//C_NULL_CHAR)
        task_biogem_tracercoupling_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"biogem_tracercoupling_wrapper"//C_NULL_CHAR)

        task_diag_biogem_timeslice_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"diag_biogem_timeslice"//C_NULL_CHAR)
        task_diag_biogem_timeseries_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"diag_biogem_timeseries"//C_NULL_CHAR)

        task_cpl_flux_ocnatm_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"cpl_flux_ocnatm_wrapper"//C_NULL_CHAR)
        task_cpl_flux_ocnsed_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"cpl_flux_ocnsed_wrapper"//C_NULL_CHAR)
        task_cpl_comp_ocnsed_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"cpl_comp_ocnsed_wrapper"//C_NULL_CHAR)
        task_reinit_flux_rokocn_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"reinit_flux_rokocn_wrapper"//C_NULL_CHAR)


        task_biogem_step = FITT_STRING_HANDLE_CREATE(C_CHAR_"biogem_step"//C_NULL_CHAR)

        task_surfux_wrapper = FITT_STRING_HANDLE_CREATE(C_CHAR_"surfux_wrapper"//C_NULL_CHAR)


        task_goldstein_tstepo_flux = FITT_STRING_HANDLE_CREATE(C_CHAR_"tstepo_flux"//C_NULL_CHAR)

#endif
    end subroutine

#ifdef INTEL_PROFILE
    subroutine itt_profile_begin(task)
        USE, INTRINSIC :: ISO_C_BINDING
        use itt_fortran
        implicit none
        type(c_ptr) :: task

        call FITT_TASK_BEGIN(itt_domain, task)
    end subroutine
#endif
#ifdef INTEL_PROFILE
    subroutine itt_profile_end()
        USE, INTRINSIC :: ISO_C_BINDING
        use itt_fortran
        implicit none

        call FITT_TASK_END(itt_domain)

    end subroutine
#endif

end module itt_profile

MODULE ITT_FORTRAN

    USE, INTRINSIC :: ISO_C_BINDING

    INTERFACE

        SUBROUTINE FITT_RESUME() &
            BIND(C, NAME='fortran_itt_resume')
        END SUBROUTINE FITT_RESUME

        SUBROUTINE FITT_PAUSE() &
            BIND(C, NAME='fortran_itt_pause')
        END SUBROUTINE

        FUNCTION FITT_DOMAIN_CREATE(name) &
            BIND(C, NAME='fortran_itt_domain_create')
            use iso_c_binding, only: c_char, c_ptr
            character(kind=c_char) :: name(*)
            type(c_ptr) :: FITT_DOMAIN_CREATE
        END FUNCTION

        FUNCTION FITT_STRING_HANDLE_CREATE(name) &
            BIND(C, NAME='fortran_itt_string_handle_create')
            use iso_c_binding, only: c_char, c_ptr
            character(kind=c_char) :: name(*)
            type(c_ptr) :: FITT_STRING_HANDLE_CREATE
        END FUNCTION

        subroutine FITT_TASK_BEGIN(domain, handle) &
            BIND(C, NAME='fortran_itt_task_begin')
            use iso_c_binding, only: c_ptr
            type(c_ptr), intent(in), value :: domain
            type(c_ptr), intent(in), value :: handle
        END subroutine

        subroutine FITT_TASK_END(domain) &
            BIND(C, NAME='fortran_itt_task_end')
            use iso_c_binding, only: c_ptr
            type(c_ptr), intent(in), value :: domain
        END subroutine

    END INTERFACE

END MODULE ITT_FORTRAN

!> This module contains interstitial schemes that are specific to new Tiedtke cumulus scheme,
!> which is part of MMM physics.
module cu_ntiedtke_compat
    implicit none

    private
    public :: cu_ntiedtke_compat_pre_run
    public :: cu_ntiedtke_compat_init
    public :: cu_ntiedtke_compat_run
    public :: cu_ntiedtke_diagnostics_init
    public :: cu_ntiedtke_diagnostics_run
contains
    !> \section arg_table_cu_ntiedtke_compat_pre_run Argument Table
    !! \htmlinclude cu_ntiedtke_compat_pre_run.html
    subroutine cu_ntiedtke_compat_pre_run( &
            rthdynten, rthblten, rthratenlw, rthratensw, exner, &
            rqvdynten, rqvblten, &
            landfrac, &
            ptf, pqvf, hfx, evap, zprecc, &
            lndj, &
            errmsg, errflg)
        use ccpp_kinds, only: kind_phys

        real(kind_phys), intent(in) :: rthdynten(:, :), rthblten(:, :), rthratenlw(:, :), rthratensw(:, :), exner(:, :)
        real(kind_phys), intent(in) :: rqvdynten(:, :), rqvblten(:, :)
        real(kind_phys), intent(in) :: landfrac(:)
        real(kind_phys), intent(out) :: ptf(:, :), pqvf(:, :), hfx(:), evap(:), zprecc(:)
        integer, intent(out) :: lndj(:)
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        ptf(:, :) = (rthdynten(:, :) + rthblten(:, :) + rthratenlw(:, :) + rthratensw(:, :)) * exner(:, :)
        pqvf(:, :) = rqvdynten(:, :) + rqvblten(:, :)

        where (landfrac >= 0.5_kind_phys)
            lndj = 1
        elsewhere
            lndj = 0
        end where
    end subroutine cu_ntiedtke_compat_pre_run

    !> \section arg_table_cu_ntiedtke_compat_init Argument Table
    !! \htmlinclude cu_ntiedtke_compat_init.html
    subroutine cu_ntiedtke_compat_init( &
            con_cp, con_rd, con_rv, con_xlv, con_xls, con_xlf, con_grav, &
            errmsg, errflg)
        use ccpp_kinds, only: kind_phys
        use cu_ntiedtke, only: cu_ntiedtke_init

        real(kind_phys), intent(in) :: con_cp, con_rd, con_rv, con_xlv, con_xls, con_xlf, con_grav
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        call cu_ntiedtke_init( &
            con_cp, con_rd, con_rv, con_xlv, con_xls, con_xlf, con_grav, &
            errmsg, errflg)
    end subroutine cu_ntiedtke_compat_init

    !> \section arg_table_cu_ntiedtke_compat_run Argument Table
    !! \htmlinclude cu_ntiedtke_compat_run.html
    subroutine cu_ntiedtke_compat_run( &
            pu, pv, pt, pqv, pqc, pqi, pqvf, ptf, poz, pzz, pomg, &
            pap, paph, evap, hfx, zprecc, lndj, lq, km, km1, dt, dx, &
            errmsg, errflg)
        use ccpp_kinds, only: kind_phys
        use cu_ntiedtke, only: cu_ntiedtke_run

        real(kind_phys), intent(inout) :: pu(:, :), pv(:, :), pt(:, :), pqv(:, :), pqc(:, :), pqi(:, :), &
                                          zprecc(:)
        real(kind_phys), intent(in) :: pqvf(:, :), ptf(:, :), poz(:, :), pzz(:, :), pomg(:, :), &
                                       pap(:, :), paph(:, :), evap(:), hfx(:), &
                                       dt, dx(:)
        integer, intent(in) :: lndj(:), lq, km, km1
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        call cu_ntiedtke_run( &
            pu, pv, pt, pqv, pqc, pqi, pqvf, ptf, poz, pzz, pomg, &
            pap, paph, evap, hfx, zprecc, lndj, lq, km, km1, dt, dx, &
            errmsg, errflg)
    end subroutine cu_ntiedtke_compat_run

    !> \section arg_table_cu_ntiedtke_diagnostics_init Argument Table
    !! \htmlinclude cu_ntiedtke_diagnostics_init.html
    subroutine cu_ntiedtke_diagnostics_init( &
            errmsg, errflg)
        use cam_history, only: history_add_field
        use cam_history_support, only: horiz_only

        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0
    end subroutine cu_ntiedtke_diagnostics_init

    !> \section arg_table_cu_ntiedtke_diagnostics_run Argument Table
    !! \htmlinclude cu_ntiedtke_diagnostics_run.html
    subroutine cu_ntiedtke_diagnostics_run( &
            errmsg, errflg)
        use cam_history, only: history_out_field
        use ccpp_kinds, only: kind_phys

        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0
    end subroutine cu_ntiedtke_diagnostics_run
end module cu_ntiedtke_compat

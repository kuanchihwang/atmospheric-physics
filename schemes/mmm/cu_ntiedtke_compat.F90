!> This module contains interstitial schemes that are specific to new Tiedtke cumulus scheme,
!> which is part of MMM physics.
module cu_ntiedtke_compat
    implicit none

    private
    public :: cu_ntiedtke_compat_init
    public :: cu_ntiedtke_compat_run
    public :: cu_ntiedtke_diagnostics_init
    public :: cu_ntiedtke_diagnostics_run
contains
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

        ! The "bl_gwdo" physics scheme makes a distinction between X/Y winds and eastward/northward winds. See
        ! the "cu_ntiedtke_compat_pre" interstitial scheme for details. However, here we just refer to its diagnostics as
        ! eastward/northward to make them more familiar to CAM-SIMA users.
        call history_add_field('bl_gwdo_dtaux3d', 'tendency_of_eastward_wind_due_to_orographic_gwd', 'lev', 'avg', 'm s-2')
        call history_add_field('bl_gwdo_dtauy3d', 'tendency_of_northward_wind_due_to_orographic_gwd', 'lev', 'avg', 'm s-2')
        call history_add_field('bl_gwdo_dusfcg', 'atmosphere_eastward_stress_due_to_orographic_gwd', horiz_only, 'avg', 'Pa')
        call history_add_field('bl_gwdo_dvsfcg', 'atmosphere_northward_stress_due_to_orographic_gwd', horiz_only, 'avg', 'Pa')
    end subroutine cu_ntiedtke_diagnostics_init

    !> \section arg_table_cu_ntiedtke_diagnostics_run Argument Table
    !! \htmlinclude cu_ntiedtke_diagnostics_run.html
    subroutine cu_ntiedtke_diagnostics_run( &
            dtaux3d, dtauy3d, dusfcg, dvsfcg, &
            errmsg, errflg)
        use cam_history, only: history_out_field
        use ccpp_kinds, only: kind_phys

        real(kind_phys), intent(in) :: dtaux3d(:, :), dtauy3d(:, :), dusfcg(:), dvsfcg(:)
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        call history_out_field('bl_gwdo_dtaux3d', dtaux3d)
        call history_out_field('bl_gwdo_dtauy3d', dtauy3d)
        call history_out_field('bl_gwdo_dusfcg', dusfcg)
        call history_out_field('bl_gwdo_dvsfcg', dvsfcg)
    end subroutine cu_ntiedtke_diagnostics_run
end module cu_ntiedtke_compat

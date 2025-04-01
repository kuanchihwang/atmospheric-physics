!> This module contains interstitial schemes that are specific to YSU orographic gravity wave drag scheme,
!> which is part of MMM physics.
module bl_gwdo_compat
    use ccpp_kinds, only: kind_phys

    implicit none

    private
    public :: bl_gwdo_pre_init
    public :: bl_gwdo_pre_run
    public :: bl_gwdo_post_run
    public :: bl_gwdo_diagnostics_init
    public :: bl_gwdo_diagnostics_run
contains
    !> \section arg_table_bl_gwdo_pre_init Argument Table
    !! \htmlinclude bl_gwdo_pre_init.html
    pure subroutine bl_gwdo_pre_init( &
            omega, rearth, &
            dxmeter, sina, cosa, &
            errmsg, errflg)
        real(kind_phys), intent(in) :: omega(:), rearth
        real(kind_phys), intent(out) :: dxmeter(:), sina(:), cosa(:)
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        ! These variables do not change with time. Set them just once at model initialization for better performance.

        ! The "bl_gwdo" physics scheme needs grid sizes in meters. This is trivial for models with regular grids like WRF,
        ! but not so straightforward for models with unstructured grids like CAM-SIMA. Here, the square root of cell area is used.
        dxmeter(:) = sqrt(omega(:) * (rearth ** 2))

        ! The "bl_gwdo" physics scheme was originally designed to be used with regional models like WRF, where the positive X and
        ! Y directions may not always point to the east and north, respectively. This is no longer the case for global models like
        ! CAM-SIMA.

        ! The angle of rotation from east to X is zero.
        sina(:) = 0.0_kind_phys
        cosa(:) = 1.0_kind_phys
    end subroutine bl_gwdo_pre_init

    !> \section arg_table_bl_gwdo_pre_run Argument Table
    !! \htmlinclude bl_gwdo_pre_run.html
    pure subroutine bl_gwdo_pre_run( &
            ncol, &
            u, v, &
            prsi, prsl, prslk, zl, q1, t1, &
            rublten, rvblten, &
            uproj, vproj, &
            errmsg, errflg)
        use mmm_physics_compat, only: reverse

        integer, intent(in) :: ncol
        real(kind_phys), intent(in) :: u(:, :), v(:, :)
        real(kind_phys), intent(inout) :: prsi(:, :), prsl(:, :), prslk(:, :), zl(:, :), q1(:, :), t1(:, :)
        real(kind_phys), intent(inout) :: rublten(:, :), rvblten(:, :)
        real(kind_phys), intent(out) :: uproj(:, :), vproj(:, :)
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        integer :: i

        errmsg = ''
        errflg = 0

        ! The "bl_gwdo" physics scheme was originally designed to be used with regional models like WRF, where the positive X and
        ! Y directions may not always point to the east and north, respectively. This is no longer the case for global models like
        ! CAM-SIMA.

        ! X and Y winds are just eastward and northward winds, respectively.
        uproj(:, :) = u(:, :)
        vproj(:, :) = v(:, :)

        ! All members of MMM physics expect vertical indexes to be in ascending order from bottom to top of atmosphere,
        ! which is the exact opposite to CAM-SIMA.
        !
        ! To address this, for all variables:
        ! 1. with a vertical dimension;
        ! 2. with either `intent(in)` or `intent(inout)`;
        ! they must be flipped upside down.

        ! Reverse vertical index order.
        do i = 1, ncol
            ! `intent(in)` with a vertical dimension in `bl_gwdo.meta`.
            prsi(i, :) = reverse(prsi(i, :))
            prsl(i, :) = reverse(prsl(i, :))
            prslk(i, :) = reverse(prslk(i, :))
            zl(i, :) = reverse(zl(i, :))
            q1(i, :) = reverse(q1(i, :))
            t1(i, :) = reverse(t1(i, :))
            uproj(i, :) = reverse(uproj(i, :))
            vproj(i, :) = reverse(vproj(i, :))

            ! `intent(inout)` with a vertical dimension in `bl_gwdo.meta`.
            rublten(i, :) = reverse(rublten(i, :))
            rvblten(i, :) = reverse(rvblten(i, :))
        end do
    end subroutine bl_gwdo_pre_run

    !> \section arg_table_bl_gwdo_post_run Argument Table
    !! \htmlinclude bl_gwdo_post_run.html
    pure subroutine bl_gwdo_post_run( &
            ncol, &
            prsi, prsl, prslk, zl, q1, t1, &
            dtaux3d, dtauy3d, rublten, rvblten, &
            uproj, vproj, &
            errmsg, errflg)
        use mmm_physics_compat, only: reverse

        integer, intent(in) :: ncol
        real(kind_phys), intent(inout) :: prsi(:, :), prsl(:, :), prslk(:, :), zl(:, :), q1(:, :), t1(:, :)
        real(kind_phys), intent(inout) :: dtaux3d(:, :), dtauy3d(:, :), rublten(:, :), rvblten(:, :)
        real(kind_phys), intent(inout) :: uproj(:, :), vproj(:, :)
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        integer :: i

        errmsg = ''
        errflg = 0

        ! For all variables that were flipped upside down as well as those:
        ! 1. with a vertical dimension;
        ! 2. with `intent(out)`;
        ! they must be flipped back.

        ! Restore vertical index order.
        do i = 1, ncol
            ! `intent(in)` with a vertical dimension in `bl_gwdo.meta`.
            prsi(i, :) = reverse(prsi(i, :))
            prsl(i, :) = reverse(prsl(i, :))
            prslk(i, :) = reverse(prslk(i, :))
            zl(i, :) = reverse(zl(i, :))
            q1(i, :) = reverse(q1(i, :))
            t1(i, :) = reverse(t1(i, :))
            uproj(i, :) = reverse(uproj(i, :))
            vproj(i, :) = reverse(vproj(i, :))

            ! `intent(inout)` with a vertical dimension in `bl_gwdo.meta`.
            rublten(i, :) = reverse(rublten(i, :))
            rvblten(i, :) = reverse(rvblten(i, :))

            ! `intent(out)` with a vertical dimension in `bl_gwdo.meta`.
            dtaux3d(i, :) = reverse(dtaux3d(i, :))
            dtauy3d(i, :) = reverse(dtauy3d(i, :))
        end do
    end subroutine bl_gwdo_post_run

    !> \section arg_table_bl_gwdo_diagnostics_init Argument Table
    !! \htmlinclude bl_gwdo_diagnostics_init.html
    subroutine bl_gwdo_diagnostics_init( &
            errmsg, errflg)
        use cam_history, only: history_add_field
        use cam_history_support, only: horiz_only

        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        call history_add_field('bl_gwdo_dtaux3d', 'tendency_of_x_wind_due_to_orographic_gwd', 'lev', 'avg', 'm s-2')
        call history_add_field('bl_gwdo_dtauy3d', 'tendency_of_y_wind_due_to_orographic_gwd', 'lev', 'avg', 'm s-2')
        call history_add_field('bl_gwdo_dusfcg', 'atmosphere_x_stress_due_to_orographic_gwd', horiz_only, 'avg', 'Pa')
        call history_add_field('bl_gwdo_dvsfcg', 'atmosphere_y_stress_due_to_orographic_gwd', horiz_only, 'avg', 'Pa')
        call history_add_field('bl_gwdo_rublten', 'tendency_of_x_wind_due_to_pbl_processes', 'lev', 'avg', 'm s-2')
        call history_add_field('bl_gwdo_rvblten', 'tendency_of_y_wind_due_to_pbl_processes', 'lev', 'avg', 'm s-2')
    end subroutine bl_gwdo_diagnostics_init

    !> \section arg_table_bl_gwdo_diagnostics_run Argument Table
    !! \htmlinclude bl_gwdo_diagnostics_run.html
    subroutine bl_gwdo_diagnostics_run( &
            dtaux3d, dtauy3d, dusfcg, dvsfcg, rublten, rvblten, &
            errmsg, errflg)
        use cam_history, only: history_out_field

        real(kind_phys), intent(in) :: dtaux3d(:, :), dtauy3d(:, :), dusfcg(:), dvsfcg(:), rublten(:, :), rvblten(:, :)
        character(*), intent(out) :: errmsg
        integer, intent(out) :: errflg

        errmsg = ''
        errflg = 0

        call history_out_field('bl_gwdo_dtaux3d', dtaux3d)
        call history_out_field('bl_gwdo_dtauy3d', dtauy3d)
        call history_out_field('bl_gwdo_dusfcg', dusfcg)
        call history_out_field('bl_gwdo_dvsfcg', dvsfcg)
        call history_out_field('bl_gwdo_rublten', rublten)
        call history_out_field('bl_gwdo_rvblten', rvblten)
    end subroutine bl_gwdo_diagnostics_run
end module bl_gwdo_compat

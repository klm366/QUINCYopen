!> Contains time control for JSBACH
!>
!> ICON-Land
!>
!> ---------------------------------------
!> Copyright (C) 2013-2024, MPI-M, MPI-BGC
!>
!> Contact: icon-model.org
!> Authors: AUTHORS.md
!> See LICENSES/ for license information
!> SPDX-License-Identifier: BSD-3-Clause
!> ---------------------------------------
!>
MODULE mo_jsb_time
#ifndef __NO_JSBACH__

  USE mo_kind,           ONLY: wp
  USE mo_exception,      ONLY: message, message_text, finish
  USE mo_jsb_time_iface, ONLY: is_time_experiment_start, is_time_ltrig_rad_m1, &
    &                          t_datetime, deallocateDatetime, get_date_components, get_year_day, get_year_length

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: is_time_experiment_start, is_time_ltrig_rad_m1, is_newday, is_newyear, t_datetime, get_year, get_year_length

  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_time'

CONTAINS

  ! ======================================================================================================= !
  !>
  !> returns true if the current and previous time step are from different days
  LOGICAL FUNCTION is_newday(current, dt)

    USE mo_jsb_time_iface, ONLY: get_time_previous

    TYPE(t_datetime), POINTER, INTENT(in) :: current
    REAL(wp),                  INTENT(in) :: dt

    TYPE(t_datetime), POINTER :: previous

    INTEGER :: current_day, previous_day

    previous => get_time_previous(current, dt)

    ! CALL get_date_components(current, day=current_day)
    ! CALL get_date_components(previous, day=previous_day)
    current_day = INT(get_year_day(current))
    previous_day = INT(get_year_day(previous))

    is_newday = current_day /= previous_day

    CALL deallocateDatetime(previous)

  END FUNCTION is_newday

  ! ======================================================================================================= !
  !>
  !> returns true if the current and previous time step are from different years
  LOGICAL FUNCTION is_newyear(current, dt)

    USE mo_jsb_time_iface, ONLY: get_time_previous

    TYPE(t_datetime), POINTER, INTENT(in) :: current
    REAL(wp),                  INTENT(in) :: dt

    TYPE(t_datetime), POINTER :: previous

    INTEGER :: current_year, previous_year

    previous => get_time_previous(current, dt)

    ! CALL get_date_components(current, year=current_year)
    ! CALL get_date_components(previous, year=previous_year)
    current_year  = get_year(current)
    previous_year = get_year(previous)

    is_newyear = current_year /= previous_year

    CALL deallocateDatetime(previous)

  END FUNCTION is_newyear

  ! ======================================================================================================= !
  !>
  !> returns the year of the provided datetime variable
  INTEGER FUNCTION get_year(this_datetime)

    TYPE(t_datetime), POINTER, INTENT(in) :: this_datetime

    INTEGER :: year

    CALL get_date_components(this_datetime, year=year)

    get_year = year

  END FUNCTION get_year

  ! ======================================================================================================= !
  !>
  !> returns the day of the provided datetime variable
  INTEGER FUNCTION get_day(this_datetime)

    TYPE(t_datetime), POINTER, INTENT(in) :: this_datetime

    INTEGER :: day

    CALL get_date_components(this_datetime, day=day)

    get_day = day

  END FUNCTION get_day

#endif
END MODULE mo_jsb_time

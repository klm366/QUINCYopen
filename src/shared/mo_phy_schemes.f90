!> Routines for physical schemes
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
!>#### Contains some routines for physical schemes
!>
MODULE mo_phy_schemes
#ifndef __NO_JSBACH__

  USE mo_kind,                 ONLY: wp

  IMPLICIT NONE
  PRIVATE
  ! PUBLIC :: qsat
  PUBLIC :: q_effective, surface_dry_static_energy, heat_transfer_coef, &
    &       thermal_radiation, lwnet_from_lwdown
  PUBLIC :: calc_peaked_arrhenius_function

  CHARACTER(len=*), PARAMETER :: modname = 'mo_phy_schemes'

CONTAINS

!   !>
!   ! Returns saturation specific humidity for given temperature and pressure (at some atmospheric level or at surface)
!   ! Uses Eq. 2.27 of Fundamentals of Atmospheric Modeling for the saturated case, but the saturation vapor pressure
!   ! of water over a liquid surface resp. ice (see pp 32-34 of Ref.) is computed as in ECHAM5 (mo_convect_tables)
!   !
!   REAL(wp) ELEMENTAL FUNCTION qsat(temperature, pressure)
! 
!     USE mo_jsb_convect_tables_iface, ONLY: tlucua, &                       ! Table for water vapor pressure e_s multiplied by R_d/R_v
!       &                                    jptlucu1, jptlucu2
!     USE mo_jsb_physical_constants, ONLY: rvd1                    ! = R_v/R_d - 1
! 
!     REAL(wp), INTENT(in) :: temperature      ! Air temperature [K]
!     REAL(wp), INTENT(in) :: pressure         ! Pressure [Pa]
! 
!     INTEGER :: it
!     REAL(wp) :: tluc
! 
!     !$claw sca routine
! 
!     it = NINT(temperature*1000._wp)
! !!$    it = MIN(MAX(NINT(temperature*1000._wp), 50000),400000)    ! Temporary fix
!     tluc = tlucua(it) !<- TODO catch seg fault for unplausible temperatures?
! 
!     IF (it >= jptlucu1 .AND. it <= jptlucu2) THEN
!       qsat = tluc / (pressure - rvd1*tluc)
!     ELSE
!       qsat = HUGE(1._wp)
!     END IF
! 
!   END FUNCTION qsat


  !-----------------------------------------------------------------------------------------------------
  !> FUNCTION q_effective
  !!
  !! out: q_effective
  !-----------------------------------------------------------------------------------------------------
  REAL(wp) FUNCTION q_effective(qsat, qair, fsat, fair)

    !$ACC ROUTINE SEQ

    REAL(wp), INTENT(in) :: &
      & qsat,               & !< Surface saturation specific humidity
      & qair,               & !< Air specific humidity
      & fsat, fair            !< Weighing factors for qsat and qair accounting for only partially wet surface.

    q_effective = fsat * qsat + (1._wp -fair) * qair

  END FUNCTION q_effective


  !-----------------------------------------------------------------------------------------------------
  !> FUNCTION surface_dry_static_energy
  !!
  !! out: surface_dry_static_energy
  !-----------------------------------------------------------------------------------------------------
  REAL(wp) FUNCTION surface_dry_static_energy(t_srf, qsat_srf, cpd_or_cvd, jsb_standalone)

    !$ACC ROUTINE SEQ

    USE mo_jsb_physical_constants, ONLY: &
      & cpvd1        !< cpv/cpd-1

    REAL(wp), INTENT(in) :: &
      & t_srf,                      & !< Surface temperature
      & qsat_srf,                   & !< Surface saturation specific humidity
      & cpd_or_cvd

    LOGICAL, INTENT(in) :: &
      & jsb_standalone

    IF (jsb_standalone) THEN
      ! Todo: Check if this formulation is valid. We re-implement it here, as it was used up to now in
      !       jsbach:dev, and we do not want to change the model behavior with this merge.
      surface_dry_static_energy = t_srf * cpd_or_cvd * ( 1._wp + cpvd1 * qsat_srf)
    ELSE
      surface_dry_static_energy = t_srf * cpd_or_cvd
    END IF

  END FUNCTION surface_dry_static_energy


  !-----------------------------------------------------------------------------------------------------
  !> FUNCTION thermal_radiation
  !!
  !! out: thermal_radiation
  !-----------------------------------------------------------------------------------------------------
#ifndef _OPENACC
  ELEMENTAL &
#endif
  REAL(wp) FUNCTION thermal_radiation(t_srf)

    USE mo_jsb_physical_constants, ONLY: &
      stbo,       &  !< Stefan-Boltzmann constant
      zemiss_def     !< Surface emissivity

    REAL(wp), INTENT(in) :: t_srf

    thermal_radiation = stbo * zemiss_def * t_srf**4._wp

  END FUNCTION thermal_radiation


  !-----------------------------------------------------------------------------------------------------
  !> FUNCTION lwnet_from_lwdown
  !!
  !! out: lwnet_from_lwdown
  !-----------------------------------------------------------------------------------------------------
  REAL(wp) FUNCTION lwnet_from_lwdown(lwdown, t_srf)

    !$ACC ROUTINE SEQ

    USE mo_jsb_physical_constants, ONLY: &
      stbo,       &  !< Stefan-Boltzmann constant
      zemiss_def     !< Surface emissivity

    REAL(wp), INTENT(in) :: lwdown   ! downward longwave radiation
    REAL(wp), INTENT(in) :: t_srf    ! surface temperature

    lwnet_from_lwdown = zemiss_def * (lwdown - stbo * t_srf**4._wp)

  END FUNCTION lwnet_from_lwdown


  !-----------------------------------------------------------------------------------------------------
  !> FUNCTION heat_transfer_coef
  !!
  !! out: heat_transfer_coef
  !-----------------------------------------------------------------------------------------------------
  REAL(wp) FUNCTION heat_transfer_coef(drag, steplen, alpha)

    !$ACC ROUTINE SEQ

    USE mo_jsb_physical_constants, ONLY: grav

    REAL(wp), INTENT(in) :: &
      & drag,      &
      & steplen,   &
      & alpha

    heat_transfer_coef = drag / (alpha * grav * steplen)

  END FUNCTION heat_transfer_coef

  ! ======================================================================================================= !
  !>Calculates temperature response factor according to the peaked Arrhenius equation as defined in
  !>  Medlyn et al. 2002, PCE, eq. 18
  !>  NB if sensitivity of zero is specified, 1 will be returned
  !>
  !>  Input: temperature, (de-)activiation energy, optimum temperature
  !>
  !>  Output: rate modifier (unitless)
  !>
  ELEMENTAL FUNCTION calc_peaked_arrhenius_function(temp,Ea,Ed,t_opt) RESULT(rate_modifier)

    USE mo_jsb_physical_constants,  ONLY: r_gas
    USE mo_jsb_math_constants,      ONLY: eps8
    !------------------------------------------------------------------------------------------------------ !
    REAL(wp), INTENT(in)  :: temp          !< temperature [K]
    REAL(wp), INTENT(in)  :: Ea            !< activation energy [J mol-1]
    REAL(wp), INTENT(in)  :: Ed            !< deactivation energy [J mol-1]
    REAL(wp), INTENT(in)  :: t_opt         !< optimum temperature [K]
    REAL(wp)              :: rate_modifier !< temperature rate modifier [unitless]
    !------------------------------------------------------------------------------------------------------ !
    REAL(wp)              :: hlp1, hlp2
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':calc_peaked_arrhenius_function'

    IF(Ea > eps8) THEN
      hlp1 = temp - t_opt
      hlp2 = temp * t_opt * r_gas
      rate_modifier = (Ed * EXP(Ea * hlp1 / hlp2)) / (Ed - Ea * (1._wp - EXP(Ed * hlp1 / hlp2)))
    ELSE
      rate_modifier = 1._wp
    ENDIF

  END FUNCTION calc_peaked_arrhenius_function 

#endif
END MODULE mo_phy_schemes

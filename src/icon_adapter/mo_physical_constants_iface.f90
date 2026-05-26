!>
!! @par Copyright
!!  This code is subject to the MPI-M-Software - License - Agreement in it's most recent form.
!!  Please see URL http://www.mpimet.mpg.de/en/science/models/license .
!!
!! @brief Contains interface to physcial constants -> largely copied from ICON for consistency
!!
!!
!! @par Revision History
!! First version               by Reiner Schnur (2016-03-18)
!!
MODULE mo_physical_constants_iface

  USE mo_kind, ONLY: wp

  IMPLICIT NONE

  PUBLIC

  REAL(wp), PARAMETER :: grav = 9.80665_wp       !> [m/s2] av. gravitational acceleration

  REAL(wp), PARAMETER ::          &
    & rhoi         = 917.0_wp,    & ! density of sea ice         [kg / m**3]
    & rhos         = 300.0_wp       ! density of snow            [kg / m**3]

  !> Mixed species
  REAL(wp), PARAMETER :: amd   = 28.970_wp        !> [g/mol] dry air
  !! Pure species
  REAL(wp), PARAMETER :: amco2 = 44.011_wp        !>[g/mol] CO2

  !> Dry air
  REAL(wp), PARAMETER :: rd    = 287.04_wp        !> [J/K/kg] gas constant
  REAL(wp), PARAMETER :: cpd   = 1004.64_wp       !! [J/K/kg] specific heat at constant pressure

  ! WMO/SI values
  REAL(wp), PARAMETER :: argas = 8.314472_wp      !! [J/K/mol] molar/universal/ideal gas constant
  REAL(wp), PARAMETER :: stbo  = 5.6704E-8_wp     !! [W/m2/K4] Stephan-Boltzmann constant

  !> H2O
  !! - gas
  REAL(wp), PARAMETER :: rv    = 461.51_wp        !> [J/K/kg] gas constant for water vapor
  REAL(wp), PARAMETER :: cpv   = 1869.46_wp       !! [J/K/kg] specific heat at constant pressure
  !> - liquid / water
  REAL(wp), PARAMETER :: rhoh2o= 1000._wp         !> [kg/m3]  density of liquid water
  !> - phase changes
  REAL(wp), PARAMETER :: alv   = 2.5008e6_wp      !> [J/kg]   latent heat for vaporisation
  REAL(wp), PARAMETER :: als   = 2.8345e6_wp      !! [J/kg]   latent heat for sublimation

  !> constants for radiation module
  REAL(wp), PARAMETER :: zemiss_def = 0.996_wp  !> lw sfc default emissivity factor

  !> Auxiliary constants
  REAL(wp), PARAMETER :: vtmpc1= rv/rd-1._wp      !! [ ]
  REAL(wp), PARAMETER :: vtmpc2= cpv/cpd-1._wp    !! [ ]

  REAL(wp), PARAMETER :: rvd1  = vtmpc1    !< = rv/rd-1                              []
  REAL(wp), PARAMETER :: cpvd1 = vtmpc2    !< = cpv/cpd-1                            []
  
  REAL(wp), PARAMETER :: rd_o_cpd = rd/cpd !< source: icon-mpim shared/mo_physical_constants.f90

END MODULE mo_physical_constants_iface

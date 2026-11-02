! ICON
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2026, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! SPDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

! This module provides physical constants for the ICON general circulation models.
!
! Physical constants are grouped as follows:
! - Natural constants
! - Molar weights
! - Earth and Earth orbit constants
! - Thermodynamic constants for the dry and moist atmosphere
! - Constants used for the computation of lookup tables of the saturation
!    mixing ratio over liquid water (*c_les*) or ice(*c_ies*)
!    (to be shifted to the module that computes the lookup tables)

MODULE mo_physical_constants

  USE mo_kind,            ONLY: lp

  IMPLICIT NONE

  PUBLIC

  ! Natural constants
  ! -----------------
  !
!!$  ! ECHAM values
!!$  REAL(lp), PARAMETER :: avo   = 6.022045e23_lp   ! [1/mo]    Avogadro constant
!!$  REAL(lp), PARAMETER :: ak    = 1.380662e-23_lp  ! [J/K]     Boltzmann constant
!!$  REAL(lp), PARAMETER :: argas = 8.314409_lp      ! [J/K/mol] molar/universal/ideal gas constant
!!$  REAL(lp), PARAMETER :: stbo  = 5.67E-8_lp       ! [W/m2/K4] Stephan-Boltzmann constant
  ! WMO/SI values
  REAL(lp), PARAMETER :: avo   = 6.02214179e23_lp !> [1/mo]    Avogadro constant
  REAL(lp), PARAMETER :: ak    = 1.3806504e-23_lp !! [J/K]     Boltzmann constant
  REAL(lp), PARAMETER :: argas = 8.314472_lp      !! [J/K/mol] molar/universal/ideal gas constant
  REAL(lp), PARAMETER :: stbo  = 5.6704E-8_lp     !! [W/m2/K4] Stephan-Boltzmann constant


  !> Molar weights
  !! -------------
  !!
  !! Pure species
  REAL(lp), PARAMETER :: amco2 = 44.011_lp        !>[g/mol] CO2
  REAL(lp), PARAMETER :: amch4 = 16.043_lp        !! [g/mol] CH4
  REAL(lp), PARAMETER :: amo3  = 47.9982_lp       !! [g/mol] O3
  REAL(lp), PARAMETER :: amo2  = 31.9988_lp       !! [g/mol] O2
  REAL(lp), PARAMETER :: amn2o = 44.013_lp        !! [g/mol] N2O
  REAL(lp), PARAMETER :: amc11 =137.3686_lp       !! [g/mol] CFC11
  REAL(lp), PARAMETER :: amc12 =120.9140_lp       !! [g/mol] CFC12
  REAL(lp), PARAMETER :: amw   = 18.0154_lp       !! [g/mol] H2O
  REAL(lp), PARAMETER :: amo   = 15.9994_lp       !! [g/mol] O
  REAL(lp), PARAMETER :: amno  = 30.0061398_lp    !! [g/mol] NO
  REAL(lp), PARAMETER :: amn2  = 28.0134_lp       !! [g/mol] N2
  REAL(lp), PARAMETER :: amso4 = 96.0626_lp       !! [g/mol] SO4
  REAL(lp), PARAMETER :: ams   = 32.06_lp         !! [g/mol] S
  !
  !> Mixed species
  REAL(lp), PARAMETER :: amd   = 28.970_lp        !> [g/mol] dry air
  !
  !> Auxiliary constants
  ! ppmv2gg converts ozone from volume mixing ratio in ppmv
  ! to mass mixing ratio in g/g
  REAL(lp), PARAMETER :: ppmv2gg=1.e-6_lp*amo3/amd
  REAL(lp), PARAMETER :: o3mr2gg=amo3/amd

  !> Conversion factor from CO2 volume to mass mixing ratio [kg(CO2)/kg(Air)/(mol(CO2)/mol(Air))].
  REAL(lp), PARAMETER :: vmr_to_mmr_co2 = amco2 / amd
  !> Conversion factor from CH4 volume to mass mixing ratio [kg(CH4)/kg(Air)/(mol(CH4)/mol(Air))].
  REAL(lp), PARAMETER :: vmr_to_mmr_ch4 = amch4 / amd
  !> Conversion factor from N2O volume to mass mixing ratio [kg(N2O)/kg(Air)/(mol(N2O)/mol(Air))].
  REAL(lp), PARAMETER :: vmr_to_mmr_n2o = amn2o / amd
  !> Conversion factor from CFC11 volume to mass mixing ratio [kg(CFC11)/kg(Air)/(mol(CFC11)/mol(Air))].
  REAL(lp), PARAMETER :: vmr_to_mmr_c11 = amc11 / amd
  !> Conversion factor from CFC12 volume to mass mixing ratio [kg(CFC12)/kg(Air)/(mol(CFC12)/mol(Air))].
  REAL(lp), PARAMETER :: vmr_to_mmr_c12 = amc12 / amd

  !> Earth and Earth orbit constants
  !! -------------------------------
  !!
!   REAL(lp), PARAMETER :: re    = 6.371229e6_lp    !! [m]    average radius
!   REAL(lp), PARAMETER :: rre   = 1._lp/ph_re         !! [1/m]
  REAL(lp), PARAMETER :: earth_radius           = 6.371229e6_lp    !! [m]    average radius
  REAL(lp), PARAMETER :: inverse_earth_radius   = 1._lp/earth_radius         !! [1/m]
  REAL(lp), PARAMETER :: earth_angular_velocity = 7.29212e-5_lp    !! [rad/s]  angular velocity
  !
!!$  ! ECHAM values
!!  REAL(lp), PARAMETER :: grav  = 9.80616_lp       ! [m/s2] av. gravitational acceleration
  ! WMO/SI value
  REAL(lp), PARAMETER :: grav  = 9.80665_lp       !> [m/s2] av. gravitational acceleration
  !$ACC DECLARE COPYIN(grav)
  REAL(lp), PARAMETER :: rgrav = 1._lp/grav       !! [s2/m]
  !
  REAL(lp), PARAMETER :: rae   = 0.1277E-2_lp     !> [m/m]  ratio of atm. scale height
  !                                               !!        to Earth radius


  ! Thermodynamic constants for the dry and moist atmosphere
  ! --------------------------------------------------------
  !
  !> Dry air
  REAL(lp), PARAMETER :: rd    = 287.04_lp        !> [J/K/kg] gas constant
  !$ACC DECLARE COPYIN(rd)
  REAL(lp), PARAMETER :: cpd   = 1004.64_lp       !! [J/K/kg] specific heat at constant pressure
  !$ACC DECLARE COPYIN(cpd)
  REAL(lp), PARAMETER :: cvd   = cpd-rd           !! [J/K/kg] specific heat at constant volume
  !$ACC DECLARE COPYIN(cvd)
  REAL(lp), PARAMETER :: con_m = 1.50E-5_lp       !! [m^2/s]  kinematic viscosity of dry air
  REAL(lp), PARAMETER :: con_h = 2.20E-5_lp       !! [m^2/s]  scalar conductivity of dry air
  REAL(lp), PARAMETER :: con0_h= 2.40e-2_lp       !! [J/m/s/K]thermal conductivity of dry air
  REAL(lp), PARAMETER :: eta0d = 1.717e-5_lp      !! [N*s/m2] dyn viscosity of dry air at tmelt
  !
  !> H2O
  !! - gas
  REAL(lp), PARAMETER :: rv    = 461.51_lp        !> [J/K/kg] gas constant for water vapor
  !$ACC DECLARE COPYIN(rv)
  REAL(lp), PARAMETER :: cpv   = 1869.46_lp       !! [J/K/kg] specific heat at constant pressure
  REAL(lp), PARAMETER :: cvv   = cpv-rv           !! [J/K/kg] specific heat at constant volume
  !$ACC DECLARE COPYIN(cvv)
  REAL(lp), PARAMETER :: dv0   = 2.22e-5_lp       !! [m^2/s]  diff coeff of H2O vapor in dry air at tmelt
  !> - liquid / water
  REAL(lp), PARAMETER :: rhoh2o= 1000._lp         !> [kg/m3]  density of liquid water
  !> - solid / ice
  REAL(lp), PARAMETER :: rhoice=  916.7_lp        !> [kg/m3]  density of pure ice

 !REAL(lp), PARAMETER :: clw   = 4186.84_lp       !! [J/K/kg] specific heat of water
                                                  !!  see below
  REAL(lp), PARAMETER ::  cv_i =  2000.0_lp
  !> - phase changes
  REAL(lp), PARAMETER :: alv   = 2.5008e6_lp      !> [J/kg]   latent heat for vaporisation
  !$ACC DECLARE COPYIN(alv)
  REAL(lp), PARAMETER :: als   = 2.8345e6_lp      !! [J/kg]   latent heat for sublimation
  !$ACC DECLARE COPYIN(als)
  REAL(lp), PARAMETER :: alf   = als-alv          !! [J/kg]   latent heat for fusion
  !$ACC DECLARE COPYIN(alf)
  REAL(lp), PARAMETER :: tmelt = 273.15_lp        !! [K]      melting temperature of ice/snow
  REAL(lp), PARAMETER :: t3    = 273.16_lp        !! [K]      Triple point of water at 611hPa
  !
  !> Auxiliary constants
  REAL(lp), PARAMETER :: rdv   = rd/rv            !> [ ]
  !$ACC DECLARE COPYIN(rdv)
  REAL(lp), PARAMETER :: vtmpc1= rv/rd-1._lp      !! [ ]
  !$ACC DECLARE COPYIN(vtmpc1)
  REAL(lp), PARAMETER :: vtmpc2= cpv/cpd-1._lp    !! [ ]
  REAL(lp), PARAMETER :: rcpv  = cpd/cpv-1._lp    !! [ ]
  REAL(lp), PARAMETER :: alvdcp= alv/cpd          !! [K]
  REAL(lp), PARAMETER :: alsdcp= als/cpd          !! [K]
  REAL(lp), PARAMETER :: rcpd  = 1._lp/cpd        !! [K*kg/J]
  REAL(lp), PARAMETER :: rcvd  = 1._lp/cvd        !! [K*kg/J]
  REAL(lp), PARAMETER :: rcpl  =  3.1733_lp       !! cp_d / cp_l - 1
  !
  REAL(lp), PARAMETER :: clw   = (rcpl + 1.0_lp) * cpd !> specific heat capacity of liquid water
  !$ACC DECLARE COPYIN(clw)
  REAL(lp), PARAMETER :: cv_v  = (rcpv + 1.0_lp) * cpd - rv
  !
  REAL(lp), PARAMETER :: o_m_rdv  = 1._lp-rd/rv   !> [ ]
  !$ACC DECLARE COPYIN(o_m_rdv)
  REAL(lp), PARAMETER :: rd_o_cpd = rd/cpd        !! [ ]
  !$ACC DECLARE COPYIN(rd_o_cpd)
  REAL(lp), PARAMETER :: cvd_o_rd = cvd/rd        !! [ ]
  !$ACC DECLARE COPYIN(cvd_o_rd)
  !
  REAL(lp), PARAMETER :: p0ref     = 100000.0_lp   !> [Pa]  reference pressure for Exner function

  !> Variables for computing cloud cover in RH scheme
  REAL(lp), PARAMETER ::  uc1  = 0.8_lp
  REAL(lp), PARAMETER ::  ucl  = 1.00_lp

  !> U.S. standard atmosphere vertical tropospheric temperature gradient
  REAL(lp), PARAMETER :: dtdz_standardatm = -6.5e-3_lp    ! [ K/m ]

  !> constants for radiation module
  REAL(lp), PARAMETER :: zemiss_def = 0.996_lp  !> lw sfc default emissivity factor

  !> salinity factor for reduced saturation vapor pressure over oceans
  REAL(lp), PARAMETER :: salinity_fac = 0.981_lp

  !> dielectric constants at reference temperature 0 degree Celsius for radar reflectivity calculation:
  REAL(lp), PARAMETER :: K_w_0 = 0.93_lp
  REAL(lp), PARAMETER :: K_i_0 = 0.176_lp

!------------below are parameters for ocean model---------------
  ! coefficients in linear EOS
  REAL(lp), PARAMETER :: a_T = 2.55E-04_lp,     & ! thermal expansion coefficient (kg/m3/K)
    &                    b_S = 7.64E-01_lp         ! haline contraction coefficient (kg/m3/psu)

  ! density reference values, to be constant in Boussinesq ocean models
  REAL(lp), PARAMETER :: rho_ref = 1025.022_lp         ! reference density [kg/m^3]
  REAL(lp), PARAMETER :: rho_inv = 0.0009755881663_lp  ! inverse reference density [m^3/kg]
  REAL(lp), PARAMETER :: sal_ref = 35.0_lp             ! reference salinity [psu]

  REAL(lp), PARAMETER :: SItodBar = 1.0e-4_lp          !Conversion from pressure [p] to pressure [bar]
                                                       !used in ocean thermodynamics
  REAL(lp),PARAMETER :: sfc_press_pascal = 101300.0_lp
  REAL(lp),PARAMETER :: sfc_press_bar    = 101300.0_lp*SItodBar

  REAL(lp), PARAMETER :: p0sl_bg         = 101325._lp  ! [Pa]     sea level pressure

!----------below are parameters for sea-ice and lake model---------------
  REAL(lp), PARAMETER ::            &
    ks           = 0.31_lp,         & ! heat conductivity snow     [J  / (m s K)]
    ki           = 2.1656_lp,       & ! heat conductivity ice      [J  / (m s K)]
    rhoi         = 917.0_lp,        & ! density of sea ice         [kg / m**3]
    rhos         = 300.0_lp,        & ! density of snow            [kg / m**3]
    ci           = 2106.0_lp,       & ! Heat capacity of ice       [J / (kg K)]
    cs           = 2090._lp,        &!  Heat capacity of snow      [J / (kg K)]

    Tf           = -1.80_lp,        & ! Temperature ice bottom     [C]
    mu           = 0.054_lp,        & ! Constant in linear freezing-
                                      ! point relationship         [C/ppt]
                                      ! (aka melting) temperature) [C]
!   muS          = -(-0.0575 + 1.710523E-3*Sqrt(Sice) - 2.154996E-4*Sice) * Sice
    albedoW      = 0.07_lp,         & ! albedo of the ocean used in atmosphere


    fr_fac       = 1.1925_lp,       & ! Frank Roeske energy budget closing factor for OMIP
!   fr_fac       = 1.0_lp,          ! factor not active

! CCSM3 albedo scheme - not used for coupling
    alb_ice_vis  = 0.73_lp,         & ! Albedo of dry ice  (visible)
    alb_ice_nir  = 0.33_lp,         & ! Albedo of dry ice  (near-infrared)
    alb_sno_vis  = 0.96_lp,         & ! Albedo of dry snow (visible)
    alb_sno_nir  = 0.68_lp,         & ! Albedo of dry snow (near-infrared)
    !I_0          = 0.3             ! Ice-surface penetrating shortwave fraction
    I_0          = 0.17_lp            ! Ice-surface penetrating shortwave fraction
  !$ACC DECLARE COPYIN(ci)

!--------- parameters for NWP sea-ice model (we should agree on a single value)-----
!_cdm>
! The value of the salt-water freezing point is the same as in GME and COSMO (-1.7 dgr C).
! Note that a different value (Tf=-1.8 dgr C) is defined in "mo_physical_constants".
!_cdm<
  REAL (lp), PARAMETER ::                             &
    &  tf_salt      = 271.45_lp     !< salt-water freezing point [K]
                                    !< (note that it differs from Tf)

END MODULE mo_physical_constants

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

! Parameters for the VDIFF turbulence scheme.
!
! References:
!     Angevine, W. M., Jiang, H., & Mauritsen T. (2010).
!           Performance of an eddy diffusivity mass flux scheme for shallow cumulus boundary layers.
!           Monthly Weather Review, 138(7), 2895-2912. https://doi.org/10.1175/2010MWR3142.1
!     Mauritsen, T., & Svensson, G. (2007).
!           Observations of stably stratified shear-driven atmospheric turbulence at low and high Richardson numbers.
!           Journal of the Atmospheric Sciences, 64(2), 645-655. https://doi.org/10.1175/JAS3856.1

MODULE mo_turb_vdiff_params
  USE mo_kind, ONLY: lp

  IMPLICIT NONE
  PUBLIC

  INTEGER, PARAMETER :: VDIFF_TURB_TTE = 1 !< TTE scheme, use in t_vdiff_config%turb
  INTEGER, PARAMETER :: VDIFF_TURB_3DSMAGORINSKY = 2 !< 3D Smagorinsky scheme, use in t_vdiff_config%turb

  ! Parameters
  REAL(lp),PARAMETER :: ckap    = 0.4_lp       !< karman constant.
  REAL(lp),PARAMETER :: cchar   = 0.018_lp     !< charnock constant.
  REAL(lp),PARAMETER :: cb      = 5._lp        !< stability parameter near neutrality.
  REAL(lp),PARAMETER :: cc      = 5._lp        !< stability parameter for unstable cases.

  REAL(lp),PARAMETER :: eps_shear = 1.e-5_lp   !< zepshr in sbr. vdiff of ECHAM6
  REAL(lp),PARAMETER :: eps_corio = 5.e-5_lp   !< zepcor in sbr. vdiff of ECHAM6
  REAL(lp),PARAMETER :: totte_min = 1.e-10_lp  !< minimum total turbulent energy

  REAL(lp),PARAMETER :: chneu  = 0.3_lp

  REAL(lp),PARAMETER :: cons5  = 3._lp*cb*cc

  REAL(lp),PARAMETER :: shn = 2.22_lp*0.22_lp*SQRT(2._lp)
  REAL(lp),PARAMETER :: smn = shn*1.24_lp*2.37_lp/3.69_lp
  REAL(lp),PARAMETER :: da1 = 1._lp/smn**3

  ! Parameters related to time step weighting in *rhs* of *vdiff* and *scv*

  REAL(lp),PARAMETER :: cvdifts = 1.5_lp
  REAL(lp),PARAMETER :: tpfac1  = cvdifts
  REAL(lp),PARAMETER :: tpfac2  = 1._lp / tpfac1
  REAL(lp),PARAMETER :: tpfac3  = 1._lp - tpfac2
  REAL(lp),PARAMETER :: tpfac4  = 1._lp + tpfac3

END MODULE mo_turb_vdiff_params

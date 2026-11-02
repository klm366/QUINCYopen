! ICON
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2025, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! SPDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

! Constants used for the computation of lookup tables of the saturation
! mixing ratio over liquid water (*c_les*) or ice(*c_ies*)

MODULE mo_lookup_tables_constants

  USE mo_kind, ONLY: lp
  USE mo_physical_constants, ONLY: alv, als, cpd, rd, rv, tmelt

! Needed for ACCWA
#if defined(_CRAYFTN)
  USE mo_exception           ,ONLY: finish
#endif

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: c1es, c2es, c3les, c3ies, c4les, c4ies, c5les, c5ies, &
    &       c5alvcp, c5alscp, alvdcp, alsdcp

  PUBLIC :: init_satpres_coeffs

!ACCWA (Cray Fortran => 17.0.1) : non-parameter scalars have zero values in GPU when used via renames CAST-37645
#if defined(_CRAYFTN)
  REAL (lp), PARAMETER :: c1es  = 610.78_lp              !
  REAL (lp), PARAMETER :: c2es  = c1es*rd/rv             !
  REAL (lp), PARAMETER :: c3les = 17.269_lp              !
  REAL (lp), PARAMETER :: c3ies = 21.875_lp              !
  REAL (lp), PARAMETER :: c4les = 35.86_lp               !
  REAL (lp), PARAMETER :: c4ies = 7.66_lp                !
  REAL (lp), PARAMETER :: c5les = c3les*(tmelt-c4les)    !
  REAL (lp), PARAMETER :: c5ies = c3ies*(tmelt-c4ies)    !
  REAL (lp), PARAMETER :: c5alvcp = c5les*alv/cpd        !
  REAL (lp), PARAMETER :: c5alscp = c5ies*als/cpd        !
  REAL (lp), PARAMETER :: alvdcp  = alv/cpd              !
  REAL (lp), PARAMETER :: alsdcp  = als/cpd              !
  !$ACC DECLARE COPYIN(c1es, c2es, c3les, c3ies, c4les, c4ies, c5les, c5ies) &
  !$ACC   COPYIN(c5alvcp, c5alscp, alvdcp, alsdcp)

  CONTAINS


  SUBROUTINE init_satpres_coeffs(itype_satpres)

    INTEGER, INTENT(IN), OPTIONAL :: itype_satpres

    INTEGER :: itype

    IF (PRESENT(itype_satpres)) THEN
      itype = itype_satpres
    ELSE
      itype = 1
    ENDIF

    IF (itype == 2) THEN
      CALL finish('mo_look_table:init_satpres_coeffs', &
                  'due to compiler bug itype=2 is not supported')
    END IF

  END SUBROUTINE

#else

  REAL (lp) :: c1es
  REAL (lp) :: c2es
  REAL (lp) :: c3les
  REAL (lp) :: c3ies
  REAL (lp) :: c4les
  REAL (lp) :: c4ies
  REAL (lp) :: c5les
  REAL (lp) :: c5ies
  REAL (lp) :: c5alvcp
  REAL (lp) :: c5alscp
  REAL (lp) :: alvdcp
  REAL (lp) :: alsdcp

  !$ACC DECLARE CREATE(c1es, c2es, c3les, c3ies, c4les, c4ies, c5les, c5ies, c5alvcp, c5alscp, alvdcp, alsdcp)

  CONTAINS


  SUBROUTINE init_satpres_coeffs(itype_satpres)

    INTEGER, INTENT(IN), OPTIONAL :: itype_satpres

    INTEGER :: itype

    IF (PRESENT(itype_satpres)) THEN
      itype = itype_satpres
    ELSE
      itype = 1
    ENDIF

    ! Set coefficients for saturation pressure (originally set as fortran parameters in the declaration above, but this
    ! needed to be moved in order to switch between the old COSMO model coefficients and the more accurate IFS coefficients)

    IF (itype == 1) THEN ! DWD coefficients (namelist default)   NOTE: old COSMO model coefficients !
      c3les = 17.269_lp
      c3ies = 21.875_lp
      c4les = 35.86_lp
      c4ies = 7.66_lp
    ELSE IF (itype == 2) THEN ! IFS coefficients   NOTE: more accurate
      c3les = 17.502_lp
      c3ies = 22.587_lp
      c4les = 32.19_lp
      c4ies = -0.7_lp
    ENDIF
    c1es  = 610.78_lp
    c2es  = c1es*rd/rv
    c5les = c3les*(tmelt-c4les)
    c5ies = c3ies*(tmelt-c4ies)
    c5alvcp = c5les*alv/cpd
    c5alscp = c5ies*als/cpd
    alvdcp  = alv/cpd
    alsdcp  = als/cpd

    !$ACC UPDATE DEVICE(c1es, c2es, c3les, c3ies, c4les, c4ies, c5les, c5ies, c5alvcp, c5alscp, alvdcp, alsdcp)

  END SUBROUTINE init_satpres_coeffs
#endif ! ACCWA

END MODULE mo_lookup_tables_constants

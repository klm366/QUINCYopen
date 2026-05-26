!> Contains routines for var_lists/streams
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
MODULE mo_jsb_varlist
#ifndef __NO_JSBACH__

  USE mo_kind,              ONLY: wp
  USE mo_jsb_varlist_iface, ONLY: VARNAME_LEN

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: VARNAME_LEN
  PUBLIC :: BASIC, MEDIUM, FULL

  ENUM, BIND(C)
    ENUMERATOR :: BASIC=1, MEDIUM, FULL
  END ENUM

  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_varlist'

!CONTAINS

#endif
END MODULE mo_jsb_varlist

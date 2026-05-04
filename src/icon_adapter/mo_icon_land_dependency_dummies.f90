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

! Contains modules just used in ICON-Land but required as dummies for compiling and running QUINCY standalone

MODULE mo_alcc_config_class

  USE mo_jsb_config_class,   ONLY: t_jsb_config

  IMPLICIT NONE
  PUBLIC :: t_alcc_config

  TYPE, EXTENDS(t_jsb_config)      :: t_alcc_config
   CONTAINS
     PROCEDURE :: Init => Init_alcc_config
  END type t_alcc_config

CONTAINS

  ! ====================================================================================================== !
  !
  !> Initialize alcc process
  !
  ! -------------------------------------------------------------------------------------------------------
  SUBROUTINE Init_alcc_config(config)

    ! -------------------------------------------------------------------------------------------------- !
    CLASS(t_alcc_config), INTENT(inout) :: config !<Configuration type of process (t_alcc_config)
    ! -------------------------------------------------------------------------------------------------- !

    config%active = .FALSE.

  END SUBROUTINE Init_alcc_config

END MODULE mo_alcc_config_class


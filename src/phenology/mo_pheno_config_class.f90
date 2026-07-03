!>
!!  @par Copyright
!!    QUINCY terrestrial biosphere model with coupled carbon, nitrogen, and phosphorus cycles
!!    Copyright (C) 2019 Max Planck Institute for Biogeochemistry (MPI-BGC), Jena, Germany 
!!    Contributors: Caldararu S., Engel J., Kern M., Pallandt M., Schnur R., Thum T., Yu L., Zaehle S.
!!
!!    This file is part of the QUINCY model.
!!    The QUINCY model is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License version 3,
!!    as published by the Free Software Foundation.
!!
!!    The QUINCY model is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with the QUINCY model.  If not, see <https://www.gnu.org/licenses/>.
!! 
!! @brief Contains structures and methods for config of the phenology process
!!
!! @author
!!  Reiner Schnur, MPI-M Hamburg
!!
!! @par Revision History
!! First version by Jan Engel, MPI BGC (20161214)
!!
MODULE mo_pheno_config_class
#ifndef __NO_JSBACH__

  USE mo_kind,              ONLY: wp
  USE mo_exception,         ONLY: message
  USE mo_io_units,          ONLY: filename_max
  USE mo_jsb_config_class,  ONLY: t_jsb_config

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: t_pheno_config

  ! ======================================================================================================= !
  !> PHENO_ configuration parameters
  !>
  TYPE, EXTENDS(t_jsb_config) :: t_pheno_config
    ! ----------------------------------------------------------------------------------------------------- !
    !> MODEL_JSBACH
    !>
    LOGICAL             :: l_forestRegrowth           !< If TRUE, the maxLAI is derived from biomass via allometric relationships
    CHARACTER(len=15)   :: scheme_jsbach              !< Phenology scheme to use ('logrop', 'climatology')
    !LOGICAL :: first_call_phenology_this_day         !< TRUE marks if a new day has just started and
    !                                                 !< update_phenology is called for the top tile
    ! ----------------------------------------------------------------------------------------------------- !
    !> MODEL_QUINCY
    !> 
    REAL(wp)            :: lai_max              !< site specific lai, used to init lai_max in PHENO_
    CONTAINS
      PROCEDURE :: Init => Init_pheno_config
  END type t_pheno_config

  CHARACTER(len=*), PARAMETER :: modname = 'mo_pheno_config_class'

CONTAINS

  ! ======================================================================================================= !
  !> read PHENO_ namelist and init configuration parameters
  !>
  SUBROUTINE Init_pheno_config(config)
#ifdef __QUINCY_STANDALONE__
    USE mo_namelist,              ONLY: open_nml, POSITIONED, position_nml, close_nml
#else
    USE mo_jsb_namelist_iface,    ONLY: open_nml, POSITIONED, position_nml, close_nml
#endif
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_pheno_config), INTENT(inout) :: config
    ! ----------------------------------------------------------------------------------------------------- !
    LOGICAL                     :: active
    LOGICAL                     :: l_forestRegrowth
    REAL(wp)                    :: lai_site_specific      !< site specific lai, used to init lai_max in PHENO_
    CHARACTER(len=15)           :: scheme                 !< scheme_jsbach
    CHARACTER(len=filename_max) :: ic_filename, bc_filename
#ifdef __QUINCY_STANDALONE__
    NAMELIST /phenology_ctl/  &
#else
    NAMELIST /jsb_pheno_nml/  &
#endif
      & active,               &
      & l_forestRegrowth,     &
      & scheme,               &
      & ic_filename,          &
      & bc_filename,          &
      & lai_site_specific
    INTEGER :: nml_handler, nml_unit, istat
    CHARACTER(len=*), PARAMETER :: routine = modname//':Init_pheno_config'
    ! ----------------------------------------------------------------------------------------------------- !
    CALL message(TRIM(routine), 'Starting phenology configuration')
    ! ----------------------------------------------------------------------------------------------------- !
    ! Set defaults
    active              = .TRUE.
    l_forestRegrowth    = .FALSE.
    scheme              = 'logrop'
    bc_filename         = 'bc_land_phenology.nc'
    ic_filename         = 'ic_land_phenology.nc'
    lai_site_specific   = 6.0_wp
    !first_call_phenology_this_day = .FALSE.
    ! ----------------------------------------------------------------------------------------------------- !
    ! Read namelist
    nml_handler = open_nml(TRIM(config%namelist_filename))
#ifdef __QUINCY_STANDALONE__
    nml_unit = position_nml('phenology_ctl', nml_handler, STATUS=istat)
    IF (istat == POSITIONED) READ(nml_unit, phenology_ctl)
#else
    nml_unit = position_nml('jsb_pheno_nml', nml_handler, STATUS=istat)
    IF (istat == POSITIONED) READ(nml_unit, jsb_pheno_nml)
#endif
    ! ----------------------------------------------------------------------------------------------------- !
    ! Write namelist values into config
    config%active                     = active
    config%l_forestRegrowth           = l_forestRegrowth
    config%scheme_jsbach              = TRIM(scheme)
    config%ic_filename                = ic_filename
    config%bc_filename                = bc_filename
    config%lai_max                    = lai_site_specific
    CALL close_nml(nml_handler)

    !> model-specific routines
    !>
    ! ...

  END SUBROUTINE Init_pheno_config

#endif
END MODULE mo_pheno_config_class

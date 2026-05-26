!>
!>  @par Copyright
!>    QUINCY terrestrial biosphere model with coupled carbon, nitrogen, and phosphorus cycles
!>    Copyright (C) 2019 Max Planck Institute for Biogeochemistry (MPI-BGC), Jena, Germany
!>    Contributors: Caldararu S., Engel J., Kern M., Pallandt M., Schnur R., Thum T., Yu L., Zaehle S.
!>
!>    This file is part of the QUINCY model.
!>    The QUINCY model is free software: you can redistribute it and/or modify
!>    it under the terms of the GNU General Public License version 3,
!>    as published by the Free Software Foundation.
!>
!>    The QUINCY model is distributed in the hope that it will be useful,
!>    but WITHOUT ANY WARRANTY; without even the implied warranty of
!>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!>    GNU General Public License for more details.
!>
!>    You should have received a copy of the GNU General Public License
!>    along with the QUINCY model.  If not, see <https://www.gnu.org/licenses/>.
!>
!> @brief module to determine the spitfire fire ignition
!>
!> @author
!>  Albert Jornet Puig, MPI-BGC Jena
!>
MODULE mo_dfire_sf_ignition
#ifndef __NO_JSBACH__

  USE mo_kind,                ONLY: wp
  USE mo_jsb_control,         ONLY: debug_on
  USE mo_exception,           ONLY: message

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: update_dfire_sf_ignition

  CHARACTER(len=*), PARAMETER :: modname = 'mo_dfire_sf_ignition'

CONTAINS
  
  ! ======================================================================================================= !
  !>
  !> Calculate how likely the fire will ignite
  !>
  SUBROUTINE update_dfire_sf_ignition(tile, options)
    USE mo_jsb_class,               ONLY: Get_model
    USE mo_jsb_tile_class,          ONLY: t_jsb_tile_abstract
    USE mo_jsb_task_class,          ONLY: t_jsb_task_options
    USE mo_jsb_model_class,         ONLY: t_jsb_model
    USE mo_jsb_lctlib_class,        ONLY: t_lctlib_element
    USE mo_jsb_process_class,       ONLY: DFIRE_
    dsl4jsb_Use_memory(DFIRE_)
    dsl4jsb_Use_config(DFIRE_)
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_jsb_tile_abstract), INTENT(inout)     :: tile         !< one tile with data structure for one lct
    TYPE(t_jsb_task_options),   INTENT(in)        :: options      !< model options
    ! ----------------------------------------------------------------------------------------------------- !
    TYPE(t_jsb_model),      POINTER       :: model
    TYPE(t_lctlib_element), POINTER       :: lctlib     !< land-cover-type library - parameter across pft's
    INTEGER                               :: iblk, ics, ice, nc
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':update_dfire_sf_ignition'
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_memory(DFIRE_)
    dsl4jsb_Def_config(DFIRE_)
    ! ----------------------------------------------------------------------------------------------------- !
    iblk    = options%iblk
    ics     = options%ics
    ice     = options%ice
    nc      = options%nc
    ! ----------------------------------------------------------------------------------------------------- !
    IF (.NOT. tile%Is_process_calculated(DFIRE_)) RETURN
    IF (debug_on() .AND. iblk == 1) CALL message(TRIM(routine), 'Starting on tile '//TRIM(tile%name)//' ...')
    ! ----------------------------------------------------------------------------------------------------- !
    model  => Get_model(tile%owner_model_id)
    lctlib => model%lctlib(tile%lcts(1)%lib_id) 
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_memory(DFIRE_)
    dsl4jsb_Get_config(DFIRE_)

    ! ----------------------------------------------------------------------------------------------------- !
    ! Go science
    ! ----------------------------------------------------------------------------------------------------- !

    !>  1.0 Calculate how likely a fire is about to ignite
    !>
    CALL calc_sf_ignition(dsl4jsb_Config(DFIRE_)%density_human_population, &
      &                   dsl4jsb_Config(DFIRE_)%lightning_frequency)
  END SUBROUTINE update_dfire_sf_ignition
  
  ! ======================================================================================================= !
  !>
  !> Calculate how likely a fire is about to ignite
  !>
  !> lightning caused events
  !> human caused events
  !> total (lightning + human) caused events
  !>
  SUBROUTINE calc_sf_ignition(density_human_population, lightning_frequency)
    REAL(wp), intent(in) :: density_human_population !< average human population density in the grid cell
                                                     !< [individuals km-2]
    REAL(wp), intent(in) :: lightning_frequency      !< avrg number of all lightnings [ha-1 d-1]
    ! ----------------------------------------------------------------------------------------------------- !
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':calc_sf_ignition'

    ! ----------------------------------------------------------------------------------------------------- !
    ! Go science
    ! ----------------------------------------------------------------------------------------------------- !
  END SUBROUTINE calc_sf_ignition

#endif
END MODULE mo_dfire_sf_ignition

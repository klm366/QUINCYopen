!>
!! @par Copyright
!! This code is subject to the MPI-M-Software - License - Agreement in it's most recent form.
!! Please see URL http://www.mpimet.mpg.de/en/science/models/license .
!!
!! @brief Contains the definition of variable types used across JSBACH4
!!
!! @par Revision History
!!  First version by Jan Engel (20160815); no changes compared to JSBACH4 except for not USEing mo_jsb_varlist_iface
!!  2nd   version by Jan Engel (20170927); remove operator overload functions from variables
!!  3rd   version by Jan Engel; add t_jsb_struct and t_jsb_n_struct for pool/fluxes infrastructure of QUINCY
!!
MODULE mo_jsb_var_class
#ifndef __NO_JSBACH__

  USE mo_kind,                ONLY: wp, dp
  USE mo_exception,           ONLY: finish, message
  USE mo_jsb_varlist_iface,   ONLY: VARNAME_LEN 
  USE mo_jsb_grid_class,      ONLY: t_jsb_grid, t_jsb_vgrid
  USE mo_jsb_io,              ONLY: t_cf, t_grib1, t_grib2

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: t_jsb_var_real2d, t_jsb_var_real3d, t_jsb_var, t_jsb_var_p
  PUBLIC :: REAL2D, REAL3D


  ENUM, BIND(C)
    ENUMERATOR :: REAL1D=1, REAL2D, REAL3D
  END ENUM

  !-----------------------------------------------------------------------------------------------------
  !> Abstract Variable TYPE - t_jsb_var
  !! 
  !! 
  !-----------------------------------------------------------------------------------------------------
  TYPE, ABSTRACT :: t_jsb_var
    REAL(wp), POINTER     :: ptr2d(:,:)   => NULL()
    REAL(wp), POINTER     :: ptr3d(:,:,:) => NULL()
    REAL(wp), ALLOCATABLE :: field(:,:,:,:,:)

    CHARACTER(len=VARNAME_LEN)    :: name                      ! Name of variable
    CHARACTER(len=VARNAME_LEN)    :: full_name                 ! Full name of variable in varlist, including prefix and suffix
    CHARACTER(len=:), ALLOCATABLE :: unit
    INTEGER                       :: type                      ! REAL1D / REAL2D / REAL3D
    INTEGER                       :: subset_type               ! ON_DOMAIN / ON_CHUNK
    CHARACTER(len=:), ALLOCATABLE :: element_name              ! 
    CHARACTER(len=:), ALLOCATABLE :: element_shortname         ! Short name of element
    INTEGER                       :: element_id                ! 
    INTEGER                       :: owner_model_id            ! 
    INTEGER                       :: owner_proc_id = -1        ! 
    INTEGER, ALLOCATABLE          :: owner_tile_path(:)        ! 
    INTEGER, ALLOCATABLE          :: child_idx(:)              ! Position-numbers of same variable for for the child tiles 
                                                               ! (according to mem%vars(:)).
    !! TYPE(t_subset), POINTER     :: sub  <= not needed; because it is available via: sub = var%Get_subset() !!
    REAL(dp)                      :: missval
    LOGICAL                       :: l_aggregate_all = .FALSE. ! When aggregating, consider fractions of all child tiles even
                                                  ! of those where variable doesn't exist
  END TYPE t_jsb_var

!#pragma GCC diagnostic push
!#pragma GCC diagnostic ignored "-Wsurprising"


  !-----------------------------------------------------------------------------------------------------
  !> 2D REAL Variable TYPE
  !! 
  !! ics:ice,iblk
  !-----------------------------------------------------------------------------------------------------
  TYPE, EXTENDS(t_jsb_var) :: t_jsb_var_real2d
    REAL(wp), POINTER :: ptr(:,:) => NULL()  ! associated with ptr2d(:,:)
    REAL(wp), POINTER :: ptrm1(:) => NULL()  ! points to ptr(ics:ice,iblk) updated each timestep with Associate_pointers
  CONTAINS
    FINAL     :: finalize_jsb_var_real2d
  END TYPE t_jsb_var_real2d

  !-----------------------------------------------------------------------------------------------------
  !> 3D REAL Variable TYPE
  !! 
  !! ics:ice,:,iblk
  !-----------------------------------------------------------------------------------------------------
  TYPE, EXTENDS(t_jsb_var) :: t_jsb_var_real3d
    REAL(wp), POINTER :: ptr(:,:,:) => NULL()  ! associated with ptr3d(:,:,:)
    REAL(wp), POINTER :: ptrm1(:,:) => NULL()  ! points to ptr(ics:ice,:,iblk) updated each timestep with Associate_pointers
  CONTAINS
    FINAL     :: finalize_jsb_var_real3d
  END TYPE t_jsb_var_real3d

  !-----------------------------------------------------------------------------------------------------
  !> Pointer to the abstract Variable TYPE
  !! 
  !! 
  !-----------------------------------------------------------------------------------------------------
  TYPE t_jsb_var_p
    CLASS(t_jsb_var), POINTER :: p
  END TYPE t_jsb_var_p

!!$  INTERFACE t_jsb_var
!!$    PROCEDURE :: Create_jsb_var
!!$  END INTERFACE t_jsb_var

  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_var_class'


CONTAINS

  ! ======================================================================================================= !
  !>
  !>
  SUBROUTINE finalize_jsb_var_real2d(this)

    TYPE(t_jsb_var_real2d) :: this

    CHARACTER(len=*), PARAMETER :: routine = modname//':finalize_jsb_var_real2d'

    !CALL message(routine, 'Finalizing '//TRIM(this%name))

    IF (ALLOCATED(this%owner_tile_path)) DEALLOCATE(this%owner_tile_path)
    IF (ALLOCATED(this%child_idx)) DEALLOCATE(this%child_idx)

    IF (ASSOCIATED(this%ptr)) NULLIFY(this%ptr)
    IF (ALLOCATED(this%field)) DEALLOCATE(this%field)

  END SUBROUTINE finalize_jsb_var_real2d


  ! ======================================================================================================= !
  !>
  !>
  SUBROUTINE finalize_jsb_var_real3d(this)

    TYPE(t_jsb_var_real3d) :: this

    CHARACTER(len=*), PARAMETER :: routine = modname//':finalize_jsb_var_real3d'

    !CALL message(routine, 'Finalizing '//TRIM(this%name))

    IF (ALLOCATED(this%owner_tile_path)) DEALLOCATE(this%owner_tile_path)
    IF (ALLOCATED(this%child_idx)) DEALLOCATE(this%child_idx)

    IF (ASSOCIATED(this%ptr)) NULLIFY(this%ptr)
    IF (ALLOCATED(this%field)) DEALLOCATE(this%field)

  END SUBROUTINE finalize_jsb_var_real3d

#endif
END MODULE mo_jsb_var_class

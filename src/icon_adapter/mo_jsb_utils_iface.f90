!>
!! 
!! @brief Contains methods, functions, routines, code from the ICON-MPIM host model 
!!
!! @author
!!  Jan Engel, MPI-BGC Jena
!!
!! @par Revision History
!!  created for compatibility of QUINCY standalone with QUINCY in ICON-Land
!!
MODULE mo_jsb_utils_iface

  USE mo_fortran_tools,       ONLY: assign_if_present, assign_if_present_allocatable

  IMPLICIT NONE

  PUBLIC :: assign_if_present
  PUBLIC :: assign_if_present_allocatable

  PRIVATE
  
  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_utils_iface'

ENDMODULE mo_jsb_utils_iface

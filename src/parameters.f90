!!+parameters.f90
!!
!! module: useful definitions and constants
!!
!!-
MODULE parameters
  IMPLICIT NONE

  ! numerical precision
  INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: I2B = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: I1B = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: SP  = KIND(1.0)
  INTEGER, PARAMETER :: DP  = KIND(1.d0)
  
END MODULE parameters

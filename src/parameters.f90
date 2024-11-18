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

  REAL (KIND=dp), PARAMETER    :: PI = acos(-1.d0) 
  ! Gravitational const. in m^3kg^-1s^-2, Astronomical unit in m
  REAL (KIND=dp), PARAMETER    :: G = 6.673e-11, au = 1.49598e11
  ! Solarradius in au, Solarmass in kg
  REAL (KIND=dp), PARAMETER    :: Solarradius = 0.00464912633, Solarmass = 1.98892e30

  
END MODULE parameters

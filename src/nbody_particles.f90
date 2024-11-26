!!+nbody_particles.f90
!!
!! module: provides the particle and nbodies types to
!!         be used in nbody porgrams
!!-
MODULE nbody_particles
  USE parameters
  IMPLICIT NONE
  PRIVATE

  ! data type for single particles
  TYPE :: particle
     PRIVATE
     REAL(DP)               :: mass
     REAL(DP), DIMENSION(3) :: pos
     REAL(DP), DIMENSION(3) :: vel
     REAL(DP), DIMENSION(3) :: acc
  END TYPE particle

  ! data type for an array of particles
  ! (Note: ALLOCATABLE arrays in TYPE are possible since Fortran 95) 
  TYPE :: nbodies
     PRIVATE
     TYPE(particle), DIMENSION(:), ALLOCATABLE :: bodies
  END TYPE nbodies

  INTERFACE set_body
     MODULE PROCEDURE set_body_particle, set_body_array7
  END INTERFACE

  ! public entities 
  PUBLIC :: particle, nbodies
  PUBLIC :: new_nbodies, delete_nbodies
  PUBLIC :: get_n, get_body, get_mass, get_pos, get_vel, get_total_mass
  PUBLIC :: set_body
  PUBLIC :: set_acc, get_acc, set_pos, set_vel

CONTAINS

  !
  ! get number of particles
  !
  FUNCTION get_n(this) RESULT(n)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this
    INTEGER(I4B)              :: n

    n = size(this%bodies)

  END FUNCTION get_n

  !
  ! set a particle
  !
  SUBROUTINE set_body_particle(this, idx, p)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(inout) :: this
    TYPE(particle), INTENT(in)   :: p
    INTEGER(I4B), INTENT(in)     :: idx

    this%bodies(idx) = p

  END SUBROUTINE set_body_particle

  !
  ! set a particle
  !
  SUBROUTINE set_body_array7(this, idx, p)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(7)       :: p 
    INTEGER(I4B), INTENT(in)     :: idx

    this%bodies(idx)%mass   = p(1)
    this%bodies(idx)%pos(1) = p(2)
    this%bodies(idx)%pos(2) = p(3)
    this%bodies(idx)%pos(3) = p(4)
    this%bodies(idx)%vel(1) = p(5)
    this%bodies(idx)%vel(2) = p(6)
    this%bodies(idx)%vel(3) = p(7)

  END SUBROUTINE set_body_array7


  !
  !
  !
  SUBROUTINE set_acc(this, idx, a)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(3)       :: a
    INTEGER(I4B), INTENT(in)     :: idx

    this%bodies(idx)%acc = a

  END SUBROUTINE set_acc

  !
  !
  !
  SUBROUTINE set_pos(this, idx, p)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(3)       :: p
    INTEGER(I4B), INTENT(in)     :: idx

    this%bodies(idx)%pos = p

  END SUBROUTINE set_pos

  !
  !
  !
  SUBROUTINE set_vel(this, idx, v)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(3)       :: v
    INTEGER(I4B), INTENT(in)     :: idx

    this%bodies(idx)%vel = v

  END SUBROUTINE set_vel
  !
  ! get a particle
  !
  FUNCTION get_body(this, idx) RESULT(p)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this
    TYPE(particle)            :: p
    INTEGER(I4B), INTENT(in)  :: idx

    p = this%bodies(idx)

  END FUNCTION get_body

  !
  ! get a particle mass
  !
  FUNCTION get_mass(this) RESULT(mass)
    IMPLICIT NONE
    TYPE(particle), INTENT(in) :: this
    REAL(DP)                   :: mass

    mass = this%mass

  END FUNCTION get_mass

  !
  ! get total mass
  !
  FUNCTION get_total_mass(this) RESULT(mass)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this
    REAL(DP)                   :: mass, m
    INTEGER(I4B) :: i, n

    n = get_n(this)
    m = 0.
    DO i=1, n
      m = m + get_mass(get_body(this, i))
    END DO

    mass = m

  END FUNCTION get_total_mass

  !
  ! get a particle position
  !
  FUNCTION get_pos(this) RESULT(pos)
    IMPLICIT NONE
    TYPE(particle), INTENT(in) :: this
    REAL(DP), DIMENSION(3)     :: pos

    pos = this%pos

  END FUNCTION get_pos

  !
  ! get a particle position
  !
  FUNCTION get_acc(this) RESULT(acc)
    IMPLICIT NONE
    TYPE(particle), INTENT(in) :: this
    REAL(DP), DIMENSION(3)     :: acc

    acc = this%acc

  END FUNCTION get_acc

  !
  ! get a particle velocity
  !
  FUNCTION get_vel(this) RESULT(vel)
    IMPLICIT NONE
    TYPE(particle), INTENT(in) :: this
    REAL(DP), DIMENSION(3)     :: vel

    vel = this%vel

  END FUNCTION get_vel


  ! 
  ! create body data
  !
  SUBROUTINE new_nbodies(this, n)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(out) :: this
    INTEGER(I4B), INTENT(in)   :: n

    INTEGER(I4B)               :: i
    REAL(DP), DIMENSION(3)     :: tmp

    ALLOCATE(this%bodies(n))

    tmp = 0.0_dp
    DO i=1,n
       this%bodies(i) = particle( 0.0_dp, tmp, tmp, tmp)
    END DO

  END SUBROUTINE new_nbodies

  !
  ! delete body data
  !
  SUBROUTINE delete_nbodies(this)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(inout) :: this
  
    DEALLOCATE(this%bodies)
  END SUBROUTINE delete_nbodies

END MODULE nbody_particles

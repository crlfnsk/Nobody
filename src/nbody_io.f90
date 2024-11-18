!!+nbody_io.f90
!!
!! module: provides functions to read and write nbody data, supported format
!!         is one line of header with number of particles N follow by N
!!         lines with particles data (mass, pos, vel --> 7 REALs)
!!       
!!-
MODULE nbody_io
  USE parameters
  USE nbody_particles
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: load_bodies, print_bodies

CONTAINS

  !
  ! read particles from STDIN and load them into this also allocating
  ! the required memory
  !
  SUBROUTINE load_bodies(this)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(out) :: this

    INTEGER(I4B)               :: i, n
    REAL(DP), DIMENSION(7)     :: p

    ! first input line is number of particles
    READ*, n

    CALL new_nbodies(this, n)

    ! no read n lines of particles data
    DO i=1, n
       READ*, p
       CALL set_body(this, i, p)
    END DO

  END SUBROUTINE load_bodies

  !
  ! print bodies to STDOUT 
  !
  SUBROUTINE print_bodies(this)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this

    INTEGER(I4B)              :: i, n
    TYPE(particle)            :: p

    n = get_n(this)

    PRINT*, n
    
    DO i=1,n

       p = get_body(this, i)
       PRINT*, get_mass(p), get_pos(p), get_vel(p)

    END DO

  END SUBROUTINE print_bodies

END MODULE nbody_io

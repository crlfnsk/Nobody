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

  PUBLIC :: load_bodies, print_bodies, print_acc, print_pos, print_pos2d, print_E
  

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
    
    DO i=1,n

       p = get_body(this, i)
       PRINT*, get_mass(p), get_pos(p), get_vel(p)

    END DO

  END SUBROUTINE print_bodies

  !
  ! print acceleration to STDOUT 
  !
  SUBROUTINE print_acc(this)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this

    INTEGER(I4B)              :: i, n
    TYPE(particle)            :: p

    n = get_n(this)

    PRINT*, n
    
    DO i=1,n

       p = get_body(this, i)
       PRINT*, get_acc(p)

    END DO

  END SUBROUTINE print_acc

  !
  ! print positions to STDOUT 
  !
  SUBROUTINE print_pos(this, idx, stepsize)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this

    INTEGER(I4B)              :: i, n, idx, stepsize
    TYPE(particle)            :: p
    REAL(DP), DIMENSION(3)    :: r

    n = get_n(this)
    if (mod(idx-1,stepsize) == 0) then
      write(6,"(I0, ' ')", advance="no") idx
      do i=1, n
        p = get_body(this, i)
        r = get_pos(p)
        if(i/=n) then
          write(6,"(F20.16,' ',F20.16,' ',F20.16,' ')", advance="no") r(1), r(2), r(3)
        else
          write(6,"(F20.16,' ',F20.16,' ',F20.16)", advance="no") r(1), r(2), r(3)
        endif
      end do
      write(*,*)
    endif

  END SUBROUTINE print_pos

  SUBROUTINE print_pos2d(this)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this

    INTEGER(I4B)              :: i, n
    TYPE(particle)            :: p
    REAL(DP), DIMENSION(3)    :: r

    n = get_n(this)
    do i=1, n
      p = get_body(this, i)
      r = get_pos(p)
      if(i/=n) then
        write(6,"(F20.16,',',F20.16,',')", advance="no") r(1), r(2)
      else
        write(6,"(F20.16,',',F20.16,',')", advance="no") r(1), r(2)
      endif
    end do
    write(*,*)

  END SUBROUTINE print_pos2d

  SUBROUTINE print_E(this, idx, stepsize)
    IMPLICIT NONE
    TYPE(nbodies), INTENT(in) :: this

    INTEGER(I4B)              :: n, idx, stepsize
    REAL(DP)                  :: total_energy

    OPEN(UNIT=20, file='energy.txt')
    n = get_n(this)
    if (mod(idx-1,stepsize) == 0) then
      write(20,"(I0, ' ')", advance="no") idx
      write(20, *) total_energy
    endif
    
  END SUBROUTINE print_E

END MODULE nbody_io

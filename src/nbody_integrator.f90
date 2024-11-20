!!+nbody_integrator
!!
!! program: program to integrate an nbody sytem (sofar it only reads and
!!          prints a data set
!!
!!-
PROGRAM nbody_integrator
  USE parameters
  USE nbody_particles
  USE nbody_io
  IMPLICIT NONE
  INTEGER(I4B) :: i, N = 100

  ! creates a variable for the nbody system
  TYPE(nbodies) :: system

  ! allocates memory and reads the particle information
  CALL load_bodies(system)

  ! prints the particles in system
  !CALL print_bodies(system)

  CALL update_a(system)
  
  DO i=1, N
    CALL update_r(system)
    CALL update_v(system)
    CALL print_pos(system)
  END DO


  ! free memory
  CALL delete_nbodies(system)

END PROGRAM nbody_integrator


SUBROUTINE update_a(this)
  use parameters
  use nbody_particles
  use nbody_io
  IMPLICIT NONE
  INTEGER(I4B) :: i, j, n
  TYPE(nbodies), INTENT(inout) :: this
  REAL(DP), DIMENSION(3)    :: p_i, p_j, r, a
  REAL(DP)                  :: m_j, r_ij

  n = get_n(this)

  do i = 1, n

    a = (/0,0,0/)

    do j = 1, n
      if(i /= j) then
        p_j = get_pos(get_body(this, j))
        p_i = get_pos(get_body(this, i))
        m_j = get_mass(get_body(this, i))
        r = (p_i - p_j)
        r_ij = (r(1)**2 + r(2)**2 + r(3)**2)**1.5
        a = a + (m_j / r_ij) * r
      endif
    end do

    CALL set_acc(this, i, -a)

  end do

END SUBROUTINE update_a

SUBROUTINE update_r(this)
  use parameters
  use nbody_particles
  use nbody_io
  IMPLICIT NONE
  INTEGER(I4B) :: i, n
  TYPE(nbodies), INTENT(inout) :: this
  REAL(DP), DIMENSION(3)    :: r
  REAL(DP)                  :: dt

  n = get_n(this)
  dt = 0.01

  do i = 1, n

    r = get_pos(get_body(this, i)) + get_vel(get_body(this, i))*dt + get_acc(get_body(this, i))*dt**2

    CALL set_pos(this, i, r)
  end do

END SUBROUTINE update_r

SUBROUTINE update_v(this)
  use parameters
  use nbody_particles
  use nbody_io
  IMPLICIT NONE
  INTEGER(I4B) :: i, n
  TYPE(nbodies), INTENT(inout) :: this
  TYPE(nbodies):: that
  REAL(DP), DIMENSION(3)    :: v
  REAL(DP)                  :: dt

  n = get_n(this)
  dt = 0.01

  that = this

  CALL update_a(this)

  do i = 1, n

    v = get_vel(get_body(this, i)) + (get_acc(get_body(this, i)) + get_acc(get_body(that, i)))*dt*0.5

    CALL set_vel(this, i, v)
  end do

END SUBROUTINE update_v

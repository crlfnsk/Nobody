!!+nbody_integrator_mod.f90
!!
!! module: provides subroutines for calculating dynamic of an n body system
!!         using a leapfrog integrator. 
!!  	     provides functions for calculating total mass of a system and
!!         Center of Mass position and velocity.
!!       
!!-
MODULE nbody_integrator
    USE parameters
    USE nbody_particles
    use nbody_io
    IMPLICIT NONE
    PRIVATE
  
    PUBLIC :: update_r, update_v, update_a, total_energy, norm_vec, r_CoM, v_CoM
  
  CONTAINS
 
  !
  ! calculate particle accelerations as discribed on the Lab Sheet 
  ! and update them
  !
  SUBROUTINE update_a(this)
    IMPLICIT NONE
    INTEGER(I4B) :: i, j, n
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(3)    :: p_i, p_j, r, a
    REAL(DP)                  :: m_j, r_ij
  
    n = get_n(this)
  
    do i = 1, n
  
      ! initiate accelaretion for summation
      a = (/0,0,0/)
  
      do j = 1, n
        if(i /= j) then
          p_i = get_pos(get_body(this, i))
          p_j = get_pos(get_body(this, j))
          m_j = get_mass(get_body(this, j))
  
          ! calculate accileration and accumalate 
          r = (p_i - p_j)
          r_ij = (norm_vec(r))**3
          a = a + m_j/r_ij * r
        endif
      end do
  
      ! update acceleration
      CALL set_acc(this, i, -a)
  
    end do
  
  END SUBROUTINE update_a
  
  !
  ! calculates new positions with a given precision
  SUBROUTINE update_r(this, dt)
    IMPLICIT NONE
    INTEGER(I4B) :: i, n
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(3)    :: r
    REAL(DP)                  :: dt
  
    n = get_n(this)
  
    do i = 1, n
      ! calculate positions
      r = get_pos(get_body(this, i)) + get_vel(get_body(this, i))*dt + get_acc(get_body(this, i))*dt**2*0.5
  
      ! update positions
      CALL set_pos(this, i, r)
    end do
  
  END SUBROUTINE update_r
  
  !
  ! calculate new velocities and update them
  ! 
  SUBROUTINE update_v(this, dt)
    IMPLICIT NONE
    INTEGER(I4B) :: i, n
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(:,:), ALLOCATABLE:: temp_a
    REAL(DP), DIMENSION(3)    :: v
    REAL(DP)                  :: dt
  
    n = get_n(this)
    allocate(temp_a(n, 3))
  
    ! store old acceleration data
    DO i=1, n
      temp_a(i,:) = get_acc(get_body(this, i))
    END DO
  
    ! update acceleration
    CALL update_a(this)
  
    do i = 1, n
      ! calculate new velocities using old and new accelerations
      v = get_vel(get_body(this, i)) + (get_acc(get_body(this, i)) + temp_a(i,:))*dt*0.5

      ! update velocity
      CALL set_vel(this, i, v)
    end do
  
  END SUBROUTINE update_v
  
  !
  ! function for calculating total energy of a system
  !
  FUNCTION total_energy(this) RESULT(E)
    IMPLICIT NONE
    INTEGER(I4B) :: i, j, n
    TYPE(nbodies), INTENT(inout) :: this
    REAL(DP), DIMENSION(3)    :: p_i, p_j, r
    REAL(DP) :: E, U, T, m_i, m_j
  
    n = get_n(this)
    T=0.
    U=0.
  
    !U=SUM(1<=i<j<=n) G*m1*m2/(|ri - rj|)
    DO i=1, n
      DO j=1, n
        IF (i<j) then
          p_i = get_pos(get_body(this, i))
          p_j = get_pos(get_body(this, j))
          m_i = get_mass(get_body(this, i))
          m_j = get_mass(get_body(this, j))
          r = (p_i - p_j)
          U = U - m_i*m_j / norm_vec(r)
        ENDIF
      END DO
      !T=SUM(1<i<n) |v|^2/(2*mi)
      m_i = get_mass(get_body(this, i))
      T = T + norm_vec(get_vel(get_body(this, i)))**2 / (2 * m_i)
    END DO
  
    E = T + U
  END FUNCTION total_energy
  
  !
  ! function for finding the magnitude of a vector
  !  
  FUNCTION norm_vec(vector) RESULT(norm)
    IMPLICIT NONE
    REAL(DP), DIMENSION(3)    :: vector
    REAL(DP) :: norm
  
    norm = ABS(SQRT(vector(1)**2 + vector(2)**2 + vector(3)**2))
  END FUNCTION norm_vec
  
  !
  ! function for calculating CoM position
  !
  FUNCTION r_CoM(this) RESULT(r_c)
    IMPLICIT NONE
    INTEGER(I4B) :: i, n
    TYPE(nbodies), INTENT(in) :: this
    REAL(DP) :: M
    REAL(DP), DIMENSION(3) :: r_c
    REAL(DP), DIMENSION(3):: r
  
    n = get_n(this)
  
    M = get_total_mass(this)
    r = 0.
  
    DO i=1, n
      r = r + get_mass(get_body(this, i)) * get_pos(get_body(this, i))
    END DO
  
    r_c = r/M 
    
  END FUNCTION r_CoM
  
  !
  ! function for calculating CoM velocities
  !
  FUNCTION v_CoM(this) RESULT(v_c)
    IMPLICIT NONE
    INTEGER(I4B) :: i, n
    TYPE(nbodies), INTENT(in) :: this
    REAL(DP) :: M
    REAL(DP), DIMENSION(3) :: v_c
    REAL(DP), DIMENSION(3):: v
  
    n = get_n(this)
  
    M = get_total_mass(this)
    v = 0.
  
    DO i=1, n
      v = v + get_mass(get_body(this, i)) * get_vel(get_body(this, i))
    END DO
  
    v_c = v/M 
    
  END FUNCTION v_CoM
  
  END MODULE nbody_integrator
  
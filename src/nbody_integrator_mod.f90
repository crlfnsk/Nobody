!!+nbody_io.f90
!!
!! module: provides functions to read and write nbody data, supported format
!!         is one line of header with number of particles N follow by N
!!         lines with particles data (mass, pos, vel --> 7 REALs)
!!       
!!-
MODULE nbody_integrator
    USE parameters
    USE nbody_particles
    IMPLICIT NONE
    PRIVATE
  
    PUBLIC :: update_r, update_v, update_a, total_energy, norm_vec, r_CoM, v_CoM
  
  CONTAINS
 
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
  
      ! update accelaration
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
    dt = 0.0001
  
    do i = 1, n
  
      r = get_pos(get_body(this, i)) + get_vel(get_body(this, i))*dt + get_acc(get_body(this, i))*dt**2*0.5
  
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
    REAL(DP), DIMENSION(:,:), ALLOCATABLE:: temp_a
    REAL(DP), DIMENSION(3)    :: v
    REAL(DP)                  :: dt
  
    n = get_n(this)
    allocate(temp_a(n, 3))
  
    dt = 0.0001
  
    !that = this
  
    DO i=1, n
      temp_a(i,:) = get_acc(get_body(this, i))
    END DO
  
    CALL update_a(this)
  
    do i = 1, n
  
      v = get_vel(get_body(this, i)) + (get_acc(get_body(this, i)) + temp_a(i,:))*dt*0.5
      CALL set_vel(this, i, v)
    end do
  
  END SUBROUTINE update_v
  
  
  FUNCTION total_energy(this) RESULT(E)
    use parameters
    use nbody_particles
    use nbody_io
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
  
  
  FUNCTION norm_vec(vector) RESULT(norm)
    USE nbody_particles
    USE parameters
    USE nbody_io
    IMPLICIT NONE
    REAL(DP), DIMENSION(3)    :: vector
    REAL(DP) :: norm
  
    norm = ABS(SQRT(vector(1)**2 + vector(2)**2 + vector(3)**2))
  END FUNCTION norm_vec
  
  
  FUNCTION r_CoM(this) RESULT(r_c)
    USE nbody_particles
    USE parameters
    USE nbody_io
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
  
  FUNCTION v_CoM(this) RESULT(v_c)
    USE nbody_particles
    USE parameters
    USE nbody_io
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
  
!!+nbody_integrator
!!
!! program: program to integrate an nbody sytem 
!!
!!-
PROGRAM nbody_integrator
  USE parameters
  USE nbody_particles
  USE nbody_io
  USE nbody_integrator
  IMPLICIT NONE
  INTEGER(I4B) :: i, N

  ! creates a variable for the nbody system
  TYPE(nbodies) :: system


  ! allocates memory and reads the particle information
  CALL load_bodies(system)

  ! prints the particles in system
  !CALL print_bodies(system)

  CALL update_a(system)

  N = 100
  
  DO i=1, N
    CALL update_r(system)
    CALL update_v(system)
    !CALL print_pos2d(system)
    !print*, v_CoM(system)
  END DO


  ! free memory
  CALL delete_nbodies(system)

END PROGRAM nbody_integrator



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

  ! creates a variable for the nbody system
  TYPE(nbodies) :: system

  ! allocates memory and reads the particle information
  CALL load_bodies(system)

  ! prints the particles in system
  CALL print_bodies(system)

  ! free memory
  CALL delete_nbodies(system)

END PROGRAM nbody_integrator

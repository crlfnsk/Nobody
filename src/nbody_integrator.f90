!!+nbody_integrator.f90
!!
!! Main Program to call subroutines from nbody_integrator_mod.f90 to integrate an n body system.
!! 
!!
!!-
PROGRAM nbody_integrator
  USE parameters
  USE nbody_particles
  USE nbody_io
  USE nbody_integrator
  IMPLICIT NONE
  INTEGER(I4B)  :: i, N, stepsize
  REAL(DP)      :: dt

  ! creates a variable for the nbody system
  TYPE(nbodies) :: system


  ! allocates memory and reads the particle information
  CALL load_bodies(system)

  ! prints the particles in system
  !CALL print_bodies(system)

  ! initialize acceleration
  CALL update_a(system)

  ! Number of integration staps to be made
  N = 40000
  dt = 0.00005

  ! stepsize for printing only every n_stepsize timestep
  stepsize = 1
  
  ! Loop over number of timesteps
  DO i=1, N
    ! calculate new positions and velocities
    CALL update_r(system, dt)
    CALL update_v(system, dt)

    ! printing timesteps and positions
    !write(6, "(I0, ' ')", advance='no') i     ! comment out to leave out timesteps

    CALL print_pos(system, i, stepsize)    ! prints x,y,z positions of particle
    !CALL print_pos2d(system)              ! prints x,y positions of particle

    ! printing Center of Mass (CoM) positions and velocities
    !print*, r_CoM(system)         ! prints CoM position 
    !print*, v_CoM(system)        ! prints CoM velocity
  END DO


  ! free memory
  CALL delete_nbodies(system)

END PROGRAM nbody_integrator



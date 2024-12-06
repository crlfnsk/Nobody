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
  REAL(DP)      :: t, dt, E0, error

  ! creates a variable for the nbody system
  TYPE(nbodies) :: system

  ! Number of integration steps to be made ! worked for : N =  500000 , dt = 0.0001
  READ*, t   
  READ*, dt 
  N = int(t/dt)
  ! stepsize for printing only every n_stepsize timestep
  READ*, stepsize

  ! allocates memory and reads the particle information
  CALL load_bodies(system)

  ! prints the particles in system
  !CALL print_bodies(system)

  ! initialize acceleration
  CALL update_a(system)

  ! Initialize total energy
  E0=total_energy(system)
  error=0.
  
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

    error = (total_energy(system) - E0) / E0
    !Call print_E(system, i, stepsize)
    OPEN(UNIT=20, file='energy.txt')
    WRITE(20,"(I0, ' ')", advance="no") i
    !WRITE(20, *) error
    WRITE(20, *) error
  END DO
  
  CLOSE(20)
  ! free memory
  CALL delete_nbodies(system)

END PROGRAM nbody_integrator

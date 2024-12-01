# N-body System

This repository is designed to calculate and display the evolution of an N-body system. You can set up the initial condition of the system you desire to analyze via an input file. The results are supposed to be presented as a video.

You can achieve this by executing the program in two steps as following:
$ ./nbody < {input_file} > movie/out
$ ./plot_frames.sh

This program was designed by Caro, Antonio and Carl. 

SUBROUTINE a_r(this)
  IMPLICIT NONE
  INTEGER(I4B) :: j, n
  TYPE(nbodies), INTENT(in) :: this
  TYPE(particle)            :: p_i, p_j

  do i = 1, n
    
    p_i = get_body(this, i)

    do j = 1, N
      if(i /= j) then
        p_j = get_body(this, j)
        a_i = a_i + this%bodies(j)%mass * (p_i - p_j) / 
      endif
    end do

  end do

END SUBROUTINE
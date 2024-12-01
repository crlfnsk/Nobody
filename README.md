# N-body System

This readme file is supposed to explain how to use the nbody program.

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
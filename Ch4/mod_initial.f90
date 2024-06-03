module mod_initial
    use iso_fortran_env, only : int32, real32
    implicit none

    contains

    pure subroutine set_gaussian(x, icenter_, decay_)
        real(real32), intent(in out) :: x(:)
        integer(int32), intent(in) :: icenter_
        real(real32), intent(in) :: decay_
        integer(int32) :: i
    
        do concurrent(i =1:size(x))
            x(i) = exp(-decay_ * (i - icenter_) ** 2)
        end do
    
    end subroutine set_gaussian

end module mod_initial
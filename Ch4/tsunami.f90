program tsunami

    use iso_fortran_env, only: int32, real32
    use mod_diff, only: diff => diff_centered
    use mod_initial, only: set_gaussian   

    implicit none

    integer(int32) :: n
    integer(int32), parameter :: grid_size = 100
    integer(int32), parameter :: num_time_steps = 5000
    real(real32), parameter :: dt = 0.02, dx = 1, g = 9.8
    real(real32), parameter :: hmean = 10
    real(real32) :: h(grid_size), u(grid_size)

    integer(int32), parameter :: icenter = 25
    real, parameter :: decay = 0.02
    
    
    !real :: h(grid_size), dh(grid_size) !Water height values

    if (grid_size <= 0) stop 'grid_size must be > 0'
    if (dt <= 0) stop 'time step dt must be > 0'
    if (dx <= 0) stop 'grid spacing dx must be > 0'
    !if (c <= 0) stop 'background flow speed c must be > 0'

    call set_gaussian(h, icenter, decay)
    u = 0
    ! do concurrent (i = 1:grid_size)
    !     h(i) = exp(-decay * (i - icenter)**2)
    ! end do

    print *, 0, h

    time_loop: do n = 1, num_time_steps
        
        u = u - (u * diff(u) + g * diff(h)) / dx * dt
        h = h - diff(u * (hmean + h)) / dx * dt
        print *, n, h

    end do time_loop

contains

subroutine add(a, b, res)
integer, intent(in) :: a, b
integer, intent(out) :: res
res = a + b
end subroutine add

end program tsunami

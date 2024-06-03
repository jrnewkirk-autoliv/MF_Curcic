program tsunami

    implicit none

    integer :: i, n
    integer, parameter :: grid_size = 100
    integer, parameter :: num_time_steps = 100
    integer, parameter :: icenter = 23

    real, parameter :: dt = 1, dx = 1, c =1
    real, parameter :: decay = 0.02
    real :: h(grid_size), dh(grid_size) !Water height values

    if (grid_size <= 0) stop 'grid_size must be > 0'
    if (dt <= 0) stop 'time step dt must be > 0'
    if (dx <= 0) stop 'grid spacing dx must be > 0'
    if (c <= 0) stop 'background flow speed c must be > 0'

    do concurrent (i = 1:grid_size)
        h(i) = exp(-decay * (i - icenter)**2)
    end do

    time_loop: do n = 1, num_time_steps

        h = h-c * diff(h) / dx * dt
        print *, n, h

    end do time_loop

contains

function diff(x) result(dx)
    real, intent(in) :: x(:)
    real :: dx(size(x))
    integer :: im
    im = size(x)
    dx(1) = x(1) - x(im)
    dx(2:im) = x(2:im) - x(1:im-1)
end function diff 

subroutine add(a, b, res)
integer, intent(in) :: a, b
integer, intent(out) :: res
res = a + b
end subroutine add

subroutine set_gaussian(x, icenter, decay)
    real, intent(in out) :: x(:)
    integer, intent(in) :: icenter
    integer :: i

    do concurrent(i =1:size(x))
        x(i) = exp(-decay * (i - icenter) ** 2)
    end do

end subroutine set_gaussian

end program tsunami

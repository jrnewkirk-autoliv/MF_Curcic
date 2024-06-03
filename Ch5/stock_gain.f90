
program stock_gain

    implicit none
    
    character(len=4), allocatable :: symbols(:)
    character(len=4), allocatable :: time(:)
    real, allocatable :: open(:), high(:), low(:), &
                        close(:), adjclose(:), volume(:)
    integer :: n
    
    symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ',&
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

    do n = 1, size(symbols)
        print *, 'Working on ' // symbols(n)        
    end do
    
    contains

    subroutine read_stock(filename, time, open, high,&
                        low, close, adjclose, volume)
        character(*), intent(in) :: filename
        character(:), allocatable, intent(inout) :: time(:)
        real, allocatable, intent(inout) :: open(:), &
        high(:), low(:), close(:), adjclose(:), volume(:)
    end subroutine
 end program stock_gain
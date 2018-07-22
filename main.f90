program pointfoolish
use mod_szk
use mod_grobals
use mod_pointfoolish

! Pointfoolish ver 1.00 (2018/07/10)
! created by Suzuki

implicit none
character(256) :: cinp
integer :: i = 1

!character(:), allocatable :: test(:)

lwrt = 6
lread = getInputMethod()

allocate(dgrd(1,1,1)) ! initializeGrid内部で一度deallocateするため
call initializeGrid

!call echo(toString(splitall('import grid.xyz', ' ', test)))
call echo(' ')
call echo(line('%'))
call echo('Hello! Welcome to Pointfoolish!')
call echo('Now, you have 2 × 2 = 4 points (1 m × 1 m grid)')
call echo('Is there anything I can do for you?')
call echo(' ')
call listBasicCommand

do while(fg_loop)
  call echo('Input command...')
  read(lread,'(a)') cinp

  ! backup grid

  ! parse command
  if(parseCommand(trim(cinp), i))then
    ! restore grid
    call echo('something bad has happened in command "'//trim(cinp)//'". restoring backup grid...')
    call echo('未実装')
    call echo('ok, grid has been restored.')
  endif
  i = i + 1
enddo

call echo('See you again...')
call echo(' ')
lread = releaseInputMethod()
stop
end program

program pointfoolish
use mod_szk
use mod_globals
use mod_pointfoolish

! Pointfoolish ver 1.00 (2018/07/10)
! created by Suzuki

implicit none
character(256) :: cinp
integer :: i = 1

lwrt = 5
lread = getInputMethod()

allocate(dgrd(1,1,1)) ! initializeGrid内部で一度deallocateするため
call initializeGrid

call echo('Hello! Welcome to Pointfoolish!')
call echo('Now, you have 2 × 2 = 4 points (1 m × 1 m grid)')
call echo('Is there anything I can do for you?')
call echo(' ')
call echo('Command List')
call echo('initialize  : initialize current grid and make 2 × 2 = 4 points (1 m × 1 m grid)')
call echo('command *   : read command list file and execute [*: filename]')
call echo('import *    : discard current grid and import xyz file [*: filename]')
call echo('export *    : export current grid as a xyz file [*: filename]')
call echo('help        : display other useful command')
call echo('end         : end this program')
call echo(' ')

do while(fg_loop)
  read(lread,*) cinp

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

call echo('See you, again...')
lread = releaseInputMethod()
stop
end program

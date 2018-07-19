program pointfoolish
use mod_szk
use mod_globals

implicit none

call initialize
do while(fg_loop)
  call input
enddo
call output

contains

subroutine initialize
  real(8) :: agrd(2)

  lwrt = 6
  lread = 5
  write(lwrt,*)
  fg_loop = .true.
  ljlen = 2
  lklen = 2
  allocate(dgrd(ljlen, lklen, lilen))
  dgrd = 0.d0
  dgrd(2, 2, 1) =
endsubroutine

subroutine input
endsubroutine

subroutine output
endsubroutine

end program

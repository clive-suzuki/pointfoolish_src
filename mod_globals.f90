module mod_grobals

!===========================Grid====================
  real(8), save, allocatable :: dgrd(:,:,:)
  integer, save :: ljlen, lklen
  integer, save, parameter :: lilen = 2

!===========================IO====================
  logical, save :: fg_loop
  integer :: lread, lwrt
endmodule

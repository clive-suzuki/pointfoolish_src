module mod_grobals


!===========================IO====================
  integer, save :: lread, lwrt


!===========================Grid====================
  real(8), save, allocatable :: dgrd(:,:,:)
  integer, save :: ljnum, lknum, ljbgn, ljend, lkbgn, lkend
  integer, save, parameter :: linum=2, libgn=1, liend=2



endmodule

module mod_pointfoolish
use mod_szk
use mod_grobals
implicit none

logical, save :: fg_saved = .true., fg_loop = .true.



contains

function validArg(iinplst, icrt, kidx, fg_parse)
  integer, intent(in) :: kidx, iinplst, icrt
  logical, intent(out) :: fg_parse
  logical :: validArg
  fg_parse = .false.
  validArg = .true.

  if(iinplst /= icrt)then
    call echo('Error!!! line('//toString(kidx)//') : num of arguments, '//toString(iinplst)//' differs from correct num, '//toString(icrt))
    validArg = .false.
    fg_parse = .true.
  endif
endfunction

subroutine listBasicCommand
  call echo('Basic Command List')
  call echo('initialize  : initialize current grid and make 2 × 2 = 4 points (1 m × 1 m grid)')
  call echo('command *   : read command list file and execute [*: filename]')
  call echo('import *    : discard current grid and import xyz file [*: filename]')
  call echo('export *    : export current grid as a xyz file [*: filename]')
  call echo('help        : display other useful command')
  call echo('end         : end this program')
  call echo(' ')
endsubroutine

subroutine listAdvancedCommand
  call echo('Advanced Command List')
  call echo('i: integer, r: real number, a: character')
  call echo(' ')
  call echo('info')
  call echo(' 現在の格子点の概要を表示する')
  call echo(' ')
  call echo('scale 1r 2r')
  call echo(' 全ての格子点の座標を定数倍する')
  call echo(' 1r: Xi方向の係数')
  call echo(' 2r: Eta方向の係数')
  call echo(' ')
  call echo('shift 1r 2r')
  call echo(' 全ての格子点の座標を平行移動する')
  call echo(' 1r: Xi方向の移動量')
  call echo(' 2r: Eta方向の移動量')
  call echo(' ')
  call echo('shift0 1a')
  call echo(' jbgnまたはkbgnを0に合わせるように，全ての格子点の座標を平行移動する')
  call echo(' 1a: 方向 ("j":jbgn / "k":kbgn / "jk":両方')
  call echo(' ')
  call echo('arith 1a 2r 3i')
  call echo(' jmaxまたはkmaxから等差数列的に格子点を増やす')
  call echo(' 1a: 方向 ("j":Xi方向 / "k":Eta方向')
  call echo(' 2r: 公差 (0: jmaxまたはkmaxの格子幅を公差とする)')
  call echo(' 3i: 増やす数')
  call echo(' ')
  call echo('geo 1a 2r 3r 4i')
  call echo(' jmaxまたはkmaxから等比数列的に格子点を増やす')
  call echo(' 1a: 方向 ("j":Xi方向 / "k":Eta方向')
  call echo(' 2r: 初項 (0: jmaxまたはkmaxの格子幅×公比を初項とする)')
  call echo(' 3r: 公比')
  call echo(' 4i: 増やす数')
  call echo(' ')
  call echo('trim 1i 2i 3i 4i')
  call echo(' 領域を抽出する')
  call echo(' 1i: 新しいjbgnとなるj')
  call echo(' 2i: 新しいjendとなるj')
  call echo(' 3i: 新しいkbgnとなるk')
  call echo(' 4i: 新しいkendとなるk')
  call echo(' ')
endsubroutine


function parseCommand(ecmd, kidx)
  character(*), intent(in) :: ecmd
  integer, intent(in) :: kidx
  character(:), allocatable :: cinplst(:)
  integer :: iinplst
  logical :: parseCommand
  parseCommand = .false.

  iinplst = splitall(trim(ecmd), ' ', cinplst)
  select case(cinplst(1))

! basic command=================================================================
  case('initialize')
    if(validArg(iinplst, 1, kidx, parseCommand))then
      call initializeGrid
    endif
  case('command')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      if(executeCommand(cinplst(2))) then
        call echo('Error!!! line('//toString(kidx)//') : cannot continue parsing')
        parseCommand = .true.
      endif
    endif
  case('import')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      if(importGrid(cinplst(2)))then
        call echo('Error!!! line('//toString(kidx)//') : cannot import grid')
        parseCommand = .true.
      endif
    endif
  case('export')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      if(exportGrid(cinplst(2)))then
        call echo('Error!!! line('//toString(kidx)//') : cannot export grid')
        parseCommand = .true.
      endif
    endif
  case('help')
    call help
  case('end')
    if(validArg(iinplst, 1, kidx, parseCommand))then
      call endPointfoolish
    endif
!===============================================================================


! advanced command==============================================================

  case('info')
    if(validArg(iinplst, 1, kidx, parseCommand))then
      call infoGrid
    endif
  case('scale')
    if(validArg(iinplst, 3, kidx, parseCommand))then
      call scaleGrid(toDouble(cinplst(2)),toDouble(cinplst(3)))
    endif
  case('shift')
    if(validArg(iinplst, 3, kidx, parseCommand))then
      call shiftGrid(toDouble(cinplst(2)),toDouble(cinplst(3)))
    endif
  case('shift0')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      call echo('未実装')
      stop
    endif
  case('arith')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      call echo('未実装')
      stop
    endif
  case('geo')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      call echo('未実装')
      stop
    endif
  case('trim')
    if(validArg(iinplst, 5, kidx, parseCommand))then
      if(trimGrid(toInteger(cinplst(2)),toInteger(cinplst(3)),toInteger(cinplst(4)),toInteger(cinplst(5))))then
        call echo('Error!!! line('//toString(kidx)//') : cannot trim grid')
        parseCommand = .true.
      endif
    endif
!===============================================================================

  case default
    call echo('Sorry, unknown command...')
    parseCommand = .true.
  end select
endfunction

subroutine initializeGrid
  ljbgn = 1
  ljend = 2
  lkbgn = 1
  lkend = 2
  ljnum = 2
  lknum = 2
  deallocate(dgrd)
  allocate(dgrd(ljnum, lknum, linum))
  dgrd(1,1,:) = 0.d0
  dgrd(2,2,:) = 1.d0
  dgrd(1,2,1) = 0.d0
  dgrd(1,2,2) = 1.d0
  dgrd(2,1,1) = 1.d0
  dgrd(2,1,2) = 0.d0
endsubroutine

function executeCommand(cfil)
  character(*), intent(in) :: cfil
  character(256) :: cinp
  integer :: ifil, istt, i = 1
  logical :: executeCommand
  executeCommand = .false.

  call echo('entering into '//cfil//' ...')
  ifil = open2(file=cfil, status='old', iostat=istt)
  if(istt /= 0)then
    call echo('Error!!! cannot open file. fortran error code: '//toString(istt))
    executeCommand = .true.
  else
    do
      read(ifil, *, iostat=istt) cinp
      if(istt<0) exit
      if(parseCommand(trim(cinp), i)) exit
      i = i + 1
    enddo
    ifil = close2(ifil)
  endif
  call echo('exiting from '//cfil//' ...')
endfunction

function importGrid(cfil)
  character(*), intent(in) :: cfil
  integer :: ifil, istt, j, k, i
  logical :: importGrid
  importGrid = .false.

  ifil = open2(file=cfil, status='old', form='unformatted', iostat=istt)
  if(istt /= 0)then
    call echo('Error!!! cannot open file. fortran error code: '//toString(istt))
    importGrid = .true.
  else
    deallocate(dgrd)
    read(ifil) ljnum, lknum
    ljbgn = 1
    ljend = ljnum
    lkbgn = 1
    lkend = lknum
    allocate(dgrd(ljnum, lknum, linum))
    read(ifil) (((dgrd(j, k, i), j=ljbgn, ljend), k=lkbgn, lkend), i=libgn, liend)
    ifil = close2(ifil)
  endif
endfunction

function exportGrid(cfil)
  character(*), intent(in) :: cfil
  integer :: ifil, istt, j, k, i
  logical :: exportGrid
  exportGrid = .false.

  ifil = open2(file=cfil, status='replace', form='unformatted', iostat=istt)
  if(istt /= 0)then
    call echo('Error!!! cannot open file. fortran error code: '//toString(istt))
    exportGrid = .true.
  else
    write(ifil) ljnum, lknum
    write(ifil) (((dgrd(j, k, i), j=ljbgn, ljend), k=lkbgn, lkend), i=libgn, liend)
    fg_saved = .true.
    ifil = close2(ifil)
  endif
endfunction

subroutine help
  call listBasicCommand
  call listAdvancedCommand
  call echo('↑ マウスホイールでスクロール')
  call echo(' ')
endsubroutine

subroutine endPointfoolish
  character(1) :: cbuf
  if(lread /= 5 .or. fg_saved)then
    fg_loop = .false.
  else
    call echo('Editing grid has not been saved!')
    call echo('Discard grid and end this program right now? (y/n)')
    read(lread,*) cbuf
    if(cbuf=='y') fg_loop = .false.
  endif
endsubroutine

subroutine infoGrid
  call echo('points        : '//toString(ljnum)//' × '//toString(lknum)//' = '//toString(lknum*ljnum))
  call echo('coord at jbgn : '//toString(dgrd(ljbgn,1,1))//' m')
  call echo('              : '//toString(dgrd(ljbgn,1,1)*1.d6)//' um')
  call echo('coord at kbgn : '//toString(dgrd(1,lkbgn,2))//' m')
  call echo('              : '//toString(dgrd(1,lkbgn,2)*1.d6)//' um')
  call echo('coord at jend : '//toString(dgrd(ljend,1,1))//' m')
  call echo('              : '//toString(dgrd(ljend,1,1)*1.d6)//' um')
  call echo('coord at kend : '//toString(dgrd(1,lkend,2))//' m')
  call echo('              : '//toString(dgrd(1,lkend,2)*1.d6)//' um')
endsubroutine

subroutine scaleGrid(rj, rk)
  real(8), intent(in) :: rj, rk
  integer :: j, k

  do k=lkbgn, lkend
    do j=ljbgn, ljend
      dgrd(:,:,1) = dgrd(:,:,1) * rj
      dgrd(:,:,2) = dgrd(:,:,2) * rk
    enddo
  enddo
endsubroutine

subroutine shiftGrid(rj, rk)
  real(8), intent(in) :: rj, rk
  integer :: j, k

  do k=lkbgn, lkend
    do j=ljbgn, ljend
      dgrd(:,:,1) = dgrd(:,:,1) + rj
      dgrd(:,:,2) = dgrd(:,:,2) + rk
    enddo
  enddo
endsubroutine

function trimGrid(kjbgn, kjend, kkbgn, kkend)
  integer, intent(in) :: kjbgn, kjend, kkbgn, kkend
  integer :: i, j, k, ijcnt, ikcnt, ijnum, iknum
  real(8), allocatable :: agrd(:,:,:)
  logical :: trimGrid
  trimGrid = .false.

  if(kjbgn<ljbgn .or. kkbgn<lkbgn .or. kjend<ljend .or. kkend>lkend)then
    call echo('Error!!! grid number is invalid')
    trimGrid = .true.
  else
    ijcnt = sign(1, kjend - kjbgn)
    ikcnt = sign(1, kkend - kkbgn)
    ijnum = ijcnt * (kjend - kjbgn) + 1
    iknum = ikcnt * (kkend - kkbgn) + 1
    allocate(agrd(ijnum, iknum, linum))
    do i=libgn, liend
      do k=kkbgn, kkend, ikcnt
        do j=kjbgn, kjend, ijcnt
          agrd((j-kjbgn)*ijcnt+1, (k-kkbgn)*ikcnt+1, i) = dgrd(j, k, i)
        enddo
      enddo
    enddo
    deallocate(dgrd)
    allocate(dgrd(ijnum, iknum, linum))
    dgrd = agrd
  endif
endfunction


endmodule

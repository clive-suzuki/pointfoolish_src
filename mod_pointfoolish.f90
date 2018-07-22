module pointfoolish
implicit none

logical, save :: fg_saved = .true., fg_loop = .true.



contains

function validArg(iinplst, icrt, kidx, fg_parse)
  integer, intent(in) :: kidx, iinplst, icrt
  logical, intent(out) :: fg_parse = .false.
  logical :: validArg = .true.

  if(iinplst /= icrt)then
    call echo('Error!!! line('//toString(kidx)//') : invalid arguments')
    valArg = .false.
    fg_parse = .true.
  endif
endfunction

function parseCommand(ecmd, kidx)
  character(*), intent(in) :: ecmd
  integer, intent(in) :: kidx
  character(:), allocatable :: cinplst(:)
  integer :: iinplst
  logical :: parseCommand = .false.

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
      if(scaleGrid(cinplst(2)))then
        call echo('Error!!! line('//toString(kidx)//') : cannot scale grid')
        parseCommand = .true.
      endif
    endif
  case('shift')
    if(validArg(iinplst, 3, kidx, parseCommand))then
      call echo('未実装')
      stop
    endif
  case('shift0')
    if(validArg(iinplst, 2, kidx, parseCommand))then
      call echo('未実装')
      stop
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
  logical :: executeCommand = .false.

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
  logical :: importGrid = .false.
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
    do
      read(ifil) (((dgrd(j, k, i), j=ljbgn, ljend), k=lkbgn, lkend), i=libgn, liend)
    enddo
    ifil = close2(ifil)
  endif
endfunction

function exportGrid(cfil)
  character(*), intent(in) :: cfil
  integer :: ifil, istt, j, k, i
  logical :: exportGrid = .false.
  ifil = open2(file=cfil, status='replace', form='unformatted', iostat=istt)
  if(istt /= 0)then
    call echo('Error!!! cannot open file. fortran error code: '//toString(istt))
    importGrid = .true.
  else
    write(ifil) ljnum, lknum
    do
      write(ifil) (((dgrd(j, k, i), j=ljbgn, ljend), k=lkbgn, lkend), i=libgn, liend)
    enddo
    fg_saved = .true.
    ifil = close2(ifil)
  endif
endfunction

subroutine help
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
  call echo('coord at jbgn : '//toString(dgrd(ljbgn))//' m')
  call echo('              : '//toString(dgrd(ljbgn)*1.d6)//' um')
  call echo('coord at kbgn : '//toString(dgrd(lkbgn))//' m')
  call echo('              : '//toString(dgrd(lkbgn)*1.d6)//' um')
  call echo('coord at jend : '//toString(dgrd(ljend))//' m')
  call echo('              : '//toString(dgrd(ljend)*1.d6)//' um')
  call echo('coord at kend : '//toString(dgrd(lkend))//' m')
  call echo('              : '//toString(dgrd(lkend)*1.d6)//' um')
endsubroutine

endmodule

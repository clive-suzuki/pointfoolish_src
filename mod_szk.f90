module mod_szk
!ver 1.01
!    180719

! 定数===============================================================
  integer, parameter :: lszk_FullUnitList   = 100000 ! lszkuseuniが満席
  character(*), parameter :: bszk_FindList = '_bszk_FindList_szk_mod.txt'
  integer, parameter :: lszk_UsedUnitMax = 50, lszk_UsedUnitMin = 30


! Global変数めいめいきそく===========================================
! 1文字目    : l-整数，d-実数(8)，s-実数(4)，b-文字列
! 2-4文字目  : szk
! 5文字目以降: fnd-find
!              lst-list
!              max-max, min-min
!              red-read
!              uni-unit, use-used
!====================================================================
  integer, save, private :: lszkuseuni(lszk_UsedUnitMax)
  integer, save, private :: lszkred = 5


  interface toString
    module procedure toStringK, toStringF, toStringD
  endinterface


contains

  ! atrim==================
  !** 文字列の左右の空白を削除
  ! * a      文字列
  ! * return トリム後の文字列
  !
  ! "  abc def   " => "abc def"
  !========================
  function atrim(a)
    character(*), intent(in) :: a
    character(:), allocatable :: atrim
    character(:), allocatable :: buf
    allocate(character(len(a)) :: buf)
    buf = adjustl(a)
    allocate(character(len_trim(buf)) :: atrim)
    atrim = trim(buf)
  endfunction

  ! toString alias toStringK===============
  !** 整数を文字列へ
  ! * i      整数
  ! * return 文字列
  !========================
  function toStringK(k)
    integer, intent(in) :: k
    character(:), allocatable :: toStringK
    character(20) :: str
    write(str, *) k
    str = atrim(str)
    allocate(character(len_trim(str)) :: toStringK)
    toStringK = trim(str)
  endfunction

  ! toString alias toStringF===============
  !** 実数を文字列へ
  ! * f      実数
  ! * return 文字列
  !========================
  function toStringF(f)
    real(4), intent(in) :: f
    character(:), allocatable :: toStringF
    character(40) :: str
    write(str, *) f
    str = atrim(str)
    allocate(character(len_trim(str)) :: toStringF)
    toStringF = trim(str)
  endfunction

  ! toString alias toStringD===============
  !** 実数を文字列へ
  ! * f      実数
  ! * return 文字列
  !========================
  function toStringD(d)
    real(8), intent(in) :: d
    character(:), allocatable :: toStringD
    character(40) :: str
    write(str, *) d
    str = atrim(str)
    allocate(character(len_trim(str)) :: toStringD)
    toStringD = trim(str)
  endfunction

  ! toInteger==============
  !** 文字列を整数へ
  ! * a      文字列
  ! * return 整数
  !========================
  function toInteger(a)
    character(*), intent(in) :: a
    integer :: toInteger
    read(a, *) toInteger
  endfunction

  ! toDouble==============
  !** 文字列を実数へ
  ! * a      文字列
  ! * return 実数
  !========================
  function toDouble(a)
    character(*), intent(in) :: a
    real(8) :: toDouble
    read(a, *) toDouble
  endfunction

  ! countstr===============
  !** 文字列検索
  ! * a      テキスト全文
  ! * s      検索文字列
  ! * an     $aのバイト数
  ! * sn     $sのバイト数
  ! * return $aの中にある$sの数
  !========================
  function countstr(a, s)
    character(*), intent(in) :: a, s
    integer :: ls, i , ii
    integer :: countstr
    ls = len(s)
    countstr = 0
    ii = len(a) - ls + 1
    do i=1, ii
      if(a(i:i+ls-1) == s) countstr = countstr + 1
    enddo
  endfunction

  ! split===============
  !** 文字列分割
  ! * a        テキスト全文(trim済を渡すこと！)
  ! * s        検索文字列
  ! * idx      何文字目から検索か
  ! * idx(out) 次は何文字目から検索すべきか(0: 終端)
  ! * return   $aの$idx文字目から，$sが現れる直前or終端までの文字列(未trim)
  !========================
  function split(a, s, idx)
    character(*), intent(in) :: a, s
    character(:), allocatable:: split
    integer, intent(inout) :: idx
    integer :: n
    integer :: i, ii, ls, la
    la = len(a)
    ls = len(s)
    ii = la - ls + 1
    i = idx
    do
      if(a(i:i+ls-1)==s)then
        allocate(character(i-idx) :: split)
        split = a(idx:i-1)
        idx = i + ls
        if(idx > la) idx = 0
        exit
      else if(i == ii)then
        allocate(character(la-idx+1) :: split)
        split = a(idx:la)
        idx = 0
        exit
      endif
      i = i + 1
    enddo
  endfunction

  function splitall(a, s, ret , kmax)
    character(*), intent(in) :: a, s
    character(:), allocatable:: ret(:)
    integer :: splitall
    integer, intent(in), optional :: kmax
    integer :: imax
    integer :: ispllen, iidx, i
    if(present(kmax))then
      imax = kmax
    else
      imax = len(a)
    endif
    ispllen = countstr(a, s) + 1
    allocate(character(imax) :: ret(ispllen))
    iidx = 1
    i = 1
    do while(iidx /= 0)
      ret(i) = split(a, s, iidx)
      i = i + 1
    enddo
    splitall = ispllen
  endfunction


  ! deleteSpace==================
  !** 文字列中のスペースを削除
  ! * estr   文字列
  ! * return スペースを削除した文字列
  !
  ! "  abc def   " => "abcdef"
  !========================
  function deleteSpace(estr)
    character(*), intent(in) :: estr
    character(:), allocatable :: deleteSpace
    character(:), allocatable :: sbuf
    character(1), parameter :: cspc = ' '
    integer :: ilen1,ilen2 , i1, i2
    ilen1 = len(estr)
    ilen2 = ilen1 - countstr(estr, cspc)
    allocate(character(ilen2) :: sbuf)
    sbuf = ''
    i2 = 1
    do i1=1, ilen2
      if(estr(i1:i1) /= cspc)then
        sbuf = trim(sbuf) // estr(i1:i1)
        i2 = i2 + 1
      endif
    enddo
    allocate(character(len_trim(sbuf)) :: deleteSpace)
    deleteSpace = trim(sbuf)
  endfunction

  ! openFileListStream=====
  !** 名称変更予定，コマンドの出力結果ファイルのストリームを開く
  ! * command   コマンド
  ! * return    ファイルハンドル（必ず閉じること！）
  !========================
  function openFileListStream(command)
    character(*), intent(in), optional :: command
    integer :: openFileListStream
    if(present(command)) then
      call system( command // ' > ' // bszk_FindList)
    else
      call system('ls > ' // bszk_FindList)
    endif
    openFileListStream = open2(bszk_FindList, 'old', 'formatted')
  endfunction

  ! closeFileListStream=====
  !** 名称変更予定，コマンドの出力結果ファイルのストリームを閉じる
  ! * hfile   ファイルハンドル
  ! * return  ファイルハンドル（hfileと同じ）
  !========================
  function closeFindListStream(hfile)
    integer, intent(in) :: hfile
    integer :: closeFindListStream
    closeFindListStream = close2(hfile)
    call system('rm ' // bszk_FindList)
  endfunction

  ! lock2==================
  !** 空いているUnit番号を確保（必ず返却すること）
  ! * return  確保したUnit番号（空きがなかったら0）
  !========================
  function lock2()
    integer :: lock2
    integer :: i
    lock2 = 0
    do i=1, lszk_UsedUnitMax
      if(lszkuseuni(i)==0)then
        lszkuseuni(i) = 1
        lock2 = lszk_UsedUnitMin + i
        return
      endif
    enddo
    write(6,*) 'ERROR MOD_SZK!! : '//toString(lszk_FullUnitList)
  endfunction

  ! release2==================
  !** 確保しているUnit番号を返却
  ! * unit    確保しているUnit番号
  ! * return  確保していたUnit番号（unitと同じ）
  !========================
  function release2(unit)
    integer, intent(in) :: unit
    integer :: release2
    if(unit > lszk_UsedUnitMin) then
      release2 = unit
      lszkuseuni(unit - lszk_UsedUnitMin) = 0
    endif
  endfunction

  ! open2==================
  !** 空いているUnit番号を確保し，それでファイルを開く（必ず返却すること）
  ! * file    ファイル名
  ! * status  (optional, default:'unknown') ファイルを開く条件（公式ガイド参照）
  ! * form    (optional, default:'formatted') ファイルの書式
  ! * iostat  (optional) オープンに成功したら0，失敗したら正の値（エラーコード）
  ! * return  確保したUnit番号（空きがなかったら0）
  !========================
  function open2(file , status, form, iostat)
    integer :: open2
    character(*), intent(in) :: file
    character(*), intent(in), optional :: status, form
    integer, intent(out), optional :: iostat
    character(30) :: cstt = 'unknown', cfrm = 'formatted'
    integer :: istt

    open2 = lock2()
    if(present(status)) cstt = status
    if(present(form)) cfrm = form
    if(open2/=0)then
      open(unit=open2, file=file, status=cstt, form=cfrm, iostat=istt)
      if(istt > 0)then
        open2 = release2(open2)
        open2 = 0
      endif
    else
      istt = lszk_FullUnitList
    endif
    if(present(iostat)) iostat = istt
  endfunction

  ! close2==================
  !** ファイルを閉じ，確保しているUnit番号を返却
  ! * unit    確保しているUnit番号
  ! * return  確保していたUnit番号（unitと同じ）
  !========================
  function close2(unit)
    integer, intent(in) :: unit
    integer :: close2
    close(unit)
    close2 = release2(unit)
  endfunction

  ! getInputMethod==================
  !** コマンドライン引数が存在していれば，そのファイルを開く
  ! * kidx    (optional, default:1) 何番目の引数か
  ! * return  引数があればUnit番号，なければ5
  !========================
  function getInputMethod(kidx)
    integer, intent(in), optional :: kidx
    integer :: iidx = 1, ichrlen, ierr
    character(:), allocatable :: cfil

    if(present(kidx)) iidx = kidx
    if(iidx > command_argument_count())then
      lszkred = 5
    else
      call get_command_argument(iidx, length=ichrlen)
      allocate(character(ichrlen)::cfil)
      call get_command_argument(iidx, cfil)
      lszkred = open2(file=cfil, status='old', iostat=ierr)
      if(ierr /= 0)then
        lszkred = releaseInputMethod()
      endif
    endif
    getInputMethod = lszkred
  endfunction

  function releaseInputMethod()
    if(lszkred /= 5) lszkred = close2(lszkred)
    lszkred = 5
    releaseInputMethod = lszkred
  endfunction

  subroutine echo(earg, kuni)
    character(*), intent(in) :: earg
    integer, intent(in), optional :: kuni
    integer :: iuni = 6

    if(present(kuni)) iuni = kuni
    write(iuni,*) earg
  endsubroutine

  function line(elin, knum)
    character(*), intent(in) :: elin
    integer, intent(in), optional :: knum
    integer :: inum = 64, i
    character(:), allocatable :: line
    if(present(knum)) inum = knum
    allocate(character(inum*len(elin))::line)
    kine = ''
    do i=1, inum
      line = trim(line) // elin
    enddo
  endfunction

end module mod_szk

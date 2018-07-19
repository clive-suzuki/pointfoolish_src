module mod_szk
!ver 1.01
!    180719


  character(*), parameter, private :: findlist = '_findlist_szk_mod.txt'
  integer, parameter, private :: usedunitmax = 50
  integer, private :: usedunit(usedunitmax)

  interface operator(+)
    module procedure &
      & operator_real8Array_real8_pls &
      & operator_intArray_int_pls
  endinterface
  interface operator(.of.)
    module procedure &
      & operator_real8Array_of &
      & operator_intArray_of
  endinterface


  !class intArray
    type intArray
      integer, allocatable :: data(:)
      integer :: size=0, pointer=1
    endtype

    function operator_intArray_int_pls(a, b) result(ret)
      type(intArray), intent(in) :: a
      integer, intent(in) :: b
      type(intArray) :: ret
      integer, allocatable :: buf(:)
      integer :: i, inewsiz, ioldsiz
      ioldsiz = a%size
      if(ioldsiz <= a%pointer)then
        inewsiz = ioldsiz + 100
        allocate(buf(inewsiz))
        do i=1, ioldsiz
          buf(i) = a%data(i)
        enddo
        if(ioldsiz /= 0) deallocate(a%data)
        allocate(a%data(inewsiz))
        do i=1, ioldsiz
          a%data(i) = buf(i)
        enddo
        ioldsiz = inewsiz
        a%size = ioldsiz
      endif
      a%data(a%pointer) = b
      a%pointer = a%pointer + 1
    endfunction

    function operator_intArray_of(a, b) result(ret)
      integer, intent(in) :: a
      type(intArray), intent(in) :: b
      integer :: ret
      ret = b%data(a)
    endfunction
  !endclass

  !class real8Array
    type real8Array
      real(8), allocatable :: data(:)
      integer :: size=0, pointer=1
    endtype

    function operator_real8Array_real8_pls(a, b) result(ret)!====
      type(real8Array), intent(in) :: a           !====
      real(8), intent(in) :: b                    !====
      type(real8Array) :: ret                     !====
      real(8), allocatable :: buf(:)              !====
      integer :: i, inewsiz, ioldsiz
      ioldsiz = a%size
      if(ioldsiz <= a%pointer)then
        inewsiz = ioldsiz + 100
        allocate(buf(inewsiz))
        do i=1, ioldsiz
          buf(i) = a%data(i)
        enddo
        if(ioldsiz /= 0) deallocate(a%data)
        allocate(a%data(inewsiz))
        do i=1, ioldsiz
          a%data(i) = buf(i)
        enddo
        ioldsiz = inewsiz
        a%size = ioldsiz
      endif
      a%data(a%pointer) = b
      a%pointer = a%pointer + 1
    endfunction

    function operator_real8Array_of(a, b) result(ret)!====
      integer, intent(in) :: a
      type(real8Array), intent(in) :: b              !====
      real(8) :: ret                                 !====
      ret = b%data(a)
    endfunction
  !endclass

  !class stringArray
    type stringArray
      character(:), allocatable :: data(:)
      integer, allocatable :: length(:)
      integer :: size=0, pointer=1, maxlength=0
    endtype

    function operator_stringArray_string_pls(a, b) result(ret)
      type(stringArray), intent(in) :: a          !====
      character(*), intent(in) :: b               !====
      type(stringArray) :: ret                    !====
      character(:), allocatable :: buf(:)         !====
      integer, allocatable :: ibuf(:)
      integer :: i, inewsiz, ioldsiz, inewlen, ioldlen, ipnt
      integer :: iblen
      logical :: fg_exp
      ioldsiz = a%size
      ioldlen = a%maxlength
      iblen = len(b)
      if(ioldsiz <= a%pointer)then
        inewsiz = ioldsiz + 100
        fg_exp = .true.
      else
        inewsiz = ioldsiz
      endif
      if(ioldlen < iblen)then
        inewlen = iblen
        fg_exp = .true.
      else
        inewlen = ioldlen
      endif
      if(fg_exp)then
        allocate(character(inewlen) :: buf(inewsiz), ibuf(inewsiz))
        do i=1, ioldsiz
          buf(i) = a%data(i)
          ibuf(i) = a%length(i)
        enddo
        if(ioldsiz /= 0) deallocate(a%data)
        allocate(character(inewlen) :: a%data(inewsiz), a%length(inewsiz))
        do i=1, ioldsiz
          a%data(i) = buf(i)
          a%length(i) = ibuf(i)
        enddo
        a%size = inewsiz
        a%maxlength = inewlen
      endif
      ipnt = a%pointer
      a%data(ipnt) = b
      a%length(ipnt) = iblen
      a%pointer = ipnt + 1
    endfunction

    function operator_stringArray_of(a, b) result(ret)
      integer, intent(in) :: a
      type(stringArray), intent(in) :: b
      character(:), allocatable :: ret                        !====
      allocate(character(b%length(a)) :: ret)
      ret = b%data(a)
    endfunction
  !endclass

  interface toString
    module procedure toStringK, toStringF
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
    character(20) :: str
    write(str, *) f
    str = atrim(str)
    allocate(character(len_trim(str)) :: toStringF)
    toStringF = trim(str)
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
      call system( command // ' > ' // findlist)
    else
      call system('ls > ' // findlist)
    endif
    openFileListStream = open2(findlist, 'old', 'formatted')
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
    call system('rm ' // findlist)
  endfunction

  ! lock2==================
  !** 空いているUnit番号を確保（必ず返却すること）
  ! * return  確保したUnit番号（空きがなかったら0）
  !========================
  function lock2()
    integer :: lock2
    integer :: i
    lock2 = 0
    do i=1, usedunitmax
      if(usedunit(i)==0)then
        usedunit(i) = 1
        lock2 = 20 + i
        return
      endif
    enddo
    write(6,*) 'ERROR MOD_SZK !! Cannot open file!'
  endfunction

  ! release2==================
  !** 確保しているUnit番号を返却
  ! * unit    確保しているUnit番号
  ! * return  確保していたUnit番号（unitと同じ）
  !========================
  function release2(unit)
    integer, intent(in) :: unit
    integer :: release2
    release2 = unit
    usedunit(unit - 20) = 0
  endfunction

  ! open2==================
  !** 空いているUnit番号を確保し，それでファイルを開く（必ず返却すること）
  !   Optional引数関連のバグがあるため，すべて指定することを推奨
  ! * file    ファイル名
  ! * status
  ! * form
  ! * return  確保したUnit番号（空きがなかったら0）
  !========================
  function open2(file , status, form)!lockに統合？
    integer :: open2
    character(len=*), intent(in) :: file
    character(len=*), intent(in), optional :: status, form
    open2 = lock2()
    if(open2/=0)then
      open(unit=open2, file=file, status=status, form=form)
      return
    endif
    write(6,*) 'ERROR MOD_SZK !! Cannot open file!'
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

end module mod_szk

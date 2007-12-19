" ZoomWin:	Brief-like ability to zoom into/out-of a window
" Author:	Charles Campbell
"			original version by Ron Aaron
" Date:		Apr 07, 2006
" Version:	22
" History: see :help zoomwin-history {{{1
" GetLatestVimScripts: 508 1 :AutoInstall: ZoomWin.vim

" ---------------------------------------------------------------------
" Load Once: {{{1
if &cp || exists("g:loaded_ZoomWin")
 finish
endif
let s:keepcpo        = &cpo
let g:loaded_ZoomWin = "v22"
set cpo&vim
"DechoTabOn

" ---------------------------------------------------------------------
"  Public Interface: {{{1
if !hasmapto("<Plug>ZoomWin")
 nmap <unique> <c-w>o  <Plug>ZoomWin
endif
nnoremap <silent> <script> <Plug>ZoomWin :set lz<CR>:silent call ZoomWin()<CR>:set nolz<CR>
com! ZoomWin :set lz|silent call ZoomWin()|set nolz

au VimLeave * call <SID>CleanupSessionFile()

" ---------------------------------------------------------------------
" ZoomWin: toggles between a single-window and a multi-window layout {{{1
"          The original version was by Ron Aaron.
fun! ZoomWin()
"  let g:decho_hide= 1		"Decho
"  call Dfunc("ZoomWin() winbufnr(2)=".winbufnr(2))

  " if the vim doesn't have +mksession, only a partial zoom is available {{{2
  if !has("mksession")
   if !exists("s:partialzoom")
    echomsg "missing the +mksession feature; only a partial zoom is available"
	let s:partialzoom= 0
   endif
   if v:version < 630
   	echoerr "***sorry*** you need an updated vim, preferably with +mksession"
   elseif s:partialzoom
   	" partial zoom out
	let s:partialzoom= 0
	exe s:winrestore
   else
   	" partial zoom in
	let s:partialzoom= 1
	let s:winrestore = winrestcmd()
	res
   endif
"  call Dret("ZoomWin : partialzoom=".s:partialzoom)
   return
  endif

  " Close certain windows {{{2
  call s:ZoomWinPreserve(0)

  " save options.  Force window minimum height/width to be >= 1 {{{2
  let keep_hidden = &hidden
  let keep_write  = &write

  if v:version < 603
   if &wmh == 0 || &wmw == 0
    let keep_wmh = &wmh
    let keep_wmw = &wmw
    silent! set wmh=1 wmw=1
   endif
  endif
  set hidden write

  if winbufnr(2) == -1
    " there's only one window - restore to multiple-windows mode {{{2
"	call Decho("there's only one window - restore to multiple windows")

    if exists("s:sessionfile") && filereadable(s:sessionfile)
	  " save position in current one-window-only
"	  call Decho("save position in current one-window-only in sponly")
      let sponly     = s:SavePosn(0)
      let s:origline = line(".")
      let s:origcol  = virtcol(".")

      " source session file to restore window layout
	  let ei_keep= &ei
	  set ei=all
      exe 'silent! so '.s:sessionfile
"	  Decho("@@<".@@.">")
      let v:this_session= s:sesskeep

      if exists("s:savedposn1")
        " restore windows' positioning and buffers
"		call Decho("restore windows, positions, buffers")
        windo call s:RestorePosn(s:savedposn{winnr()})|unlet s:savedposn{winnr()}
        call s:GotoWinNum(s:winkeep)
        unlet s:winkeep
      endif

	  if line(".") != s:origline || virtcol(".") != s:origcol
	   " If the cursor hasn't moved from the original position,
	   " then let the position remain what it was in the original
	   " multi-window layout.
"	   call Decho("restore position using sponly")
       call s:RestorePosn(sponly)
	  endif

	  " delete session file and variable holding its name
"	  call Decho("delete session file")
      call delete(s:sessionfile)
      unlet s:sessionfile
	  let &ei=ei_keep
    endif

  else " there's more than one window - go to only-one-window mode {{{2
"	call Decho("there's multiple windows - goto one-window-only")

    let s:winkeep    = winnr()
    let s:sesskeep   = v:this_session

	" doesn't work with the command line window (normal mode q:)
	if &bt == "nofile" && expand("%") == "command-line"
	 echoerr "***error*** ZoomWin doesn't work with the command line window"
"     call Dret("ZoomWin : commandline window error")
	 return
	endif
"	call Decho("1: @@<".@@.">")

	" disable all events (autocmds)
"	call Decho("disable events")
    let ei_keep= &ei
	set ei=all
"	call Decho("2: @@<".@@.">")

    " save window positioning commands
"	call Decho("save window positioning commands")
    windo let s:savedposn{winnr()}= s:SavePosn(1)
    call s:GotoWinNum(s:winkeep)

    " set up name of session file
"	call Decho("3: @@<".@@.">")
    let s:sessionfile= tempname()
"	call Decho("4: @@<".@@.">")

    " save session
"	call Decho("save session")
    let ssop_keep = &ssop
    let &ssop     = 'blank,help,winsize'
"	call Decho("5: @@<".@@.">")
    exe 'mksession! '.s:sessionfile
"	call Decho("6: @@<".@@.">")
	let keepyy= @@
	let keepy0= @0
	let keepy1= @1
	let keepy2= @2
	let keepy3= @3
	let keepy4= @4
	let keepy5= @5
	let keepy6= @6
	let keepy7= @7
	let keepy8= @8
	let keepy9= @9
    set lz ei=all bh=
	if v:version >= 700
     exe "keepalt keepmarks new! ".s:sessionfile
     silent! keepjumps keepmarks v/wincmd\|split\|resize/d
     keepalt w!
     keepalt bw!
	else
     exe "new! ".s:sessionfile
     v/wincmd\|split\|resize/d
     w!
     bw!
    endif
	let @@= keepyy
	let @0= keepy0
	let @1= keepy1
	let @2= keepy2
	let @3= keepy3
	let @4= keepy4
	let @5= keepy5
	let @6= keepy6
	let @7= keepy7
	let @8= keepy8
	let @9= keepy9
"	call Decho("7: @@<".@@.">")

    " restore user's session options and restore event handling
"	call Decho("restore user session options and event handling")
    set nolz
    let &ssop = ssop_keep
    silent! only!
"	call Decho("8: @@<".@@.">")
    let &ei   = ei_keep
    echomsg expand("%")
"	call Decho("9: @@<".@@.">")
  endif

  " restore user option settings {{{2
"  call Decho("restore user option settings")
  let &hidden= keep_hidden
  let &write = keep_write
  if v:version < 603
   if exists("keep_wmw")
    let &wmh= keep_wmh
    let &wmw= keep_wmw
   endif
  endif

  " Re-open certain windows {{{2
  call s:ZoomWinPreserve(1)

"  call Dret("ZoomWin")
endfun

" ---------------------------------------------------------------------
" SavePosn: this function sets up a savedposn variable that {{{1
"          has the commands necessary to restore the view
"          of the current window.
fun! s:SavePosn(savewinhoriz)
"  call Dfunc("SavePosn(savewinhoriz=".a:savewinhoriz.") file<".expand("%").">")
  let swline    = line(".")
  if swline == 1 && getline(1) == ""
   " empty buffer
   let savedposn= "silent b ".winbufnr(0)
"   call Dret("SavePosn savedposn<".savedposn.">")
   return savedposn
  endif
  let swcol     = col(".")
  let swwline   = winline()-1
  let swwcol    = virtcol(".") - wincol()
  let savedposn = "silent b ".winbufnr(0)."|".swline."|silent norm! z\<cr>"
  if swwline > 0
   let savedposn= savedposn.":silent norm! ".swwline."\<c-y>\<cr>:silent norm! zs\<cr>"
  endif
  let savedposn= savedposn.":silent call cursor(".swline.",".swcol.")\<cr>"

  if a:savewinhoriz
   if swwcol > 0
    let savedposn= savedposn.":silent norm! ".swwcol."zl\<cr>"
   endif

   " handle certain special settings for the multi-window savedposn call
   "   bufhidden buftype buflisted
   let settings= ""
   if &bh != ""
   	let settings="bh=".&bh
	setlocal bh=hide
   endif
   if !&bl
   	let settings= settings." nobl"
	setlocal bl
   endif
   if &bt != ""
   	let settings= settings." bt=".&bt
	setlocal bt=
   endif
   if settings != ""
   	let savedposn= savedposn.":setlocal ".settings."\<cr>"
   endif

  endif
"  call Dret("SavePosn savedposn<".savedposn.">")
  return savedposn
endfun

" ---------------------------------------------------------------------
" s:RestorePosn: this function restores noname and scratch windows {{{1
fun! s:RestorePosn(savedposn)
"  call Dfunc("RestorePosn(savedposn<".a:savedposn.">) file<".expand("%").">")
  exe a:savedposn
"  call Dret("RestorePosn")
endfun

" ---------------------------------------------------------------------
" CleanupSessionFile: if you exit Vim before cleaning up the {{{1
"                     supposed-to-be temporary session file
fun! s:CleanupSessionFile()
"  call Dfunc("CleanupSessionFile()")
  if exists("s:sessionfile") && filereadable(s:sessionfile)
"   call Decho("sessionfile exists and is readable; deleting it")
   silent! call delete(s:sessionfile)
   unlet s:sessionfile
  endif
"  call Dret("CleanupSessionFile")
endfun

" ---------------------------------------------------------------------
" GotoWinNum: this function puts cursor into specified window {{{1
fun! s:GotoWinNum(winnum)
"  call Dfunc("GotoWinNum(winnum=".a:winnum.") winnr=".winnr())
  if a:winnum != winnr()
   exe a:winnum."wincmd w"
  endif
"  call Dret("GotoWinNum")
endfun


" ---------------------------------------------------------------------
" ZoomWinPreserve:  This function, largely written by David Fishburn, {{{1
"   allows ZoomWin to "preserve" certain windows:
"
"   	TagList, by Yegappan Lakshmanan
"   	  http://vim.sourceforge.net/scripts/script.php?script_id=273
"
"   	WinManager, by Srinath Avadhanula
"   	  http://vim.sourceforge.net/scripts/script.php?script_id=95
"
"  It does so by closing the associated window upon entry to ZoomWin
"  and re-opening it upon exit by using commands provided by the
"  utilities themselves.
fun! s:ZoomWinPreserve(open)
"  call Dfunc("ZoomWinPreserve(open=".a:open.")")

  if a:open == 0

   " Close Taglist
   if exists('g:zoomwin_preserve_taglist') && exists('g:loaded_taglist')
       " If taglist window is open then close it.
       let s:taglist_winnum = bufwinnr(g:TagList_title)
       if s:taglist_winnum != -1
           " Close the window
           exec "silent! Tlist"
       endif
   endif

   " Close Winmanager
   if exists('g:zoomwin_preserve_winmanager') && exists('g:loaded_winmanager')
       " If the winmanager window is open then close it.
       let s:is_winmgr_vis = IsWinManagerVisible()
       if s:is_winmgr_vis == 1
           exec "WMClose"
       endif
   endif

  else

   " Re-open Taglist
   if exists('g:zoomwin_preserve_taglist') && exists('g:loaded_taglist')
       " If taglist window was open, open it again
       if s:taglist_winnum != -1
           exec "silent! Tlist"
       endif
   endif

   " Re-Open Winmanager
   if exists('g:zoomwin_preserve_winmanager') && exists('g:loaded_winmanager')
       " If the winmanager window is open then close it.
       if s:is_winmgr_vis == 1
           exec "WManager"
       endif
   endif
  endif

"  call Dret("ZoomWinPreserve")
endfun

let &cpo= s:keepcpo
unlet s:keepcpo
" ---------------------------------------------------------------------
"  Modelines: {{{1
" vim: ts=4 fdm=marker

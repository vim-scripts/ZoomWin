" ZoomWin:	Brief-like ability to zoom into/out-of a window
" Author:	Charles Campbell
"			original version by Ron Aaron
" Date:		May 10, 2005
" Version:	21
" History: see :help zoomwin-history {{{1
" GetLatestVimScripts: 508 1 :AutoInstall: ZoomWin.vim

" ---------------------------------------------------------------------
" Load Once: {{{1
if &cp || exists("g:loaded_ZoomWin")
 finish
endif
let s:keepcpo        = &cpo
let g:loaded_ZoomWin = "v21"
set cpo&vim

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

    let s:winkeep    = winnr()
    let s:sesskeep   = v:this_session

	" doesn't work with the command line window (normal mode q:)
	if &bt == "nofile" && expand("%") == "command-line"
	 echoerr "***error*** ZoomWin doesn't work with the command line window"
"     call Dret("ZoomWin : commandline window error")
	 return
	endif

	" disable all events (autocmds)
"	call Decho("disable events")
    let ei_keep= &ei
	set ei=all

    " save window positioning commands
"	call Decho("save window positioning commands")
    windo let s:savedposn{winnr()}= s:SavePosn(1)
    call s:GotoWinNum(s:winkeep)

    " set up name of session file
    let s:sessionfile= tempname()

    " save session
"	call Decho("save session")
    let ssop_keep = &ssop
    let &ssop     = 'blank,help,winsize'
    exe 'mksession! '.s:sessionfile
    set lz ei=all bh=
     exe "new! ".s:sessionfile
     v/wincmd\|split\|resize/d
     w!
	 bw!

    " restore user's session options and restore event handling
"	call Decho("restore user session options and event handling")
    set nolz
    let &ssop = ssop_keep
    only!
    let &ei   = ei_keep
    echomsg expand("%")
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
" HelpExtractor:
"  Author:	Charles E. Campbell, Jr.
"  Version:	3
"  Date:	Sep 09, 2004
"
"  History:
"    v2 Nov 24, 2003 : On Linux/Unix, will make a document directory
"                      if it doesn't exist yet
"
" GetLatestVimScripts: 748 1 HelpExtractor.vim
" ---------------------------------------------------------------------
set lz
let s:keepcpo= &cpo
set cpo&vim
let docdir = substitute(expand("<sfile>:r").".txt",'\<plugin[/\\].*$','doc','')
if !isdirectory(docdir)
 if has("win32")
  echoerr 'Please make '.docdir.' directory first'
  unlet docdir
  finish
 elseif !has("mac")
  exe "!mkdir ".docdir
 endif
endif

let curfile = expand("<sfile>:t:r")
let docfile = substitute(expand("<sfile>:r").".txt",'\<plugin\>','doc','')
exe "silent! 1new ".docfile
silent! %d
exe "silent! 0r ".expand("<sfile>:p")
silent! 1,/^" HelpExtractorDoc:$/d
exe 'silent! %s/%FILE%/'.curfile.'/ge'
exe 'silent! %s/%DATE%/'.strftime("%b %d, %Y").'/ge'
norm! Gdd
silent! wq!
exe "helptags ".substitute(docfile,'^\(.*doc.\).*$','\1','e')

exe "silent! 1new ".expand("<sfile>:p")
1
silent! /^" HelpExtractor:$/,$g/.*/d
silent! wq!

set nolz
unlet docdir
unlet curfile
"unlet docfile
let &cpo= s:keepcpo
unlet s:keepcpo
finish

" ---------------------------------------------------------------------
" Put the help after the HelpExtractorDoc label...
" HelpExtractorDoc:
*ZoomWin.txt*	Zoom into/out-of a window		May 10, 2005
Authors: Charles E. Campbell, Jr.			*zoomwin*
         Ron Aaron
Version: 21

==============================================================================
1. Usage						*zoomwin-usage*

   :call ZoomWin()
   :ZoomWin
   <c-w>o

   Either of the two commands or the normal mode <c-w>o will toggle between
	* selecting the current window for display as the only window or
	* to restore the original multiple-window view.

==============================================================================
2. Setup						*zoomwin-setup*

   Simply put ZoomWin.vim into your .vim/plugin directory (you may need to
   make such a directory first).  Under Windows that should be
   vimfiles\plugin.  ZoomWin now uses the HelpExtractor method to
   automatically extract help and to make it known to vim by running helptags
   on it.

==============================================================================
3. History						*zoomwin-history*

	v21 Oct 12, 2004 : * v14 fixed a bug when wmw and/or wmv equal to 0;
			     v21 will invoke the patch only if the version <= 603.
			     For vim version 6.3 users, this fix allows more files
			     to be handled by ZoomWin.
	    May 10, 2005   * When :version shows -mksession, and the vim version
			     is at least 6.3, ZoomWin will now do a partial zoom
	v20 Jul 26, 2004 : * bugfix - ZoomWin didn't always retain the
			     position in the former zoomed-in window after
			     the window layout was restored.  It was restoring
			     the position when the zoom-in occurred.
	v19 May 26, 2004 : * bugfix - winmanager has events firing that,
	                     amongst other things, reset the bufhidden
			     option to delete for some windows while
			     ZoomWin worked.  ZoomWin now works
			     successfully with winmanager.
	v18 May 20, 2004 : * bugfix - didn't adversely affect anything, but
	                     ZoomWin was deleting its session file twice.
			   * bugfix -- a multi-source file + minibufexplorer
			     + Taglist interaction bug -- minibufexplorer's
			     autocmd events were firing, generating a new
			     window while ZoomWin was attempting to restore
			     the display.  ZoomWin didn't have restoration
			     information for the new window and so reported
			     an error.  Events are now temporarily disabled
			     while ZoomWin is restoring the layout.
	v17 Mar 26, 2004 : * ZoomWin command installed.  Works nicely with
	                     taglist:  vim +Tlist +ZoomWin filename
	v16 Dec 22, 2003 : * handles bufhidden and nobl windows (TagList support).
	                   * Now also works with quickfix window (:copen) but
			     still not with |cmdline-window| (q:)
	v15 Dec 19, 2003 : * SavePosn()/RestorePosn() needed to be preceded
	                     by s: to prevent clashes
	v14 Dec 18, 2003 : * works around a restoration-bug with mksession
			     when either wmw or wmh settings are zero
			   * Bwipes internal temporary buffers
			   * Known bugs: will not work with command-line
			   * Editing window (|cmdline-window|) nor the
			     quickfix window (|copen|).
	v13 Dec 18, 2003 : Uses eventignore to prevent events/autocmds from
			    firing while changing the mksession results.
	v12 Dec 12, 2003 : uses hidden and a minimalist mksession save
	v11 Oct 14, 2003 : bug fix: apparently RestorePosn()'s variables,
			    which were b:, weren't always defined, so s:
			    ones are now used.
	v10 Sep 22, 2003 : Bug fix: when a single window is showing, the user
			    moves the cursor, then <c-w>o used to restore
			    screen, the current cursor position wasn't retained
			   Restores v:this_session.
			   Bug fix: change a window, use <c-w>o, then write.
			   Was saving file only to temporary file instead of
			    actual file, but when the actual file was brought back,
			    the changes were lost.
	v9 Aug 15, 2003 :  v8 managed to trash syntax highlighting on
			   reload, this one removes the eventignore
			   handling.  Will need more pondering...
	v8 Aug 14, 2003 :  now handles not-modified but not filereadable
			   buffers, nowrite buffers uses eventignore to
			   bypass autocmd firing
	v7 May 23, 2003 :  bugfix - GotoWinNum() didn't always get the
			   cursor into the correct window
	v6 Mar 25, 2003 :  more cleanup included
	v5 Mar 14, 2003 :  includes support for handling scratch buffers,
			   no-name buffer windows, and modified-buffer
			   windows.  All windows' contents will be saved to
			   temporary buffers
	v4 Dec 12, 2002 :  Zak Beck contributed code to clean up temporary
			   session files if one leaves vim while zoomed-in
	v3 Dec 11, 2002 :  plugin-ized
	v2 Nov 08, 2002 :  A guaranteed-to-be-unique to this
			   session file is used for session information.
			   Modified but not yet saved files are made hidden
			   during zoom in.
	v1 the epoch    :  Ron Aaron's original

vim:tw=78:ts=8:ft=help

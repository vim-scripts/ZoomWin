" ZoomWin: Brief-like ability to zoom into/out-of a window
"  Author: Ron Aaron
"          modified by Charles Campbell
" Version: 17
" History: see :he zoomwin-history

" ---------------------------------------------------------------------
if &cp || exists("s:loaded_zoomwin")
 finish
endif
let s:loaded_zoomwin= 1

" ---------------------------------------------------------------------
"  Public Interface:
if !hasmapto("<Plug>ZoomWin")
 nmap <unique> <c-w>o  <Plug>ZoomWin
endif
nnoremap <silent> <script> <Plug>ZoomWin :set lz<CR>:call ZoomWin()<CR>:set nolz<CR>
com! ZoomWin :set lz|silent call ZoomWin()|set nolz

au VimLeave * call <SID>CleanupSessionFile()

" ---------------------------------------------------------------------

" ZoomWin: toggles between a single-window and a multi-window layout
"          The original was by Ron Aaron and has been extensively
"          modified by Charles E. Campbell.
fun! ZoomWin()  
  let keep_hidden = &hidden
  let keep_write  = &write
  if &wmh == 0 || &wmw == 0
   let keep_wmh = &wmh
   let keep_wmw = &wmw
   set wmh=1 wmw=1
  endif
  set hidden write

  if winbufnr(2) == -1
    " there's only one window - restore to multiple-windows mode
    
    if exists("s:sessionfile") && filereadable(s:sessionfile)
      let sponly= s:SavePosn(0)

      " read session file to restore window layout
      exe 'silent! so '.s:sessionfile
      silent! call delete(s:sessionfile)
      let v:this_session= s:sesskeep

      if exists("s:savedposn1")
        " restore windows' positioning and buffers
        windo call s:RestorePosn(s:savedposn{winnr()})|unlet s:savedposn{winnr()}
        call s:GotoWinNum(s:winkeep)
        unlet s:winkeep
      endif
	  if line(".") != s:origline || virtcol(".") != s:origcol
	   " If the cursor hasn't moved from the original position,
	   " then let the position remain that of the multi-window layout
	   " restoration.
       call s:RestorePosn(sponly)
	  endif
      call delete(s:sessionfile)
      unlet s:sessionfile
    endif

  else " there's more than one window - go to only-one-window mode

    let s:winkeep    = winnr()
    let s:sesskeep   = v:this_session
	let s:origline   = line(".")
	let s:origcol    = virtcol(".")

	" doesn't work with the command line window (normal mode q:)
	if &bt == "nofile" && expand("%") == "command-line"
	 echoerr "***error*** ZoomWin doesn't work with the command line window"
	 return
	endif

    " save window positioning commands
    windo let s:savedposn{winnr()}= s:SavePosn(1)
    call s:GotoWinNum(s:winkeep)

    " set up name of session file
    let s:sessionfile= tempname()

    " save session
    let ssop_keep = &ssop
    let &ssop     = 'blank,help,winsize'
    let ei_keep   = &ei
    exe 'mksession! '.s:sessionfile
    set lz
     set ei=all
     exe "new! ".s:sessionfile
     v/wincmd\|split\|resize/d
     w!
	 bw!
    set nolz
    let &ssop = ssop_keep
    let &ei   = ei_keep
    only!
    echomsg expand("%")
  endif

  " restore user option settings
  let &hidden= keep_hidden
  let &write = keep_write
  if exists("keep_wmw")
   let &wmh= keep_wmh
   let &wmw= keep_wmw
  endif
endfun

" ---------------------------------------------------------------------

" SavePosn: this function sets up a savedposn variable that
"          has the commands necessary to restore the view
"          of the current window.
fun! s:SavePosn(savewinhoriz)
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
	setlocal bh=
   endif
   if !&bl
   	let settings= settings." nobl"
	setlocal bl
   endif
   if &bt == "quickfix"
   	let settings= settings." bt=quickfix"
	setlocal bt=
   endif
   if settings != ""
   	let savedposn= savedposn.":setlocal ".settings."\<cr>"
   endif

  endif
  return savedposn
endfun

" ---------------------------------------------------------------------

" s:RestorePosn: this function restores noname and scratch windows
fun! s:RestorePosn(savedposn)
  exe a:savedposn
endfun

" ---------------------------------------------------------------------

" CleanupSessionFile: if you exit Vim before cleaning up the
"                     supposed-to-be temporary session file
fun! s:CleanupSessionFile()
  if exists("s:sessionfile") && filereadable(s:sessionfile)
   silent! call delete(s:sessionfile)
   unlet s:sessionfile
  endif
endfun

" ---------------------------------------------------------------------

" GotoWinNum: this function puts cursor into specified window
fun! s:GotoWinNum(winnum)
  if a:winnum != winnr()
   exe a:winnum."wincmd w"
  endif
endfun

" ---------------------------------------------------------------------
" vim: ts=4
" HelpExtractor:
set lz
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
finish

" ---------------------------------------------------------------------
" Put the help after the HelpExtractorDoc label...
" HelpExtractorDoc:
*ZoomWin.txt*	Zoom into/out-of a window		Dec 22, 2003
Authors: Charles E. Campbell, Jr.			*zoomwin*
         Ron Aaron		
Version: 17

==============================================================================
1. Usage						*zoomwin-usage*

   :call ZoomWin()
   :ZoomWin
   <c-w>o                

   Either of these two commands will cause current window to be selected
   and become the only window or to restore the multiple-window view.

==============================================================================
2. Setup						*zoomwin-setup*

   Simply put ZoomWin.vim into your .vim/plugin directory (you may need
   to make such a directory first).  ZoomWin now uses the HelpExtractor
   method to automatically extract this help and run helptags on it.

==============================================================================
3. History						*zoomwin-history*

	v17 Mar 26, 2004 : ZoomWin command installed.  Works nicely with
	                   taglist:  vim +Tlist +ZoomWin filename
	v16 Dec 22, 2003 : handles bufhidden and nobl windows (TagList support).
	                   Now also works with quickfix window (:copen) but
			   still not with |cmdline-window| (q:)
	v15 Dec 19, 2003 : SavePosn()/RestorePosn() needed to be preceded
	                   by s: to prevent clashes
	v14 Dec 18, 2003 : works around a restoration-bug with mksession
			    when either wmw or wmh settings are zero
			   Bwipes internal temporary buffers
			   Known bugs: will not work with command-line
			   Editing window (|cmdline-window|) nor the
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

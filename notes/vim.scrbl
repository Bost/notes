#lang notes

#+title: Vim

@block{@block-name{Vim}
  #+BEGIN_SRC vim
  " Text Object (type :h text-objects in vim for a complete description)
  " This feature allows to operate on a block of text where the cursor is.
  " TODO: color commands belonging together
  " TODO: C-r register, C-r C-o / C-r / C-p

  " substitute (replace) pattern and save changes in all buffers
  ```vim
  :bufdo %s/pattern/replacement/ge | update
  ```
  " substitute (replace) pattern from current line
  :.,$s/pattern/replacement/gc
  " confirm, ignore case, case sensitive, number of matches; print lines
  :[range]s/bacon/lettuce/[ciInp] [count]
  " substitute (replace) only next 10 lines; https://vim.fandom.com/wiki/Ranges
  :.,.+9s/foo/bar/g
  " substitute (replace) - use previous search pattern
  :[range]s//baz/...
  " substitute (replace) last replacement string
  :%s/~/qux/igc
  " substitute (replace) char / line / to the end of line
  s / S (or cc) / C

  " go to next / previous buffer
  :bnext (:bn) / :bprev (:bp)

  " mark everything from the cursor up to &lt;pattern&gt;
  v/pattern

  " save a file as a sudo
  :w !sudo tee %

  "
  :[range]g/pattern/cmd

  " delete all lines matching / not matching a pattern
  :g/pattern/d  /  :g!/pattern/d

  " copy all lines matching a pattern to end of file
  :g/pattern/t$

  " :registers yank all lines matching a pattern to register \'a\'
  0"ay0:g/pattern/y A

  " :registers place the contents of register \'a\' in the search, and replace it with \'bar\'
  %s/&lt;C-R&gt;a/bar/g

  " grep
  :g/regexp/p

  " create new window
  :new

  " paste last yanked text
  "0P

  " paste from / cut line to system clipboard / system selection (X11)
  "*p / "*dd / "+p / "+dd

  " break lines according to :set textwidth. (see wrapmargin too)
  gq

  " move down / up when lines are wrapped
  gj / gk

  " format line / file
  == / gg=G

  " format whole file to columns
  :%!column -t

  " start / stop macro recording, play down recorded macro
  qq .... q @"@"q/@"@"@"@"

  " open / close the quickfix window
  :cwindow (:cw) / :cclose

  " open / write all buffers at once
  :ba / :ball / :wa / :wall
  :edit /path/to/file
  :e    /path/to/file

  " edit macro
  C-x C-k e

  " call sort command from shell
  :! sort

  " filter out duplicate rows (unique rows only)
  :sort u

  " lower / upper of movement m
  gum / gUm/
  " lisp to cammelCode conversion: cammel-code to cammelCode
  :'<,'>s/-\(.\)/\U\1/g

  " replace mode, replace 1 char, replace char and move right
  R r ~

  " execute command and read in its output
  :r! command

  " execute current line in the shell
  .!sh / !!sh

  " last edit location (~ key)/ line (&auml; key)
  `. / \'.

  " :jump to last cursor location / last cursor line
  `` / \'\'

  " :jump go back / forth (older / newer locations)
  :jumps / C-o / C-i

  " :jump to next / previous diff
  ]c / [c

  " :jump marks (for jumping etc); m - marks something?
  :marks, m

  " :jump to column 30
  30|

  " :jump cursor forward / backward
  ; / ,

  " :jump move window / jump cursor to the middle, bottom, top
  zz zb zt / M L H

  " :jump (backward): sentence / paragraph / section (keyword function)

  " close, open, toggle current / all folds from cursor/ all folds
  zc zo za / zC zO zA / zR

  " display current guifont / guifond dialog window
  :set guifont=? / :set guifont=*

  " change to the directory of the current file
  :lcd %:p:h

  " :registers redirect messages to the clipboard (system register)
  :redir @"@"*

  " display / print (error) messages
  :messages

  " terminate redirection
  :redir END

  " reverts the document back to how it was 15 minutes ago / reverse the :earlier command
  :earlier 15m / :later

  " figure out where cindent got set/unset (for debuging)
  :verbose set cindent?

  " :registers clear / copy register val: m &lt;- \'\' / m &lt;- n
  :let @"@"m=\'\'  /  :let @"@"m=@"@"n

  " :registers redirect output to register a
  :redir @"@"a

  " :registers paste register a into new window
  :put! a

  " :registers display registers: system, default, a, b, c
  :reg *0abc


  " :registers store curret line to register q
  "qY

  " :registers current / alternate filename
  "% / "#

  " :registers last small delete (? the blackhole register?) / last inserted text
  "_ / ".

  " :registers last search / last ex command
  "/ / ":

  " :registers insert name of the current / alternate file
  C-r % / C-r #

  " calculator; (accessible from insert mode; can access every vim-function)
  C-r =

  " yank current line, paste it below, select copied line, replace every char with =
  yypVr=


  " switch to the alternate file (the one with #)
  C-^ / C-6

  " align text to right / left in insert mode
  C-t / C-d

  " print (insert) 78 "-" chars at once
  78i-<Esc>;


  " :visual change the marking direction in visual mode
  o

  " :visual re-select last visual block
  gv

  " find / till (until) forward / backward
  f/t F/T

  " lists all lines with the last search pattern
  :g//

  " swap chars
  xp

  " find 3rd joe cursor set to end of match plus 1 [C]
  3/joe/e+1

  " find 5th joe cursor set to start of match minus 2
  5/joe/s-2  or   5/joe/b-2

  " find joe cursor and move 4 lines down
  /joe/+4

  " :matching lazy matching

  " :matching min, max occurences

  " :matching min 1 occurence
  \\+

  " :matching max 1 occurence
  \\?

  " open command-line history window in edit mode / forward / backward search. Exit CTRL-C
  q: / q/ / q?

  " open command-line history when editing search pattern
  c-f

  " hightlight and search forwards/backwards / remove all persist hightlighting
  * / " / &lt;leader&gt; SPC

  " hightlight and search for parts of a word
  g* / g"

  " :changelist go forth / back in the insert mode change list
  :changes / g; / g,

  " edit /path/to/file / refresh the file
  :e /path/to/file / :e

  " open file under cursor (goto file)
  gf

  " open file under cursor in a new window
  C-w f

  " increment / decrement next number on the current line
  C-a / C-x

  " :completition word completition in insert mode (next / previous)
  C-n / C-p

  " :completition line completition / function name completition (omni completition)
  C-x C-l / C-x C-o

  " :completition file completition
  C-x C-f

  " pull cword (current word) onto search/command line
  /C-r C-w

  " temporarily change the insert- for normal mode
  C-o

  " :vimdiff gvim with tabs
  gvim -p file1 file2

  " :vimdiff gvim / vim in diffmode
  gvim -d file1 file2 / vimdiff file1 file2

  " :vimdiff diff current two buffers / windows
  :diffthis / :windo diffthis

  " :vimdiff diff current buffer with a filename
  :vert diffsplit filename

  " :vimdiff obtain difference under cursor from the other viewport
  do :diffg :diffget

  " :vimdiff put difference under cursor to the other viewport
  dp :diffput

  " :vimdiff update / switch off the diffmode for the current window
  :diffupdate / :diffoff

  " set current buffer to readonly mode
  set nomodifiable

  "
  set fileformat=dos|unix|mac

  "
  set filetype=html|xml|...

  " :splits resize vertical viewport 5 chars to the left / right / bottom / top
  C-w 5&lt; / C-w 5&gt; / C-w 5- / C-w 5+

  " :splits move around split viewports
  C-w C-w

  " :splits move around viewports according to given direction
  C-w h/j/k/l

  " :splits rotate window down-&gt;right / up-&gt;left
  C-w r / R

  " :splits close other windows
  C-w o / :on

  " :splits :tabs break out current window into a new tabview
  C-W T

  " :splits swap top/bottom or left/right split
  C-W R

  " :splits maximize vertically / horizontally
  C-w |  /  C-w _

  " :splits horizontal / vertical viewport split
  :sp filename / :vsp filename

  " :splits open the file browser in a new window split
  :vsplit ./:vsplit./:vsp ./:sp./:split.

  " :splits open horizontal viewport 10 lines higt (good for notes)
  :10sp

  " open vim / gvim from the command line with file0, file1 in separate tabs
  vim -p file0 file1 / gvim -p file0 file1

  " open vim / gvim from the command line as a file browser
  vim . / gvim .

  " shift text right / left / align text
  &gt; / &lt; / =

  " :help follow link / go back
  C-] / C-t

  " change to the dir of current file (probably)
  :cd %:h

  " place increasing 10.0.0.1, 10.0.0.2, etc.
  :for i in range(1,255) | .put=\'10.0.0.\'.i | endfor

  " like tail -f
  :setlocal autoread

  " show files adjacent to the one edited one; :Explore move up one directory
  :Explore

  " :plugin list all plugins, _vimrcs loaded (super)
  :scriptnames

  " :plugin list all user-defined functions (just names & args) / full code of function Foo
  :function / :function Foo

  " reveals value of history and where set
  :verbose set history?

  " :vim-fugitive stage / unstage given file in Gstatus / Gcommit viewport
  -

  " :vim-fugitive git checkout -- filename
  :Gread

  " :vim-fugitive git mv / git rm
  :Gmove / :Gremove

  " :vim-fugitive git status / git commit; p - Interactively choose hunks of patch (git add -p)
  :Gstatus / :Gcommit

  " :vim-fugitive perform vimdiff
  :Gdiff

  " :vim-fugitive load and move between versions :cprev / :cnext / :cfirst / :clast
  :Glog [q / ]q / [Q / ]Q

  " :vim-fugitive go back to "normal" file (working copy)
  :Gedit

  " :vim-fugitive open the current file on GitHub
  :Gbrowse

  " :vim-fugitive open output of a command in a temp file
  :Git!

  " :vim-fugitive execute any git commands
  :Git

  " :surround mark / yank / change / delete the "innerHTML"
  vit / yit / cit / dit

  " :surround mark / yank / change / delete the whole html tag
  vat / yat / cat / dat



  " :surround delete html tag / current paragraph (f.e. a function) / word
  da&lt; / dap / daw

  " :NERDTree change drive to q: under windows
  :NERDTree q:

  " :NERDTree open split / change dir / refresh / bookmarks
  i / cd / r / B

  " code snippets
  snipMate

  " finds file, need L9 vim plugin (does not work somehow :(
  fuzzyFinder

  " :bufexplorer :BufExplorer - in current window
  \\be

  " :bufexplorer :BufExplorerHorizontalSplit / :BufExplorerVerticalSplit
  \\bs / \\bv

  " :spellcheck switch on / off
  :set spell / :set nospell

  " :spellcheck next / previous mistake
  ]s / [s

  " :spellcheck spelling suggestions / auto replace with 1st suggestion
  z= / 1z=

  " :spelllang ally spell language to viewport / buffer
  :windo set spelllang=en_us / :bufdo set spelllang=en_us

  " :spellcheck add word under cursor to spellfile / editing session
  zg / zG

  " :vimclojure start REPL (with the namespace of the current buffer)
  \\sr / \\sR

  " :vimclojure prompt for input and lookup with (source) / (find-doc)
  \\si / \\fd

  " :vimclojure :evaluate line / file / paragraph / visual block (in visual mode) / function
  \\el / \\ef / \\ep / \\eb / \\et

  " :vimclojure close a window
  \\p

  " :vundle list configured bundles
  :BundleList

  " :vundle install (or update) bundles
  :BundleInstall(!)

  " :vundle search (or refresh cache first) plugin
  :BundleSearch(!) plugin

  " :vundle confirm (or auto-approve) removal of unused bundles
  :BundleClean(!)

  " :orgmode insert active / inactive date
  \\sa / \\si

  " Spacevim update / upgrade
  " https://spacevim.org/documentation/#update-and-rollback
  :SPUpdate

  " SpaceVim: Updating failed, The plugin dir is dirty
  " a branch must by checked-out. "Detached HEAD" won't work
  cd ~/.SpaceVim && git status

  " page up / down: forward / backward
  C-b / C-f
  #+END_SRC
}

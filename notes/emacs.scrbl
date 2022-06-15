#lang notes

#+title: Emacs

@block{@block-name{The Emacs thesis}
  composite programs in a high-level extension language running on a kernel in a
  low-level language.
}

@block{@block-name{Diverse}
  ;; emacsclient in Guix is in the emacs-with-editor package

  ;; broken icons in the *spacemacs* buffer
  M-x all-the-icons-install-fonts

  [[https://emacsconf.org/2021/talks/imaginary/][Imaginary Programming]] IP
  - based on / an abstraction over Prompt Engineering and Language Models (LMs)

  Notable users: [[https://www.youtube.com/c/ProtesilaosStavrou/][Protesilaos]], Karthik / [[https://karthinks.com/][Karthinks]]

  | ~SPC h d~           | spacemacs: help-describe                |
  | ~SPC s~             | spacemacs: search                       |
  | ~SPC t g~           | spacemacs: windows: toggle golden ratio |
  | ~SPC h SPC <topic>~ | spacemacs: helm-spacemacs-help          |
  | ~SPC SPC~           | spacemacs: ~M-x~ emulation              |
  | ~SPC f e v~         | spacemacs: version                      |
  | ~SPC w p m~         | spacemacs: popup \*Messages\*           |

  ;; Shift-Tab
  ~<s-tab>~ / ~<backtab>~

  ;;
  M-x narrow-to-defun ~s-n~ / M-x widen ~s-N~

  ;; open file and jump to / start on a line:column
  emacs +line:column path/to/file

  ;; unreachable repositories: use http instead of https
  emacs --insecure

  ;; e.g. visualization; for log analysis
  M-x highlight-symbol-at-point

  ;; paredit: wrap square brackets around sexp
  M-x paredit-wrap-square

  ;; paredit
  | ~C-k~       | kill the rest in the sexp           |
  | ~M-(~       | insert-parentheses                  |
  | ~M-)~       | move-past-close-and-reindent        |
  | ~M-s-right~ | paredit-forward-slurp-sexp (vcucni) |
  | ~M-s-left~  | paredit-forward-barf-sexp (vygrcaj) |

  ;; indent: move text left by four spaces (M-x indent-rigidly ~C-x TAB~)
  ~C-u -4 C-x TAB~

  ;; https://www.emacswiki.org/emacs/RegularExpression#regexp
  | \\#                | increment the number found         |
  | \\s-               | increment the whitespace           |
  | %s#\(.\{2\}\)#aa#g | match / find exactly 2 occurrences |
  | \(.*?\)            | lazy match                         |
  | \(.\{2\}\)         | match / find exactly 2 occurrences |
  | \(http[[:print:]]*\)/  | match / find url                   |

  ;; regexp - syntax classes must be used within square brackets
  | [[:space:]] | whitespace character, as defined by the syntax table, typically [\t\r\n\v\f] |
  | \\s-    | whitespace character, as defined by the syntax table, typically [\t\r\n\v\f] |
  | [[:blank:]] | a space or tab character                                                     |

  ;; utf8 unicode
  M-x describe-char       ;; describe char at the point
  M-x ucs-insert RET 2211 ;; insert unicode char upper-case sigma U+2211 ∑

  hide: show: folding: enable folding M-x hs-minor-mode
  | ~C-c @"@" C-c~ | toggle hiding: fold / unfold |

  ;; hide: show: folding: (also see origami folding in .spacemacs)
  | ~z o~ | evil open fold             |
  | ~z z~ | evil close fold            |
  | ~z a~ | evil toggle fold           |
  | ~z r~ | evil open (all) folds      |
  | ~z m~ | evil close (all) folds     |
  | ~z t~ | evil-scroll-line-to-top    |
  | ~z b~ | evil-scroll-line-to-bottom |

  z RET ;; evil keyboard macro ???

  ;; evil: move view 5 chars to the right
  ~5z<Right>~

  ;; files: file format
  ~C-x RET f~

  ;; nrepl: M-x nrepl-jack-in - ??? this is probably for clojure
  ~C-c M-j~

   ;; repl: M-x ielm ELISP>
  ~M-m m'~

  ;; testing: startup: skip ~/.emacs (if messed up) / don't load the init file
  emacs --no-init-file     ;; also: emacs -q
  emacs --no-window-system ;; also: emacs -nw

  | M-x browse-url-at-point | open web browser of the OS |
  | M-x eww                 | emacs web browser          |

  ;; byte-compile an *.el file
  M-x byte-compile-file

  ;; keyboard: does not work on cygwin
  M-x quail-set-keyboard-layout

  ;; keyboard: show layout
  M-x quail-show-keyboar-layout

  | ~C-x <~ | scroll left  |
  | ~C-x >~ | scroll right |

  ;; evil: Edit the search string in the minibuffer. (isearch-edit-string)
  ~M-e~

  ;; evil: toggle (evil-mode 0) / (evil-mode 1)
  ~C-z~

  ;; help: ? emacs manual?
  ~C-h i m emacs~

  | ~C-h m~ | M-x describe-mode     |                       |
  | ~C-h k~ | M-x describe-key      |                       |
  | ~C-h f~ | M-x describe-function |                       |
  | ~C-h v~ | M-x describe-variable |                       |
  | ~C-h b~ | M-x describe-bindings | available keybindings |
  ;; package command-log-mode - show pressed keybindings (when screen casting)

  ;; dynamic vs. lexical binding: https://www.emacswiki.org/emacs/LexicalBinding
  (setq lexical-binding t)

  ;; dynamic vs. lexical binding:
  ;; https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding
  ;; lexical binding is for closures
  ;; -*- lexical-binding: t -*-

  ;; dynamic vs. lexical binding:
  EmacsLisp: dynamic; Scheme, CommonLisp: lexical

  ;; CLOS: Common Lisp Object System (CLOS)
  differs from OOP facilities found in C++ or Java

  ;; help: show content of the variable containing installed packages
  C-h v package-activated-list

  ;; install new packages
  M-x package-list-packages

  ;; packages grouped by keyword
  M-x finder-by-keyword

  ;; delete word
  ~M-d~
  ;; delete line from cursor
  ~C-k~

  M-x goto-line

  ;; menu bar
  ~M-`~ / ~F10~

  ;; jump back to the last mark (there is a mark-ring)
  ~C-u C-SPC~

  ;; enlarge-window, shrink-window horizontally / vertically
  ~C-x ^~
  ~C-x {~
  ~C-x }~

  ;; diff against any chosen revision
  ~C-u C-x v =~

  ;; discard changes
  ~C-x v u~

  ;; checkout any version: master~3 - last 3th version
  ~C-x v \~~

  ;; commit log: f - view revision; d - view diff
  ~C-x v l~

  ;; switch window / frame (o = other)
  ~C-x o~

  ;; add to version control system
  ~C-x v i~

  ;; files: find file / find alternate file (reload / refresh file)
  ~C-x C-f~
  ~C-x C-v~

  ;; Transparent Remote (file) Access / Editing, Multiple Protocol (TRAMP)
  ;; method can be: ssh if anything doesn't work:
  ;; 1. delete ~/.bashrc
  ;; 2. emacs -q --eval "(setq tramp-verbose 10)" &
  ~C-x C-f~ /method:user@"@"remotehost#port:filename
  ~C-x C-f~ /ssh:test@"@"host#2222:/tmp

  ;; files / buffers
  | ~C-x k~   | M-x kill-buffer               |                |
  | ~C-x C-b~ | M-x list-buffers              |                |
  | ~C-x b~   | M-x ido-switch-buffer         |                |
  | ~C-x 4 f~ | find-file-other window        | ctl-x-4-prefix |
  | ~C-x 4 b~ | switch-to-buffer-other-window | ctl-x-4-prefix |
  | ~C-x C-s~ | M-x save-buffer               | save file      |
  | ~C-x s~   | M-x save-some-buffers         | save all files |
  | ~C-x C-w~ | M-x write-file                | save as        |

  ;; eshell: ifconfig > #<buffer interfaces>

  ;; cli: batch: noninteractive run
  emacs --batch --eval '(message "Hello world")'

  ;; cli: batch: run emacs lisp file from command line:
  ;; chmod +x ./hello.el; ./hello.el
  #!/bin/sh
  ":"; exec emacs --script "$0" "$@"@""
  ;; # -*- mode: emacs-lisp; lexical-binding: t; -*-
  (message "Hello world")

  ;; cli: batch: noninteractive run
  #!/usr/bin/emacs --script
  (message "Hello world")

  ;; eshell: combing elisp functions (message) with OS programs
  ;; (/usr/bin/cut) in eshell
  message "Hello world" | cut -f 1 -d ' '

  ;; cssh ? ssh shell ? (somehow strange)

  ;; color-theme-calm-forest ? does not work?

  ;; yasnippet - yet another snippets. Example
  ;; `defn' `M-/' type in the defn-name, then `TAB' to complete ...
  ;; hippie-expand (dabbrev-expand?) (code completition)
  ~M-/~

  ;; delete 1 whole line
  M-x kill-whole-line
  C-S-Backspace

  ;; M-x evilnc-comment-or-uncomment-lines
  ~M-x ;~

  ;; jump forward / backward to matching brace
  ;; cursor may need to be behind closing ')'
  ~C-M-f~
  ~C-M-b~

  ;; immediate eval
  ~C-M-x~

  ;; auto indent block
  ~C-M-'~

  ;; M-x query-replace
  ~M-%~

  ;;
  | ~M-u~ | M-x upcase-word     |
  | ~M-l~ | M-x downcase-word   |
  | ~M-c~ | M-x capitalize-word |

  ;; check a small region
  M-x ispell-region
  M-x ispell-buffer

  ;;
  | ~C-x (~               | macro: start                       |
  | ~C-x )~               | macro: stop                        |
  | ~C-x e~ or ~<f4>~     | macro: execute (e - execute again) |
  | ~M-5 <f4>~ or ~C-x e~ | macro: execute 5 times             |

  ;; repeat n times following command
  ~C-u n~

  ;;
  | ~C-x u~        | M-x undo-tree-visualize |
  | ~C-_~ or ~C-/~ | undo                    |
  | ~C-f C-_~      | redo                    |

  ;; next-buffer / previous-buffer
  ~<XF86Forward>~, ~C-x <C-right>~, ~C-x <right>~ / ~<XF86Back>~, ~C-x <C-left>~, ~C-x <left>~

  ;; forward / backward one sentence
  ~M-a~ / ~M-e~

  ;; mark / hilite / highlight whole buffer / mark paragraph
  ~C-x h~ / ~M-h~

  ;; M-x forward-paragraph / backward-paragraph
  ~M-}~ / ~<C-down>~ / ~M-{~ / ~<C-up>~

  ;; jump to the next (compilation error(s), grep results etc.)
  ~C-x `~

  ;; files: writte buffer to a different file
  ~C-x C-w~

  ;; files: next-buffer / previous-buffer
  ~C-x <left>~ / ~C-x <right>~

  ;; copy-paste: kill line / kill sentence / yank
  ~C-k~ / ~M-k~

  ;; copy-paste: kill region (cut)
  ~C-w~

  ;; copy-paste: kill ring save (copy) / yank (paste last killed entry)
  ~M-w~ / ~C-y~
}

@block{@block-name{Git & Magit}
  ;; magit: (magit-copy-section-value) i.e. current sha1 to clipboard
  ~y s~

  ;; magit: (magit-copy-buffer-revision) i.e. top sha1 to clipboard
  ~M-w~

  ;; copy-paste: cycle back through previous entries in the kill ring
  ~M-y~

  ;; magit: spin-off / spinoff
  git branch --track <new-branch-name>

  ;; check word
  M-x spell

  ;; ? check all document ?
  M-x flyspell-mode

  ;; Error enabling Flyspell mode: No word lists can be found for the language "en_US"
  ;; sudo apt install --yes aspell-en

  | ~M-<~ | beginning of buffer |
  | ~M->~ | end of buffer |

  ;; page up/down
  ~M-v~ / ~C-v~

  | ~C-t~         | transpose chars         |
  | ~M-t~         | transpose words         |
  | ~C-x C-t~     | transpose lines         |
  | ~C-l~ or ~zz~ | center the screen lines |

  ;; start a bash command line
  M-x shell / M-x term / eshell

  ;; eshell: example
  egrep -r 'something' *

  ;; Dired Refecene Card / Cheatsheet
  http://www.gnu.org/software/emacs/refcards/pdf/dired-ref.pdf
  ;; TODO have a look at dired sorting
  https://www.emacswiki.org/emacs/DiredSortBySizeAndExtension
  https://github.com/jojojames/dired-sidebar
  http://ergoemacs.org/emacs/dired_sort.html

  ;; dired:
  | ~S~           | symlink                                    |
  | ~Z~           | zip: compress or uncompress (extract) file |
  | ~* . <ext>~   | mark all: toggle marking                   |
  | ~* s~         | mark all: executables                      |
  | ~* *~         | mark all: files (with extention)           |
  | ~* . <ext> D~ | mark & delete all files with extention     |
  | ~* c~         | change all marks                           |
  | ~\~~          | markup: all backup files                   |
  | ~#~           | markup: auto-save files                    |
  | ~g~           | refresh buffer                             |
  | ~+~           | M-x dired-create-directory                 |
  | ~R~           | M-x dired-do-rename                        |
  | ~(~           | toggle listing details                     |
  | ~(~           | M-x dired-hide-details-mode                |
  | ~C-x C-q~     | perform operations by editing dired buffer |
  | ~C-x C-q~     | M-x dired-toggle-read-only                 |
  |               | M-x wdired-finish-edit                     |
  ;; start dired and create newfile
  ~C-x C-f <ENTER>~ / <newfile>

  ;; dired: TODO check this
  | ~m~ | mark / unmark / toggle marking         |
  | ~*~ | mark / unmark / toggle marking         |
  | ~u~ | mark / unmark / toggle marking         |
  | ~U~ | mark all / unmark all / toggle marking |
  | ~t~ | mark / unmark (all) / toggle marking   |

  ;; fill / reflow text - see also auto-fill-mode
  ;; spacemacs/toggle-auto-fill-mode SPC t F
  M-x fill-paragraph (M-q)
  M-x fill-region ;; reflow all the paragraphs in the area

  ;; parameter key
  C-u

  ;; sets the line wrap to 40 characters, M-q # activate the wrap
  C-u 40 C-x f

  ;; center for given line width
  M-o M-s

  ;; isearch-forward-regexp
  C-M-s~

  ;; incremental search forward / backward
  ~C-s~ / ~C-r~

  ;; query-replace-regexp
  ~C-M-%~

  ;;
  M-x dbg / ediff / compile / man / erc

  ;; read news, email, rss / grep / speedbar /
  ;; Superior Lisp Interaction Mode for Emacs
  M-x gnus
  M-x grep
  M-x speedbar

  ;; line numbers: relative / absolute
  M-x linum-relative-toggle / global-linum-mode

  ;; M-x eval-expression
  ~M-:~

  ;; documentation reader
  ~M-g g~

  ;; move forward 4 lines
  ~C-u C-n~

  ;; increase / decrease font size
  ~C-x C-+~ / ~C-x C--~

  ;; problem: emacs does not uses fonts from /usr/share/fonts
  sudo apt install --yes libgtk2.0-dev
  ./configure --with-x-toolkit=gtk

  ;; compiling emacs on the GuixOS
  guix install gtk dconf udiskie makeinfo autoconf \
               libxaw3d gnutls libtiff libungif libjpe libxpm
  unset EMACSLOADPATH

  ;; slime: reprint last command to the REPL
  ~M-p~

  ;; gui: toggle vertical scroll bar (vertical scroll bar does not exist in emacs)
  M-x toggle-scroll-bar

  ;; gui: toggle menu-bar
  M-x menu-bar-mode

  ;; align at the given regexp
  M-x align-regexp

  ;; auto completition
  ~C-n~

  ;; region: set mark (start region)
  ~C-SPC~

  ;; region: kill selected region
  ~C-x r k~

  ;; save region to a file
  M-x write-region

  ;; splits: close / only one buffer / horizontal / vertical
  ~C-x 0~ / ~C-x 1~ / ~C-x 2~ / ~C-x 3~

  ;; does not work
  M-x clean-buffer-list

  ;; remedy against "newer than byte-compiled file" try also:
  ;; cd $dev/emacs/lisp; and make autoloads
  M-x byte-recompile-directory

  ;; helm: minibuffer: minibuffer-force-complete
  ~C-M-i~

  ;; Helm: toggle horizontal / vertical listing
  ~M-x C-t~

  ;; M-x helm-toggle-visible-mark / M-x helm-copy-to-buffer
  ~C-SPC~ / ~C-c C-i~

  ;; helm: htop: top: linux:
  M-x helm-top / M-x proced

  ;; helm: apt:
  M-x helm-apt

  ;; spacemacs: SPC r r; helm: clipboard: registers:
  M-x helm-register / :reg

  ;; spacemacs: SPC r e; evil: clipboard: registers:
  M-x evil-show-registers

  ;; paste from register
  ;; "<register>p

  ;; locate:
  M-x locate

  ;; highlighting
  M-x hi-lock-mode / highlight-regexp

  ;; magit: http://magit.github.io/master/magit.html
  ;; M-x magit-commit
  ~C-c C-c~

  ;; magit: cancel (abandon) commit
  M-x with-editor-cancel
  ~C-c C-k~
  ~C-x k~

  ;; M-x magit-status;
  ;; inc / dec / reset hunk size / split hunk / add to .gitignore /
  ;; add to .git/info/exclude
  + / - / 0 / select hunk / i / I

  ;; M-x magit-status; remoting / log / branching / bisecting / diff / fetch /
  ;; merge / rewrite
  ~M~ / ~l~ / ~b~ / ~B~ / ~d~ / ~f~ / ~m~ / ~r~

  ;; M-x magit-log; show commit details and stay in log / jump to details /
  ;; put sha1 to clipboard / reset HEAD to given commit
  ~SPC~ / ~RET~ / ~C-w~ / ~x~

  ;; M-x magit-status: section visibility
  (M-)1 / (M-)2 / (M-)3 / (M-)4

  ;; M-x magit-status: section visibility: hide (all) / show (all)
  ~M-h~ (H) / ~M-s~ (S)

  ;; M-x magit-commit: log-edit-commit-ring / Kill commit / Tested / Signed-off by
  ~M-p~ / ~M-n~ / ~C-c C-k~ / ~C-c C-t~ / ~C-c C-s~

  ;; M-x magit-status: rebase / ineractive-rebase
  ~R~ / ~E~

  ;; M-x magit-reset-quickly - press:
  ~o~
  ;; then type: "HEAD~"

  ;; M-x magit-ineractive-rebase: squash / pick / reword
  ~s~ / ~c~ / ~r~

  ;; M-x magit-status: reset (discard all uncommited) changes
  ;; working tree unchanged
  ~x~ (X)

  ;; The info manual
  emacs -q -e 'info' & disown
  M-x info
  M-x info-apropos
  M-x info-emacs-manual
  M-x info-display-manual

  ;; macros
  M-x kmacro-name-last-macro  ;; 1.
  M-x insert-kbd-macro        ;; 2.
  M-x kmacro-bind-to-key      ;; 3.

  ;; launch emacs and eval string
  emacs --eval '(message "ufo")' / emacs -e configuration-layer/update-packages

  ;; M-x shell-command; execute
  ~M-!~ / ~SPC !~

  ;; execute shell command and read-in / capture its output (in the current buffer
  ;; if in evil-insert mode)
  ~C-u M-! <cmd>~
  ~C-u SPC ! <cmd>~

  ;; M-x shell-command: top: capture top output from stdout
  ~M-! RET top -c -n -1 -b -w 200~

  ;; M-x git-timemachine git:
  p prev / n next / w Copy abbreviated hash / W Copy full hash / g Goto nth rev /
  q Exit

  ;; profiler
  M-x profiler-start profiler-report profiler-stop

  ;; evil: global search & replace, starting from the cursor position
  :,$s/BEFORE/AFTER/gc
  :,$s/BEFORE/AFTER/gc|1,''-&&

  ;; M-x ielm - alternative to Lisp Interactive mode; elisp REPL
  Inferior Emacs Lisp Mode

  ;; hyper - none of following works. See http://superuser.com/a/920967
  ;; https://github.com/trishume/dotfiles/blob/master/emacs%2B/spacemacs.symlink
  ;; C-x @"@" h 9 means H-9
  ;; (setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
  (define-key local-function-key-map (kbd "<rwindow>")
  'event-apply-super-modifier)
  (define-key local-function-key-map (kbd "<rwindow>")
  'event-apply-hyper-modifier)
  ;;
  ;; local keymaps
  ;; Major modes customize Emacs by providing their own key bindings in local keymaps

  ;; buffer's major mode:
  (message "%s" major-mode)

  (defun enable-hyper-super-modifiers-linux-x ()
  ;; on nowadays linux, <windows> key is usually configured to Super

  ;; menu key as hyper (for H-s release <menu> key before pressing 's')
  (define-key key-translation-map [menu] 'event-apply-hyper-modifier) ;H-
  (define-key key-translation-map [apps] 'event-apply-hyper-modifier)

  ;; by default, Emacs bind <menu> to execute-extended-command (same as M-x)
  ;; now <menu> defined as 'hyper, we need to press <menu> twice to get <H-menu>
  (global-set-key (kbd "<H-menu>") 'execute-extended-command)
  )
  ;; (global-set-key [(hyper 9)] (lambda () (message "[(hyper 9)]")))
  ;; (global-set-key (kbd "<rwindow>-9")
  ;;                 (lambda () (message "(kbd context-menu-9)")))
  ;; (global-set-key [(hyper 9)] (lambda () (message "[(hyper 9)]")))

  ;; M-x delete-horizontal-space - delete whitespaces around point
  M-\

  ;; test yasnippet
  emacs -Q -L . -l yasnippet-tests.el -f ert &

  ;; helm-locate - see 'man locate'
  ~SPC f L~

  ;; window transient mode - window management w/o using key binding - for layouts
  ~SPC w .~

  ;; workspaces and layouts
  ;; layout 1. contains all buffers;
  ;; layouts 2., 3., ... contain only selected buffers
  ~SPC l 0..9~ ; create new layout
  ~SPC l s~    ; save layout to a file
  ~SPC l L~    ; load layout from a file

  ;; M-s h l hilite lines e.g. log file evaluation; see:
  ;; https://www.masteringemacs.org/article/highlighting-by-word-line-regexp
  M-x highlight-lines-matching-regexp

  ;; replace-all: recursive find & replace / substitute all occurences of a string
  M-x find-name-dired  then  't' (toggle mark) then  'Q' (Query replace in files)

  ;; edit as a root / super user
  M-x spacemacs/sudo-edit
  ~s-SPC f E~
  ~SPC f E~

  ;; toggle neotree
  ~SPC f T~

  ;; Collapse every form of it when first opened - put the following block in the
  ;; bottom of init.el:
  ;; Local Variables:
  ;; eval: (hs-hide-all)
  ;; End:

  ;; vertical line indicating too long lines; spacemacs-light / -dark themes
  ;; don't show contrasting background - use the default theme
  M-x fill-column-indicator / ~SPC t f~
  M-x whitespace-toggle-options

  ;; emacs current directory
  (setq default-directory "~/.emacs.d/")

  ;; truncate / fold long lines (wrapping long lines creates new lines)
  M-x toggle-truncate-lines

  ;; movement
  C-M-a / M-x beginning-of-defun
  C-M-e / M-x end-of-defun

  ;; quit / delete multiple cursors
  M-x evil-mc-undo-all-cursors

  ;; Toggle preview of the LaTeX fragment at point.
  M-x org-latex-preview
  ~C-c C-x C-l~

  ;; TODO latex-preview-pane

  ;; https://stackoverflow.com/a/29461536
  ;; increment numbers in visual vertical block selection in emacs evil:
  ;; select e.g. 3 lines of visual block ~C-v 3~ then ~C-x r N~
}

@block{@block-name{Source Code Pro font}
  #+BEGIN_SRC shell
  guix install font-adobe-source-code-pro
  # clean font cache
  fc-cache --verbose --force
}

@block{@block-name{Mapping Functions}
  [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html][Mapping Functions]]
  #+BEGIN_SRC emacs-lisp
  mapconcat
  (mapcar 'string "abc")
  (mapcar 'list '(a b c d)) ; => ((a) (b) (c) (d))
  (mapcan 'list '(a b c d)) ; => (a b c d)  ;; i.e. with reduction
  mapc ;; like mapcar; used for side-effects only

  ;; mapconcat is like joins result list into a string with a separator:
  (mapconcat 'symbol-name '(The cat in the hat) "-") ; => "The-cat-in-the-hat"
  #+END_SRC

  https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html

  #+BEGIN_SRC emacs-lisp
  (split-string "[  aaa
   bbb   ]" (or split-string-default-separators (rx (or "[" "]"))))
  ;; => ("[" "aaa" "bbb" "]")
  #+END_SRC

  Filter list:
  #+BEGIN_SRC emacs-lisp
  (remove-if (lambda (e) (eq e 1)) '(1 2))
  ;; https://www.reddit.com/r/emacs/comments/7dp6oa/comment/dpzi5hz/?utm_source=share&utm_medium=web2x&context=3
  (seq-filter (apply-partially #'< 3) '(1 2 3 4 5 6))
  #+END_SRC
}

@block{@block-name{TODOs}
  Comment buffer ??? See [[https://www.youtube.com/watch?v=NlP3EDS6WGE][System Crafters: Planning the New Emacs From Scratch]]
  (towards the end of the stream) crdt.el is a real-time collaborative editing
  environment for Emacs using Conflict-free Replicated Data Types.

  [[https://www.youtube.com/watch?v=wqdT0xKMQT8][System Crafters: The Hidden Value of the Tab Bar]]
}

@block{@block-name{Startup / Loading process}
  [[https://youtu.be/74zOY-vgkyw?t=432][Emacs From Scratch #1 - Getting Started with a Basic Usable Configuration]]
  `(require ...)` looks in the directories defined by `load-path` variable
  #+BEGIN_SRC fish :results output
  mkdir emacs-from-scratch && cd emacs-from-scratch
  touch init.el
  emacs --no-init-file --load init.el & disown
  #+END_SRC
}

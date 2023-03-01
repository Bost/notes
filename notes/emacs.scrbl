#lang notes

@block{@block-name{debug / edebug}
  System Crafters: Learning To Use the Emacs Debugger
  https://youtu.be/LfwSc-lfFxM

  Debugging Lisp Programs
  https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugging.html

  Two debuggers

  M-x toggle-debug-on-error
  M-x debug-on-variable-change ;; works also on local override
  M-x debug-on-entry ;; for a function?

  edebug main function: step through execution of function call
  (debug can't do it)
  downside: need to explicitelly instrument a function
  ?`t` - automatically can execute code?
}

@block{@block-name{TODOs}
  Comment buffer - see System Crafters: Planning the New Emacs From Scratch
  https://www.youtube.com/watch?v=NlP3EDS6WGE
  (towards the end of the stream) crdt.el is a real-time collaborative editing
  environment for Emacs using Conflict-free Replicated Data Types.

  System Crafters: The Hidden Value of the Tab Bar
  https://www.youtube.com/watch?v=wqdT0xKMQT8
}

@block{@block-name{Spacemacs: Windows and Layouts / perspectives}
  YouTube - CDQuest: Spacemacs: Windows and Layouts (tutorial)
  https://youtu.be/I2C6QTtxfe8
  ;; window transient mode - window management for layouts w/o using key binding
  ~SPC w .~

  Layout 1. contains all buffers, layouts 2., 3., ... contain only selected
  buffers
  | ~SPC l 0..9~  | create new layout       |
  | ~SPC l s~     | save layout to a file   |
  | ~SPC l L~     | load layout from a file |
  | ~SPC l <tab>~ | last layout             |
  | ~SPC p l~     | project layout          |
}

@block{@block-name{EIEIO Enhanced Implementation of Emacs Interpreted Objects}
  - system for creating and working with object-oriented code in Emacs Lisp.
  - additional features on top of the Emacs Lisp object system
}

@block{@block-name{Various}
  (if (string-match "<regexp>" "string containing substring / regexp")
    (message "found")
   (message "not found"))

  ;; Set operations / sets:
  ;; https://www.gnu.org/software/emacs/manual/html_node/cl/Lists-as-Sets.html#Lists-as-Sets
  (cl-set-difference '(4 3 2 1) '(1))

  ;; list hash-table keys
  (hash-table-keys my=ht)
  (hash-table-size my=ht)  ;; size that can be used as an argument to `make-hash-table`
  (hash-table-count my=ht) ;; number of elements in the table

  (setq lst '(a b c))     ; => (a b c)
  (remove 'b lst)         ; => (a c)
  (remove 'b '(a b c))    ; => (a c)
  (add-to-list 'lst 'b)   ; => (a b c)
  (add-to-list '(a c) 'b) ; => Wrong type argument: symbolp, (a c)

  (sort (list 4 8 21 17 33 7 21 7) #'<)
  (sort (mapcar #'number-to-string (list 4 8 21 17 33 7 21 7)) #'string<)

  ;; on Ubuntu, when 'libtool not found.  Please install libtool'
  sudo apt install --yes libtool-bin

  emacs-forge: work with Git forges, such as Github and Gitlab
  Error: forge-db: The Forge database was created with a newer Forge version.
         You need to update the Forge package.
  Solution: M-x forge-reset-database / ~M-:~ (forge-reset-database)

  ;; The command arguments are munched together. Whitespace chars are not
  ;; escaped, that means arguments can't be reliably separated from each other.
  ;; E.g. when started with `emacs --eval '(setq variable "value")'`, the `ps`
  ;; returns the string: "emacs --eval (setq variable \"value\")". Here a
  ;; heuristics may be applied by trying to apply `read-string` or
  ;; `read-from-string` at the value of '--eval', assuming it is a valid sexp,
  ;; i.e. surrounded by parens or alike.
  (format "%s" (shell-command-to-string
                 (format "ps -ho command -p %s" (emacs-pid))))

  (f-write-text "write text to a file" 'utf-8 "/path/to/file.ext")
  (file-name-nondirectory "/home/jim/foo") ;; => "foo" ; `basename` in shell

  ;; CAPF completion-at-point-functions

  ;; take first 3 elements from a list
  (let ((lst '("one" "two" "three" "four" "five")))
    (butlast lst (- (length lst) 3)))

  ;; equality
  (eq 'foo 'foo)                             ;; =>  t
  (eq ?A ?A)                                 ;; =>  t
  ;; Equal floats may or may not be the same object.
  (eq 3.0 3.0)                               ;; =>  t or nil
  (eq (make-string 3 ?A) (make-string 3 ?A)) ;; =>  nil
  ;; Equal string constants or may not be the same object.
  (eq "asdf" "asdf")                         ;; =>  t or nil
  (string= "abc" "abc")                      ;; =>  t
  (string= "abc" "ABC")                      ;; =>  nil
  (string= "ab" "ABC")                       ;; =>  nil
  ;; unlike equal, if either argument is not a string or symbol, string= signals
  ;; an error.
  ;; (string= "abc" 1)                       ;; => error
  (eq '(1 (2 (3))) '(1 (2 (3))))             ;; =>  nil
  (setq foo '(1 (2 (3))))                    ;; =>  (1 (2 (3)))
  (eq foo foo)                               ;; =>  t
  (eq foo '(1 (2 (3))))                      ;; =>  nil
  (eq [(1 2) 3] [(1 2) 3])                   ;; =>  nil
  (eq (point-marker) (point-marker))         ;; =>  nil
  ;; The make-symbol function returns an uninterned symbol, distinct from the
  ;; symbol that is used if you write the name in a Lisp expression. Distinct
  ;; symbols with the same name are not eq. See Creating and Interning Symbols.
  (eq (make-symbol "foo") 'foo)              ;; =>  nil
  ;; symbol-to-string / symbol->string:
  (symbol-name 'some-symbol)                 ;; => "some-symbol"
  ;; string-to-symbol / string->symbol:
  (intern "some-symbol")                     ;; => some-symbol

  ;; drawing, schemes, painting; draw lines, rectangles and ellipses with mouse
  ;; and/or keyboard.
  M-x artist-mode   ;; more friendly than picture-mode / edit-picture
  ;; Use mouse middle-click for context menu

  | ~C-h l~     | M-x view-lossage        | show last input keystrokes / commands |
  | ~C-x C-k l~ | M-x kmacro-edit-lossage | ... and turn them to a macro          |

  M-x org-align-table ;; TODO put align-related stuff together

  | ~s~ | M-x evil-surround-edit | surround text with a single char... |
  | ~S~ | M-x evil-Surround-edit | ... on a line above and bellow      |

  Dedicated window - doesn't display any other buffer.

  Emacs extension language: Emacs Lisp
  Any extension is restricted to the functionality provided by Emacs’s built-in
  set of primitive operations.
  Guile also supports the loading of extension libraries written in C. This
  enables user code to add new primitive operations to Guile, and so to bypass
  the limitation present in Emacs Lisp.

  alist - association list
  plist - property list
  @lisp{
    (plist-get (plist-put (list ':host "localhost") ':port 42)  ':port)
    ;; => 42 (#o52, #x2a, ?*)
  }

  ;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
  ;; Enable byte-compilation by using hash / sharp quote for every symbol that
  ;; is a function name.
  (funcall (apply-partially #'+ 1) 2)                ;; => 3
  ;; (defalias 'plus-1 (apply-partially #'+ 1))
  (defalias #'plus-1 (apply-partially #'+ 1))
  (plus-1 2)                                         ;; => 3
  (funcall (-compose #'number-to-string #'1+) 1)     ;; => "2"
  (funcall (-compose #'number-to-string #'plus-1) 1) ;; => "2"
  conversion: number-to-string
  inc / increment function: 1+

  The Emacs thesis:
  Composite programs in a high-level extension language running on a kernel in a
  low-level language.

  | (boundp 'my=variable)      | test if symbol is defined   |
  | (functionp 'dbg=function)  | test if function is defined |
  | (functionp #'dbg=function) | test if function is defined |

  # emacsclient in Guix is in the package
  guix install emacs-with-editor

  ;; broken icons in the *spacemacs* buffer
  M-x all-the-icons-install-fonts

  Imaginary Programming
  https://emacsconf.org/2021/talks/imaginary/
  based on / an abstraction over Prompt Engineering and Language Models

  Notable emacs users:
  https://www.youtube.com/c/ProtesilaosStavrou/
  https://karthinks.com/

  Protesilaos Stavrou - Vlog: Moral lessons from switching to Emacs
  "I object to the characterization of the free software expert as some sort of a weirdo"
  https://youtu.be/gwT5PoXrLVs?t=2131

  | ~SPC h d~   | M-x help-describe                      |            |
  | ~SPC t g~   | M-x spacemacs/toggle-golden-ratio      | windows    |
  | ~SPC h SPC~ | M-x helm-spacemacs-help                | <topic>    |
  | ~SPC SPC~   | M-x spacemacs/helm-M-x-fuzzy-matching  | as ~M-x~   |
  | ~SPC f e v~ | M-x spacemacs/display-and-copy-version |            |
  | ~SPC w p m~ | M-x popwin:messages                    | *Messages* |

  | ~SPC s~ | spacemacs: search           |
  | ~C-M-s~ | isearch-forward-regexp      |
  | ~C-s~   | incremental search forward  |
  | ~C-r~   | incremental search backward |

  ;; Shift-Tab
  ~<s-tab>~ / ~<backtab>~

  | ~s-n~ | M-x narrow-to-defun |
  | ~s-N~ | M-x widen           |

  ;; open file and jump to / start on a line:column
  emacs +line:column path/to/file

  ;; unreachable repositories: use http instead of https
  emacs --insecure

  ;; e.g. visualization; for log analysis
  M-x highlight-symbol-at-point

  ;; wrap square brackets around sexp
  M-x paredit-wrap-square

  ;; paredit
  | ~C-k~       | kill the rest in the sexp           |
  | ~M-@"("~    | insert-parentheses                  |
  | ~M-@")"~    | move-past-close-and-reindent        |
  | ~M-s-right~ | paredit-forward-slurp-sexp (vcucni) |
  | ~M-s-left~  | paredit-forward-barf-sexp (vygrcaj) |

  ;; https://www.emacswiki.org/emacs/RegularExpression#regexp
  | \\#                   | increment the number found         |
  | \\s-                  | increment the whitespace           |
  | %s#\(.\{2\}\)#aa#g    | match / find exactly 2 occurrences |
  | \(.*?\)               | lazy match                         |
  | \(.\{2\}\)            | match / find exactly 2 occurrences |
  | \(http[[:print:]]*\)/ | match / find url                   |

  ;; regexp, syntax table, syntax classes; symbols must be used within square brackets
  | [[:space:]] | whitespace char, as defined by the syntax table, typically [\t\r\n\v\f]        |
  | \\s-        | see [:space:]                                                                  |
  | [[:blank:]] | a space or tab character                                                       |
  | [[:print:]] | printing char - either whitespace, or a graphic character matched by [:graph:] |

  ;; utf8 unicode
  | M-x describe-char       | describe char at the point                    |
  | M-x ucs-insert RET 2211 | insert unicode char upper-case sigma U+2211 ∑ |

  hide: show: folding: enable folding M-x hs-minor-mode
  | ~C-c @"@" C-c~ | toggle hiding: fold / unfold |

  ;; hide: show: folding: (also see origami folding in .spacemacs)
  | ~z o~                 | evil open fold                                 |
  | ~z z~                 | evil close fold                                |
  | ~z a~                 | evil toggle fold                               |
  | ~z r~                 | evil open (all) folds                          |
  | ~z m~                 | evil close (all) folds                         |
  | ~z t~ or ~z <return>~ | M-x evil-scroll-line-to-top                    |
  | ~z b~                 | M-x evil-scroll-line-to-bottom                 |
  | ~5z<Right>~           | move view 5 chars to the right                 |
  | ~M-x ;~               | M-x evilnc-comment-or-uncomment-lines          |
  | ~C-z~                 | M-x evil-mode - toggle emacs / evil            |
  | ~M-e~                 | M-x isearch-edit-string                        |
  |                       | evil: Edit the search string in the minibuffer |

  | ~C-x <return> f~            | M-x set-buffer-file-coding-system i.e. file format |
  | ~C-c M-j~                   | nrepl: M-x nrepl-jack-in - ? for Clojure ?         |
  | ~SPC m '~ from elisp buffer | repl: M-x ielm ELISP>                              |
  | M-x zprint-mode             | The pretty-printer for Clojure(Script) in Emacs    |

  | M-x browse-url-at-point | open web browser of the OS |
  | M-x eww                 | emacs web browser          |

  | M-x byte-compile-file | byte-compile an emacs-lisp / elisp / *.el file |

  | M-x quail-set-keyboard-layout | doesn't work in cygwin |
  | M-x quail-show-keyboar-layout |                        |

  | ~C-x <~ | scroll left  |
  | ~C-x >~ | scroll right |

  ;; help: ? emacs manual?
  ~C-h i m emacs~

  ;; help-mode
  | ~[~ / ~g b~ / ~C-c C-b~ | M-x help-go-back    |
  | ~]~ / ~g f~ / ~C-c C-f~ | M-x help-go-forward |

  | ~C-h m~           | M-x describe-mode     |                            |
  | ~C-h k~           | M-x describe-key      |                            |
  | ~C-h f~           | M-x describe-function |                            |
  | ~C-h v~           | M-x describe-variable |                            |
  | ~C-h b~ / ~SPC ?~ | M-x describe-bindings | show available keybindings |
  ;; package command-log-mode - show pressed keybindings (when screen casting)

  ;; dynamic vs. lexical binding: https://www.emacswiki.org/emacs/LexicalBinding
  (setq lexical-binding t)

  ;; dynamic vs. lexical binding:
  ;; https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding
  ;; lexical binding is for closures
  ;; -*- lexical-binding: t -*-

  ;; dynamic vs. lexical binding:
  EmacsLisp: dynamic; Scheme, CommonLisp: lexical

  ;; help: show content of the variable containing installed packages
  C-h v package-activated-list

  ;; install new packages
  M-x package-list-packages

  ;; packages grouped by keyword
  M-x finder-by-keyword

  | ~M-d~ | delete word             |
  | ~C-k~ | delete line from cursor |

  M-x goto-line

  ;; menu bar
  ~M-`~ / ~F10~

  ;; jump back to the last mark (there is a mark-ring)
  ~C-u C-SPC~

  | ~C-x ^~                    | M-x enlarge-window              |
  | ~C-x @"{"~ or ~SPC w @"["~ | M-x shrink-window-horizontally  |
  | ~C-x @"}"~ or ~SPC w @"]"~ | M-x enlarge-window-horizontally |

  ;; version control vc
  | ~C-u C-x v =~ | diff against any chosen revision                  |
  | ~C-x v u~     | discard changes                                   |
  | ~C-x v \~~    | checkout any version: master~3 - last 3th version |
  | ~C-x v l~     | commit log: f - view revision; d - view diff      |
  | ~C-x v i~     | add to version control system                     |

  ;; Transparent Remote (file) Access / Editing, Multiple Protocol (TRAMP)
  ;; method can be: ssh if anything doesn't work:
  ;; 1. delete ~/.bashrc
  ;; 2. emacs --no-init-file --eval "(setq tramp-verbose 10)" &
  ~C-x C-f~ /method:user@"@"remotehost#port:filename
  ~C-x C-f~ /ssh:test@"@"host#2222:/tmp

  | ~C-x C-f~ | M-x spacemacs/helm-find-files     | open file                        |
  | ~SPC b R~ | M-x spacemacs/safe-revert-buffer  | reload / refresh file            |
  | ~C-x x g~ | M-x revert-buffer-quick           | force reload / refresh file      |
  | ~C-x k~   | M-x kill-buffer                   | close file                       |
  | ~C-x C-b~ | M-x list-buffers                  |                                  |
  | ~C-x b~   | M-x ido-switch-buffer             |                                  |
  | ~C-x 4 f~ | M-x ido-find-file-other-window    | ctl-x-4-prefix                   |
  | ~C-x 4 b~ | M-x switch-to-buffer-other-window | ctl-x-4-prefix                   |
  | ~C-x o~   | M-x other-window                  | switch window / frame; o = other |
  | ~C-x C-s~ | M-x save-buffer                   | save file                        |
  | ~C-x s~   | M-x save-some-buffers             | save all files                   |
  | ~SPC f c~ | M-x spacemacs/save-as             |                                  |
  | ~C-x C-w~ | M-x write-file                    | save as                          |

  ;; Introduction to EShell: https://youtu.be/RhYNu6i_uY4
  ;; Video ransscript: http://howardism.org/Technical/Emacs/eshell-present.html
  ;; open command output in a buffer
  ifconfig > #<buffer interfaces>
  ;; combing elisp functions (message) with OS programs
  ;; (/usr/bin/cut) in eshell
  message "Hello world" | cut -f 1 -d ' '
  ;; Open stuff in
  ;; eshell/egrep is a compiled Lisp function in ‘em-unix.el’.
  egrep --recursive 'something' *
  rg --color=always 'population' {pwd}
  l --color=always
  exa -abghHliS --color=always --time-style=long-iso

  ;; cli: noninteractive run
  emacs --batch --eval '(message "Hello world")'
  ;; noninteractive run of an eslip script
  #!/usr/bin/emacs --script
  (message "Hello world")

  ;; run elisp file from command line
  ;; chmod +x ./hello.el; ./hello.el
  #!/bin/sh
  ":"; exec emacs --script "$0" "$@"@""
  ;; # -*- mode: emacs-lisp; lexical-binding: t; -*-
  (message "Hello world")


  ;; cssh ? ssh shell ? (somehow strange)

  ;; color-theme-calm-forest ? does not work?

  ;; yasnippet - yet another snippets. Example
  ;; `defn' `M-/' type in the defn-name, then `TAB' to complete ...
  ;; hippie-expand (dabbrev-expand?) (code completition)
  ~M-/~

  | ~C-S-Backspace~ | M-x kill-whole-line - delete whole line |

  | ~C-M-f~ | jump forward to matching brace  |
  | ~C-M-b~ | jump backward to matching brace |
  ;; cursor may need to be behind closing parenthesis '@")"'

  | ~C-M-x~ | M-x eval-defun    |
  | ~M-%~   | M-x query-replace |

  | ~M-u~ | M-x upcase-word     |
  | ~M-l~ | M-x downcase-word   |
  | ~M-c~ | M-x capitalize-word |

  ;; check a small region for spelling errors
  M-x ispell-region
  M-x ispell-buffer

  | ~C-x (~               | M-x kmacro-start-macro             |
  | ~C-x )~               | M-x kmacro-end-macro i.e. stop     |
  | ~C-x e~ or ~<f4>~     | macro: execute (e - execute again) |
  | ~M-5 <f4>~ or ~C-x e~ | macro: execute 5 times             |

  ;; repeat 4 times following command
  ~C-u n <some-command>~
  ;; indent: move text left by four spaces (M-x indent-rigidly ~C-x TAB~)
  ~C-u -4 C-x TAB~
  ;; move forward 4 lines
  ~C-u 4 C-n~

  | ~C-x u~        | M-x undo-tree-visualize |
  | ~C-_~ or ~C-/~ | M-x undo-tree-undo      |
  | ~M-_~ or ~C-?~ | M-x undo-tree-redo      |

  ;; next-buffer / previous-buffer
  ~<XF86Forward>~, ~C-x <C-right>~, ~C-x <right>~ / ~<XF86Back>~, ~C-x <C-left>~, ~C-x <left>~

  ;; mark / hilite / highlight whole buffer / mark paragraph
  ~C-x h~ / ~M-h~

  | ~C-x <left>~  or ~SPC b n~ | M-x next-buffer         |
  | ~C-x <right>~ or ~SPC b p~ | M-x previous-buffer     |
  | ~<C-down>~                 | M-x scroll-left         |
  | ~<C-up>~                   | M-x scroll-right        |
  | ~M-e~                      | M-x forward-sentence    |
  | ~M-a~                      | M-x backward-sentence   |
  | ~M-@"}"~ or ~@"}"~         | M-x forward-paragraph   |
  | ~M-@"{"~ or ~@"{"~         | M-x backward-paragraph  |
  | ~M-<~                      | M-x beginning-of-buffer |
  | ~M->~                      | M-x end-of-buffer       |

  ;; fill / reflow text - see also auto-fill-mode
  | ~SPC t F~ | M-x spacemacs/toggle-auto-fill-mode                  |
  | ~M-q~     | M-x fill-paragraph                                   |
  |           | M-x fill-region  - reflow all paragraphs in the area |

  ;; jump to the next (compilation error(s), grep results etc.)
  ~C-x `~

  ;; dotspacemacs-leader-key ~SPC~ and dotspacemacs-emacs-leader-key ~M-m~ offer
  ;; the same sub-menus, however ~SPC~ shows textual description defined by
  ;; `spacemacs|spacebind`

  | ~C-x C-w~ | M-x write-file (buffer) to a different file |
  | ~SPC f c~ | M-x spacemacs/save-as                       |

  | ~C-k~ | copy-paste: kill line                                            |
  | ~M-k~ | copy-paste: kill sentence - yank                                 |
  | ~C-w~ | copy-paste: kill region - cut                                    |
  | ~M-w~ | copy-paste: kill ring save - copy                                |
  | ~C-y~ | copy-paste: yank - paste last killed entry                       |
  | ~M-y~ | copy-paste: cycle back through previous entries in the kill ring |

  | ~y s~ | M-x magit-copy-section-value   | copy current sha1 to clipboard |
  | ~y y~ | M-x magit-show-refs            | list branches                  |
  | ~M-w~ | M-x magit-copy-buffer-revision | top sha1 to clipboard          |
  ;; magit: spin-off / spinoff
  git branch --track <new-branch-name>

  | M-x spell         | check word             |
  | M-x flyspell-mode | ? check all document ? |
  ;; Error enabling Flyspell mode: No word lists can be found for the language "en_US"
  ;; sudo apt install --yes aspell-en

  | ~C-t~         | transpose chars         |
  | ~M-t~         | transpose words         |
  | ~C-x C-t~     | transpose lines         |
  | ~C-l~ or ~zz~ | center the screen lines |

  ;; start a bash command line
  M-x shell / M-x term / M-x eshell

  ;; Dired Refecene Card / Cheatsheet
  http://www.gnu.org/software/emacs/refcards/pdf/dired-ref.pdf
  ;; TODO have a look at dired sorting
  https://www.emacswiki.org/emacs/DiredSortBySizeAndExtension
  https://github.com/jojojames/dired-sidebar
  http://xahlee.info/emacs/emacs/dired_sort.html

  ;; change file attributes: readonly / writable / executable
  ~M~ / M-x dired-do-chmod ; then enter e.g. '+x'
  M-x chmod

  ;; dired:
  | ~<normal-state> S~ | symlink                                    |
  | ~s~                | sort by ...                                |
  | ~Z~                | zip: compress / uncompress (extract) file  |
  | ~* . <ext>~        | mark all: toggle marking                   |
  | ~* s~              | mark all: executables                      |
  | ~* *~              | mark all: files (with extention)           |
  | ~* . <ext> D~      | mark & delete all files with extention     |
  | ~* c~              | change all marks                           |
  | ~\~~               | markup: all backup files                   |
  | ~#~                | markup: auto-save files                    |
  | ~g~                | refresh buffer                             |
  | ~+~                | M-x dired-create-directory                 |
  | ~R~                | M-x dired-do-rename                        |
  | ~@"("~             | toggle listing details                     |
  | ~@")"~             | M-x dired-hide-details-mode                |
  | ~C-x C-q~          | perform operations by editing dired buffer |
  | ~C-x C-q~          | M-x dired-toggle-read-only                 |
  |                    | M-x wdired-finish-edit                     |

  | ~C-x C-f [type in something] <return>~ | dired - create newfile |

  ;; dired: TODO check this
  | ~m~ | mark / unmark / toggle marking         |
  | ~*~ | mark / unmark / toggle marking         |
  | ~u~ | mark / unmark / toggle marking         |
  | ~U~ | mark all / unmark all / toggle marking |
  | ~t~ | mark / unmark (all) / toggle marking   |

  ;; parameter key / (universal-argument)
  ~C-u~

  ;; sets the line wrap to 40 characters, M-q # activate the wrap
  ~C-u 40 C-x f~

  ;; center for given line width
  M-o M-s

  ;; query-replace-regexp
  ~C-M-%~

  ;; slime - Superior Lisp Interaction Mode for Emacs
  ;; M-x gnus - read news, email, rss
  ;; M-x grep
  ;; M-x dbg
  ;; M-x ediff
  ;; M-x compile
  ;; M-x man
  ;; M-x erc/default-servers or M-x erc

  ;; M-x speedbar
  ;; Summarize information related to the current buffer. Its original inspiration
  ;; is the “explorer”

  | M-x linum-relative-toggle | line numbers: relative |
  | M-x global-linum-mode     | line numbers: absolute |

  ~M-:~ ;; M-x eval-expression

  ;; M-x eval-last-sexp and insert the result in the buffer; works only in the
  ;; evil-insert-mode
  ~C-U C-x C-e~

  M-: (font-get (face-attribute 'default :font) :size)
  | ~C-x C-+~ | increase font size |
  | ~C-x C--~ | decrease font size |

  ;; problem: emacs does not uses fonts from /usr/share/fonts
  sudo apt install --yes libgtk2.0-dev
  ./configure --with-x-toolkit=gtk
  ;; See: M-x describe-variable system-configuration-features
  ;; which is a list of some of the main features this Emacs was compiled with.

  ;; compiling emacs on the GuixOS
  guix install gtk dconf udiskie makeinfo autoconf \
               libxaw3d gnutls libtiff libungif libjpe libxpm
  unset EMACSLOADPATH

  ;; slime: reprint last command to the REPL
  ~M-p~

  ;; gui
  | M-x toggle-scroll-bar | toggle vertical scroll bar; horizontal scroll bar does not exist in emacs |
  | M-x menu-bar-mode     | toggle menu-bar                                                           |

  | M-x align-regexp | align at the given regexp |

  ;; auto completition
  ~C-n~

  | ~C-SPC~          | region: set mark (start region) |
  | ~C-x r k~        | region: kill selected region    |
  | M-x write-region | save region to a file           |

  ;; splits: close / only one buffer / horizontal / vertical
  ~C-x 0~ / ~C-x 1~ / ~C-x 2~ / ~C-x 3~

  | ~s-K~ | M-x my=kill-buffers--unwanted                  |
  |       | M-x clean-buffer-list doesn't work as expected |

  ;; remedy against "newer than byte-compiled file" try also:
  ;; cd $dev/emacs/lisp && make autoloads
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
  ;; \"<register>p

  ;; GNU locate - Finding files in a database
  M-x locate

  ;; highlighting
  M-x hi-lock-mode / highlight-regexp

  ;; magit: http://magit.github.io/master/magit.html
  | ~C-c C-c~            | M-x magit-commit       |                                |
  | ~C-c C-k~ or ~C-x k~ | M-x with-editor-cancel | cancel / abandon / kill commit |

  ;; after M-x magit-status
  | ~+~            | M-x magit-diff-more-context    | increase chunk size                   |
  | ~=~            | M-x magit-diff-less-context    | decrease chunk size                   |
  | ~0~            | M-x magit-diff-default-context | reset chunk size                      |
  | "select chunk" |                                | split chunk                           |
  | ~i~            | M-x magit-gitignore            | add to .gitignore / .git/info/exclude |

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
  easier kmacro counter https://youtu.be/CvmDtnnrYDo

  ;; Customize / extend keyboard functionality https://github.com/kmonad
  ;; emacs package https://github.com/kmonad/kbd-mode
  ;; https://github.com/jtroo/kanata#similar-projects

  ;; launch emacs and eval string
  emacs --eval '(message "ufo")' / emacs -e configuration-layer/update-packages

  ;; M-x shell-command; execute
  ~M-!~ / ~SPC !~

  ;; execute shell command and read-in / capture its output:
  ;; in evil-insert mode - in the current buffer
  ;; in evil-normal mode - in the *Shell Command Output* buffer
  ~C-u M-! <cmd>~
  ~C-u SPC ! <cmd>~
  ~C-u~ M-x shell-command

  ;; M-x shell-command: top: capture top output from stdout
  ~M-! RET top -c -n -1 -b -w 200~

  ;; M-x git-timemachine git:
  p prev / n next / w Copy abbreviated hash / W Copy full hash / g Goto nth rev /
  q Exit

  M-x profiler-start
  M-x profiler-stop
  M-x profiler-report ;; analyze execution profile

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
    (global-set-key (kbd "<H-menu>") 'execute-extended-command))

  ;; (global-set-key [(hyper 9)] (lambda () (message "[(hyper 9)]")))
  ;; (global-set-key (kbd "<rwindow>-9")
  ;;                 (lambda () (message "(kbd context-menu-9)")))
  ;; (global-set-key [(hyper 9)] (lambda () (message "[(hyper 9)]")))

  ;; M-x delete-horizontal-space - delete whitespaces around point
  ~M-\~

  ;; test yasnippet
  emacs -Q -L . -l yasnippet-tests.el -f ert &

  ;; helm-locate - see 'man locate'
  ~SPC f L~

  ;; ~M-s h l~ hilite lines e.g. log file evaluation; see:
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
  (setq fill-column 100)
  ;; `fill-column` + 1, so that in the magit-diffs everything looks fine
  (setq whitespace-line-column 101)  ;  M-x set-fill-column

  ;; emacs current directory
  (setq default-directory "~/.emacs.d/")

  ;; truncate / fold long lines (wrapping long lines creates new lines)
  M-x toggle-truncate-lines

  ;; movement
  ~C-M-a~ / M-x beginning-of-defun
  ~C-M-e~ / M-x end-of-defun

  ;; quit / delete multiple cursors
  M-x evil-mc-undo-all-cursors

  ;; Toggle preview of the LaTeX fragment at point.
  M-x org-latex-preview
  ~C-c C-x C-l~

  ;; join two lists
  (append '(1 2) '(3 4)) ; => (1 2 3 4)

  ;; TODO latex-preview-pane

  ;; https://stackoverflow.com/a/29461536
  ;; increment numbers in visual vertical block selection in emacs evil:
  ;; select e.g. 3 lines of visual block ~C-v 3~ then ~C-x r N~
}

@block{@block-name{Source Code Pro font}
  guix install font-adobe-source-code-pro
  # clean font cache
  fc-cache --verbose --force
}

@block{@block-name{Mapping Functions}
  https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html
  https://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html
  @lisp{
    mapconcat
    (mapcar 'string "abc")
    (mapcar 'list '(a b c d)) ; => ((a) (b) (c) (d))
    (mapcan 'list '(a b c d)) ; => (a b c d)  ;; i.e. with reduction
    ;; -partial is from dash.el
    (mapcar (-partial #'apply #'+) '((1 2 3) (4 5 6))) ;; => (6 15)

    mapc ;; like mapcar; used for side-effects only

    ;; mapconcat is like joins result list into a string with a separator:
    (mapconcat 'symbol-name '(The cat in the hat) "-") ; => "The-cat-in-the-hat"

    (split-string "[  aaa
     bbb   ]" (or split-string-default-separators (rx (or "[" "]"))))
    ;; => ("[" "aaa" "bbb" "]")

    ;; filter list
    (remove-if (lambda (e) (eq e 1)) '(1 2))
    ;; https://www.reddit.com/r/emacs/comments/7dp6oa/comment/dpzi5hz/?utm_source=share&utm_medium=web2x&context=3
    (seq-filter (apply-partially #'< 3) '(1 2 3 4 5 6))
  }
}

@block{@block-name{Startup / Loading process}
  System Crafters: Emacs From Scratch #1
  https://youtu.be/74zOY-vgkyw?t=432
  `(require ...)` looks in the directories defined by `load-path` /
  EMACSLOADPATH variable
  echo $EMACSLOADPATH

  ;; testing: startup: skip ~/.emacs (if messed up) / don't load the init file
  | emacs --no-init-file     | also: emacs -q  |
  | emacs --no-window-system | also: emacs -nw |

  mkdir emacs-from-scratch && cd emacs-from-scratch
  touch init.el
  emacs --no-init-file --load init.el & disown

  Andrew Tropin: GNU Guix as Emacs package manager
  https://youtu.be/gqmZjovuomc?t=90
  init.el
  default.el      ;; may be located in '/usr/local/share/emacs/site-lisp'
  ;; may be located in '/usr/local/share/emacs/site-lisp'; loaded before init.el?
  site-start.el

  Emacs package change management with quelpa and use-package
  https://youtu.be/GhItV6dE0Fo

  autoload:
  - code should be evaluated even if a package itself is NOT loaded
  - it's for lazy loading mechanism. E.g. you can provide here a function signature
  M-x list-packages
  M-x find-library

  ;; Guix channel for automatically generated emacs packages
  https://github.com/babariviere/guix-emacs
  ;; No external dependecies are captured in this channel - good only for pure
  ;; elisp packages, which don't depend on any binaries like ripgrep etc.

  emacs-tree-sitter

  spacemacs package load process should use the mechanism provided by Guix

  ;; package descriptor; It the file doesn't exit the package will not appear in
  ;; the M-x list-packages
  <package-name>-pkg.el
}

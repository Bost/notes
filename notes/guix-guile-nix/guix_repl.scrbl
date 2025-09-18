#lang notes

@block{@block-name{`guile` vs. `guix repl`}
  `info "(guile)Concept Index"`
  `guix repl` guarantees that all the Guix modules and all its dependencies are
  available in the search path. `guile` doesn't give such guarantees.

  REPL-commands (start with ',')
  https://www.gnu.org/software/guile/manual/guile.html#REPL-Commands
  https://guix.gnu.org/manual/devel/en/html_node/Using-Guix-Interactively.html

  ;; pretty-print from shell / execute a scheme expression
  $ guile -c '(use-modules (ice-9 pretty-print)) (pretty-print \'(begin ...))'
  $ guix repl
  ,use (ice-9 pretty-print) ;; ,use is an alias for ,import or (import ...)
  ;; ,use (guix read-print) ; pretty-printer smarter than (ice-9 pretty-print).
  ;; (pretty-print-with-comments (current-output-port) '1)
  ,pretty-print '(begin ...)
  ,h inspect       ;; REPL debugging:
  ,inspect EXP                 [,i] - Inspect the result(s) of evaluating EXP.
  ,pretty-print EXP           [,pp] - Pretty-print the result(s) of evaluating EXP.
  ,pk              ;; peek

  ;; repl: access module variables without importing the whole module
  (@"@" (my module) public-variable)
  (@"@"@"@" (my module) private-variable)

  # disable colors and readline bindings defined in the .guile
  @; INSIDE_EMACS=1     # bash
  set INSIDE_EMACS 1    # fish-shell
  guix repl --listen=tcp:37146 &
  M-x geiser-connect

  ;; debugging: guix repl: Print a backtrace / callstack
  > ,bt
  ;; Change the stackframe
  > ,up [COUNT] ; ,frame [COUNT] ,down [COUNT]
  ;; Show locally-bound variables in the selected frame
  ,locals

  ;; backtrace abbreviated / path expansion
  strace -e open,openat -o /tmp/outfile.strace guix home roll-back | rg guix

  ;; guix repl / List procedures provided by the REPL:
  scheme@"@"(guile-user)> ,module (srfi srfi-1)
  scheme@"@"(srfi srfi-1)> ,help module
  scheme@"@"(srfi srfi-1)> ,binding

  ;; guix repl: <list of procedures>
  scheme@"@"(srfi srfi-1)> ,pretty-print (module-uses (current-module))
  $_ = (#<interface (guile) 7ffa54060dc0>
   #<autoload (system base compile) 7ffa540edb40>
   #<interface (ice-9 session) 7ffa52cce0a0>
   #<interface (ice-9 regex) 7ffa54043a00>
   #<interface (ice-9 threads) 7ffa540a2aa0>
   #<interface (value-history) 7ffa52cce5a0>
   #<interface (geiser emacs) 7ffa51539f00>)

  scheme@"@"(guile-user)> ,use (gnu packages base)
  scheme@"@"(guile-user)> coreutils
  ;; quit / terminate the repl with exit / return code 0
  scheme@"@"(guile-user)> (primitive-exit 0)

  $ guix repl --load-path=.
  $ guix repl << EOF
    ;; it won't work - %default-system-profile is not exported
    ;; (use-modules (guix scripts home))    ,pp %default-system-profile
    (use-modules (guix download))           ,pp %mirrors
    (use-modules (guix channels))           ,pp %default-channels
    (use-modules (gnu system file-systems)) ,pp %fuse-control-file-system
    (use-modules (gnu services base))       ,pp %base-services
    (use-modules (gnu packages))
    (format #t "%patch-path:\n  ~a\n" (string-join (%patch-path) "\n  "))
    ,pp %load-path           ; guile module load-path
    ,pp %load-compiled-path
    ,pp (%site-dir)
  EOF

  $ guix repl
  scheme@"@"(guix-user)> ,use (gnu packages shells)
  scheme@"@"(guix-user)> ,use (guix gexp)
  scheme@"@"(guix-user)> #~#$fish-foreign-env
  $1 = #<gexp #<gexp-input #<package fish-foreign-env@"@"0.20190116 gnu/packages/shells.scm:278 7efedecbd0b0>:out> 7efedf25c270>

  $ guix repl
  scheme@"@"(guile-user)> (use-modules (gnu packages))
  scheme@"@"(guile-user)> (specification->package+output "glib:bin")
  ;; "out" is the default package output
  scheme@"@"(guile-user)> (specification->package+output "glib")

  $ guix repl
  scheme@"@"(guix-user)> ,option value-history
  #t
  scheme@"@"(guix-user)> ,option value-history #f
  scheme@"@"(guix-user)> ,option value-history
  #f
  scheme@"@"(guix-user)> ,option value-history #t
  scheme@"@"(guix-user)> ,option
    compile-options       (#:warnings (shadowed-toplevel use-before-definition arity-mismatch format duplicate-case-datum bad-case-datum non-idempotent-definition))
    optimization-level    #f
    warning-level         #f
    trace                 #f
    interp                #f
    prompt                #<procedure generate-colored-prompt (repl)>
    print                 #<procedure colorized-repl-printer (repl val)>
    value-history         #t
    on-error              debug
  scheme@"@"(guix-user)> ,use (guix monad-repl)
  scheme@"@"(guix-user)> ,use (guix store)
  scheme@"@"(guix-user)> (define tf (text-file "foo" "Hello!"))
  scheme@"@"(guix-user)> tf
  $3 = #<procedure 7f1daff15fc0 at guix/store.scm:2085:2 (store)>
  scheme@"@"(guix-user)> ,inspect tf
  #<procedure 7f1daf…> inspect> disassemble
  Disassembly of #<procedure 7f1daff15fc0 at guix/store.scm:2085:2 (store)> at #x7f1db7bd6484:
  ;;
     0    (instrument-entry 128571)                             at guix/store.scm:2085:2
     2    (assert-nargs-ee/locals 2 8)    ;; 10 slots (1 arg)
     3    (scm-ref/immediate 7 9 2)
     4    (scm-ref/immediate 7 7 1)
     5    (static-ref 3 117116)           ;; #f                 at guix/store.scm:1095:33
     7    (scm-ref/immediate 2 9 4)
     8    (handle-interrupts)                                   at guix/store.scm:1095:32
     9    (call-label 6 2 -17737)         ;; string->utf8@"@"rnrs/bytevectors at #x7f1db7bc4f84
    12    (receive 3 6 10)
    14    (scm-ref/immediate 5 9 3)
    15    (scm-ref/immediate 0 9 5)
    16    (mov 4 7)                                             at guix/store.scm:1095:2
    17    (mov 3 8)
    18    (mov 2 5)
    19    (mov 1 6)
    20    (handle-interrupts)
    21    (call 5 5)
    23    (receive 0 5 10)
    25    (reset-frame 2)                 ;; 2 slots            at guix/store.scm:2086:4
    26    (handle-interrupts)
    27    (return-values)
  ;;
  ----------------------------------------
  Disassembly of string->utf8@"@"rnrs/bytevectors at #x7f1db7bc4f84:
  ;;
     0    (instrument-entry 134865)                             at guix/store.scm:1095:2
     2    (immediate-tag=? 1 7 0)         ;; heap-object?
     4    (je 7)                          ;; -> L1
     5    (call-scm<-scmn-scmn 1 90486 134852 112)
     9    (static-set! 1 134840)          ;; #f
  L1:
    11    (scm-ref/immediate 1 1 1)
    12    (handle-interrupts)
    13    (tail-call)
  #<procedure 7f1daf…> inspect> quit
  scheme@"@"(guix-user)> (gexp? #~#$fish-foreign-env)
  $2 = #t
  scheme@"@"(guile-user)> ,use (gnu packages base)
  scheme@"@"(guile-user)> coreutils
  $3 = #<package coreutils@"@"8.29 gnu/packages/base.scm:327 3e28300>
  scheme@"@"(guile-user)> (macroexpand '(unquote foo))
  ...

  ;; Configure interpreter by modifying the ~/.guile file
  ;; (create it if it doesn't exist) and put the following lines into it:
  ;; requires `guix install guile-colorized guile-readline`
  (use-modules (ice-9 readline) (ice-9 colorized))
  (activate-readline)
  (activate-colorized)
}

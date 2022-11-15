#lang notes

@block{@block-name{Guile Scheme}
  https://sourcegraph.com/search
  https://sourcehut.org
  https://codeberg.org

  alist - association list, i.e. dictionary
  https://www.gnu.org/software/guile/manual/html_node/Alist-Example.html
  plist - property list (see plist in emacs-lisp)

  Create and publish Guile projects
  https://gitlab.com/a-sassmannshausen/guile-hall

  define-syntax vs. defmacro / defmacro-public

  https://wiki.c2.com/

  Google: daviwil system common

  ;; Read a sexp from the input port PORT, or from the current input
  ;; port if PORT is not specified
  (read)
  (+ 1 2)  ;; manually typed-in
  $7 = 3
  (read-line) ;; doesn't exist?
  (eval-string "(+ 1 2)" (interaction-environment))
  (eval '(+ 1 2) (interaction-environment))
  (cdr (syntax->datum (call-with-input-string "(+ 1 2)" read-syntax)))
  ;; read a string and turn in to a sexp and evaluate a part of it
  (eval (cadr (syntax->datum
             (call-with-input-string
                 "(foo (+ 1 2))"
               read-syntax)))
      (interaction-environment))
   ;; read a string and show position where the sexp starts
   (syntax-source (call-with-input-string "  (+ 1 2)" read-syntax))
   => ((line . 0) (column . 2))

  - Monads are programmatic representations of stateful
  computation - which is more powerful than just 'doing' the computation, as
  it's basically homoiconicity.

  - The IO Monad is simply applying a function on the world.

  https://www.gnu.org/software/guile/manual/guile.html#Latent-Typing
  https://www.gnu.org/software/guile/manual/guile.html#Dynamic-Types
  Guile is dynamically (latent) typed. Types exist only at run time.
  Haskell types might be used in Guile
  http://www.haskell.org/haskellwiki/FFI_Introduction

  In Guile, this uniform representation of all Scheme values is the C type SCM.
  This is an opaque type and its size is typically equivalent to that of a
  pointer to void. Thus, SCM values can be passed around efficiently and they
  take up reasonably little storage on their own.

  Never access a SCM value directly. Pass it to functions / macros defined in
  libguile. E.g. the sum of two SCM values can't be computed by adding them with
  the C + operator. The libguile function scm_sum must be used.

  Embeding Guile interpreter into a program / library - it with libguile.

  #<unspecified>
  the evaluated expression has no result specified

  Tail Call Optimisation
  the compiler will rewrite the recursive form into a serialised iterative form.

  Scheme Requests for Implementation SRFI (read surfi):
  https://srfi.schemers.org/
  can be done via individual initiative.
  ;;
  Revised-n Report on the Algorithmic Language Scheme RnRS:
  reached by consensus, e.g. https://small.r7rs.org/attachment/r7rs.pdf

  Installing Guile Scheme:
  https://jeko.frama.io/en/install.html

  Ports are the way that Guile performs input and output
}

@block{@block-name{`guile` vs. `guix repl`}
  `guix repl` guarantees that all the Guix modules and all its dependencies are
  available in the search path. `guile` doesn't give such guarantees

  `info "(guile)Concept Index"`

  https://www.rohleder.de/~mike/guix-workflow/guix-workflow.html

  ;; Configure interpreter by modifying the ~/.guile file
  ;; (create it if it doesn't exist) and put the following lines into it:
  (use-modules (ice-9 readline)
               ;; requires `guix install guile-colorized`
               (ice-9 colorized))
  ;;
  (activate-readline)
  (activate-colorized)
}

@block{@block-name{Partial function application.}
  Notation for Specializing Parameters without Currying
  https://srfi.schemers.org/srfi-26/srfi-26.html

  @lisp{
    ;; 'lambda*' is ‘lambda’ with allowed optional and keyword argument.
    ;; 'define*' is syntactic sugar for 'lambda*'

    (use-modules (srfi srfi-26))
    (map (cut * 2 <>) (1 2 3 4))
    ;; also variadic function arguments
    (define (partial fun . args)
    ;; Note: 'cons' is a bit faster than 'append'
    (lambda x (apply fun (append args x))))
    ;; or
    (define* (partial fun #:rest args)
    (lambda x (apply fun (append args x))))
    ;; '#:rest' is a synonym for the dotted syntax rest argument.
  }
}

@block{@block-name{Various code snippets}
  @lisp{
    ;; Set operations / sets:
    (use-modules (srfi srfi-1))
    ,use (srfi srfi-1)
    ;; (lset-difference eqv? '(3 2 1) '(1 2)) ;; > (3)

    ;; https://stackoverflow.com/a/38397019/5151982
    ;; console as input and output
    (use-modules (ice-9 textual-ports))
    ;; TODO check this:
    (display (get-string-n (current-input-port) 6))

    (access? some-file F_OK) ;; check if file exists
    (access? some-file W_OK) ;; check if file is writable

    ;; check for empty list:
    (null? '())  ; => #t
    (null? '(1)) ; => #f
    ;; check for empty string
    (string-null? "")  ; => #t
    (string-null? "1") ; => #f

    ;; in clojure: (some pred coll); or `some->` etc.
    ;; find pred lst
    ,use (srfi srfi-1)
    (find (lambda (s) (string? "abc" s)) '("a" "b" "abc" "d"))

    ;; Pattern matching with let*:
    (define mapping (list ...))
    ;;
    (define d-this (car mapping))
    (define d-remaining (cdr mapping))
    (define d-name (car this))
    (define d-value (cdr this))
    ;;
    (let* ((((l-name . l-value) . l-remaining) mapping))
    (format #t "~a\n" (equal? l-name      d-name))
    (format #t "~a\n" (equal? l-value     d-value))
    (format #t "~a\n" (equal? l-remaining d-remaining)))


    (use-modules (ice-9 rdelim)
                 (ice-9 popen)
                 (ice-9 regex)
                 (srfi srfi-1) ;; fold
                 #| (language cps intmap) |#)
    ;;
    (define (get-type o)
    "TODO implement: 1 is a number and an integer in the same type"
    (cond
      ((port? o) 'port)
      ((boolean? o) 'boolean)
      ((string? o) 'string)
      ((symbol? o) 'symbol)
      ((list? o) 'list)
      ((vector? o) 'vector)
      ((procedure? o) 'procedure)
      ((complex? o) 'complex)
      ((real? o) 'real)
      ((integer? o) 'integer)
      ((number? o) 'number)
      (#t 'unknown-type)))

    (define (partial fun . args)
      (lambda x (apply fun (append args x))))

    (define (read-all port)
      "Return a list of all lines from the PORT."
      (let loop ((res '())
                 (str (read-line port)))
        (if (and str (not (eof-object? str)))
            (loop (append res (list str))
                  (read-line port))
            res)))

    ;; TODO see 'push all branches to all remotes'
    ;; https://stackoverflow.com/a/18674313/5151982

    (define (main args)
      ((compose
      (partial format #t "~a\n")
      (partial map
               (compose
                (lambda (cmd)
                  (let* ((port (open-input-pipe cmd))
                         (res (read-all port)))
                    (close-pipe port)
                    res))
                (lambda (s)
                  ;; TODO implement pretty-print for bash commands
                  ;; ~a - outputs an argument like display
                  ;; ~s - outputs an argument like write (i.e. print to string)
                  (format #t "\n~a\n\n" s)
                  s)
                (lambda (cmd) (string-join cmd " "))))
      (partial map (lambda (remote)
                     (append
                      (list "git" "push" "--follow-tags" "--verbose" remote)
                      (cdr args))))
      ;; map-reduce
      (partial fold-right (lambda (a d) (if (string? a) (cons a d) d)) '())
      (partial map
               (compose
                (lambda (match-structure) (if match-structure
                                              (match:substring match-structure 1)))
                (partial string-match "remote\\.(.*?)\\.url")))
      (lambda (cmd)
        (let* ((port (open-input-pipe cmd))
               (res (read-all port)))
          (close-pipe port)
          res))
      (lambda (s)
        ;; TODO implement pretty-print for bash commands
        ;; ~a - outputs an argument like display
        ;; ~s - outputs an argument like write (i.e. print to string)
        (format #t "\n~a\n\n" s)
        s)
      (lambda (cmd) (string-join cmd " ")))
     (list
      "git" "config" "--local" "--list")))

    ;; `letrec` - like `let`, but enables function definitons which can refer each
    ;; other
    (letrec ((even? (lambda (n)
                      (if (zero? n)
                          #t
                          (odd? (- n 1)))))
             (odd? (lambda (n)
                     (if (zero? n)
                         #f
                         (even? (- n 1))))))
      (even? 88))
  }
}

@block{@block-name{Keywords}
  (keyword? #:foo) ; => #t
  ;; (keyword? :foo) ; => error
  ;; (keyword? foo:) ; => error
  (read-set! keywords 'prefix)
  (keyword? :foo) ; => #t
  (read-set! keywords 'postfix)
  (keyword? foo:) ; => #t
  The `(keyword? :foo:)` will work if any of the `(read-set! ...)` is evaluated.
}

@block{@block-name{G-expression - gexp}
  - can expand paths in the store and act similar to backquote and comma for
    list expansion - but use ‘#~’ and ‘#$’ instead. It can be used to
    generate derivations.
  - a form of S-expression adapted to build expressions. It can contain a
    package record or any file-like object which will be replaced by its '/gnu/'
  - a way of staging code to run as a derivation.
  - make it really easy to do complicated things, mostly by providing an easy
    way to handle the representation of things that can end up as store items.
  - can contain a package record or any other "file-like object" and, when that
    'gexp' is serialized for eventual execution, the package is replaced by its
    /gnu/store/... file name.
  - derivation represents a sequence of low-level build actions and the
    environment in which they are performed to produce an item in the store

  Syntactic forms:
  | #~     | gexp            | quasiquote                                                      |
  | #$     | ungexp          | unquote                                                         |
  | #+     |                 | same role as #$, but it's a reference to a native package build |
  | #$@"@" | ungexp-splicing | unquote-splicing / splice                                       |

  Examples:
  $ guix repl
  scheme@(guix-user)> ,use (gnu packages shells)
  scheme@(guix-user)> ,use (guix gexp)
  scheme@(guix-user)> #~#$fish-foreign-env
  $1 = #<gexp #<gexp-input #<package fish-foreign-env@0.20190116 gnu/packages/shells.scm:278 7efedecbd0b0>:out> 7efedf25c270>

  $ guix repl
  scheme@(guix-user)> ,option value-history
  #t
  scheme@(guix-user)> ,option value-history #f
  scheme@(guix-user)> ,option value-history
  #f
  scheme@(guix-user)> ,option value-history #t
  scheme@(guix-user)> ,option
    compile-options       (#:warnings (shadowed-toplevel use-before-definition arity-mismatch format duplicate-case-datum bad-case-datum non-idempotent-definition))
    optimization-level    #f
    warning-level         #f
    trace                 #f
    interp                #f
    prompt                #<procedure generate-colored-prompt (repl)>
    print                 #<procedure colorized-repl-printer (repl val)>
    value-history         #t
    on-error              debug
  scheme@(guix-user)> ,use (guix monad-repl)
  scheme@(guix-user)> ,use (guix store)
  scheme@(guix-user)> (define tf (text-file "foo" "Hello!"))
  scheme@(guix-user)> tf
  $3 = #<procedure 7f1daff15fc0 at guix/store.scm:2085:2 (store)>
  scheme@(guix-user)> ,inspect tf
  #<procedure 7f1daf…> inspect> disassemble
  Disassembly of #<procedure 7f1daff15fc0 at guix/store.scm:2085:2 (store)> at #x7f1db7bd6484:

     0    (instrument-entry 128571)                             at guix/store.scm:2085:2
     2    (assert-nargs-ee/locals 2 8)    ;; 10 slots (1 arg)
     3    (scm-ref/immediate 7 9 2)
     4    (scm-ref/immediate 7 7 1)
     5    (static-ref 3 117116)           ;; #f                 at guix/store.scm:1095:33
     7    (scm-ref/immediate 2 9 4)
     8    (handle-interrupts)                                   at guix/store.scm:1095:32
     9    (call-label 6 2 -17737)         ;; string->utf8@rnrs/bytevectors at #x7f1db7bc4f84
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

  ----------------------------------------
  Disassembly of string->utf8@rnrs/bytevectors at #x7f1db7bc4f84:

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
  scheme@(guix-user)> (gexp? #~#$fish-foreign-env)
  $2 = #t
  scheme@(guile-user)> ,use (gnu packages base)
  scheme@(guile-user)> coreutils
  $3 = #<package coreutils@8.29 gnu/packages/base.scm:327 3e28300>
  scheme@(guile-user)> (macroexpand '(unquote foo))
  ...
}

@block{@block-name{Module installation}
  see `info "(guile)Installing Site Packages"`
}

@block{@block-name{begin}
  (begin
    ...)
  A begin form in a definition context splices(! not sequences) its subforms
  into its place.

  Splicing and sequencing are different. It can make sense to splice zero forms,
  because it can make sense to have zero internal definitions before the
  expressions in a procedure or lexical binding form. However it does not make
  sense to have a sequence of zero expressions, because in that case it would
  not be clear what the value of the sequence would be, because in a sequence of
  zero expressions, there can be no last value. Sequencing zero expressions is
  an error.
}

@block{@block-name{Equality}
  |        | returns `#t' if X and Y are:                         |
  | equal? | the same type, and their contents or value are equal |
  | eq?    | the same object, except for numbers and characters   |
  | =      | numerically equal                                    |
}

@block{@block-name{SXML}
  Alternative syntax for writing XML data (more precisely, XML Infosets[1]) as
  S-expressions, to facilitate working with XML data in Lisp and Scheme. An
  associated suite of tools[which?] implements XPath, SAX and XSLT for SXML in
  Scheme[2][3] and are available in the GNU Guile implementation of that
  language.
}

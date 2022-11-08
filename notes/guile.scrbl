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

    ;; Formatted output like fprintf
    ;; https://www.gnu.org/software/guile/docs/docs-1.6/guile-ref/Formatted-Output.html
    (format #t "\n~a\n\n" s)
    (format #f "\n~a\n\n" s) ;; print to string

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

  Syntactic forms:
  | #~     | gexp            | quasiquote                |
  | #$     | ungexp          | unquote                   |
  | #$@"@" | ungexp-splicing | unquote-splicing / splice |
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

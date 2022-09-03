#lang notes


https://www.gnu.org/software/guile/manual/html_node/Dynamic-Types.html
Guile Scheme is dynamically-typed. Types only become apparent at run time.

In Guile, this uniform representation of all Scheme values is the C type SCM.
This is an opaque type and its size is typically equivalent to that of a pointer
to void. Thus, SCM values can be passed around efficiently and they take up
reasonably little storage on their own.

The most important rule is: You never access a SCM value directly; you only pass
it to functions or macros defined in libguile.

As an obvious example, although a SCM variable can contain integers, you can of
course not compute the sum of two SCM values by adding them with the C +
operator. You must use the libguile function scm_sum.

TO embed the Guile Scheme interpreter into your program or library, you need to
link it against the libguile.

@block{@block-name{SXML}
  alternative syntax for writing XML data (more precisely, XML Infosets[1]) as
  S-expressions, to facilitate working with XML data in Lisp and Scheme. An
  associated suite of tools[which?] implements XPath, SAX and XSLT for SXML in
  Scheme[2][3] and are available in the GNU Guile implementation of that
  language.
}

@block{@block-name{Guile vs. guix repl}
  `guix repl` guarantees that all the Guix modules and all its dependencies are
  available in the search path. `guile` doesn't give such guarantees

  Guile is an implementation of the Scheme programming language.
  See `info "(guile)Concept Index"`

  Scheme Requests for Implementation SRFI
  https://srfi.schemers.org/
  https://small.r7rs.org/attachment/r7rs.pdf

  Installing Guile Scheme:
  https://jeko.frama.io/en/install.html

  Ports are the way that Guile performs input and output

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
  [https://srfi.schemers.org/srfi-26/srfi-26.html]

  ;; ‘lambda*’ is ‘lambda’, with allowed optional and keyword arguments
  ;; ‘define*’ is syntactic sugar for defining procedures using ‘lambda*’

  (use-modules (srfi srfi-26))
  (map (cut * 2 <>) (1 2 3 4))
  ;; also variadic function arguments
  (define (partial fun . args)
    (lambda x (apply fun (append args x))))
  ;; or
  (define* (partial fun #:rest args)
    (lambda x (apply fun (append args x))))
  ;; '#:rest' is a synonym for the dotted syntax rest argument.
}

@block{@block-name{Various code snippets}
  ;; https://stackoverflow.com/a/38397019/5151982
  ;; console as input and output
  (use-modules (ice-9 textual-ports))
  ;; TODO check this:
  (display (get-string-n (current-input-port) 6))

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

@block{@block-name{Formatted output}
  like fprintf. See [[https://www.gnu.org/software/guile/docs/docs-1.6/guile-ref/Formatted-Output.html][Formatted Output]]
  (format #t "\n~a\n\n" s)
  (format #f "\n~a\n\n" s) ;; print to string
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

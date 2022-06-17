#lang notes

#+title: Guile

@block{@block-name{Guile vs. guix repl}
  `guix repl` guarantees that all the Guix modules and all its dependencies are
  available in the search path. `guile` doesn't give such guarantees

  Guile is an implementation of the Scheme programming language.

  See `info "(guile)Concept Index"`

  [[https://srfi.schemers.org/][Scheme Requests for Implementation SRFI]]

  https://jeko.frama.io/en/install.html

  Ports are the way that Guile performs input and output

  #+BEGIN_SRC bash :results output
  #+END_SRC

  Then, you can configure your interpreter by modifying the ~/.guile file (create
  it if it doesn't exist) and put the following lines into it:

  (use-modules (ice-9 readline)
               ;; requires `guix install guile-colorized`
               (ice-9 colorized))

  (activate-readline)
  (activate-colorized)
}

@block{@block-name{Partial function application.}
  See [[https://srfi.schemers.org/srfi-26/srfi-26.html][Notation for Specializing Parameters without Currying]]

  (use-modules (srfi srfi-26))
  (map (cut * 2 <>) (1 2 3 4))
  ;; also
  (define (partial fun . args)
    (lambda x (apply fun (append args x))))
}

@block{@block-name{Various code snippets}
  (use-modules (ice-9 rdelim)
               (ice-9 popen)
               (ice-9 regex)
               (srfi srfi-1) ;; fold
               ;; (language cps intmap)
               )

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
  #+BEGIN_SRC scheme
  (keyword? #:foo) ; => #t
  ;; (keyword? :foo) ; => error
  ;; (keyword? foo:) ; => error
  (read-set! keywords 'prefix)
  (keyword? :foo) ; => #t
  (read-set! keywords 'postfix)
  (keyword? foo:) ; => #t
  #+END_SRC
  The `(keyword? :foo:)` will work if any of the `(read-set! ...)` is evaluated.
}

@block{@block-name{Formatted output}
  like fprintf. See [[https://www.gnu.org/software/guile/docs/docs-1.6/guile-ref/Formatted-Output.html][Formatted Output]]
  #+BEGIN_SRC scheme
  (format #t "\n~a\n\n" s)
  (format #f "\n~a\n\n" s) ;; print to string
  #+END_SRC
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
  | #~  | gexp            | quasiquote       |
  | #$  | ungexp          | unquote          |
  | #$@"@" | ungexp-splicing | unquote-splicing |

}

@block{@block-name{Module installation}
  see `info "(guile)Installing Site Packages"`
}

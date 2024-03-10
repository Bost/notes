#lang notes

@block{@block-name{Guile Scheme}
  ;; `iota` in Guile Scheme is similar to `range` in Clojure
  (iota 6)        ⇒ (0 1 2 3 4 5)
  (iota 4 2.5 -2) ⇒ (2.5 0.5 -1.5 -3.5)

  (define my-list '(a b c d e))
  (list-ref my-list 2)  ;; nth element of a list

  https://sourcegraph.com/search
  https://sourcehut.org
  https://codeberg.org

  (define circular-list (let ((x '(1 2 3))) (set-cdr! (cddr x) x) x))
  (length+ circular-list) ;; => #f

  alist - association list, i.e. dictionary
  https://www.gnu.org/software/guile/manual/html_node/Alist-Example.html
  @lisp{
    (define capitals '(("New York" . "Albany")
                       ("Oregon"   . "Salem")
                       ("Florida"  . "Miami")))
    ;; What's the capital of Oregon?
    (assoc "Oregon" capitals)       ;; ⇒ ("Oregon" . "Salem")
    ;; Get value stored under the key "Oregon" from the alist `capitals`
    (assoc-ref capitals "Oregon")   ;; ⇒ "Salem"
  }
  plist - property list (see plist in emacs-lisp)

  (use-modules (ice-9 hash-table))
  (define ht (make-hash-table))
  (hash-set! ht "one" 1)
  (hash-set! ht "two" 2)
  (hash-set! ht "three" 3)
  (hash-for-each (lambda (key value) (format #t "~a => ~a~%" key value)) ht)

  Create and publish Guile projects
  https://gitlab.com/a-sassmannshausen/guile-hall

  Programming concepts & philosophy about SW development & design
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
  (unspecified? *unspecified*)         ;; => #t
  (equal? (when #f #t) *unspecified*)  ;; => #t
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

@block{@block-name{Partial function application.}
  Notation for Specializing Parameters without Currying
  https://srfi.schemers.org/srfi-26/srfi-26.html

  @lisp{
    ;; https://www.gnu.org/software/guile/manual/guile.html#lambda_002a-and-define_002a
    ;; 'lambda*' is ‘lambda’ with allowed optional and keyword argument.
    ;; 'define*' is syntactic sugar for 'lambda*'

    (use-modules (srfi srfi-26))
    (map (cut * 2 <>) '(1 2 3 4)) ;; => (2 4 6 8)
    (map (cut * 2) '(1 2 3 4)) ;; => Wrong number of arguments ...
    ;; Also variadic function arguments:
    (define (partial fun . args)
      ;; Note: 'cons' is a bit faster than 'append'
      (lambda x (apply fun (append args x))))
    ;; or:
    ;; '#:rest' is a synonym for the dotted syntax rest argument.
    (define* (partial fun #:rest args)
      (lambda x (apply fun (append args x))))

    (define* (fun x #:key kw (kw-opt #f) #:rest args)
      "kw-opt is a keyword and optional argument at the same time"
      ;; (apply list p kw kw-opt args) ;; flattens the output list
      (list x kw kw-opt args))
    ;;
    (fun 'x #:kw '42 'args)             ;; (x 42 #f (#:kw 42 args))
    (fun 'x #:kw '42 #:kw-opt 'x 'args) ;; (x 42 x (#:kw 42 #:kw-opt x args))
    (apply fun (list 'x #:kw '42 'args));; (x 42 #f (#:kw 42 args))

    (define* (fun x #:optional (y 'y-default-val))
      (list x y))
    (fun 1)                             ;; (1 y-default-val)
    (fun 1 2)                           ;; (1 2)
  }
}

@block{@block-name{Various code snippets}
  ;; try-catch
  ;; See https://vijaymarupudi.com/blog/2022-02-13-error-handling-in-guile.html
  (begin
    (use-modules (ice-9 exceptions))
    (guard (exception (else (format #t "[catch] An exception was thrown:\n")
                            (format #t "[catch] ~a\n\n" exception)))
      (format #t "[try]")
      (/ 1 0)
      (format #t "[try] Unreachable\n"))
    (format #t "Moving on.\n"))

  ;; ignore exception
  (begin
    (use-modules (ice-9 exceptions))
    (guard (exception (else #f)) (/ 1 0))
    (format #t "Moving on.\n"))

  ;; See https://vijaymarupudi.com/blog/2022-02-13-error-handling-in-guile.html
  (use-modules (ice-9 exceptions))
  ;;
  (define (disk-space-amount) 1000)
  (define (disk-space-left? query) (< query (disk-space-amount)))
  ;;
  (define-exception-type &read-exception &exception make-read-exception read-exception?
                                          ; (field-name field-accessor) ...
    (read-reason read-exception-reason)
    (read-severity read-exception-severity))
  ;;
  (with-exception-handler
      (lambda (exception)
        (cond
         ((and (read-exception? exception)
               (eq? (read-exception-reason exception)  'almost-full))
          (format #t "the disk is almost full, only has ~a left.\n"
                  (disk-space-amount))
          (format #t "please provide a different file size: ")
          (let ((new-file-size (read)))
            (if (disk-space-left? new-file-size)
                new-file-size
                (raise-exception exception))))
         (else (raise-exception exception))))
    (lambda ()
      (let ((file-size (if (disk-space-left? 1028)
                           1028
                           (raise-continuable
                            (make-read-exception 'almost-full 'medium)))))
        (format #t "writing ~a\n" file-size))))

  (* 3-8i 2.3+0.3i) ;; complex numbers

  @lisp{
    (use-modules (srfi srfi-1))
    (remove (lambda (service)
              (member (service-kind service) (list gdm-service-type)))
            %desktop-services)
    (remove (lambda (service)
              (eq? (service-kind service) gdm-service-type))
            %desktop-services)
    ;; in guix system configuration the `delete` can be used:
    (modify-services %desktop-services (delete gdm-service-type))
    ;; TODO (partition ...)

    (use-modules (srfi srfi-13) (ice-9 regex))
    (define s "How
    Are
    You")
    (string-tokenize s (char-set-complement (char-set #\newline)))

    (define (read-scheme-file filename)
      (reverse
       (call-with-input-file filename
         (lambda (p)
           ;; (format #t "p: ~a\n" p)
           (let loop ((stx
                       (read-syntax p))
                      (result '()))
             (cond
              [(eof-object? stx) result]
              [else
               (loop (read-syntax p) (cons stx result))]))))))

    (equal? 'emacs-pkg (symbol-append 'emacs- 'pkg)) ;; => t

    ;; #\space - blank character, whitespace
    (string-split "aa bb cc" #\space) ;; => ("aa" "bb" "cc")

    (use-modules (guix utils))  ;; ,use (guix utils)
    (define* (my-function a b #:key (c 0) (d #f)) (list a b c d))
    (define my-args '(1 2 #:c 3 #:d 4))
    (define my-replacements '(((#:c c) (1+ c))
                              ((#:d d) (1+ d))))
    (define modified-args
      (eval
       `(substitute-keyword-arguments my-args ,my-replacements)
       (interaction-environment)))
    (apply my-function modified-args) ;; => (1 2 3 4)

    ;; pwd - print working directory
    (getcwd)

    (define second (time-second (current-time)))
    (define second 1607841890)
    (strftime "%Y-%m-%d %H:%M:%S" (localtime second))
    ;; => "2020-12-13 07:44:50"
    (strftime "%Y%m%d%H%M%s" (localtime second))
    ;; => "20201213074450"
    (strftime "%Y%m%d_%H%M%s" (localtime second))
    ;; => "20201213_074450"
    (strftime "%Y%m%d_%H%M%S" (localtime second))
    ;; => "20201213_074450"

    ;; Set operations / sets:
    ,use (srfi srfi-1) ;; or (use-modules (srfi srfi-1))
    (lset-difference eqv? '(4 3 2 1) '(1) '(2 3))     ;; > (4)
    (lset-intersection eqv? '(1 2) '(2 3 4) '(2 5 6)) ;; > (2)
    (lset-union eqv? '(1) '(2))                       ;; > (2 1)

    ;; https://stackoverflow.com/a/38397019/5151982
    ;; console as input and output
    (use-modules (ice-9 textual-ports))
    ;; TODO check this:
    (display (get-string-n (current-input-port) 6))

    (access? some-file F_OK) ;; check if file or directory exists
    (access? some-file W_OK) ;; check if file or directory is writable

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

@block{@block-name{Module installation}
  see `info "(guile)Installing Site Packages"`
}

@block{@block-name{The begin form}
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

@block{@block-name{Testing}
  ,use (srfi srfi-64)
  ;; Initialize and give a name to a simple testsuite.
  (test-begin "vec-test")
  (define v (make-vector 5 99))
  ;; Require that an expression evaluate to true.
  (test-assert (vector? v))
  ;; Test that an expression is eqv? to some other expression.
  (test-eqv (vector-ref v 2) 99)
  (vector-set! v 2 7)
  (test-eqv (vector-ref v 2) 7)
  ;; Finish the testsuite, and report results.
  (test-end "vec-test")
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

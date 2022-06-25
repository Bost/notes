#lang notes

#+title: Racket

#lang video

In Ubuntu VM
sudo apt update && sudo apt upgrade

sudo apt install openssh-server

sudo apt install racket ffmpeg pulseaudio libssl-dev libportaudio2

raco pkg install video-testing

# sudo systemctl restart gdm



@block{@block-name{Factorio Mods}
  @block{@block-name{Rampant}
    Last change May 2022; Lua 97.0% Racket 3.0%
    @url{https://github.com/veden/Rampant}
    @url{https://mods.factorio.com/mod/Rampant}
  }

  @block{@block-name{Planning}
    Last change June 2020; Racket 100.0%
    @url{https://github.com/jackfirth/planning.git}
    @url{https://pkgs.racket-lang.org/package/planning}
  }
}

@block{@block-name{Command line snippets}
  # package management
  # install local package
  raco pkg install
  # install 3rd party package from package repository
  raco pkg install <package>
  raco pkg install --auto <package>  # auto-install required dependecies
  raco pkg remove  <package>

  # how to run a Racket program
  racket program.rkt

  # 'Hello World' from the command line
  racket -e    '(printf "Hello World\n")'
  racket -eval '(printf "Hello World\n")'
}

@block{@block-name{Structures}
  ;; structures definition
  (struct structName (item0
                      item1
                      ; ...
                      item-n)
    #:transparent)    ;; instantiation
  (define s (structName 'a 'b 'c))
  ;; accessing fields of the structure instance via the dash '-'
  ;; structName-item1 is the structure accessor
  (structName-item1 s) ;; => 'b
}

@block{@block-name{define vs let}
  ;; @url{https://stackoverflow.com/a/5406423}
  ;; difference between `define` and `let` is in the scope:
  ;; correct:
  (define (f x)
    (let ((a 1))
      (+ a x)))
  ;; wrong:
  (define (f x)
    (let ((a 1))) ; `a` is outside of `let`
    (+ a x))
  ;; correct:
  (define (g x)
    (define a 1)
    (+ a x))
}

@block{@block-name{Various, racket snippets, etc.}
  arcade package - writing arcade games

  ;; code block
  ;; @url{https://docs.racket-lang.org/reference/begin.html}
  (begin
    (printf "hi\n")
    2)

  ;; pwd; print working directory
  (current-directory)

  ;; difference between `print`, `write` and `display`
  ;; @url{https://docs.racket-lang.org/guide/read-write.html}
  (printf "~a as a string is ~s.\n" '(3 4) "(3 4)")

  ;; followind printf-lines do the same
  (eprintf form v ...)
  (fprintf (current-error-port) form v ...)

  ;; count
  (length '(1 2 3))
  (hash-count #hash((foo . 41)))
  (hash-count #hasheq((foo . 41)))

  ;; (: flexible-length (-> (U String (Listof Any)) Integer))
  (define (flexible-length str-or-lst)
  (if (string? str-or-lst)
      (string-length str-or-lst)
      (length str-or-lst)))

  ;; function composition: compose, compose1
  (define (inc n) (+ n 1))
  ((compose str inc length) '(1 2 3))

  ;; info.rkt: collection of dependecies

  ;; show doc in browser using local copy.
  ;; C-c d / M-x racket-doc
  ;; C-c . / M-x racket-describe

  ;; sexp comment #;single-sexp
  ;; block comment #| sexp-1 sexp-2 ... |#

  ;; rhs - right hand side

  ;; namespace alias
  (require (prefix-in my: "file.rkt")) ;; see also `rename-in`
  ;; usage
  (my:function)

  ;; filter list @url{https://stackoverflow.com/a/57814082}
  (define lst '(("Ben" 2 "dog") ("Kath" 1 "cat") ("Matt" 6 "dog")))
  (filter (lambda (e) (equal? (caddr e) "dog")) lst)
  ;; => '(("Ben" 2 "dog") ("Matt" 6 "dog"))

  (map string? '(1 "a" 3))  ;; => '(#f #t #f)

  (andmap number? '(1 2 3)) ;; => #t
}

@block{@block-name{Macros: quotes, syntax, etc.}
  @url{https://docs.racket-lang.org/syntax-parse-example/}
  (quote-syntax (1 2 3))
  quote-syntax - similar to syntax. Unlike syntax (#'), quote-syntax does not
  substitute pattern variables bound by with-syntax, syntax-parse, or syntax-case.
  | '   | quote                  |
  | `   | quasiquote - see notes |
  | ,   | unquote                |
  | ,@"@"  | unquote-splicing       |
  | #'  | syntax                 |
  | #`  | quasisyntax            |
  | #,  | unsyntax               |
  | #,@"@" | unsyntax-splicing      |

  Other
  | #""       | byte-string; predicate `bytes?` |
  | (: v t)   | `v` has a type `t`              |
  | (: v : t) | `v` has a type `t`              |
}

@block{@block-name{REPL}
  The `enter!` form both loads the code and switches the evaluation context to
  the inside of the module, just like DrRacket’s Run button.
  (enter "mymodule.rkt")
}

@block{@block-name{Cons and List}
  `list` is an abbreviation for a series of `cons`:
  (equal? (list 1 2 3)
          (cons 1 (cons 2 (cons 3 null))))
}

@block{@block-name{Read and evaluate code from string}
  (eval (read (open-input-string "(+ 1 2)")))
  ;; or:
  (with-input-from-string "(+ 1 2)"
    (lambda () (eval (read))))
  ;; or:
  (eval (call-with-input-string "(+ 1 2)" read)) ;; string port is auto-closed
}

@block{@block-name{Empty string predicate}
  See [[https://rosettacode.org/][Rosetta Code]]
  (define empty-string "")
  (define (string-null? s) (string=? "" s))
  (define (string-not-null? s) (string<? "" s))
}

@block{@block-name{Converting Values to Strings}
  [[https://docs.racket-lang.org/reference/strings.html#%28part._format%29][Converting Values to Strings]]
  (require racket/format)
  (~a "Hi" 1 2 'People)                 ; => "Hi12People"
  (~a "Hi" 1 2 'People '(Around))       ; => "Hi12People(Around)"
  (~a #:separator "-" "Hi" 1 2 'People) ; => "Hi-1-2-People"
  ;; See
  ;; ~a ~v ~s ~e ~r ~.a ~.v ~.s
}

@block{@block-name{External commands}
  (system/exit-code "ls")   ;; terminate (REPL) with return code
}

@block{@block-name{PLT PLaneT}
  Racket's deprecated, old package system. Racket was originally created by the
  PLT research group.
}

@block{@block-name{PEG - advance over regex}
  Matches more languages (e.g. balanced brackets) and can be paired with
  semantic actions to produce structured results from a parse.
}

@block{@block-name{Scribble and Pollen}
  Scribble - collection of tools for creating prose documents—papers, books,
  library documentation, etc.—in HTML or PDF (via Latex) form.

  echo \
  "#lang scribble/base

@"@"title{On the Cookie-Eating Habits of Mice}

If you give a mouse a cookie, he's going to ask for a
glass of milk.
" > /tmp/mouse.scrbl
  ls -la /tmp/mouse.scrbl
  # scribble         /tmp/mouse.scrbl  # creates "mouse.html"
  # scribble --htmls /tmp/mouse.scrbl  # creates "mouse/index.html" for Sub-sections
  scribble --pdf   /tmp/mouse.scrbl  # creates "mouse.pdf". Requires `guix install texlive` (2.6 GB)
  # scribble --latex /tmp/mouse.scrbl  # creates "mouse.tex"


  echo \
  "#lang scribble/report

@"@"; scribble --pdf crypto_mining.scrbl   # compile
@"@"; evince crypto_mining.pdf & disown    # view / verify

@"@"(require scribble/core scribble/manual)

@"@"(define some-constant \"foo\")

@"@"; the dot '.' means variable number of arguments
@"@"(define (block name . items)
  (make-element
   (make-style #f null)
                (cons name items)))

@"@"(define (block-name name)
   (string-append name \"\n\"))
" > /tmp/report.scrbl
  ls -la /tmp/report.scrbl
  # scribble         /tmp/report.scrbl  # creates "report.html"
  # scribble --htmls /tmp/report.scrbl  # creates "report/index.html" for Sub-sections
  scribble --pdf   /tmp/report.scrbl  # creates "report.pdf". Requires `guix install texlive` (2.6 GB)
  # scribble --latex /tmp/report.scrbl  # creates "report.tex"

  Pollen - publishing system for writing functional digital web-based books. It
  can also be used as a dynamic preview server for Scribble files.

  # raco pkg install pollen
  # cd /tmp && raco pollen start
  echo \
  "#lang pollen
Hello world
" > /tmp/hello.txt.pp
  ls -la /tmp/hello.txt.pp
  # racket /tmp/hello.txt.pp
  # raco pollen render hello.txt.pp # created hello.txt
  # in the browser open
  #   @url{http://localhost:8080/index.ptree}
  # or
  #   @url{http://localhost:8080/hello.txt}

  # raco pkg install pollen
  # cd /tmp && raco pollen start
  echo \
  "#lang pollen
◊(define metal \"Plutonium\")

Markdown & Pollen + ◊metal
---------------

+ You **wanted** ◊metal — you _got_ it.

+ [search for ◊metal](https://google.com/search?q=◊metal)
" > /tmp/downtown.html.pmd

  # raco pkg install pollen
  # cd /tmp && raco pollen start
  echo \
  "#lang pollen

◊headline{Pollen markup}

◊items{
  ◊item{You ◊strong{wanted} it — you ◊em{got} it.}
  ◊item{◊link[\"https://google.com/search?q=racket\"]{search for Racket}}
}
" > /tmp/uptown.html.pm
}

@block{@block-name{Source code documentation, docstrings}
  Write in-source documentation by using scribble/srcdoc
  See @url{https://stackoverflow.com/a/53991442}

  When using define/doc then examples can also be used as unit tests.
  @url{https://github.com/greghendershott/frog/blob/master/frog/private/define-doc.rkt}

  (require (for-syntax racket/syntax))

  (define-syntax (define/doc stx)
  (syntax-case stx ()
    [(_ id doc-string expr)
     (with-syntax
         ([name (format-id #'id "~a-doc" #'id)])
       #'(begin (define id expr)
                (define name doc-string)))]))
}

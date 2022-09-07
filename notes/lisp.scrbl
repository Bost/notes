#lang notes

@block{@block-name{lisp-1 vs. lisp-2}
  lisp-1: single-value-name languages: Scheme, Clojure, etc.
  lisp-2: multi-value-name languages: Emacs Lisp, Common Lisp
          allows the same name for both function and variable
  http://xahlee.info/emacs/emacs/lisp1_vs_lisp2.html

  https://www.more-magic.net/posts/thoughts-on-clojure.html
}

@block{@block-name{Lisp Implementations}
  CLOS Common Lisp Object System / CLOS-based object orientation differs from
  OOP facilities found in C++ or Java

  @lnk{Common Lisp VS Racket - testimonies.md
  https://gist.github.com/vindarel/c1ef5e043773921e3b11d8f4fe1ca7ac}

  Common Lisp:
  symbol-based macro system
  has fairly unique condition system approach to handling errors.

  Guile Scheme, MIT Scheme, Racket Schemes: (TODO verify this)
  hygienic macros, however Racket does provide non-hygienic macros

  Racket Scheme:
  general-purpose, multi-paradigm
}

@block{@block-name{Common Lisp Implementations}
  @lnk{YouTube - Baggers: Common Lisp relates stuff
  https://www.youtube.com/user/CBaggers/featured}

  abcl ccl clisp
  https://en.wikipedia.org/wiki/CLISP

  GNU Common Lisp
  https://en.wikipedia.org/wiki/GNU_Common_Lisp

  ECL Embeddable Common Lisp
  https://en.wikipedia.org/wiki/Embeddable_Common_Lisp

  gitlab: Embeddable Common Lisp
  https://gitlab.com/embeddable-common-lisp/ecl

  SCBL Steel Bank Common Lisp
  https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
}

@block{@block-name{Koans}
  @lnk{github: Common Lisp Koans
  https://github.com/google/lisp-koans}

  @lnk{github: Racket Koans
  https://github.com/zyrolasting/racket-koans}

  Monad Koans in Clojure:
  @lnk{github: bost
  https://github.com/Bost/monad_koans}
  @lnk{github: benedekfazekas
  https://github.com/benedekfazekas/monad_koans}

  @lnk{HN News - List of koans
  https://news.ycombinator.com/item?id=5802960}
}

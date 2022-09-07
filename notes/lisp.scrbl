#lang notes

@block{@block-name{lisp-1 vs. lisp-2}
  lisp-1: single-value-name languages: Scheme, Clojure, etc.
  lisp-2: multi-value-name languages: Emacs Lisp, Common Lisp
          allows the same name for both function and variable
  http://xahlee.info/emacs/emacs/lisp1_vs_lisp2.html

  https://www.more-magic.net/posts/thoughts-on-clojure.html
}

@block{@block-name{Lisp Implementations}

  Arguably(!) it is easier to reason about macros than functions, because in the
  vast majority of cases their expansions are deterministic. Macros can be
  expanded and inspected at compile-time, before any code has run.

  Reddit - r/lisp
  https://www.reddit.com/r/lisp/

  Common Lisp VS Racket - testimonies.md
  https://gist.github.com/vindarel/c1ef5e043773921e3b11d8f4fe1ca7ac

  Common Lisp:
  - symbol-based macro system
  - has fairly unique condition system approach to handling errors.
  - subreddit tends to be heavily oriented towards Common Lisp
  - more interactive that Racket
  - has libraries for working with persistent data structures, but the CL
    community doesn't have a culture of creating by default thread-safe
    data-structures.
  - more powerful, flexible language (than Clojure)
  - CLOS (Common Lisp Object System) based object orientation differs from OOP
    facilities found in C++ or Java
  - the CL standard is from 1994, so it doesn't specify concurrency (Clojure was
    built with concurrency in mind), networking etc.

  Concurrency is easy. Synchronisation is hard. I.e. how do you get the data
  from your concurrent operations safely back to a single thread?

  Clojure-style concurrency abstractions / operations ported to CL
  https://github.com/dtenny/clj-con

  Clojure:
  - provides immutability (or more correctly persistence) by default.
    (Mind-bending!)

  CLEDE Common Lisp Emacs Development Environment
  https://emacsconf.org/2021/talks/clede/

  Common Lisp - Concurrent and Parallel Programing tutorial
  https://docs.google.com/document/d/10MGPC7j4lpGrMlQS4xtQTEGrvpBecgDUnqu1OrjSVxk

  Learn Common Lisp
  https://github.com/rabbibotton/clog/blob/main/LEARN.md

  CLOG Common Lisp Omnificent GUI
  https://github.com/rabbibotton/clog

  Learn CLOG
  https://github.com/rabbibotton/clog/blob/main/LEARN.md

  Guile Scheme, MIT Scheme, Racket Schemes, etc. all have
  hygienic macros, i.e. you can't modify everything
  (TODO verify it).

  Racket Scheme:
  - general-purpose, multi-paradigm
  - has the most advanced macro system and metaprogramming capabilities of
    any programming language. It also provides non-hygienic macros
  - has custodians (custodian ~ manager. TODO what is custodian?).
    A custodian manages a collection of threads, file-stream ports, TCP ports,
    TCP listeners, UDP sockets, byte converters, and places. Whenever a thread,
    etc., is created, it is placed under the management of the current custodian
    as determined by the current-custodian parameter. Custodians provide a more
    "pure" approach to state.

  - lisp syntax with Haskell interactivity; REPL is a debug tool; during
    development Racket process must be restarted and modules must be frequently
    reloaded

  Gerbil Scheme:
  - has huilt-in actor model (TODO verify)
}

@block{@block-name{Common Lisp Implementations}
  YouTube - Baggers: Common Lisp relates stuff
  https://www.youtube.com/user/CBaggers/featured

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
  github: Common Lisp Koans
  https://github.com/google/lisp-koans

  github: Racket Koans
  https://github.com/zyrolasting/racket-koans

  github: Monad Koans in Clojure
  https://github.com/Bost/monad_koans

  github: benedekfazekas
  https://github.com/benedekfazekas/monad_koans

  HN News - List of koans
  https://news.ycombinator.com/item?id=5802960
}

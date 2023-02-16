#lang notes

@block{@block-name{lisp-1 vs. lisp-2}
  LISP 20170717 1330
  https://youtu.be/wi2X00kvUaE
  Lisp Working Group WG
}

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

  CL Common Lisp:
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

  Clojure-style concurrency abstractions / operations in Common Lisp
  https://github.com/dtenny/clj-con

  Clojure:
  - immutability, more correctly persistence, by default. Mutation is a source
    of bugs, and should be avoided / thoughtfully limited.
  - immutable reference to data. Mutation allowed only along the explicit path
    of computations you're working through (it doesn't change, actually, you
    choose to hold onto a new reference to derived data when you want to).
  - mutable escape hatch (eg. when holding a handle to a database), every type
    of mutation in (core) Clojure has defined (and thoughtfully so) concurrency
    semantics. No(?) "not thread safe" notes in Clojure API docs like in
    JavaDocs.
  - the state of a Clojure program will most likely comprised of printable data
    literals (think JSON data types).
  - it gives you object-like read-only views into your database (like Datomic's
    `datomic.api/entity`), or help you write queries with a knowledge of your
    database schema, but most Clojure persistence solutions will explicitly
    coordinate mutation into a single 'site' because that's the only way
    maintain a coherent view of state-over-time. And that single-mutation-site
    story is the opposite of what ORMs (as commonly defined) do.

  CLEDE Common Lisp Emacs Development Environment
  https://emacsconf.org/2021/talks/clede/

  Common Lisp - Concurrent and Parallel Programing tutorial
  https://docs.google.com/document/d/10MGPC7j4lpGrMlQS4xtQTEGrvpBecgDUnqu1OrjSVxk

  Learn CLOG - The Common Lisp Omnificent GUI
  https://github.com/rabbibotton/clog/blob/main/LEARN.md

  The Common Lisp Cheat Sheet
  https://github.com/ashok-khanna/lisp-notes

  Guile Scheme, MIT Scheme, Racket Schemes, etc. all have hygienic macros, i.e.
  you can't modify everything (TODO verify it).

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

  Janet
  https://janet-lang.org
  - functional, imperative & embeddable language & bytecode interpreter / VM
  - superficially similar to Clojure, internally it's different
  - unlike Scheme/Common Lisp, it has no cons cells
  - lisp-like, but lists are replaced by other data structures (arrays, tables
    (hash table), struct (immutable hash table), tuples)
  - supports bridging to native C-code, meta-programming with macros, and
    bytecode assembly.
  - for rapid prototyping, dynamic systems,scripting to an application by
    embedding a single C file and two headers.
  - entire language (core library, interpreter, compiler, assembler, PEG) is
    less than 1MB.
  - immutable data structures (tuples and structs) implemented as immutable
    arrays and hash tables, not as hash tries. Don't use them like Clojure's
    vectors and maps, instead they work well as table keys or other identifiers.

  Kawa Scheme - on JVM
  https://www.gnu.org/software/kawa/
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
  Common Lisp Koans
  https://github.com/google/lisp-koans

  github: Racket Koans
  https://github.com/Bost/racket-koans

  github: Monad Koans in Clojure
  https://github.com/Bost/monad_koans

  HN News - List of koans
  https://news.ycombinator.com/item?id=5802960

  https://github.com/Bost/bash_koans
}

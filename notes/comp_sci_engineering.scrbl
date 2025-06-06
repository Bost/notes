#lang notes

@block{@block-name{Tree-sitter}
  library for parsing code - for text editors
  - incremental parsing
}

@block{@block-name{Leaky Abstraction}
  A Look into Modern Leaky Abstractions - Postgres, MySQL, HTTP/2, TCP, ORMs
  GraphQL, N+1, Axios, git
  https://youtu.be/4a3bI7AYsy4

  Proper abstraction:
  - e.g. MariaDB vs. Postgres: I don't need to know if 'select * from ...' runs
    on the former or the later: The commands have:
    1. Same syntax
    2. Same performance characteristics
    3 ...
  Leaky abstraction:
  E.g. finger printing by e.g. different performance characteristics.

  n+1 Problem: cannonical example of a leaky abstraction:
  Optimaly 1 select:
    select id, price form table where ...;
  vs. select from (select from (select from ...)):
    select price from (select * from table where ...);
  Assume every abstraction is leaky
}

@block{@block-name{Stacks}
  LAMP Linux Apache, MySQL, PHP
  LEMP (container stack) Linux, (E)Nginx, MariaDB (MySQL replacement), PHP
  - open source web platform
  - to run dynamic web sites and servers.
  LEPP (container stack) Linux, (E)Nginx, PostgreSQL, PHP
}

@block{@block-name{TODOs - programming}
  module system / OCaml
  hygienic macros
  threads
}

@block{@block-name{TeX, LaTeX and document-processing}
  TeX processes layout. LaTeX processes content. LaTeX has a collection of TeX
  macros and a program to process LaTeX documents, and because the plain TeX
  formatting commands are elementary, it provides ready-made commands for
  formatting and layout requirements e.g. chapter headings, footnotes,
  cross-references, bibliographies.

  printing characters.
  from the keyboard.
}

@block{@block-name{Numeric tower - numbers:}
  Natural ℕ ⊆ Integer ℤ ⊆ Rational ℚ ⊆ Real ℝ ⊆ Complex ℂ ⊆ Quaternion ℍ
}

@block{@block-name{Structure type: record datatype composing a number of fields}
  Sum type (OCaml): Generalization of enumeration. Bring heterogeneous values
  together into a common type

  (OCaml) type export: concrete / abstract - for fine-grained data encapsulation
  control

  Concrete export - full type definition remains known: so, clients of the
  modules can build or examine values of this type.

  Abstract export - only the type name is known outside the module. It then
  becomes impossible, from the outside, to create or inspect values of this
  type.
}

@block{@block-name{Pattern matching}
  - generalize the traditional case analysis construct
  - examine and name data simultaneously
}

@block{@block-name{Foreign Function Interface FFI a.k.a. Language bindings}
  Usage of different languages in one program;
  e.g. Java Native Interface JNI / Java Native Access JNA; extern "C"
}

@block{@block-name{Multiple Values}
  An expression can evaluate to multiple values

  Symmetry to a procedure accepting multiple arguments

  The continuation '(let-values ([(x y) []]) expr)' expects two result values

  The continuation (begin [] (+ 1 2)) accepts any number of result values,
  because it ignores the result(s).
}

@block{@block-name{What Data Structures are /do}
  - static (Algorithms - dynamic)
  - reasoning by an induction: consider the base case(s) and the general.
  - remove the notion of time. Change over time is one of the major sources of
    program complexity.
  - declarative. Most of the algorithms are imperative. Under declarative
    approach you don't have to trace the flow of time through a data structure.
}

@block{@block-name{Database}
  Database schema         - Category
  Database instance       - Set-valued functor
  Database transformation - Universal Construction
}

@block{@block-name{Mixin in OOP}
  A class with methods for use by another classes without having to be the
  parent class of these other classes. A Mixin class is being "included" rather
  than "Inherited".
}

@block{@block-name{
  Lisp / Minimal Scheme Implementation for use as an Extension Language}
  http://synthcode.com/wiki/chibi-scheme

  Scheme implementation meant to be embedded in a C-program, i.e. scripting
  C-applications etc.

  System Crafters Live! - Lisp Compiler Progress • Live Lisp Hacking • Q&A
  https://youtu.be/E-g3Ls1GRz4

  GNU Mes: Maxwell's Equations of Software!
  Create a computer operating system that we can trust.
  Mutual self-hosting Scheme interpreter written in C and a Nyacc-based C
  compiler written in Scheme.

}

@block{@block-name{bind mount}
  replicates existing directory tree under a different point.

  # FUSE file system for mounting a directory to another location, similar to
  # `mount --bind'. It can (among others) create a view of a directory tree:
  bindfs /some/where /else/where
}

@block{@block-name{Various}
  GIO general purpose I/O, networking, IPC, settings,
  and other high level application functionality

  POSIX Portable Operating System Interface
  Standards for compatibility between operating systems. It defines the
  application programming interface (API), command line shells and utility
  interfaces, for compatibility with variants of Unix and other OSes.

  Project Gemini
  new internet technology supporting an electronic library of interconnected
  text documents.

  Polyfill: code (usually JavaScript on the Web) used to provide modern
  functionality on older browsers that do not natively support it.

  BusyBox - Unix utilities in a single executable file; 2.1 MB in tar.bz2
  https://www.busybox.net/
  Git Repo
  https://git.busybox.net/busybox

  Wasm - WebAssembly https://webassembly.org
  Binary instruction format (bytecode) for a stack-base  d virtual machine.
  E.g. C, C++, Rust code can be compiled to wasm format, then this wasm code
  runs in the web browser's sandbox at near-native speed.
  Not a standalone programming language but a compilation target.
  "Use WebAssembly for computationally intensive tasks and JavaScript for
  'standart' web development."

  Clang - drop-in replacement for the GNU Compiler Collection (GCC)

  Web Content Accessibility Guidelines (WCAG) 2 Level AAA Conformance
  https://www.w3.org/WAI/WCAG2AAA-Conformance

  GLSL - OpenGL Shading Language

  IOPS - [eye-ops] input/output operations per second
  FLOPS - floating-point operations per second

  QPS - Queries-per-second: how much traffic a particular query server is
  handling at a given time

  Correlated hardware failures:
  If one hard drive HDD / SSD disk fails, it is more likely to see a second
  failure before getting back up if the disks are from the same manufacturing
  batch

  Cloud architecture:
  The way technology components combine to build a cloud, in which resources are
  pooled through virtualization technology and shared across a network.

  Sharding:
  A type of database partitioning that separates large databases into smaller,
  faster, more easily managed parts
  https://www.techtarget.com/searchoracle/definition/sharding
  Horizontal sharding - each new table has the same schema but unique rows
  Vertical sharding - each new table has a schema that is a faithful subset of
  the original table's schema

  Model Driven Software Engineering - Computerphile
  https://youtu.be/3aoLV5i1feo
  Modeling Language ~ Domain Specific Language
}

@block{@block-name{JavaScript 2014}
  What the heck is the event loop anyway? | Philip Roberts | JSConf EU
  https://youtu.be/8aGhZQkoFbQ
  V8 Runtime
  WebAPIs:
  - extra things provided by the browser: DOM, ajax, setTimeout, ...
  - effectively threads; one can make calls to these things
  - on the backend (i.e. nodejs) instead of WebAPIs there are C++ APIs

  event loop, callback queue
  - looks at the stack and the task queue, if the stack is empty, it takes the
    1st thing on the queue and pushes it onto the stack (that effectively runs
    it)
  javascript is single threaded, 1 thread, i.e. 1 callstack, i.e. 1 thing at a
  time blocking - asynchronous callbacks

  - setTimeout(<callback function>, 0) - set to 0 means deferring the callback
    function to the end of the stack (i.e. until the stack is clear, i.e. until
    everything is executed)

  - timeout time is the minimum timeout of execution

  - rendering every 16ms for a screen (60 FPS?)
  - render has higher prio than a callback
    23:12
}

@block{@block-name{RPC Server}
  RPC Remore Procedure Call
  accepts commands comming from some client and returns responses
}

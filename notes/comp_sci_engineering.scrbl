#lang notes

#+title: Comp-Sci Engineering

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

@block{@block-name{Structure type - record datatype composing a number of fields}
  Sum type (OCaml): Generalization of enumeration. Bring heterogeneous values
  together into a common type

  (OCaml) type export: concrete / abstract - for fine-grained data encapsulation
  control

  Concrete export - full type definition remains known: so, clients of the modules
  can build or examine values of this type.

  Abstract export - only the type name is known outside the module. It then
  becomes impossible, from the outside, to create or inspect values of this type.
}

@block{@block-name{Pattern matching}
- generalize the traditional case analysis construct
- examine and name data simultaneously
}

@block{@block-name{CLOS-based object orientation}
  CommonLisp Object System
}

@block{@block-name{Foreign Function Interface FFI a.k.a. Language bindings}
  Usage of different languages in one program;
  e.g. Java Native Interface JNI / Java Native Access JNA; extern "C"
}

TODO delimited continuations
TODO module system / OCaml
TODO hygienic macros
TODO multiple values
TODO threads

@block{@block-name{WSL}
  Windows Subsystem for Linux 2 (WSL 2)
}

@block{@block-name{Data structures (are / do):}
- static (Algorithms - dynamic)
- reasoning by an induction: consider the base case(s) and the general.
- remove the notion of time. Change over time is one of the major sources of
  program complexity.
- declarative. Most of the algorithms are imperative. Under declarative approach
  you don't have to trace the flow of time through a data structure.
}

@block{@block-name{Database}
  Database schema         - Category
  Database instance       - Set-valued functor
  Database transformation - Universal Construction
}

@block{@block-name{Mixin in OOP}
  A class with methods for use by another classes without having to be the
  parent class of these other classes.

  Mixin class is being "included" rather than "Inherited".
}

@block{@block-name{Lisp / Minimal Scheme Implementation for use as an Extension Language}
  http://synthcode.com/wiki/chibi-scheme

  Scheme implementation meant to be embedded in a C-program, i.e. scripting
  C-applications etc.

  System Crafters Live! - Lisp Compiler Progress • Live Lisp Hacking • Q&A
  https://youtu.be/E-g3Ls1GRz4
}

@block{@block-name{Stacks}
** LAMP Linux Apache, MySQL, PHP
** LEMP (container stack) Linux, (E)Nginx, MariaDB (MySQL replacement), PHP
  - open source web platform
  - to run dynamic web sites and servers.
** LEPP (container stack) Linux, (E)Nginx, PostgreSQL, PHP
}

@block{@block-name{BusyBox}
  [[https://git.busybox.net/busybox][Git Repo]] [[https://www.busybox.net/][Home page]]
  BusyBox is a software suite that provides several Unix utilities in a single
  executable file. Size 2.1 MB (compressed "tar.bz2")
}

@block{@block-name{Wasm - WebAssembly}
  https://webassembly.org
  WebAssembly is a binary instruction format for a stack-based virtual machine.
}

@block{@block-name{bind mount}
  replicates existing directory tree under a different point.
** bindfs
  FUSE file system for mounting a directory to another location, similar to
  `mount --bind'. It can (among others) create a view of a directory tree:
  #+BEGIN_SRC bash :results output
    bindfs /some/where /else/where
  #+END_SRC
}

@block{@block-name{GLSL OpenGL Shading Language}
}

@block{@block-name{JavaScript 2014}
  event loop https://www.youtube.com/watch?v=8aGhZQkoFbQ
  V8 Runtime
  WebAPIs:
  - extra things provided by the browser: DOM, ajax, setTimeout, ...
  - effectively threads; one can make calls to these things
  - on the backend (i.e. nodejs) instead of WebAPIs there are C++ APIs

  event loop, callback queue
  - looks at the stack and the task queue, if the stack is empty, it takes the 1st thing on the queue and pushes it onto the stack (that effectively runs it)
  javascript is single threaded, 1 thread, i.e. 1 callstack, i.e. 1 thing at a time
  blocking - asynchronous callbacks

  - setTimeout(<callback function>, 0) - set to 0 means deferring the callback function to the end of the stack (i.e. until the stack is clear, i.e. until everything is executed)
  - timeout time is the minimum timeout of execution

  - rendering every 16ms for a screen (60 FPS?)
  - render has higher prio than a callback
    23:12
}
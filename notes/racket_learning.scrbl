#lang notes

#+title: Racket Learning

@block{@block-name{Learning Sources}
  Tiago Cogumbreiro - [[https://cogumbreiro.github.io/teaching/cs450/s21/][GitHub]]
  Michael Sperber - [[https://media.ccc.de/v/rc3-257534-all_programming_language_suck_just_build_your_own_language_oriented_programming_with_racket][All Programming Language Suck? Just Build Your Own!]]
  Jesse Alama - The Racket Weekend
}

@block{@block-name{Server Racket}
  @block{@block-name{Servlet:}
    (HTTP Server) function: input HTTP request -> output HTTP response

    Signature: request? → response?

    Web App = Single large servlet

    struts-copy for rewriting HTTP requests / responses

    HTTP HEAD request - like a GET request but it returns no body
    - used to determine how big a resource would be with a real GET request
  }
}

@block{@block-name{Object-relational mapping}
  [[https://en.wikipedia.org/wiki/Object%E2%80%93relational_mapping][Wikipedia: Object–relational mapping]] ORM

  @block{@block-name{ORM in Racket with Racquel}
    [[https://docs.racket-lang.org/racquel/index.html][Racket Docs: Racquel]]
    [[https://github.com/brown131/racquel][Github: Racquel]]
  }
}

@block{@block-name{Various sources}
  [[https://racket-news.com/][Blog: Racket News]]
  [[https://craftinginterpreters.com/][Book: Crafting Interpreters]]
  [[https://www.micahcantor.com/blog/thoughts-typed-racket/][Typed Racket: the good and the bad]]

  @block{@block-name{Procedures and functions}
    [[https://stackoverflow.com/a/54165633][Are "procedure" and "function" synonymous in Racket?]]
    The Scheme standard uses only the term procedure.
    In Racket function is a procedure with no side effects.
  }

  @block{@block-name{Hunt the Wumpus}
    [[https://en.wikipedia.org/wiki/Hunt_the_Wumpus][Wikipedia: Hunt the Wumpus]]
    [[https://rosettacode.org/wiki/Hunt_the_Wumpus#Racket][Racket: Hunt the Wumpus]]
  }
}

@block{@block-name{RacketConf 2021}
  - https://racket.discourse.group
  - [[https://github.com/soegaard/remacs][remacs - the Emacs style editor written in Racket]]
  - [[https://youtu.be/73dDj_z66qo?t=3578][Matthias Felleisen - Every language has a slogan]]
}

@block{@block-name{The Heresy Programming Language}
  https://github.com/jarcane/heresy
  Functional Lisp/Scheme dialect implemented in Racket, with syntax inspired by
  the BASIC family of programming languages. Its principle goals are to provide
  a simple core language for BASIC and other programmers to experiment with and
  learn how to program functionally. This document will detail the general
  philosophy of the Heresy language, such as exists, as well as the language
  syntax and functions.

}

@block{@block-name{Racket: Why I Think It’s a Great Language, and Why I’m Not Using It Anymore}
  [[https://youtu.be/_wY7FBtr7_c][YouTube: Leandro Facchinetti]]

  Programs should communicate ideas to other people
  Creating a new language doesn't help - it's a new language nobody knows it.

  Better Idea:
  communicate using known ways, notably via GUIs, spreadsheets, etc.
}

@block{@block-name{Playing the Game with PLT Redex}
  https://youtu.be/NszLQNROdw0?t=306
}

@block{@block-name{Education}
  https://www.bootstrapworld.org/
  FREE, research-based, integrated Computer Science and Data Science modules for
  Math, Science, Business and Social Studies classes grades 5-12.

  @block{@block-name{Pyret}
     Panguage for programming education. Explore confluence of scripting and
     functional programming. https://www.pyret.org/

     https://www.wescheme.org/openEditor
  }
}

@block{@block-name{Modules}
  https://felleisen.org/matthias/Thoughts/Modular_Programming.html
  Requiring a module does not require its submodules and requiring a submodule loads only as much of the surrounding module as required.
}

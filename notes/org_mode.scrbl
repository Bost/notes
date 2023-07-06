#lang notes

@block{@block-name{Org Mode}
  http://ehneilsen.net/notebook/orgExamples/org-examples.html

  #+BEGIN_SRC emacs-lisp
  (message "Yeah from emacs-lisp!")
  #+END_SRC

  #+RESULTS:
  : Yeah from emacs-lisp!

  #+BEGIN_SRC bash :exports both :results output
  echo "Hello World from sh" # evaluate: ~C-c C-C~
  #+END_SRC

  #+RESULTS:
  : Hello World from sh

  #+begin_src clojure :results silent
  ;; ":results silent" causes result to be displayed in the mini-buffer
  ;; M-x cider-jack-in
  (+ 1 4)
  #+end_src

  #+BEGIN_SRC clojure
  (defproject my-project "0.1.0"
    :description "My great Clojure project"
    :dependencies
    [[org.clojure/clojure "1.9.0"]])
  (println "Hello from clojure")
  #+END_SRC

  #+BEGIN_SRC python :results output :exports both
  # Result displayed under '#+RESULTS:'
  print("Hello from Python") # evaluate: ~C-c C-c~
  #+END_SRC

  #+RESULTS:
  : Hello from Python
}

@block{@block-name{Literal Example - an example in another example}
  #+begin_src org
    ,#+BEGIN_EXAMPLE
    ,* This is a heading
    ,#+END_EXAMPLE
  #+end_src
}

@block{@block-name{BEGIN_SRC Code Blocks}
  [[https://orgmode.org/manual/Structure-of-Code-Blocks.html][Structure of Code Blocks]]
  #+NAME: <name>
  #+BEGIN_SRC <language> <switches> <header arguments>
    <body>
  #+END_SRC
  [[https://orgmode.org/worg/org-contrib/babel/languages/index.html][List of Code Block Languages supported by Babel]]
}

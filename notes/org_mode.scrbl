#lang notes

@block{@block-name{Org Mode}
  http://ehneilsen.net/notebook/orgExamples/org-examples.html

  M-x org-babel
  in Org mode ~, b ~

  #+begin_src chatgpt-shell
  Mirror, mirror, who's the most beautiful person on Earth?
  #+end_src

  #+begin_src emacs-lisp
  (message "Yeah from emacs-lisp!")
  #+end_src

  #+RESULTS:
  : Yeah from emacs-lisp!

  #+begin_src bash :exports both :results output
  echo "Hello World from bash" # evaluate: ~C-c C-C~
  #+end_src

  #+RESULTS:
  : Hello World from bash



  #+begin_src bash :exports both :results output
  echo "Hello World from bash" # evaluate: ~C-c C-C~
  #+end_src

  #+begin_src shell :exports both :results output
  echo "Hello World from shell" # evaluate: ~C-c C-C~
  #+end_src

  #+RESULTS:
  : Hello World from shell



  #+begin_src fish :exports both :results output
  crep 'mpstat\|iostat'
  #+end_src

  #+begin_src clojure :results silent
  ;; ":results silent" causes result to be displayed in the mini-buffer
  ;; M-x cider-jack-in
  (+ 1 4)
  #+end_src

  #+begin_src clojure
  (defproject my-project "0.1.0"
    :description "My great Clojure project"
    :dependencies
    [[org.clojure/clojure "1.9.0"]])
  (println "Hello from clojure")
  #+end_src

  #+begin_src python :results output :exports both
  # Result displayed under '#+RESULTS:'
  print("Hello from Python") # evaluate: ~C-c C-c~
  #+end_src

  #+RESULTS:
  : Hello from Python



  #+begin_src fish :exports both :results output
  set deps '
  {:deps {cider/cider-nrepl {:mvn/version "0.30.0"},
          nrepl/nrepl {:mvn/version "1.0.0"},
          refactor-nrepl/refactor-nrepl {:mvn/version "3.9.0"}}
   :aliases
   {:cider/nrepl
    {:main-opts
     ["-m"
      "nrepl.cmdline"
      "--middleware"
      "[com.billpiel.sayid.nrepl-middleware/wrap-sayid,refactor-nrepl.middleware/wrap-refactor,cider.nrepl/cider-middleware]"]}}}
      '
  set cmd /home/bost/.guix-home/profile/bin/clojure -Sdeps $deps -M:cider/nrepl
  # regexp: match paths surrounded with double quotes, starting with leading slash
  strace $cmd 2>&1 | rg --only-matching '"/(.+)/([^/]+)\.edn"' | sort | uniq --unique
  #+end_src

  #+RESULTS:
  : "/gnu/store/2fgxqg4rvdgj8ivn4q3qxfv0xjzap5c3-clojure-tools-1.11.1.1165/lib/clojure/tools.edn"
  : "/home/bost/.config/clojure/tools/tools.edn"

}

@block{@block-name{Literal Example - an example in another example}
  #+begin_src org
    ,#+begin_example
    ,* This is a heading
    ,#+end_example
  #+end_src
}

@block{@block-name{Code Blocks}
  Structure of Code Blocks
  https://orgmode.org/manual/Structure-of-Code-Blocks.html
  #+name: <name>
  #+begin_src <language> <switches> <header arguments>
    <body>
  #+end_src

  List of Code Block Languages supported by Babel
  https://orgmode.org/worg/org-contrib/babel/languages/index.html
}

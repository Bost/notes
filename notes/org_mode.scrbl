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

  #+begin_src bash :exports both :results output
  echo "Hello World from bash" # evaluate: ~C-c C-C~
  #+end_src

  #+begin_src bash :exports both :results output
  echo "Hello World from bash" # evaluate: ~C-c C-C~
  #+end_src

  #+begin_src shell :exports both :results output
  echo "Hello World from shell" # evaluate: ~C-c C-C~
  #+end_src

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

  #+begin_src fish :exports both :results output
  # fish_trace doesn't work in org-babel. The executed commands are not
  # displayed in the #+RESULTS:
  # set fish_trace on
  # guix home list-generations 1d       # no '=' allowed after 'list-generations'
  # set --erase fish_trace

  function trace --description "Print executed string-command"
    # set cmd gitk --all (string escape -- $argv) \&
    printf "> %s\n" (string join " " $argv)
    eval $argv
    printf "\n"
  end

  date
  trace "guix home list-generations 20h  | rg -i generation"
  trace "guix home list-generations 20h  | rg -A 4 '\bguix( |:)' | rg commit"
  trace "guix pull --list-generations=2h | rg -i generation"
  trace "guix pull --list-generations=2h | rg -A 4 '\bguix( |:)' | rg commit"
  trace "guix system list-generations 7d | rg -i generation"
  trace "guix system list-generations 7d | rg -A 4 '\bguix( |:)' | rg commit"
  #+end_src

  #+begin_src bash :exports both :results output :dir /sudo::
  # https://www.reddit.com/r/orgmode/comments/lercjw/tip_org_babel_sudo_command/
  # Doesn't work. dmidecode is not in the profile of the sudo-user
  which dmidecode
  #+end_src

  #+begin_src bash :var pw=(read-passwd "Password: ")
  # https://www.reddit.com/r/orgmode/comments/lercjw/tip_org_babel_sudo_command/
  # Doesn't work. Prints no results.
  #
  # -S, --stdin
  #         Write the prompt to the standard error and read the password from
  #         the standard input instead of using the terminal device.
  echo $pw | sudo -S dmidecode
  #+end_src
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

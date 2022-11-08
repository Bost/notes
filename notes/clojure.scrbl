#lang notes

@block{@block-name{General}
 https://stuartsierra.com/2016/01/09/how-to-name-clojure-functions
 - A pure function can be replaced with its value - use noun, i.e.
   preffer `age`, oveer `calculate-age` or `get-age.`
 - A function with side-effects - use verb, i.e.
   for constructors:      `create-...`
   information retrieval: `get-...`, `fetch-...` (e.g. query a web service)
 - Words which could be either nouns or verbs: assume noun by default then add
   words to make verb phrases. E.g. `message` constructs a new object
   representing a message, `send-message` transmits it.
 - Function returning functions - use suffix '...-fn', or `...-fun`
 - Function names should not repeat the name of the namespace
   @lisp{
     (ns products)
     (defn product-price [product] ...) ; Bad, the 'product' is redundant
     (defn price [product] ...)         ; Good
   }


  What's the difference between a "sequence" and a "seq"?
  http://www.brainonfire.net/files/seqs-and-colls/main.html

  | HotSpot | Virtual Machine for OpenJDK.                            |
  |         | old JIT (Just-In-Time) Compiler. Written in C++         |
  | GraalVM | JVM Compiler Interface: Plugin own JIT Compiler into VM |
  |         | Compiler written in Java, replacement for HotSpot       |

  Class libraries + JVM make a Java Runtime Environment (JRE) or Java Developers
  Kit (JDK).

  When JVM starts Graal begins to compile itself
  JVMCI protocol: between JMV and Graal -> {Truffle, TruffleRuby}
  Truffle language framework
  spec-provider - creates clojure spec from examples
  zprint - pretty printer for clojure; has also an emacs package 'zprint-mode'
  Substrate VM - GraalVM native image generation

  dashing: drawing with certain velocity
  cljart <3

  defrecord is a cartesian product
  GeoREPL
  ursive
  hicago Clojure - 2017-06-21: Stuart Halloway on Repl Driven Development:
  TODO Don't throw things away - save a file once in a month
  Visualisation - Desktop GUI: Seesaw - Live at REPL (as a custom inspector)
  Start java with a clojure.jar and hook up a REPL with custom prompt
  specs + graphvis: https://github.com/jebberjeb/specviz
  CJUG - 2017-06-20 - Stuart Halloway on Clojure in 10 Big Ideas: 18:43

  java https://www.root.cz/serialy/programovaci-jazyk-java-a-jvm/
  Based on stack-based processor
  Statically typed instruction set

  scala
  xml - first class support
  default get/set methods & constructor
  function & imperative
  staticaly typed
  traits (multiple inheritance)

  clojuredocs - graphic representation of example

  Rick Hickey - TBD (To Better Do)
  - User interfaces on top of programatic interfaces
  - use composable services
  - abstractions; no bespoke protocols and formats

  Clojure AI
  Eric Weinstein: Machine learning with Clojure and Apache Stark
  Apache Spark:
  Supervised learning, Generalization
  Classification or regression, generalizing from labeled to unlabeled data
  cluster computing framework - ideal for large data sets
  RDD Resilient Distrib Dataset
  Datase: RDD + Spark SQL execution engine
  DataFrame: dataset ordanized into named columns

  Los Angeles police stop data, 600.000 http://bit.ly/2f9jVwn
  Decision Trees (binary classifier) robust in noise; good for binary
  No universal approach
  Racist data leads to racist machines - bias problem

  What's Deep learning
  neural networks - computational architecture modeled after brain
  many layers
  vanishing/exploding gradient problem
  Vanishing & Exploding Gradients
  Convolutional networks; stacks of feature maps
  Max Pooling / Downsampling
  Alternating Layers

  Tale of two DSLs: Flambo vs. Sparkling
  1. Flambo
  2. Sparkling
  DL4J - Deep Learning 4 Java
  nuroko.com - Nuroko Toolkit: Advanced machine learning (painting)
  Peter Norvig / Stuart Russell: Artificial Intelligence: A Modern Approach
  Syntax of log files changes (speach) sentence structure: subject comes at the end of line
  Music: what is the relation between harmony and ryth; model of musical creativity
  How represent speach context? - history of lisp - ClojureD
  Create experience dbase
  Android App: picture comparision
  put together: chess languages ;;
  Problemy tazke pre comp, lahke pre cloveka (arimaa - until 2020, etc.)
  Lang words as a sound: celular automata: cell dyies / is born: play a tone

  Code as a Lego Block - TED Talk
  http://www.ted.com/talks/ayah_bdeir_building_blocks_that_blink_beep_and_teach.html
  Polylit - SW architecture that applies functional thinking at the system scale
  https://github.com/polyfy/polylith
  https://polylith.gitbook.io/polylith/

  Trending Clojure repositories
  https://github.com/trending/clojure

  JavaFX - SW platform to build GUI apps & Rich Internet applications (RIAs)
  standard GUI library for Java SE; Swing replacement

  Instaparse: parsers in clojure: context-free grammars as easy to use as regexs
}

@block{@block-name{nil-punning}
  https://lispcast.com/nil-punning/
  Clojure, many operations (like first or rest) on nil just return nil instead
  when you look up something in a map that doesn't exist, nil is returned. In
  of raising an error. So, when you think you are looking up something in a map,
  but the "map" is actually nil, it will not give an error, but it will return
  nil.

  Now like I said, sometimes you may get an error on nil. It's a bit unclear
  which operations are nil-punning and which will give a proper error. So when
  you finally get a nil error, you will have a hell of a time trying to trace
  compared to some other Lisps, as nil-punning is traditionally a dynamic Lisp
  back where this nil got generated, as that may have been several function
  calls ago. This is an example where I really like the strictness of Scheme as
  thing; it's not unique to Clojure.

  Compared to the insane amount of customizability that e.g. CLOS offers you,
  the design restraint shown in Clojure multimethods was nice to see, but then I
  realised this simplicity can be completely defeated by building hierarchies.
  That is, Clojure allows you to define a *hierarchy* on *keywords*. This was a
  huge wtf for me, because to me, keywords are just static entities that are
  unrelated to each other.

  When you realise how Clojure keywords can be namespaced, it makes slightly
  more sense: this gives them some separation.
}

@block{@block-name{REPL / In the shell}
  @block{@block-name{cli: script: repl from command line}
    https://github.com/borkdude/babashka
    Closh - Bash-like shell based on Clojure
    https://github.com/dundalek/closh

    # see config.fish
    set cljjar ~/.m2/repository/org/clojure/clojure/1.10.0/clojure-1.10.0.jar
    set cljsjar ~/.m2/repository/org/clojure/spec.alpha/0.2.176/spec.alpha-0.2.176.jar
    rlwrap java -cp $cljjar:$cljsjar clojure.main

    cli: run as a script: ./hello.clj
    #!/usr/bin/env boot
    (println "Hello world script via bash and boot - this is slow!")
    ;; cli: run as a script: clojure ./hello.clj
    (println "Hello world script: clj script.clj aaa bbb")
    (doseq [arg *command-line-args*]
      (printf "arg='%s'\n" arg))

    # start REPL:
    set clj_home $dec/clojure.org/clojure
    cd $clj_home
    ./antsetup.sh
    ant local
    set repl "{:port 5555 :accept clojure.core.server/repl}"
    java -Dclojure.server.repl=$repl -jar $clj_home/clojure.jar
    # boot socket-server --port 5555 wait # requires boot 2.7.2
    # bb --nrepl-server 5555              # requires babashka v0.0.89

    yarn global add unravel-repl
    unravel localhost 5555

    # start a server
    boot socket-server --port 5555 --accept clojure.core.server/io-prepl wait &; disown
    # execute (+ 1 2) and quit
    echo -e "(clojure.core/+ 1 2)\n:repl/quit" | nc localhost 5555
  }
  @block{@block-name{Rest}
    # HelloWorld: compile and run class file then create jar file and run it
    set basedir ./javasrc
    set classdir org/domain
    # specified in Main.java by: package org.domain;
    set package org.domain
    # compile to Main.class
    javac -cp $basedir $basedir/$classdir/Main.java
    # run Main.class
    java -cp $basedir $package.main
    # create jar file
    jar -cfe Main.jar $classdir.Main -C $basedir $classdir/Main.class
    # create jar file - an alternative
    jar -cfe Main.jar $package.Main -C $basedir $classdir/Main.class
    # run Main.class from jar file
    java -jar Main.jar arg0 arg1

    # java: observe jvm
    jcmd / jinfo / jstat / jstack

    # java: jar: jarsigner: keytool: jnlp: javaws:
    jarsigner -storepass PASSW -keystore ~/.keystore FILE mykey

    # java: jar: jarsigner: keytool: jnlp: javaws:
    keytool -genkeypair / keytool -list

    # java: list files in file.jar
    jar tf file.jar

    # java: extract inside.txt from file.jar + show content. File stays extracted
    jar xf file.jar ./path/inside.txt && cat ./path/inside.txt

    # java: jar: unzip: show content of a file inside a file.jar
    unzip -p file.jar ./path/inside.txt

    # java: jar: unzip: show content of META-INF/MANIFEST.MF
    unzip -p file.jar META-INF/MANIFEST.MF

    # java: list content of file
    unzip -lv file.jar

    # java: extract specific folder from a file to a given directory
    unzip file.jar 'folderToExtract/*' -d ./dst/dir

    # java: unzip: overwrite / don't overwrite
    unzip -o / -n file.jar

    # java web start
    javaws start.jnlp

    # disassemble file.class (bytecode)
    javap file.class / javap -p -s file.class

    # ubuntu: java: jdk: change default jdk / java / javac environment
    sudo apt-key adv --keyserver-options http-proxy="http://<proxy>:<port>/" \
     --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886
    sudo add-apt-repository ppa:webupd8team/java # alternatively ppa:openjdk-r/ppa
    sudo apt-get update
    sudo apt-get install openjdk-8-jdk
    sudo apt-get install openjdk-8-source # this is optional, the jdk source code
    sudo apt install oracle-java8-set-default # may or may not be desired
    sudo update-alternatives --config java / javac

    # On Guix, when following error appears:
    #   java.lang.ClassNotFoundException: jdk.javadoc.doclet.Doclet
    guix package -r openjdk -i openjdk:jdk
    # or if the profile is messed up then:
    guix package --list-profiles
    guix package --profile=$HOME/.guix-profile      --remove openjdk --install openjdk:jdk
    guix package --profile=$HOME/.guix-home/profile --remove openjdk --install openjdk:jdk

    # see /etc/profile.d/jdk.sh /etc/environment ~/.config/fish/config.fish
    # changes require logout and login
    set -x JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
    set -x JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64
  }

  @block{@block-name{Leiningen}
    lein deps :tree                  # dependency tree
    lein classpath | sed 's/:/\n/g'  # classpath
    lein -o                          # run a task offline
    # upgrade project dependencies in shell
    lein ancient upgrade :all :interactive :check-clojure :no-tests
    lein do clean, repl               # run multiple tasks
    lein cljsbuild test
    # On Guix set JAVA_CMD when this error appears:
    #   Java compiler not found; Be sure to use java from a JDK
    #   rather than a JRE by modifying PATH or setting JAVA_CMD.
    # set --export JAVA_CMD $HOME/.guix-profile/bin/java
    lein install
    lein localrepo install target/virgil-x.y.z.jar lein-virgil x.y.z
    lein deploy clojars
    # create / open remotelly accessible repl (nrepl)
    lein repl :headless :host 0.0.0.0 :port <portNr>
    #
    # deps.edn; clojure -M:outdated corresponds to `lein ancient`
    clojure -M:outdated --upgrade # --force
  }
}

@block{@block-name{Reference types}
  | Jméno       | Var                   | Ref          | Atom      | Agent     |
  | Změna stavu | synchronní            | synchronní   | sync      | async     |
  | Typ změny   | local within 1 thread | koordinovaná | nezávislá | nezávislá |
  | Tx support  | no                    | yes          | no        | no        |
}

@block{@block-name{Clojure in Emacs:}
  M-x cljr-rename-symbol refactoring:
  C-c RET r s

  M-x cider-repl-clear-buffer
  C-c M-o

  M-x cider-repl-clear-output
  C-c C-o

  M-x cider-doc
  C-c C-d C-d

  M-x cider-pprint-eval-last-sexp
  C-c C-p

  M-x cider-pprint-eval-defun-at-point
  C-c C-f

  M-x cider-refresh: reload all modified Clojure files on the classpath
  C-c C-x

  @block{@block-name{Cider debugger}
    C-M-x cider-eval-defun-at-point
    C-u C-M-x cider-debug

    https://cider.readthedocs.io/en/latest/debugging/#keys
    https://cambium.consulting/articles/2018/2/8/the-power-of-clojure-debugging
    n Next step
    i Step in to a function
    o Step out of the current sexp (like up-list)
    O Force-step out of the current sexp
    h Skip all sexps up to “here” (current position). Move the cursor before
    doing this.
    H Force-step to “here”
    c Continue without stopping
    e Eval code in current context
    p Inspect a value
    l Inspect local variables
    j Inject a value into running code
    s Show the current stack
    t Trace. Continue, printing expressions and their values.
    q Quit execution
  }
}

@block{@block-name{Debugging}
  https://github.com/clojure-emacs/sayid
  https://github.com/AppsFlyer/mate-clj
  https://github.com/dgrnbrg/spyscope
  https://blog.michielborkent.nl/blog/2017/05/25/inline-def-debugging/
  https://github.com/vvvvalvalval/scope-capture
}

@block{@block-name{Namespace}
  (ns ^{:doc "
  - Namespace as a Type: ns contains fns returning values of the same type. I.e.
    it is a set of proofs of a given proposition (i.e. of a given type).
  - Use morphism from \"complicated\" Types (i.e. Products consisting of many
    Types. E.g. maps each having many keys) to subsets of (if possible natural)
    numbers. These subsets should be in fact monads
  - namespace underscore minus dash; prefer filenames with underscores:
   `lein new foo-bar` produces src/clj/foo_bar.clj with `(ns foo-bar.core)`"
          :author "<Author's name>" :last-update "<the-date>"}
        foo-bar.core)
  ;; (meta *ns*) ; should print the whole metadata hash-map
}

@block{@block-name{Macros}
  ;; ` backtick - syntax-quote
  ;; ' apostrophe - quote
  ;; ~ tilda - unquote within a syntax-quote block
  ;; see http://stackoverflow.com/a/17810391
  '(+ x x) => (+ x x)                         ;; symbol-name quoted exactly
  `(+ x x) = > (clojure.core/+ user/x user/x) ;; symbol-name quoted with namespace
  ;; when using tilda inside syntax-quoted block then the ~form is unquoted
  `(+ ~'x x) => (clojure.core/+ x user/x)
  `(+ ~x x)  => Unable to resolve symbol: x in this context
  ;;                                ;;
  (defmacro x [] (+ 1 2))         ;; => 3
  (defmacro x [] '(+ 1 2))        ;; => 3
  (defmacro x [] `(+ 1 2))        ;; => 3
  ;;
  (defmacro x [] `('+ 1 2))       ;; => 2
  (defmacro x [] '(`+ 1 2))       ;; => 2
  (defmacro x [] `(`+ 1 2))       ;; => 2
  (defmacro x [] '('+ 1 2))       ;; => 2
  ;;
  (defmacro x [] `'(~'+ 1 2))     ;; => (+ 1 2)
  (defmacro x [] `(~'+ 1 2))      ;; => 3
  (defmacro x [] `'(~'+ 1 2))     ;; => (+ 1 2)
  (defmacro x [] `'('+ 1 2))      ;; => ('clojure.core/+ 1 2)
  (defmacro x [] `'(+ 1 2))       ;; => (clojure.core/+ 1 2)
  (defmacro x [a b] (+ a b))      ;; (x 1 2) => 3
  (defmacro x [a b] `(+ a b))     ;; (x 1 2) => No such var: goog.numbers-test/a
  (defmacro x [a b] `(+ ~a ~b))   ;; (x 1 2) => 3
  (defmacro x [a b] `(+ ~'a ~'b)) ;; (x 1 2) => Unable to resolve symbol: a in this context
  (defmacro x [a b] `(+ '~a '~b)) ;; (x 1 2) => 3
  ;;
  (defmacro x [a b] '(+ a b))     ;; (x 1 2) => Unable to resolve symbol: a in this context
  (defmacro x [a b] '(+ ~a ~b))   ;; (x 1 2) => Unable to resolve symbol: a in this context

  (defmacro def-stuff
    "Define stuff with metadata. E.g.:
    (def-stuff \"FOO\")
    ;; => #'user/foo

    (meta #'foo)
    ;; => {:const true,
    ;; =>  :tag java.lang.String,
    ;; =>  :line 1,
    ;; =>  :column 1,
    ;; =>  :file \"NO_SOURCE_PATH\",
    ;; =>  :name foo,
    ;; =>  :ns #object[clojure.lang.Namespace 0x55ea2d70 \"user\"]}
    "
    [c]
    `(let [c# ~c]
       #_{
        ;; "(clojure.core/pr c#)" (clojure.core/pr c#) ;; returns nil but prints the value of c# on *out*
        ;; "c" c ;; => error: No such var
        "c#" c#
        "~c" ~c
        "'~c" '~c
        "'c#" 'c#
        "`c" `c
        "`~c" `~c
        "`c#" `c#
        "'c" 'c
        "''c" ''c
        "(read-string c)" (read-string c)
        "(quote c)" (quote c)
        "(quote 'c)" (quote 'c)
        "(quote (quote c))" (quote (quote c))
        "(quote c#)" (quote c#) "(quote ~c)" (quote ~c) "(quote '~c)" (quote '~c) "(quote 'c#)" (quote 'c#)
        "(symbol c#)" (symbol c#) "(symbol ~c)" (symbol ~c) "(symbol '~c)" (symbol '~c) "(symbol 'c#)" (symbol 'c#)

        ;; "(symbol ~c#)" (symbol ~c#)   ;; => error: c# undefined
        ;; "(symbol '~c#)" (symbol '~c#) ;; => error: c# undefined
        "(type (symbol 'c#))" (type (symbol 'c#))

        ;; "(clojure.core/pr c)" (clojure.core/pr c) ;; => error: No such var
        }
       ;; (def (symbol ~c) c#)  ;; => error: First argument to def must be a Symbol
       ;; (def (symbol c#) c#)  ;; => error: First argument to def must be a Symbol
       ;; (def (symbol '~c) c#) ;; => error: First argument to def must be a Symbol
       ;; (def ~c c#)           ;; => error: First argument to def must be a Symbol
       ;; (def c# c#)           ;; => #'corona.country-codes/c__82086__auto__
       ;; (def (eval c#) c#)    ;; => error: First argument to def must be a Symbol
       ;; (def (clojure.core/pr c#) c#) ;; => error: First argument to def must be a Symbol

       ;; (list 'def '^:const (symbol (clojure.string/lower-case c#)) c#)
       ;; => (def (clojure.core/symbol (clojure.string/lower-case c__82982__auto__)) "A")

       ;; (eval (list 'def '^:const (symbol (clojure.string/lower-case c#)) c#))
       ;; => error: First argument to def must be a Symbol

       ;; (list 'def (symbol (clojure.string/lower-case c#)) c#)
       ;; (def ~(vary-meta c {:const true}) c#)

       (def ~(vary-meta (read-string (clojure.string/lower-case c))
                        assoc :const true :tag `String) ~c)))
}

@block{@block-name{comp, partial, threading macros}
  threading macros create intermediate collections in every step.
  replace them with transducers
  https://groups.google.com/g/clojure/c/BhkUEoJBpKA
  (= (conj {:a 2} {:a 1}) (->> {:a 1} (conj {:a 2})))
  (= (conj {:a 1} {:a 2}) (-> {:a 1} (conj {:a 2})))
  (as-> [:foo :bar] $ (map name $) (first $) (.substring $ 1))
  ;; swiss-arrows: diamond wand; see also harpoons
  (-<> 2 (* <> 5) (vector 1 2 <> 3 4)) ;; => [1 2 10 3 4]

  ;; threading macros: "short-circuit out" of a series of steps; the nil case is
  ;; handled only once, at the end. See also `some->>` and `cond->>`
  (-> {:a 1} :b inc)
  ;; => NPE ;; i.e. Null Pointer Exception
  (some-> {:a 1} :b inc)
  ;; => nil
  ;; use CIDER debugger to investigate
  (cond-> 1        ; we start with 1
    true inc       ; the condition is true so (inc 1) => 2
    false (* 42)   ; the condition is false so the operation is skipped
    (= 2 2) (* 3)) ; (= 2 2) is true so (* 2 3) => 6
  ;;
  (cond
    (< n 0) "negative"
    (> n 0) "positive"
    :else "zero")
  ;;
  (condp = value
    ;; (= test-expr expr) are evaluated
    1 "one"
    2 "two"
    (str "unexpected value:" value))
  ;;
  (case n
    ;; The test-constants (i.e. 0, 1) are not evaluated. They must be compile-time
    ;; literals, and need not be quoted
    0 "zero"
    1 "one"
    "other")
}

@block{@block-name{Rest}
  ;; a spaceholder - one-liners don't get matched
  (ns clj.cheat
    "The docstring")
  (meta *ns*) ;; => "The docstring"

  ;; (A * B) could be seen as a product (e.g. join)
  ;; (A + B) coproduct (e.g. disjoint union) of A and B

  ;; see https://github.com/clojure/data.priority-map
  ;; sorted map   - entries sorted by key
  ;; priority map - entries sorted by value; see conj, peek, pop

  ;; clojure.core/atom
  (def cnt (atom 0))
  (swap! cnt inc)             ;; => 1
  (swap! cnt (fn [n] (+ n 3))) ;; => 4
  (reset! cnt 0)

  ;; update an atom and return the change
  (def state-hash-map (atom {:a 1}))
  (swap! state-hash-map update-in [:a] inc) ;; => {:a 2}

  (def state (atom {:a {:aa 2 :ab 3} :b 4}))
  (swap! state update-in [:a] dissoc :aa) ;; => {:a {:ab 3}, :b 4}

  ;; destructure hash-map; default function prms / params / parameters
  (defn f
    "Values of the prm accessible under sames names as the keys"
    [{:keys [a b c] :or {c "c-default"} :as prm}] [a b c])
  (f {:b "b" :a "a"})
  ;; => ["a" "b" "c-default"]
  (defn f
    "Values of the prm accessible under different names as the keys"
    [{a-val :a b-val :b :as prm}]
    [a-val b-val prm])
  ;; => ["a" "b" {:b "b", :a "a", :c "c"}]

  ;; destructure hash-map
  (let [hm {:a 1 :b 2 :c 3} {a :a b :b} hm] [a b])

  ;; Common Lisp Object System
  CLOS

  ;; bug detection tool for Java
  ;; https://opensource.google.com/projects/error-prone

  ;; https://clojure.org/reference/namespaces
  ;; show objects in the namespace; show what is defined
  (let [the-ns *ns* #_'my.data]
    (ns-interns the-ns)
    ;; alternatively
    #_(sort (keys (ns-publics the-ns))))

  ;; undefine / clean the whole namespace from the REPL;
  ;; `cider-ns-refresh` doesn't work as expected
  (let [the-ns *ns* #_'my.data]
    (map (fn [k] (ns-unmap the-ns k)) (keys (ns-interns the-ns))))

  ;; undefine / clean just one thing
  (let [the-ns *ns* #_'my.data]
    (ns-unmap the-ns 'old-definiton)
    ;; see namespace aliasing
    (ns-unmap 'current-namespace 'local-alias))

  ;; read and evaluate src/ws/core.clj
  (load-file "src/ws/core.clj")
  ;; load libs / libraries - undocumented keywords:
  ;; https://clojuredocs.org/clojure.core/use#example-57dc37b1e4b0709b524f04fb
  (use '[ws.core] :reload)
  (use '[ws.core] :reload-all)
  (use '[ws.core] :verbose)

  (in-ns 'full.namespace) ;; switch to full.namespace
  (all-ns)                ;; returns a sequence of all namespaces

  ;; M-x cider-doc (C-c C-d C-d) / M-x cider-javadoc
  (require '[clojure.repl :refer :all])
  ;; all public defs in all currently-loaded nss matching str-or-pattern
  (doc apropos)
  (doc doc)
  (clojure.repl/doc full.namespace/objname)
  (apropos "doc") ;; Unsure about name - find fns matching str-or-regex
  (find-doc "")
  (dir clojure.repl)
  (source doc)

  ;; , r t l   /  M-x clojure-thread-last-all then try
  ;; , r u a   /  M-x clojure-unwind-all
  (apply + (filter odd? (map inc (range 5))))

  ;; sexp / block comment; the block comments sexp returns nil
  #_(foo 1 2)/ (comment foo 1 2)

  ;; print stack trace: (/ 1 0) (pst)
  (clojure.stacktrace/print-stack-trace (Exception.))
  (clojure.stacktrace/print-stack-trace (Exception. "foo"))

  ;; :stacktrace last :exception
  (clojure.stacktrace/print-stack-trace *e)

  ;; try to put it to project.clj in case of:
  ;; 'Could not locate clojure/instant__init.class or ... on classpath'
  ;; [the-dependency "X.Y.Z" :exclusions [org.clojure/clojure]]

  ;; Transpose anything / matrix:
  (= (apply mapv vector [[:a :b :c] [0 1 2]])
     (mapv vector [:a :b :c] [0 1 2])
     [[:a 0] [:b 1] [:c 2]])
  ;; also - matrix transposition is an isomorphism
  (let [v [[:a :b :c] [0 1 2]]]
    (= v
       (->> v
            (apply mapv vector)
            (apply mapv vector))))

  ;; 'map' and 'vector' "slip inside" the argument list ( '[[:a :b] [:c :d]]' ) to
  ;; give '[map vector [:a :b] [:c :d]]' which then becomes the executable form
  ;; '(map vector [:a :b] [:c :d])'

  ;; clojure.core.typed
  (t/check-ns)

  ;; memoization
  (clojure.core.memoize/memo-clear! f args)

  ;; Clojure Jython interop http://clojars.org/clojure-python
  ;; TODO it doesn't work
  (require '[midje.sweet :refer :all])
  (require '[clojure-python.core :as base])
  (with-test-interp
    (base/py-import-lib example)
    (base/import-fn example hello)
    (hello "world"))

  ;; Type Hints: http://clojure.org/reference/java_interop#typehints
  (defn len2 [^String x] (.length x))
  (set! *warn-on-reflection* true)
  ;; Use a type hint '^String' so that the call to charAt can be resolved.
  (defn foo [^String s] (.charAt s 1))
  ;; return vals
  (defn hinted (^String []) (^Integer [a]) (^java.util.List [a & args]))

  ;; put to build.boot:
  ;; [boot-deps "0.1.9"] ;; boot -d boot-deps ancient

  ;; om-next inspect app-state pretty print
  (in-ns 'ufo.client)
  (require '[cljs.pprint :as pp])
  (def norm-data (om/tree->db RootView ufo.state/app-state true))
  (pp/pprint norm-data)

  ;; om-next: execute read & mutate methods
  (def parser (om/parser {:read ufo.state/read :mutate ufo.state/mutate}))
  (parser {:state (atom ufo.state/app-state)} '[:list/rec])
  (parser {:state (atom ufo.state/app-state)} '[(ufo.meth/'activate-rec! vms)])

  ;; Write / pretty-print hash-map to an eden file; `print` writes String as
  ;; clojure.lang.Symbol Writing out under a path e.g. "/tmp/data.edn" doesn't
  ;; work. File is created in the REPL working directory
  ((comp (partial spit "data.edn") pr-str) {:a 1 :b 2})
  (clojure.pprint/pprint *large-map* (clojure.java.io/writer "/tmp/data.edn"))
  ;; read hash-map from an eden file
  ((comp read-string slurp) "/tmp/data.edn")

  ;; difference of sets
  (clojure.set/difference (set [1 2 3]) (set [1 2 4])) ;; => #{3}
  ;; Returns a negative number, zero, or a positive number
  (compare "abc" "def")
  ;; compare nested complex data structures
  (require 'clojure.data) ;; the diff won't work without this
  (clojure.data/diff {:a 1} {:b 2})

  ;; two dots: clojurescript interop
  (.. object -property -property method)
  (.. object -property -property -property)
  ;; Instead of:
  (.method (.-property (.-property object)))
  (.-property (.-property (.-property object)))

  ;; cljc: reader conditionals - for different platforms
  #?(:clj Double/NaN :cljs js/NaN :default nil)

  ;; Figwheel Controls:
  (stop-autobuild)                ;; stops Figwheel autobuilder
  (start-autobuild id ...)        ;; starts autobuilder focused on optional ids
  (switch-to-build id ...)        ;; switches autobuilder to different build
  (reset-autobuild)               ;; stops, cleans, and starts autobuilder
  (reload-config)                 ;; reloads build config and resets autobuild
  (build-once id ...)             ;; builds source one time
  (clean-builds id ..)            ;; deletes compiled cljs target files
  (print-config id ...)           ;; prints out build configurations
  (fig-status)                    ;; displays current state of system
  (figwheel.client/set-autoload false)    ;; will turn autoloading off
  (figwheel.client/set-repl-pprint false) ;; will turn pretty printing off
  ;; Switch REPL build focus:
  :cljs/quit                      ;; allows you to switch REPL to another build
  ;; Docs: (doc function-name-here)
  ;; Exit: :cljs/quit
  ;; Results: Stored in vars *1, *2, *3, *e holds last exception object
  ;; Prompt will show when Figwheel connects to your application
  ;; To quit, type: :cljs/quit

  ;; like lein but for node.js
  gulp

  ;; map: can be well parallelized / reduce:  difficult to parallelize
  (map) / (reduce)

  ;; future; A function which hasn't finished the evaluation
  ;; see also: promise (more complex than future), delay
  (def f-slow (future (Thread/sleep 3000) (println "f-slow done") 100))
  ;; when dereferenced, it blocks until the result is available
  @"@"f-slow

  ;; element in sequence
  (defn in?
    "true if seq contains elm"
    [seq elm] (boolean (some (fn [e] (= elm e)) seq)))
  ;; for hash maps just `contains?` may be used; TODO extend `utils.core/in?`
  (contains? {:a 1} :a) ; => true

  ;; brackets, parens, parenthesis conversion
  ;; M-x clojure-convert-collection-to-vector / clojure-convert-collection-to-list

  ;; brackets, parens, parenthesis converion; spacemacs clojure mode:
  ;; , r c

  ;; symbol:
  ;; - represents metaphorically objects (it's not a string)
  ;; - atomic value with fast equality check and fast hashing
  ;; - suitable for enumeration values
  'milkshake

  clojure.core.async/<!! [port]
  ;; [async/<!!] takes a val from port. Will return nil if closed. Will block if
  ;; [async/<!!] nothing is available.

  clojure.core.async/<! [port]
  ;; takes a val from port. Must be called inside a (go ...) block. Will return
  ;; nil if closed. Will park if nothing is available.

  clojure.core.async/>! [port val]
  ;; puts a val into port. nil values are not allowed. Must be called
  ;; inside a (go ...) block. Will park if no buffer space is
  ;; available. Returns true unless port is already closed.

  ;; transducer fast reducible colls / composable algorithmic transformations
  ;; clojure.java.jdbc/reducible-query
  ;; (comp filter map) replacement for a bunch of transformations and a bunch of
  ;; intermediate collections; (getting rid of intermediate collections)

  ;; Peter Norvig: "Design patterns are bug reports against your prog language"
  ;; http://norvig.com/design-patterns/design-patterns.pdf

  ;;; recursion: Clojure Tail Call Optimizer https://github.com/cjfrisz/clojure-tco
  ;; Society by Niklas Luhmann: (People are) recursing, self referencing systems of communication
  ;;; specialisations
  ;;; mastery vs. novelty (expensive)
  ;;; dealing with complexity of options
  ;; REPL: java -jar clojure; TODO see the video "The most beautifull programm"
  user=> (->> (read) eval prn (while true))
  user=> (loop [] (println (eval (read))) (recur))

  ;; TODO test; tel macro - inverse of let
  (dbg)
  (dbg (dbg))
  (dbg (dbg nil))
  (let [1 2])
  (tel [2 1])

  ;; [org.clojure/tools.logging "0.4.1"]
  ;; A Clojure(Script); debug single- and multi-threaded apps
  ;; [spyscope "0.1.6"]


  ;; interface              | list | vector | hash-map | hash-set
  ;; java.util.Collection   | y    | y      | n        | y
  ;; java.util.List         | y    | y      | n        | n
  ;; java.util.Map          | n    | n      | y        | n
  ;; java.util.Set          | n    | n      | n        | y
  ;; java.util.RandomAccess | n    | y      | n        | n
  ;; java.lang.Iterable     | y    | y      | y        | y
  ;; java.lang.Comparable   | n    | y      | n        | n

  ;;
  .. js-obj clj->js js->clj

  ;; IllegalStateException: Alias foo already exists in namespace
  (ns-unalias *ns* 'foo)

  ;; clojure.spec - examples
  (clojure.spec/exercise [spec] [spec n] [spec n overrides])

  ;; spec alternatives:
  ;; declarative data description and validation
  ;; https://github.com/plumatic/schema
  ;;
  ;; Plain data Schemas for Clojure/Script.
  ;; https://github.com/metosin/malli
  ;;
  ;; Inline clojure.spec with semi-automatic generative testing and side effect
  ;; detection
  ;; https://github.com/gnl/ghostwheel

  ;; test macro with clojure.spec
  (require '[clojure.spec.alpha :as s])
  (s/def ::my-type (s/cat :p0 int? :p1 string?))
  (defmacro my-macro [& args]
    (let [ret (s/conform ::my-type args)]
      (if (= ret :clojure.spec.alpha/invalid)
        (s/explain-str ::my-type args)
        `(do ~ret))))
  (def data (->> ::my-type s/gen clojure.test.check.generators/generate))
  (->> data (cons 'my-macro) eval) ;; => {:p0 -21, :p1 "96gJ"}

  ;; hash-map look-up with default value
  (:c {:a "a" :b "b"} "default")

  ;; regex syntax
  (apply = (map str [#"^ foo $"
                     ;; create regex
                     (re-pattern (format "%s %s %s" "^" "foo" "$"))]))
  ;; => true

  ;; CRDT - Conflict-free Replicated Data Type
  ;; synchronize state across uncoordinates nodes in an eventually consistent way

  ;;
  ;; lein boot deps.end

  ;;
  ;; nrep unrepl socket repl

  ;; deeply nested data structure: truncate the output
  (set! *print-level* 3)
  user=> {:a {:b [{:c {:d {:e 42}}}]}}
  {:a {:b [#]}}

  ;; data structure with long collections: limit the number of displayed items
  (set! *print-length* 3)
  user=> (repeat 100 (vec (range 100)))
  ([0 1 2 ...] [0 1 2 ...] [0 1 2 ...] ...)

  ;; keep / save result for longer than 3 evaluations
  (def <some-name> *1)

  ;; GUI-based data visualizations
  (require '[clojure.inspector :as insp])
  (insp/inspect-table (mapv number-summary [2 5 6 28 42]))
  (insp/inspect-tree {:a {:b 1} :c [1 2 3]})

  ;; require a library
  (require '[mylib.ns1])
  (require '[myproject.welcome] :verbose)

  ;; system-viz graphviz
  ;; visualize a system

  ;; micro dependency injection framework.
  ;; https://github.com/stuartsierra/component
  ;; https://github.com/tolitius/mount
  ;; https://github.com/danielsz/system - PostgreSQL included

  (array-map :a 10)                    ;; => {:a 10}
  (type (array-map :a 10))             ;; => clojure.lang.PersistentArrayMap
  ((juxt keys vals) (array-map :a 10)) ;; => [(:a) (10)]

  ;;
  (try
    (/ 1 0)
    (catch ArithmeticException e (str "caught exception: " (.getMessage e)))
    (catch Exception e (prn "This is the error" (ex-data e)))
    (finally (prn "final exception.")))

  ;; https://clojuredocs.org/clojure.core/if-let
  (if-let [x 0]   [(inc x) :is-bool-true] :is-bool-false) ;; => [1 :is-bool-true]
  (if-let [x nil] [(inc x) :is-bool-true] :is-bool-false) ;; => :is-bool-false

  (defn greet
    "Multi-arity - 3 possible forms; & - variadic i.e. infinite arity. E.g.:
    (greet \"Jim\" \"Joe\" \"Jack\")"
    ([] (greet "you"))
    ([name] (printf "Hi %s\n" name))
    ([name & others] (printf "Hi %s, %s\n" name
                             (reduce str (interpose " & " others)))))
  ;; invoke from another function
  (defn foo
    "E.g.:
    (greet \"Jim\" \"Joe\" \"Jack\")"
    [name & others]
    (apply greet name others))
  ;;
  (foo "Jim" "Joe" "Jack")

  ;; defprotocol (protocol ~ Java Interface)
  ;; Defines a named set of named methods & signatures.
  ;;
  ;; deftype
  ;; Defines named type. Directly supports protocols
  ;;
  ;; defrecord
  ;; Defines named type. Directly supports protocols
  ;;
  ;; reify
  ;; Defines both an anonymous type and creates an instance of that type
  ;; reify use case:
  ;; One-off implementation of protocol(s) or interface(s) and usage of the local
  ;; context. Usage similar to proxy, or anonymous inner classes in Java.

  ;; TODO derive multi

  ;; reverse / inverse / swap hash-map
  (clojure.set/map-invert {:a 1})

  ;; Discussions on solving the 4Clojure Code challenges
  ;; https://github.com/practicalli/four-clojure/tree/master/src/four_clojure

  ;; `keep` tells you which number to keep
  (keep odd? (range 10))
  ;; => (false true false true false true false true false true)

  (defn string-to-string
    "By using `is` we get expected / actual values in the output.
    Thanks to https://stackoverflow.com/a/24836592/5151982; see also
    https://clojureverse.org/t/why-are-pre-and-post-conditions-not-used-more-often/2238/3"
    [s1]
    {:pre  [(clojure.test/is (string? s1))]
     :post [(clojure.test/is (string? %))]}
    s1)

  ;; arguably better :pre / :post
  (defn wrap-fn
    "Thanks to https://stackoverflow.com/a/10778647/5151982"
    [{:keys [function pre post]}]
    (fn [& args]
      (apply pre args)
      (let [result (apply function args)]
        (apply post (cons result args)))))
  ;;
  (def my-fn
    "my-fn in a container"
    (let [tbeg (te/tnow)
          log-fmt "[%s%s%s %s /%s] %s\n"]
      (wrap-fn
       {:function (fn my-fn [a b c] {:a a :b b :c c :x 1})
        :pre (fn [& args]
               (printf "%s: Call function with args: %s\n" tbeg args))
        :post (fn [& args]
                (let [[fn-result fn-args] args]
                  (printf "%s: Return result: %s\n" tbeg fn-result)
                  fn-result))})))

  ;; lexical / static vs. dynamic binding
  ;; value of x is ... only during the execution of foo, not during it's compile
  ;; time.
  ;; TODO create a macro (or a continuation if applicable or a parallel function
  ;; mapping) capturing this compile-time vs. run-time difference.
  (def ^:dynamic x 0)
  (defn foo [] (inc x))
  (defn bar [] x)

  (clojure.set/rename-keys {:a 1, :b 2} {:a :new-a, :b :new-b})
  ;; => {:new-a 1, :new-b 2}

  ;; pwd; print current / working directory
  (System/getProperty "user.dir")
  (.getCanonicalPath (clojure.java.io/file "."))

  (def ^:const foo "FOO")
  ;; => #'user/foo
  (meta #'foo)
  ;; => {:const true, :line 1, :column 1, :file "/tmp/form-init12571418764575000652.clj", :name foo, :ns #namespace[user]}
  (def ^:const bar ["BAR"])
  ;; => #'user/bar
  (meta #'bar)
  ;; => {:const true, :line 1, :column 1, :file "/tmp/form-init12571418764575000652.clj", :name bar, :ns #namespace[user]}
  (def baz ^:const ["BAZ"])
  ;; => #'user/baz
  (meta #'baz)
  ;; => {:line 1, :column 1, :file "/tmp/form-init12571418764575000652.clj", :name baz, :ns #namespace[user]}
  (def qux ^:const "QUX")
  ;; => Syntax error reading source at (REPL:1:23).
  ;; => Metadata can only be applied to IMetas

  (select-keys {:a 1 :b 2 :c 3} [:a :b])
  ;; => {:a 1, :b 2}
  (map {:a 1 :b 2 :c 3} [:a :b]) ;; select-vals
  ;; => (1 2)

  (map-indexed (fn [idx elem] [idx elem]) [:a :b :c]) ;; => ([0 :a] [1 :b] [2 :c])

  ;; Symbols constructed on the fly are not interned
  (= 'foo 'foo)          ;; => true
  (identical? 'foo 'foo) ;; => false
  ;;
  ;; Keywords are interned and fulfill most "symbolic programming" use cases:
  (= :foo :foo)          ;; => true
  (identical? :foo :foo) ;; => true


  ;; Integers are not automatically promoted to bignums like in most Lisps that
  ;; support bignums. Use special-purpose operators like +' and -':
  (* (bit-shift-left 1 62) 2)  ;; => ArithmeticException; integer overflow
  (*' (bit-shift-left 1 62) 2) ;; => 9223372036854775808N
  ;;
  (* (bit-shift-left 1 62) 2N) ; regular * supports BigInt inputs, though
  ;; => 9223372036854775808N
  (* 1N 1) ; but small BigInts aren't normalized to Java Longs
  ;; => 1N
}

@block{@block-name{The Computer Language Benchmarks Game}
  The Computer Language Benchmarks Game
  https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html
  ;; `reduce` can be faster than `apply`...
  (time (reduce + (range 1e9))) ;; 6824.592024 msecs
  (time (apply + (range 1e9)))  ;; 8740.237518 msecs
  ;; ... but not always:
  (time (reduce + (filter odd? (map inc (range 1e8))))) ;; 2421.542711 msecs
  (time (apply + (filter odd? (map inc (range 1e8)))))  ;; 2418.872182 msecs

  #lang racket
  (require racket/sequence)
  (let* [(beg (current-inexact-milliseconds))
         ;; 1e9 is a float 1000000000.0; see 'exact' / 'inexact'
         (expr-val (sequence-fold + 0 (in-range #e1e9)))
         (end (current-inexact-milliseconds))]
    (printf "Elapsed time: ~a msecs\n~a\n" (- end beg) expr-val))
  ;; Elapsed time: 11946.357177734375 msecs

  import numpy
  import timeit

  def sum_range():
      return sum(range(int(1e9)))

  timeit.timeit(sum_range, number=1)
  # 6.388910356999986 # seconds

  def sum_numpy():
      return numpy.sum(numpy.arange(int(1e9)))

  timeit.timeit(sum_numpy, number=1)
  # 1.7422158059998765 # seconds

  And the winner is the math formula for partial sums 1 + 2 + 3 + 4 + ⋯
  https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF
}

@block{@block-name{Project Templates}
  | leiningen         | Build automation and dependency management tool                             |
  | boot              | Build automation and dependency management tool                             |
  | mies              | Minimal ClojureScript project template                                      |
  | chestnut          | Application template for ClojureScript/Om with live reloading               |
  | figwheel-template | lein template: live coding with figwheel (includes Reagent and Om variants) |
  | cljs-start        | lein template: ClojureScript lib with batteries included                    |
  | mala              | lein template: UIs in 100% Clojurescript, with Om, Garden, etc.             |
  | reagent-template  | lein template: projects using Reagent                                       |
  | re-frame-template | lein template: reagent web apps on Re-Frame; + figwheel, re-com, secretary  |
  | descjop           | lein template: Web based desktop apps with Electron(atom-shell), etc.       |
  | electron-template | lein template: Electron based ClojureScript projects using Reagent          |
  | Shadow CLJS       | ClojureScript compilation made easy                                         |
  | create-cljs-app   | Set up a modern CLJS web app by running one command.                        |


  Figwheel Main (rewrite of lein-figwheel)
  build ClojureScript and hot load it
  603 stars; https://github.com/bhauman/figwheel-main
  2.9k stars; https://github.com/bhauman/lein-figwheel

  Shadow CLJS
  ClojureScript compilation made easy
  2k stars; https://github.com/thheller/shadow-cljs
  - create-cljs-app - Set up a modern CLJS (react?) web app by running one command.
    275 stars; https://github.com/filipesilva/create-cljs-app
}

@block{@block-name{Clojure(Script) & Maps}
For npm modules in clojurescript apps you need
1. bundler (e.g. Webpack or Parcel)
2. npm packages
3. define how clojurescript should output its compiled javascript bundle so that a bundler can consume it

clojure ->                                 js                             -> browser
clojure -> webpack js (with included npm modules) -> webpack -> js bundle -> browser


Reagent - interface between ClojureScript and React. A wrapper around React.
Allows to define React components.

GIS Geographic Information System
database with geographic data
Examples:
ArcGIS, ...

OpenStreetMap - a map of the world
spacial database of geographic data (geodata) of the world.

Leaflet
36k stars; https://github.com/Leaflet/Leaflet
Comparable with OpenLayers
JavaScript library for interactive maps
Can use openstreetmap
react-leaflet - React components for Leaflet maps
                4.3k stars; https://github.com/PaulLeCam/react-leaflet

OpenLayers
9.5k stars; https://github.com/openlayers/openlayers
Comparable with Leaflet
Ajax Library for dynamic maps in any web page
rlayers - React Components for OpenLayers 6+
          86 stars; https://github.com/mmomtchev/rlayers

Mapbox        - provider of custom online maps

About PostGIS
PostGIS is a spatial database extender for PostgreSQL object-relational
database. It adds support for geographic objects allowing location queries to be
run in SQL.
}

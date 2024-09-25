#lang notes

@block{@block-name{Package Transformation Options}
  https://guix.gnu.org/manual/en/html_node/Package-Transformation-Options.html
  guix package --with-source=source
  guix package --with-source=package=source
  guix package --with-source=package@"@"version=source
  guix package --with-input=package=replacement
  guix package --with-graft=package=replacement
  guix package --with-debug-info=package
  guix package --with-c-toolchain=package=toolchain
  guix package --with-git-url=package=url
  guix package --with-branch=package=branch
  guix package --with-commit=package=commit
  guix package --with-patch=package=file
  guix package --with-latest=package
  guix package --without-tests=package
  guix package --with-version=PACKAGE=VERSION
  ;; e.g.:
  guix shell gnome-clocks --with-version=gtk=4.7.0 --without-tests=gtk -- \
      gnome-clocks
}

@block{@block-name{Various commands}
  guix install glibc    # contains ldd
  guix install gcc:lib  # contains libstdc++.so.6, defines LIBRARY_PATH
  guix install iproute2 # contains ss socket statistics

  --verbosity=LEVEL
  0 - no output
  1 - quiet
  2 - like 1 with download URLs
  3 - shows all the build log output on standard error

  # `guix package --list-profiles` doesn't know about / ignores the
  # package-profile of the home-environment (~/.guix-home/profile/manifest)
  # see also /run/current-system/profile

  # list all installed / available packages...
  # ... matching '.*ema.*'
  guix package --list-installed=ema # -I ema
  guix package --list-available=ema # -A ema

  # export the list installed packages; can't use tilda ~
  guix package --profile=$HOME/.guix-home/profile --export-manifest > manifest.scm
  # reproduce, i.e. reinstall packages
  guix package --manifest=manifest.scm # -m manifest.scm

  guix package --list-generations            # -l   # see profile
  guix package --switch-generation=PATTERN   # -S
  guix package --roll-back
  guix package --remove python --install guile # -r python -i guile
  guix install                           guile
  guix install bat@"@"0.18.3  # install specific package version

  guix package --upgrade # -u
  guix           upgrade --dry-run
  guix           upgrade --do-not-upgrade="(openjdk|postgres|fish).*"
  # guix package -i mycli --with-latest=mycli

  # remove package with different-than-default output
  $ guix package --profile=/home/bost/.guix-profile --list-installed
  bind	9.16.38	utils	/gnu/store/6rw5zfyn5ydjvsyd6xx9lrnppkqns2iw-bind-9.16.38-utils
  $ guix package --profile=/home/bost/.guix-profile --remove bind
  guix package: error: package 'bind' not found in profile
  $ guix package --profile=/home/bost/.guix-profile --remove bind:utils

  IRC discussion: How to find out from which package a binary comes from?
  https://logs.guix.gnu.org/guix/2021-10-30.log#111758

  guix package --search=hello
  guix           search hello
  # package details
  guix package --show=hello
  guix           show hello
  guix archive   # export / import package(s) from / to the store
}

@block{@block-name{Substitutes}
  https://guix.gnu.org/manual/en/html_node/Substitutes.html
  guix weather       # report on the availability of pre-built package binaries

  Substitute is a pre-built item which can be downloaded from a server, i.e. a
  substitute for local build result. It can be anything resulting from a
  derivation build. It can be a pre-built package binary (typically), source
  tarball, etc.

  https://guix.gnu.org/manual/en/html_node/Invoking-guix-weather.html
  `guix weather` nars: size of the compressed archives

  NAR / nar Nix Archive
  binary, platform independent
}

@block{@block-name{Derivations}

  Lower-level APIs are available to interact with the daemon and the store.

  To instruct the daemon to perform a build action, provide it with
  a derivation.

  A low-level representation of the build actions to be taken, and the
  environment in which these actions should occur - derivations are to package
  definitions what assembly is to C programs.
  The term "derivation" comes from the fact that build results derive from them.

  Derivation represents low-level build actions and the environment in which
  they are performed.

  Derivation contains:
  - The outputs of the derivation—derivations produce at least one file or
    directory in the store, but may produce more.
  - The inputs of the derivations—i.e., its build-time dependencies—which may be
    other derivations or plain files in the store (patches, build scripts,
    etc.).
  - The system type targeted by the derivation—e.g., x86_64-linux.
  - The file name of a build script in the store, along with the arguments to be
    passed.
  - A list of environment variables to be defined.
}

@block{@block-name{Creating packages}
  https://guix.gnu.org/cookbook/en/guix-cookbook.html#Packaging-Tutorial

  PeerTube: Guix Packaging Perl Plack::Runner
  https://video.hardlimit.com/w/p/fed80f62-f342-4f6b-980e-d98f11899236

  Bag:
  - an intermediate form between package and derivation.
  - low-level package representation. Later it will be translated to a package
    derivation, which will be built by the build-daemon

  # sha256; base32; either (A):
  # package definition is obtained using git-fetch
  git clone http://example.org/foo.git && cd foo
  git checkout <tag-or-branch>
  # https://guix.gnu.org/manual/en/html_node/Invoking-guix-hash.html
  guix hash -x --serializer=nar .   # with the dot at the end!

  # sha256; base32; or (B):
  # package definition is obtained using git-fetch
  # clone specific tag or branch
  set repo https://github.com/clojure/tools.deps.alpha
  git clone --depth=1 --branch=v0.15.1254 $repo
  guix hash -x --serializer=nar (basename $repo)

  # sha256; base32; or (C):
  # package is a file downloaded from an the URI - add it to the store, and
  # print both its file name in the store and its SHA256 hash
  guix download URI
  # compute the hash of any (already downloaded) file
  guix hash file

  # fish-shell; download source code of the package of GNU Emacs
  set expr '(use-modules (guix packages) (gnu packages emacs))
            (display (origin-uri (package-source emacs)))'
  guix download (guile -c $expr)

  # fish-shell; download source code of some <emacs-package>
  set emacs_package "eless"
  set expr "(use-modules (guix packages) (gnu packages emacs-xyz))
            (display (origin-uri (package-source $emacs_package)))"
  guix download (guile -c $expr)

  # either (A):
  guix build --load-path=$dgx/gnu/packages --keep-failed <package>
  # $dgx is some GUIX_CHECKOUT directory
  cd $dgx && guix shell --development guix         # -D, --development
  ./pre-inst-env guix build --keep-failed <package>@"@"<version>
  ./pre-inst-env guix install <package>@"@"<version>
  # or (B):
  guix package --install-from-file=my-hello.scm
  # or (C) e.g. for the Factorio stable-version:
  set lp $dev/games
  guix package --load-path=$lp --manifest=$lp/games/packages/factorio.scm --list-available=factorio
  guix package --load-path=$lp --install=factorio  # experimental-version factorio@"@"1.1.78
  @lisp{
    (use-modules (guix)     #| package-source |#
                 (guix swh) #| Software Heritage |#)
    (load "/home/bost/dev/games/games/utils.scm")
    (load "/home/bost/dev/games/games/packages/factorio.scm")
    ,module (games packages factorio)
    (origin-uri (package-source factorio-experimental))
  }

  set lp $dev/guix-packages/src
  # guix package --load-path=$lp --manifest=./bost/packages/spacemacs.scm --list-available=emacs-spacemacs
  # guix build --load-path=$lp --keep-failed --load=./spacemacs-settings.scm emacs-spacemacs
  # guix build --load-path=$lp --keep-failed --expression='(@"@" (gnu) %base-packages)' emacs-spacemacs
  set --export GUIX_PACKAGE_PATH $lp/bost/packages/patches
  guix build --load-path=$lp --keep-failed emacs-spacemacs

  # build the packages' source derivations: the [at] char must be used twice
  guix build --expression='(@"@"@"@" (bost gnu packages emacs-xyz) emacs-color-identifiers-mode)' --source

  # obtain / download / clone package source code
  guix build --source emacs-treemacs
}

@block{@block-name{Garbage collection}
  # delete generations older than 45 days and free space until at least 10 GiB
  # are available
  guix gc --delete-generations=45d --free-space=10GiB

  # delete generations older than 2 months days and collect at least 10 GiB of
  # garbage
  guix gc --delete-generations=2m  --collect-garbage=10GiB

  # attempt to delete some PATH from the store
  guix gc --delete PATH
  # guix gc --referrers /gnu/store/*-spacemacs-*/ | xargs guix gc --delete
  # attempt to delete every *-emacs-spacemacs-* directory from the store
  for e in (ls -d1 /gnu/store/*-spacemacs-*/); guix gc --delete $e; end
  # invoking garbage collector from the repl: https://issues.guix.gnu.org/25018
  # see also guix gc --list-roots
  guix gc --list-roots
  guix gc --referrers
  guix gc --list-dead

  set lst \
  /gnu/store/5d48vxdg7yqsvcq683r2d68mqslpx5hq-emacs-spacemacs-28.2-0.999.0-0.a51a79a \
  /gnu/store/5j0hk9nlc98s41y93bcpadzww27k677n-emacs-spacemacs-28.2-0.999.0-0.f6664c8
  for e in $lst; guix gc --delete $e; end;
  # for e in $lst; guix gc --referrers $e; end;
  # for e in (ls -d1 /gnu/store/*-spacemacs-rolling-*-0.999.0-*/) guix gc --delete $e end

  guix gc --list-dead > /tmp/dead.log
  rg -N profile /tmp/dead.log
  guix gc --delete ...

  guix graph --type=reverse-package
  guix graph --path

  # visualize dependencies of / packages needed by / packages required by:
  guix graph coreutils | xdot - & disown
  guix graph coreutils | dot -Tpdf > dag.pdf

  # visualize dependencies of / packages needed by / packages required by
  # including implicit inputs:
  guix graph --type=bag-emerged coreutils | xdot - & disown
  guix graph --type=bag-emerged coreutils | dot -Tpdf > dag.pdf

  # packages that explicitly depend on:
  guix graph --type=reverse-package ocaml
}

@block{@block-name{Package Inputs / Outputs}
  ;; https://www.gnu.org/software/guile/manual/html_node/Multiple-Values.html
  (use-modules (rnrs base)) ;; provides 'let-values'
  (define (divide-and-remainder a b)
    (if (= b 0)
        (error "Division by zero")
        ;; return multiple / more than one output value
        (values (quotient a b) (remainder a b))))
  ;; Read multiple values using `let-values`
  (let-values (((quot rem) (divide-and-remainder 10 3)))
    (format #t "Quotient: ~a, Remainder: ~a~%" quot rem))
  ;; Read multiple values using `call-with-values`
  (call-with-values (lambda () (values 4 5))
    (lambda (a b) b))
  ;; Read multiple values using `receive`, which is more convenient
  (use-module (ice-9 receive)) ;; or (srfi srfi-8)
  (receive (quot rem)
      (divide-and-remainder 10 3)
    (format #t "Quotient: ~a, Remainder: ~a~%" quot rem))

  A package can have multiple "Outputs", i.e. multiple store-directories, that
  serves as a mean to separate components of a program (libraries, extra tools,
  documentation, etc.).
  E.g.:
  guix install glib:doc - install the 'doc' output of glib
  guix install glib:out - install the default output 'out' of the glib

  Inputs correspond to package dependencies.
  The user profile & environment contains only the packages that the user
  explicitly installed and not necessarily the dependencies of those packages.
  * (basic) inputs
    Built for target architecture. Can be referenced by the package. Will be in
    the resulting binary file
  * native-inputs
    Built for host architecture (the build machine), e.g. built-time utils not
    needed during run-time
  * propagated-inputs
    Will be added to user profile along with the package
    When a package has propagated inputs then any package that depends on it
    will automatically have those inputs available to it.
  * direct
    All of the above
  * implicit
    Added by build system
  * development
    All of the above. Needed for e.g. `guix shell`
}

@block{@block-name{Package Build Systems}
  trivial-build-system
  copy-build-system
  gnu-build-system
  emacs-build-system
  cargo / go / clojure / maven / ant / r / etc.

  Build Phases:
  Sequentially executed list; Some build phases can provide implicit inputs
  Different build systems (may) have different build phases. See e.g.:
  $dgx/guix/build/perl-build-system.scm
  $dgx/guix/build/emacs-build-system.scm
  $dgx/guix/build/waf-build-system.scm
  $dgx/guix/build/go-build-system.scm

  # package will be scheduled for Software Heritage
  guix lint --checker=archival <package>
  guix lint --no-network <package>        # doesn't fetch CVEs hopefully

  Arguments:
  Customize build process w/o modifying it
  #:modules `((guix build utils)) ;; available for the builder below
  #:builder
  - a list of expressions which will be evaluated by the build-daemon to build
    the package during the build-phase
  - can be a g-expression
  - Example:
  (mlet %store-monad ;; ? monadic let? TODO see mlambda
        (#;(...)
         (builder -> (if (pair? builder)
                         (sexp->gexp builder)
                         builder)))
        (gexp->derivation name (with-build-variables inputs outputs builder)
                          ...))

  Dependencies:
  ...

  Utilities:
  E.g. Emacs build System provides some wrappers which allow execution of
  s-expressions using Emacs

  See also
  https://snapcraft.io/
  https://flatpak.org/
  https://nixos.org/
}

@block{@block-name{Guix Build Utils}
  guix/build/utils.scm
  - Doesn't have dependencies
  - Content:
      copy-recursively, chdir, mkdir, find-files, etc.
      substitute*
      update file content
      patch-shebang
      (for-each (lambda (prm) ...) ...)
      install-file - put file to some location
      file-is-directory? (? file-exists?)
}

@block{@block-name{G-expressions, G-exps, gexp}
  - embedded DSL (Domain Specific Language), a form of S-expressions
    adapted to build expressions.
  - a staging mechanism, i.e. a sequence of actions to be eventually
    performed, i.e staged for eventual execution, to produce a
    derivation, i.e. an item in the store. Gexp can expand paths in
    the store.
  - utilisées pour reporter l'exécution du code de construction des
    paquets, mais aussi du code du système ou de Guix Home (en gros à
    chaque fois qu'on génère du code pour une exécution ultérieure)
  - Deploy scripts using G-expressions
    https://systemreboot.net/post/deploy-scripts-using-g-expressions
  - can contain a package record or any file-like object which will be
    replaced by its '/gnu/'
  - provides a way to handle the representation of things that can end up as
    store items.
  - can contain a package record or any other "file-like object" and,
    when that 'gexp' is serialized for eventual execution, the package
    is replaced by its /gnu/store/... file name.
  - derivation represents a sequence of low-level build actions and the
    environment in which they are performed to produce an item in the store

  Syntactic forms, all of them are "in the context of the store content":
  | #~     | gexp            | quasiquote; like `                                              |
  | #$     | ungexp          | unquote; like ,                                                 |
  | #+     |                 | same role as #$, but it's a reference to a native package build |
  | #$@"@" | ungexp-splicing | unquote-splicing / splice; like ,@"@"                           |

}

@block{@block-name{Derivations}
  (;; record <derivation-output>
   ;; guix build spacemacs-rolling-release / (derivation-outputs srr-drv)
   ;; The output is empty...
   [("out","/gnu/store/rsqn0wagdp3w9wqcj1amz2pyw97zbqqy-spacemacs-rolling-release-0.999.0-0.e3c1450","","")],
   ;; record <derivation-inputs> - list of all store items used in the build
   ;; which are themselves built using (other) derivations
   ;; (derivation-inputs srr-drv)
   [("/gnu/store/jy1ky9nmij2n1zy88vpch9q1rlkb549g-guile-3.0.7.drv",["out"]),
    ("/gnu/store/m948fy0whx7sxkdjq2mvsqxq8i38askz-module-import-compiled.drv",["out"]),
    ("/gnu/store/q45wxxjmgr06c1l9jif8x5d420hidm5k-xz-5.2.5.drv",["out"]),
    ("/gnu/store/vq4xgmn4sshyqzylpk6hl9hwy1r51g4j-tar-1.34.drv",["out"])],
   ;; derivation-sources - list of all store items used in the build which
   ;; aren't themselves built using derivations
   [
    ;; build script that realises the store items when run
    "/gnu/store/1vb06z21l8vclym6kksb59438qbyvda3-spacemacs-rolling-release-0.999.0-0.e3c1450-builder",
    ;; path to a directory containing extra modules to add to the build script's
    ;; %load-path
    "/gnu/store/pgj8653w17hsapbd1srlvd44rlnhbx8n-module-import",
    ;; ... however the input contains the code I need
    "/gnu/store/qxiz6limpd1k05n000pkpds9ipz5ixqf-spacemacs-rolling-release-0.999.0-0.e3c1450"],
   ;; (derivation-system srr-drv)
   "x86_64-linux",
   ;; (derivation-builder srr-drv) / derivation-builder - the guile executable
   ;; that runs the build script
   "/gnu/store/1kws5vkl0glvpxg7arabsv6q9vazp0hx-guile-3.0.7/bin/guile",
   ;; (derivation-builder-arguments srr-drv) / arguments to pass to
   ;; derivation-builder (above)
   ["--no-auto-compile","-L","/gnu/store/pgj8653w17hsapbd1srlvd44rlnhbx8n-module-import","-C","/gnu/store/y71fry67iylixwas5gjaxddq1yb6skq9-module-import-compiled","/gnu/store/1vb06z21l8vclym6kksb59438qbyvda3-spacemacs-rolling-release-0.999.0-0.e3c1450-builder"],
   ;; (derivation-builder-environment-vars srr-drv)
   [("out","/gnu/store/rsqn0wagdp3w9wqcj1amz2pyw97zbqqy-spacemacs-rolling-release-0.999.0-0.e3c1450")])
}

@block{@block-name{Analyzing package structure / record}
  (use-modules
    ;; specification->package
    (gnu packages)
    ;; package? package-name etc
    (guix packages))
  ;;
  (define p (specification->package "emacs"))
  (record? p)
  (define rtd (record-type-descriptor p))
  (record-accessor rtd 'name)
  (package? p)
  (package-name p)
}

@block{@block-name{Nix to Guix}
  Functional Package Management with Guix; Ludovic Courtès, 2013
  https://arxiv.org/pdf/1305.4584.pdf
  Like Guix and Nix, Vesta is a purely functional build system [11]

  Scsh provides a complete interface to substitute Scheme in "shell programming"
  tasks [18].

  Nickel is an evolution of the Nix language, while trying to overcome some of
  its limitations.
  https://github.com/tweag/nickel
  Nickel grammar for tree-sitter.
  https://github.com/nickel-lang/tree-sitter-nickel

  https://nixos.wiki/wiki/Packaging/Tutorial

  nix-service-type
  runs build daemon of the Nix package manager. Which 'would allow you to use
  Nix's Firefox package'

  Example how to use it:
  (use-modules (gnu))
  (use-service-modules nix)
  (use-package-modules package-management)
  ;;
  (operating-system
    ;; ...
    (packages (append (list nix)
                      %base-packages))
    (services (append (list (service nix-service-type))
                      %base-services)))
  After `guix system reconfigure` configure Nix for your user:
  Add a Nix channel and update it. See Nix Package Manager Guide.

  Tree-sitter
  An incremental parsing system for programming tools
  https://github.com/tree-sitter/tree-sitter

  TODO ChatGPT search for
  - parsing the Nix DSL (racket FTW?)

  Declaratively yours: Composing system abstractions with GNU Guix
  https://archive.fosdem.org/2021/schedule/event/gnuguix/

  generate environment modules, as commonly used in high-performance computing
  (HPC) on shared clusters
  https://gitlab.inria.fr/guix-hpc/guix-modules
  `guix module create` ???

  racket2nix generate a Nix derivation for your Racket package
  Take an info.rkt file, produce a info.nix file.
  https://github.com/fractalide/racket2nix
}

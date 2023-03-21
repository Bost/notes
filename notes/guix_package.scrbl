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
  guix package iproute2 # provides the `ss` socket

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

  # export the list installed packages
  guix package --export-manifest > manifest.scm
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
  git clone http://example.org/foo.git
  cd foo
  git checkout <tag-or-branch>
  # https://guix.gnu.org/manual/en/html_node/Invoking-guix-hash.html
  guix hash -x --serializer=nar .   # with the dot at the end!

  # sha256; base32; or (A):
  # package is a file downloaded from an the URI - add it to the store, and
  # print both its file name in the store and its SHA256 hash
  guix download URI
  # compute the hash of any (already downloaded) file
  guix hash file

  # either (A):
  guix build --load-path=./gnu/packages --keep-failed <package>
  cd $GUIX_CHECKOUT
  guix shell --development guix         # -D, --development
  ./pre-inst-env guix build --keep-failed <package>@"@"<version>
  ./pre-inst-env guix install <package>@"@"<version>
  # or (B):
  guix package --install-from-file=my-hello.scm
  # or (C):
  guix package --load-path=./ --manifest=./games/packages/factorio.scm --list-available=factorio
  guix package --load-path=./ --install=factorio

  cd $dev/guix-packages/packages
  # guix package --load-path=./ --manifest=./bost/packages/spacemacs.scm --list-available=emacs-spacemacs
  # guix build --load-path=./ --keep-failed --load=./spacemacs-settings.scm emacs-spacemacs
  # guix build --load-path=./ --keep-failed --expression='(@"@" (gnu) %base-packages)' emacs-spacemacs
  set --export GUIX_PACKAGE_PATH (pwd)/bost/packages/patches
  guix build --load-path=./ --keep-failed emacs-spacemacs

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

  guix graph --type=reverse-package
  guix graph --path
}

@block{@block-name{Package Inputs / Outputs}
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
  * native
    Built for host architecture (the build machine), e.g. built-time utils not
    needed during run-time
  * propagated
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
  Sequentially executed list; Some build phases can provide
  implicit inputs

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

@block{@block-name{G-expressions - gexp}
  - a way of staging code to run as a derivation.
  - can expand paths in the store and act similar to backquote and comma for
    list expansion - but use '#~' and '#$' instead. It can be used to
    generate derivations.
  - a form of S-expression adapted to build expressions. It can contain a
    package record or any file-like object which will be replaced by its '/gnu/'
  - provides a way to handle the representation of things that can end up as
    store items.
  - can contain a package record or any other "file-like object" and, when that
    'gexp' is serialized for eventual execution, the package is replaced by its
    /gnu/store/... file name.
  - derivation represents a sequence of low-level build actions and the
    environment in which they are performed to produce an item in the store

  Syntactic forms:
  | #~     | gexp            | quasiquote                                                      |
  | #$     | ungexp          | unquote                                                         |
  | #+     |                 | same role as #$, but it's a reference to a native package build |
  | #$@"@" | ungexp-splicing | unquote-splicing / splice                                       |

}

@block{@block-name{Derivatinons}
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

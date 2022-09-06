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
}

@block{@block-name{Various commands}
  --verbosity=LEVEL
  0 - no output
  1 - quiet
  2 - like 1 with download URLs
  3 - shows all the build log output on standard error

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

@block{@block-name{Creating package}
  https://guix.gnu.org/cookbook/en/guix-cookbook.html#Packaging-Tutorial

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
  guix shell -D guix
  ./pre-inst-env guix build --keep-failed <package>@"@"<version>
  ./pre-inst-env guix install <package>@"@"<version>
  # or (B):
  guix package --install-from-file=my-hello.scm
  # or (C):
  guix package --load-path=./ --manifest=./games/packages/factorio.scm --list-available=factorio
  guix package --load-path=./ --install=factorio
}

@block{@block-name{Garbage collection}
  # delete generations older than 45 days and free space until at least 10 GiB
  # are available
  guix gc --delete-generations=45d --free-space=10GiB

  # delete generations older than 2 months days and collect at least 10 GiB of
  # garbage
  guix gc --delete-generations=2m  --collect-garbage=10GiB
}

@block{@block-name{Package Inputs / Outputs}
  A package can have multiple "Outputs" that serves as a mean to separate
  various component of a program (libraries, extra tools, documentation, etc.).

  Inputs ~ package dependencies.
  The user profile & environment only contains the packages the user explicitly
  installed and not necessarily the dependencies of those packages.
  * (basic) inputs
    Built for target architecture. Can be referenced by the package. Will be in
    the resulting binary file
  * native
    Built for host architecture (the build machine), e.g. built-time utils not
    needed during run-time
  * propagated
    Will be added to profile along with the package
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

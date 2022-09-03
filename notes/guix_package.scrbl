#lang notes

@block{@block-name{Package Transformation Options}
  [[https://guix.gnu.org/manual/en/html_node/Package-Transformation-Options.html][Package Transformation Options]]
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

  bag - as an intermediate form between package and derivation.

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

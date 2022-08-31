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

  [[https://logs.guix.gnu.org/guix/2021-10-30.log#111758][IRC discussion: How to find out from which package a binary comes from?]]
  guix package --search=hello
  guix           search hello
  # package details
  guix package --show=hello
  guix           show hello
  guix weather   # see also availability of substitutes:
  guix archive   # export / import package(s) from / to the store

  https://guix.gnu.org/manual/en/html_node/Invoking-guix-weather.html
  `guix weather` nars: size of the compressed archives
}

@block{@block-name{Substitutes}
  https://guix.gnu.org/manual/en/html_node/Substitutes.html
  TODO what is 'substitutes' - update 'guix help wheather'

  Substitute can be anything resulting from a derivation build. Typically
  pre-built package binaries. Can be also e.g. source tarballs.
}

@block{@block-name{Creating package}
  https://guix.gnu.org/cookbook/en/guix-cookbook.html#Packaging-Tutorial

  bag - as an intermediate form between package and derivation.

  # download a file from the URI, add it to the store, and print both its file
  # name in the store and its SHA256 hash
  guix download URI
  # compute the hash of any (already downloaded) file
  guix hash file

  #
  guix build --load-path=./gnu/packages --keep-failed <package>
  cd $GUIX_CHECKOUT
  guix shell -D guix
  ./pre-inst-env guix build --keep-failed <package>@"@"<version>
  ./pre-inst-env guix install <package>@"@"<version>
  # or
  guix package --install-from-file=my-hello.scm
  # or
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

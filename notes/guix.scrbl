#lang notes

@block{@block-name{Various}
  # if `guix pull` show a warning:
  #   channel '...' lacks 'introduction' field but '.guix-authorizations' found
  # then make sure the channel definition contains:
  # (introduction (make-channel-introduction "..." (openpgp-fingerprint "...")))

  # error: guile: warning: failed to install locale
  # solution:
  guix install glibc-locales

  Guix: A most advanced operating system
  https://ambrevar.xyz/guix-advance/
  https://notabug.org/Ambrevar/dotfiles/src/master/.config/guix

  Ludovic Courtès: Your Distro Is A Scheme Library
  https://youtu.be/CdixrlQzAN8

  https://git.sr.ht/~technomancy/

  wget https://ftp.gnu.org/gnu/guix/guix-system-install-1.3.0.x86_64-linux.iso
  wget https://ftp.gnu.org/gnu/guix/guix-system-install-1.3.0.x86_64-linux.iso.sig
  # If encountered "gpg: Can't check signature: No public key" then import the signatures:
  #   wget 'https://sv.gnu.org/people/viewgpg.php?user_id=127547' -qO - | gpg --import -
  #   wget 'https://sv.gnu.org/people/viewgpg.php?user_id=15145'  -qO - | gpg --import -
  gpg --verify guix-system-install-1.3.0.x86_64-linux.iso.sig
  # where the warning like "This key is not certified with a trusted signature!"
  # can be ignored.

  https://guix.gnu.org/guix-refcard.pdf
  https://gitlab.com/pjotrp/guix-notes
  https://github.com/pjotrp/guix-notes/blob/master/HACKING.org
  # Brainiarc7/guix-notes is 1 commit ahead, 279 commits behind pjotrp:master
  https://github.com/Brainiarc7/guix-notes/blob/master/HACKING.org
  # spoelstraethan/guix-notes is 6 commits ahead, 6 commits behind pjotrp:master
  https://github.com/spoelstraethan/guix-notes/blob/master/HACKING.org

  https://systemcrafters.cc/craft-your-system-with-guix/
  https://systemcrafters.cc/craft-your-system-with-guix/full-system-install/
  https://guix.gnu.org/en/manual/devel/en/guix.html#Running-Guix-Before-It-Is-Installed

  # Build an environment with PACKAGE-dependencies, and execute there the
  # COMMAND or an interactive shell in that environment
  guix shell [OPTION]... PACKAGE... [-- COMMAND...]

  guix shell --search-paths    # display needed environment variable definitions

  # Building / Compiling Guix:
  # https://www.rohleder.de/~mike/guix-workflow/guix-workflow.html
  # on error: 'configure: error: C compiler cannot create executables'
  guix install gcc-toolchain
  cd ~/dev/guix/
  git fetch --tags origin && git rebase
  # on error:
  #   fatal: unable to access 'https://git.savannah.gnu.org/git/guix.git/':
  #   server certificate verification failed. CAfile: none CRLfile: none
  # do:
  #   guix install nss-certs
  #   export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
  #   export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
  #   export GIT_SSL_CAINFO="$SSL_CERT_FILE"
  #
  guix shell --development guix help2man git strace --pure
  jobs=$[$(nproc) * 95 / 100] # use 95% of the available CPU cores
  # --check            check if the shell clobbers environment variables
  # --pure             unset existing environment variables
  # Alternatives:
  #   guix shell -D guix --pure
  #   guix shell --development guix --pure
  #   guix shell -D guix help2man git strace --pure
  ./bootstrap
  ./configure --localstatedir=/var
  # on error:
  #   configure: error: 'guild' binary not found; please check your Guile installation.
  # do:
  #   ./configure GUILE=$(which guile) --localstatedir=/var
  make --jobs=$jobs  # first run takes a couple of minutes
  # make --jobs=$jobs check  # optional
  # authenticate all the commits included in your checkout by running:
  make --jobs=$jobs authenticate
  make --jobs=$jobs clean-go # delete the .go (Guile Object) files
  ./pre-inst-env guix home --fallback -L $dotf/guix/home/ container $dotf/guix/home/home-configuration.scm
}

@block{@block-name{Chris Baines / GNU Guix Presentation}
  https://www.cbaines.net/projects/guix/freenode-live-2017/presentation/#/
  2004 Nix announced
  2012 Guix announced

  GNU Guix - GNU+Linux distribution, with declarative configuration for the
             system and services

  ldd (which cat)
  #  linux-vdso.so.1 (0x00007ffd5a35e000)
  #  libc.so.6 => /gnu/store/5h2w4qi9hk1qzzgi1w83220ydslinr4s-glibc-2.33/lib/libc.so.6 (0x00007f0fad11e000)
  #  /gnu/store/5h2w4qi9hk1qzzgi1w83220ydslinr4s-glibc-2.33/lib/ld-linux-x86-64.so.2 (0x00007f0fad2e2000)
}

@block{@block-name{Channels}
  Specifying Additional Channels
  https://guix.gnu.org/manual/en/html_node/Specifying-Additional-Channels.html
  Edit `~/.config/guix/channels.scm` and run `guix pull`. The result in
  `~/.config/guix/current` is the union of Guix and the
  `~/.config/guix/channels.scm`

  Inferiors - fetch a package from a previous guix revision:
  https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html

  # Run command-line scripts provided by GNU Guile and related programs.
  guild
  guild disassemble         # Disassemble a compiled .go file.

  # Hall is a command-line application and a set of Guile libraries that allow
  # you to quickly create and publish Guile projects. It allows you to
  # transparently support the GNU build system, manage a project hierarchy &
  # provides tight coupling to Guix.
  guile-hall

  guix describe
  guix describe --list-formats
  # Display information about the channels currently in use.
  guix describe --format=channels
  guix describe --format=human
}

@block{@block-name{Guix in a VM: SSH access}
  Guix in a VM: SSH access
  https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html
  # edit the /run/current-system/configuration.scm
  (service openssh-service-type
    (openssh-configuration
    (permit-root-login 'without-password)))

  sudo guix system reconfigure /path/to/configuration.scm
  ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p 10022 guest@"@"localhost

  guix deploy /path/to/some/file.scm
}

@block{@block-name{Contributing & sending patches}
  Documentation changes:
  # compile...
  make doc/guix.info
  # ...and view it with the info viewer:
  info -f doc/guix.info

  Guix Documentation source code
  https://git.savannah.gnu.org/cgit/guix.git/tree/doc/guix.texi
  Submitting Patches
  https://guix.gnu.org/manual/en/html_node/Submitting-Patches.html

  git format-patch origin
  git send-email --to=guix-patches@"@"gnu.org *.patch
  info "(guix)Submitting Patches"
}

@block{@block-name{QEMU shrink disk size - doesn't work}
  https://pve.proxmox.com/wiki/Shrink_Qcow2_Disk_Files

  dd if=/dev/zero of=mytempfile
  # that could take a some time
  sync
  rm -f mytempfile

  cp guix-system-vm-image-1.3.0.x86_64-linux.qcow2 guix-system-vm-image-1.3.0.x86_64-linux.qcow2.backup
  qemu-img convert -O qcow2 guix-system-vm-image-1.3.0.x86_64-linux.qcow2.backup guix-system-vm-image-1.3.0.x86_64-linux.qcow2
}

@block{@block-name{Guile Script portability across Linux & Guix machines}
  <leoprikler>bost: nope, only reliable shebang still is #!/bin/sh
  2021-08-22: IRC #guix channel log
  https://logs.guix.gnu.org/guix/2021-08-22.log#115020
}

@block{@block-name{Local repository clone}
 <muradm>if one ever did guix pull, gnu/system/install.scm can be found in
         ~/.cache/guix/checkouts/....../gnu/system/install.scm
 <muradm>no need to clone guix again
  2021-08-22: IRC #guix channel log
  https://logs.guix.gnu.org/guix/2021-08-22.log#181402

  set latest (ls --sort=time --almost-all ~/.cache/guix/checkouts/ | head -1)
  cd ~/.cache/guix/checkouts/$latest
}

@block{@block-name{Org mode, Pdf, LaTex}
  - `guix install texlive-bin` is not enough. It leads to: "I can't find the
    format file `pdflatex.fmt'!" where `sudo texconfig rehash` doesn't help.
    See http://tex.stackexchange.com/questions/64894/ddg#64895
  - Installing everything with `guix install texlive` (2.6 GB)
  - viewing pdf in the spacemacs:
  1. install pdf layer
  2. `guix install gwl` for the `autoreconf` program
  3. TODO fix `pdf-info-epdfinfo-program is not executable`

 M-x org-odt-export-to-odt needs
 guix install zip
 M-x org-roam-graph needs
 guix install graphviz
}

@block{@block-name{Main commands}
  guix deploy        # deploy operating systems on a set of machines
  guix describe      # describe the channel revisions currently used
  guix gc            # invoke the garbage collector
  guix home          # build and deploy home environments
  guix install       # install packages
  # also a package may be split into different outputs, and the dig is in the
  # 'utils' output
  guix install bind:utils
  guix package       # manage packages and profiles
  guix pull          # pull the latest revision of Guix
  guix remove        # remove installed packages
  guix search        # search for packages
  guix show          # show information about packages
  guix system        # build and deploy full operating systems
  guix time-machine  # run commands from a different revision
  guix upgrade       # upgrade packages to their latest version
  guix weather       # report on the availability of pre-built package binaries
}

@block{@block-name{Software development commands}
  guix container    # process isolation / run code in 'guix shell -C' containers
  guix pack         # create application bundles
  guix shell        # spawn one-off software environments
}

@block{@block-name{Packaging commands}
  guix build      # build packages or derivations without installing them
  guix challenge  # challenge substitute servers, comparing their binaries
  guix download   # download a file to the store and print its hash
  guix edit       # view and edit package definitions
  guix graph      # visualize, view and query package dependency graphs
  guix hash       # compute the cryptographic hash of a file
  guix import     # import a package definition from an external repository
  guix lint       # find errors and validate package definitions
  guix publish    # share substitutes / publish build results over HTTP
  guix refresh    # update existing package definitions
  guix size       # profile disk usage, i.e. the on-disk size of packages
  guix style      # update the style of package definitions
}

@block{@block-name{Plumbing commands}
  guix archive    # manipulate, export, import normalized archives (nars)
  guix copy       # copy items to and from a remote store over SSH
  guix git        # operate on Git repositories
  guix offload    # set up and operate build offloading
  guix processes  # list client processes / currently running sessions
  guix repl       # interactive programming of Guix in Guile
}

@block{@block-name{Crypthography}
  Pseudo Random Number Generator PRNG and Random Number Generator RNG are only
  as good as their underlying entropy source.

  entropy creation / usage / requirements / amount
  https://www.blackhat.com/docs/us-15/materials/us-15-Potter-Understanding-And-Managing-Entropy-Usage.pdf

  Entropy:
  Quantity of uncertainty of an outcome. Entropy generation - shuffling play
  cards Full entropy is 100% random. Can be measured by tests

  Randomness:
  Quality of uncertainty of an outcome. PRNG - dealing the deck of play cards
  to. Randomness either is or is not.

  The better the card shuffling (entropy), the more random the card deal will
  be(???)

  $ cat /proc/sys/kernel/random/entropy_avail
  256

  OpenSSL PFS - OpenSSL Perfect Forward Security
  National Institute of Standards and Technology NIST
}

@block{@block-name{TODO}
  https://guix.gnu.org/manual/devel/en/html_node/Using-Guix-Interactively.html

  Protesilaos Stavrou
  https://protesilaos.com/emacs/dotemacs

  Auxiliary modules for programming in GNU guile
  https://luis-felipe.gitlab.io/guile-aux/

  guix install ublock-origin-chromium
  Block unwanted content from web sites
  uBlock Origin is a wide spectrum blocker for IceCat and ungoogled-chromium.

  https://guix.gnu.org/manual/devel/en/html_node/Secure-Shell.html
  @lisp{
    (service home-openssh-service-type
      (home-openssh-configuration
        (hosts
          (list (openssh-host (name "ci.guix.gnu.org")
                (user "charlie"))
                (openssh-host (name "chbouib")
                (host-name "chbouib.example.org")
                (user "supercharlie")
                (port 10022))))
                (authorized-keys (list (local-file "alice.pub")))))
  }

  see also private.el
  auth info - gpg secrets
  EasyPG / epa - Emacs’s native support for GnuPG
  https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources

  # Set up secret environment variable
  guix install gnupg
  export SECRET_VAR=`gpg --decrypt /path/to/somekeyfile.gpg 2>/dev/null`
  # see also 1.:
  https://dev.to/heroku/how-to-manage-your-secrets-with-git-crypt-56ih
  # guix install git-crypt

  2. Andrew Tropin Dev Team Secrets with gpg, git and gopass
  https://www.youtube.com/watch?v=EB9cW9RjiSs&list=PLZmotIJq3yOJab8-of7gMYrXkZyAjWPOw&index=48
  gopass:
  Retrieving a password from user terminal or piped input. Improved `pass`,
  wrapper aroung gpg & git
  Deprecated in favor of terminal which is deprecated, too, use
  golang.org/x/term. See
  `guix install go-golang-org-x-term`

  personal password management using gopass
  The encrypted files can be stored in a (separate) github / gitlab repo

  gpg --keyserver keyserver.ubuntu.com --search-keys email@"@"address.com
  gpg --list-keys

  # add a package to the distribution
  guix import

  # Update package definitions to the latest style. `guix style` may not do any
  # change or throw: error: mkstemp: Read-only file system
  # in this case use the ./pre-inst-env :
  cd $GUIX_CHECKOUT
  guix shell --development guix         # -D, --development
  ./pre-inst-env guix style <package>
  # TODO update my own package definitions
  guix style --load-path=/path/to/channel my-package1 my-package2 ...
  guix style --load-path=~/dev/guix-packages ...

  # edit package defition
  guix edit PACKAGE

  # search for existing service type 'console'
  guix system search console

  # search in source code
  rg define-configuration ~/dev/guix/gnu/home/services
  rg "define.*\(operating-system\b" (fd 'scm$' ~/dev/guix/)
  rg "\boperating-system\s" (fd 'scm$' ~/dev/guix/)
}

@block{@block-name{GNUnet}
  Replace the old insecure Internet protocol stack.

  Alternative network stack for building secure, decentralized and
  privacy-preserving distributed applications.
}

@block{@block-name{Flatpack}
  sudo flatpak upgrade

  sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
  sudo flatpak install flathub us.zoom.Zoom
  flatpak run us.zoom.Zoom & disown
  sudo flatpak install flathub org.telegram.desktop
  flatpak run org.telegram.desktop & disown

  sudo flatpak remote-add --if-not-exists flathub https://flathub.org/apps/details/com.discordapp.Discord
  sudo flatpak install flathub com.discordapp.Discord
  flatpak run com.discordapp.Discord & disown
}

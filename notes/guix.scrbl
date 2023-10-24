#lang notes

@block{@block-name{Guix Source Code}
  Let's read the GNU Guix source code
  https://youtu.be/xccqwN7Negw?si=dFfWBcPlOCgbg_CT

  nix/boost/
  Boost.org: free peer-reviewed portable C++ source libraries.

  nix/libstore/
  Some common parts with NoxOS

  m4/
  m4 macro processor
  (L4 is a microkernel familly, seL4 Microkernel Security)

  Guix TODOs
  GNUnet - free and open-source decentralized network with mesh topology
  GNUnet DHT Distributed Hash Table - part of GNUnet
  store & retrieve key-value pairs in a distributed manner across GNUnet network
  nodes.

  libswift
  leverage the capabilities of GNUnet in the Swift programming language.
  provides a high-level abstraction over the GNUnet APIs

  FSD Free Software Directory
  a comprehensive collection of free software

  cow-store
  - Copy-On-Write Store, key component of the Guix package manager
    once a package is built and stored, it cannot be changed.
  - layered structure
  - Copy-On-Write: technique to optimize disk space. Only the changed package
    part are copied and modified. The unmodified parts are shared with the
    original package

  guix/po
  po file extension - internationalization, GNU gettext

  scripts/guix.in
  - ? initialisation ?

  PKI Public Key Infrastructure
  - component of Guix package manager
  - framework of policies, processes, and technologies used to manage and secure
    public key cryptography

  gcrypt
  - known as Libgcrypt
  - cryptographic functionality to applications
  - cryptographic engine behind many software applications. The library handles
  various cryptographic algorithms, such as symmetric and asymmetric encryption,
  hash functions, key generation, and more.

  authentication challenge
  - security measure used to verify the identity of a
    user or device by requesting additional credentials or information.

  gnu system tripplet

  Fluids
  dynamically scoped variables.
  can be accessed throughout the dynamic extent of that scope

  Fluid fields:
  - dynamic, mutable values within records, for more flexible and configurable
    data structures.
  - can be useful in concurrent or multi-threaded environments.

  'Why ice-9' by Noah Lavine
  https://lists.gnu.org/archive/html/guile-devel/2010-07/msg00046.html

  https://inria.hal.science/hal-03604971
  two performance-critical aspects in HPC: message passing (MPI) and CPU
  micro-architecture tuning.
  package multi-versioning, a technique developed for GNU Guix, a tool for
  reproducible software deployment, and show that it allows us to implement CPU
  tuning without compromising on reproducibility and provenance tracking.


  embedded code snippets are staged for eventual execution.
  gexps - staging mechanism
  explain our journey from traditional Lisp S-expressions to G-expressions,


  Oleg Kysielov - monadic programming in scheme
  https://okmij.org/ftp/Scheme/monad-in-Scheme.html

  https://programming-idioms.org

  Procedures in Guile Scheme
  differ from functions in mathematics due to the presence of side effects,
  mutable state, and language-specific features in Guile Scheme procedures.

  language-specific features: control flow constructs (loops, conditionals),
  exception handling, and support for input/output operations, which are not
  present in mathematical functions.

  (file-append shadow "/bin/passwd")

  herd.scm
  hurd.scm

}

@block{@block-name{Various}
  pwclient info <patch-id>
  # download patches sent to https://issues.guix.gnu.org/issue/53765
  https://patches.guix-patches.cbaines.net/project/guix-patches/list/?series=11955
  https://patches.guix-patches.cbaines.net/project/guix-patches/list/?series=12092

  # https://guix.gnu.org/manual/devel/en/guix.html#Keyboard-Layout
  # https://guix.gnu.org/manual/devel/en/guix.html#Keyboard-Layout-1
  # available keyboard layouts
  cat /run/current-system/profile/share/keymaps
  man loadkeys

  (use-modules (srfi srfi-19)#| date->string |#
               (guix swh)    #| Software Heritage |#)
  (define o (lookup-origin "https://github.com/Bost/corona_cases"))
  (define last-visit (car (origin-visits o)))
  (date->string (visit-date last-visit))

  # error: guile: warning: failed to install locale
  # solution:
  guix install glibc-locales

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

  # Building / Compile / Compiling Guix:
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
  guix shell help2man git strace glibc-locales --development guix --pure
  jobs=$[$(nproc) * 95 / 100] # use 95% of the available CPU cores
  # --check            check if the shell clobbers environment variables
  # --pure             unset existing environment variables
  # --development -D   include the development inputs of the next package,
  #                    i.e. 'guix'
  # Using glibc-locales should prevent 'warning: failed to install locale'
  ./bootstrap
  ./configure --localstatedir=/var
  # on error:
  #   configure: error: 'guild' binary not found; please check your Guile installation.
  # do:
  #   ./configure GUILE=$(which guile) --localstatedir=/var
  make --jobs=$jobs  # first run takes a couple of minutes
  # make --jobs=$jobs check  # optional
  # Authenticate all the commits in your checkout by:
  make --jobs=$jobs authenticate GUIX_GIT_KEYRING=keyring
  # This fetches the public key of the issuer of SIG from KEYRING, a keyring as
  # returned by 'get-openpgp-keyring'.

  # make --jobs=$jobs clean-go # delete the .go (Guile Object) files
  ./pre-inst-env guix home --fallback -L $dotf/guix/home/ container $dotf/guix/home/home-configuration.scm
  ./pre-inst-env guix system --cores=$jobs image -t iso9660 gnu/system/install.scm
  rm -rf $HOME/system-images/guix-system.img
  cp /gnu/store/<checksum>-image.iso $HOME/system-images/my-guix-image.iso
  #
  # Exit the guix-shell
  #
  qemu-img create -f qcow2 $HOME/system-images/guix-system.img 50G
  set --local cpuCores 20
  set --local ramMemory 8192
  sudo qemu-system-x86_64 -m $ramMemory -smp $cpuCores -enable-kvm \
      -nic user,model=virtio-net-pci -boot menu=on,order=d \
      -drive file=$HOME/system-images/guix-system.img \
      -drive media=cdrom,file=$HOME/system-images/nonguix-resizable-disk-image
  #
  # Qemu drop to TTY:
  # Use Ctrl + Alt + 2 to switch to the QEMU console.
  # Type sendkey ctrl-alt-f1 and press Enter .
  # Use Ctrl + Alt + 1 to switch back to the virtual system, which should now by at TTY1.
  #
  ## Set up the installation environment using herd - probably not needed?
  # herd start cow-store /mnt
  #
  git clone --depth=1 -b dev https://gitlab.com/rostislav.svoboda/dotfiles
  mkdir -p ~/.config/guix
  cp dotfiles/.config/guix/channels.scm ~/.config/guix
  guix pull
  guix hash

  guix system -L ~/.dotfiles/.config/guix/systems init path/to/config.scm /mnt

  Authenticate Git checkouts (see $dotf/bin/git-authenticate):
  - When guix pull obtains code from Git, it should be able to tell that all the
    commits it fetched were pushed by authorized developers of the project.
  - It requires cryptographically signed commits
  ;;
  guix git authenticate               \
    --keyring=$(GUIX_GIT_KEYRING)     \
    --cache-key=channels/guix --stats \
    --historical-authorizations=/home/bost/dev/.my-guix-authorisations \
    "$(channel_intro_commit)" "$(channel_intro_signer)"
  # --cache-key=path/to/KEY reads file from ~/.cache/guix/authentication/path/to/KEY
  ;;
  By signing a commit, a developer asserts that he/she is the one who made the
  commit as its author, or he/she applied somebody else's changes after review.
  This also requires a notion of authorization: commits must have a valid
  signature, and be signed by an authorized key.
  ;;
  .guix-authorizations:
  File that lists the OpenPGP key fingerprints of authorized committers
  ;;
  A commit is considered authentic if and only if it is signed by one of the
  keys listed in the .guix-authorizations file of each of its parents. This is
  the authorization invariant.

}

@block{@block-name{Guix compared}
  Guix vs. Docker
  https://www.slant.co/versus/1145/5880/~gnu-guix_vs_docker
  Nix vs. Guix
  https://www.slant.co/versus/1143/1145/~nix_vs_gnu-guix
  Main differences between Guix System (previously GuixSD) and NixOS?
  https://unix.stackexchange.com/a/630620
}

@block{@block-name{GNU Guix}
  GNU+Linux distribution, with declarative configuration for the
  system and services

  Pierre Neidhardt's homepage:
  https://web.archive.org/web/20211121152844/https://ambrevar.xyz/articles.html

  Pierre Neidhardt - Guix: A most advanced operating system
  https://web.archive.org/web/20210922000926/https://ambrevar.xyz/guix-advance/index.html

  https://notabug.org/Ambrevar/dotfiles/src/master/.config/guix

  ;; inspect package and bag
  (use-modules (gnu packages maven) (guix) (guix build-system))
  (package-derivation (open-connection) maven)
  (package-name maven)
  (package-inputs maven)
  (package-outputs maven)
  (package->bag maven)

  Ludovic Courtès: Your Distro Is A Scheme Library
  https://youtu.be/CdixrlQzAN8
  $ INSIDE_EMACS=1 guix repl --listen=tcp:37146 &
  # then in Emacs: M-x geiser-connect RET RET RET
  ;; 1. Packages & package lookup
  (use-modules (gnu) (guix) (gnu packages base))
  (package? coreutils)
  (package-name coreutils)
  (specification->package "guile")
  (length (fold-packages cons '()))
  ;;
  ;; 2. The store - store access mediated by daemon
  ,use (guix store)
  (define daemon (open-connection))
  (add-text-to-store daemon "foo.txt" "Hi REPL")
  (valid-path? daemon (add-text-to-store daemon "foo.txt" "Hi REPL"))
  ;; 3. From packages to derivations
  ;; (package-derivation daemon coreutils)
  (use-modules (guix derivations) (bost packages spacemacs))
  (build-derivations daemon (list (package-derivation daemon spacemacs-rolling-release)))
  ;; build-system
  ;; package ----> bag ----+
  ;;                       |
  ;;                       v
  ;;                   derivation
  ;;                       ^
  ;;                       |
  ;; origin ---------------+
  ;;
  ;; 4. Staging: Hosting build-side code
  #~(symlink #$coreutils #$output) ;; output is a derivation-output
  ( #~(symlink #$coreutils #$output)) ;; query the input
  ;; `gexp-inputs` shown in the video is exported by the (guix gexp) module and
  ;; thus not available from the REPL
  (gexp-input #~(symlink #$coreutils #$output))
  ,enter-store-monad
  ;; `gexp->sexp` shown in the video is exported by the (guix gexp) module and
  ;; thus not available from the REPL
  (gexp->derivation "foo" #~(symlink #$coreutils #$output))
  ;;
  ;; Doesn't work> (neither form the store)
  ;; (build-derivations (list (gexp->derivation "foo" #~(symlink #$coreutils #$output))))
  ,q ;; exit the store
  (define os (load "/home/bost/dev/dotfiles/guix/systems/syst-ecke.scm"))
  (operating-system? os)
  ,use (gnu system)  ;; exports operating-system-derivation
  ;; ,use (gnu services) ;; probably not needed
  ,enter-store-monad
  (operating-system-derivation os)

  https://git.sr.ht/~technomancy/

  Chris Baines: GNU Guix Presentation
  https://www.cbaines.net/projects/guix/freenode-live-2017/presentation/#/
  2004 Nix announced
  2012 Guix announced

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
  # commands:
  # --execute  -x  Carry out operations, instead of displaying them
  hall init <project-name> --author="Jim B" --license="gpl3+" --prefix="guile" --execute
  hall build --execute
  hall scan --execute
  hall dist --execute

  guix describe # Display information about the channels currently in use.
  guix describe --list-formats
  guix describe --format=channels
  guix describe --format=human

  # if `guix pull` show a warning:
  #   channel '...' lacks 'introduction' field but '.guix-authorizations' found
  # then make sure the channel definition contains:
  (introduction (make-channel-introduction "..." (openpgp-fingerprint "...")))

}

@block{@block-name{Guix in a virtual machine}
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

  git format-patch origin/master
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
  $dgx/guix/scripts/*.scm
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

  guix install guix-modules
  `guix shell` expects a list of packages upfront. Module interface is more
  "incremental". You can "load" / "unload" modules until you obtain the desired
  environment.
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
  guix archive    # manipulate, export, import nix / normalized archives (nars)
  guix copy       # copy items to and from a remote store over SSH
  guix git        # operate on Git repositories
  guix offload    # set up and operate build offloading
  guix processes  # list client processes / currently running sessions
  guix repl       # interactive programming of Guix in Guile
}

@block{@block-name{Nix / Normalized Archives}
  # When 'guix home: error: corrupt input while restoring archive from'
  # See "nar 404 leads to hard 'guix substitute' crash"
  # https://issues.guix.gnu.org/63634
  #
  # 1. workaround: remove bordeaux.guix.gnu.org from the substitute URLs
  guix home --substitute-urls=URL ...
  # see https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html#daemon_002dsubstitute_002durls
  # or (probably the better solution):
  # 2. clear daemon's the cached narinfos
  sudo mv /var/guix/substitute/cache/ /var/guix/substitute/cache.delete-me
  # repeat
  guix home ...
}

@block{@block-name{TODO}
  Emacs-Guix
  guix-devel-mode mode that indents and highlights Guix code
  properly (see Development in The Emacs-Guix Reference Manual).

  7.5 Channels with Substitutes

  Protesilaos Stavrou
  https://protesilaos.com/emacs/dotemacs

  Auxiliary modules for programming in GNU guile
  https://luis-felipe.gitlab.io/guile-aux/

  # Block unwanted content from web sites
  # uBlock Origin is a wide spectrum blocker for IceCat and ungoogled-chromium.
  guix install ublock-origin-chromium

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

  # PassCmd "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.passwords/gmail.gpg"
  # see also sops-nix, vauld

  # Set up secret environment variable
  guix install gnupg
  export SECRET_VAR=`gpg --decrypt /path/to/somekeyfile.gpg 2>/dev/null`
  # see also 1.:
  https://dev.to/heroku/how-to-manage-your-secrets-with-git-crypt-56ih
  # guix install git-crypt

  2. Andrew Tropin Dev Team Secrets with gpg, git and gopass
  https://www.youtube.com/watch?v=EB9cW9RjiSs&t=533s
  ;;
  gopass:
  - Retrieving a password from user terminal or piped input. Improved `pass`,
  - wrapper aroung gpg & git:
    * git is for storing editing history of passwords
    * gpg is for encryptions
  - password store is a directory containing subdirectories and ectrypted files

  # Deprecated in favor of terminal which is deprecated, too, use
  # golang.org/x/term. See
  guix install go-golang-org-x-term

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
  ./pre-inst-env guix lint <package>
  # TODO update my own package definitions
  guix style --load-path=/path/to/channel my-package1 my-package2 ...
  guix style --load-path=~/dev/guix-packages ...

  # edit package defition
  guix edit PACKAGE

  # search for existing service type 'console'
  guix system search console

  # search through all Guix and Guile source code
  rg define-configuration $dev/guix/gnu/home/services
  rg "define.*\(operating-system\b" (fd 'scm$' ~/dev/guix/)
  rg "\boperating-system\s" (fd 'scm$' ~/dev/guix/)
  rg --no-ignore-vcs -g '*.{scm,c,h}' -w "word\\s" $dgx $dev/guile
}

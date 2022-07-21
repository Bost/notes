#lang notes

#+title: Guix

@block{@block-name{Various}
  wget https://ftp.gnu.org/gnu/guix/guix-system-install-1.3.0.x86_64-linux.iso
  wget https://ftp.gnu.org/gnu/guix/guix-system-install-1.3.0.x86_64-linux.iso.sig
  # If encountered "gpg: Can't check signature: No public key" then import the signatures:
  # wget https://sv.gnu.org/people/viewgpg.php?user_id=127547 -qO - | gpg --import -
  gpg --verify guix-system-install-1.3.0.x86_64-linux.iso.sig

  https://gitlab.com/pjotrp/guix-notes
  https://github.com/pjotrp/guix-notes/blob/master/HACKING.org
  ;; Brainiarc7/guix-notes is 1 commit ahead, 279 commits behind pjotrp:master
  https://github.com/Brainiarc7/guix-notes/blob/master/HACKING.org
  ;; spoelstraethan/guix-notes is 6 commits ahead, 6 commits behind pjotrp:master
  https://github.com/spoelstraethan/guix-notes/blob/master/HACKING.org
  https://systemcrafters.cc/craft-your-system-with-guix/

  https://guix.gnu.org/en/manual/devel/en/guix.html#Running-Guix-Before-It-Is-Installed

  # guix shell --search-paths    # display needed environment variable definitions

  # building Guix
  cd ~/dev/guix/
  # guix shell -D guix --pure
  # guix shell --development guix --pure
  # guix shell -D guix help2man git strace --pure
  guix shell --development guix help2man git strace --pure
  ./bootstrap
  ./configure --localstatedir=/var
  make          # make -j22  # first run takes a couple of minutes
  # make check  # optional
  # authenticate all the commits included in your checkout by running:
  make authenticate
  make clean-go  # make -j22 clean-go # delete the *.go files
  ./pre-inst-env guix home --fallback -L ~/dev/dotfiles/guix/home/ container ~/dev/dotfiles/guix/home/home-configuration.scm
}

@block{@block-name{Chris Baines / GNU Guix Presentation}
  https://www.cbaines.net/projects/guix/freenode-live-2017/presentation/#/
  2004 Nix announced
  2012 Guix announced

  GNU Guix - GNU+Linux distribution, with declarative configuration for the
             system and services
}

@block{@block-name{RDE Reproducible Development Environment}
  Andrew Torpin: guix shell: Overview - Notes
  [https://github.com/abcdw/notes/blob/master/notes/20211111141408-guix_shell_overview.org]

  feature-emacs-eglot ;; lsp-interface for emacs
  feature-clojure     ;;

  # Build an environment with PACKAGE-dependencies, and execute there the
  # COMMAND or an interactive shell in that environment
  guix shell [OPTION]... PACKAGE... [-- COMMAND...]

  @block{@block-name{RDE channel lock / channel freeze}
     [[https://youtu.be/UMCHuHSlVWk?t=1622][YouTube: Andrew Torpin: guix shell: Overview]]
     # full freeze of Guix channels to the versions defined in 'channels.scm'
     guix describe -f channels > ./channels.scm
     guix time-machine --channels=./channels.scm -- shell REST-OF-GUIX-SHELL-ARGS
   }
}

@block{@block-name{Channels}
  [[https://guix.gnu.org/manual/en/html_node/Specifying-Additional-Channels.html][Specifying Additional Channels]]
  edit `~/.config/guix/channels.scm` and run `guix pull`.

  Inferiors - fetch a package from a previous guix revision:
  [[https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html][Inferiors]]

  Meta-commands
  [https://www.gnu.org/software/guile/manual/guile.html#REPL-Commands]
  ;; REPL debugging:
  ;; displays the call stack (aka backtrace) at the point where the debugger was
  ;; entered
  scheme@"@"(guile-user) [1]> ,bt
  ;; change the stackframe
  scheme@"@"(guile-user) [1]> ,up
  scheme@"@"(guile-user) [1]> ,frame 3
  scheme@"@"(guile-user) [1]> ,down
  ;; local variables
  scheme@"@"(guile-user) [1]> ,locals
  ;;  / List procedures provided by the REPL:
  scheme@(guile-user)> ,module (srfi srfi-1)
  scheme@(srfi srfi-1)> ,help module
  ...
  scheme@(srfi srfi-1)> ,binding
  ;; <list of procedures>
  scheme@(guile-user)> ,pretty-print '(eval-when (expand load eval) ...)

  # Run command-line scripts provided by GNU Guile and related programs.
  guild
  guild disassemble         # Disassemble a compiled .go file.

  # Hall is a command-line application and a set of Guile libraries that allow you
  # to quickly create and publish Guile projects. It allows you to transparently
  # support the GNU build system, manage a project hierarchy & provides tight
  # coupling to Guix.
  guile-hall

  guix describe
  guix describe --list-formats
  # Display information about the channels currently in use.
  guix describe --format=channels
  guix describe --format=human

  guix repl --load-path=.
  guix repl << EOF
    ;; it won't work - %default-system-profile is not exported
    ;; (use-modules (guix scripts home))    %default-system-profile
    (use-modules (guix channels))           %default-channels
    (use-modules (gnu system file-systems)) %fuse-control-file-system
    %load-path           ; guile module load-path
    %load-compiled-path
    (%site-dir)
  EOF
}

@block{@block-name{Guix in a VM: SSH access}
  [[https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html][Guix in a VM: SSH access]]
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
  [https://git.savannah.gnu.org/cgit/guix.git/tree/doc/guix.texi]
  Submitting Patches
  [https://guix.gnu.org/manual/en/html_node/Submitting-Patches.html]

  git format-patch origin
  git send-email --to=guix-patches@"@"gnu.org *.patch
  info "(guix)Submitting Patches"

  # sha256; base32: when package definition obtained using git-fetch
  git clone http://example.org/foo.git
  cd foo
  git checkout <tag-or-branch>
  # https://guix.gnu.org/manual/en/html_node/Invoking-guix-hash.html
  guix hash -x --serializer=nar .   # with the dot at the end!
}

@block{@block-name{QEMU shrink disk size - doesn't work}
  [https://pve.proxmox.com/wiki/Shrink_Qcow2_Disk_Files]

  dd if=/dev/zero of=mytempfile
  # that could take a some time
  sync
  rm -f mytempfile

  cp guix-system-vm-image-1.3.0.x86_64-linux.qcow2 guix-system-vm-image-1.3.0.x86_64-linux.qcow2.backup
  qemu-img convert -O qcow2 guix-system-vm-image-1.3.0.x86_64-linux.qcow2.backup guix-system-vm-image-1.3.0.x86_64-linux.qcow2
}

@block{@block-name{Guile Script environment portability across Linux and Guix machines}
  <leoprikler>bost: nope, only reliable shebang still is #!/bin/sh
  [[https://logs.guix.gnu.org/guix/2021-08-22.log#115020][2021-08-22: IRC #guix channel log]]
}

@block{@block-name{Local repository clone}
  <muradm>if one ever did guix pull, gnu/system/install.scm can be found in ~/.cache/guix/checkouts/....../gnu/system/install.scm
  <muradm>no need to clone guix again
  [[https://logs.guix.gnu.org/guix/2021-08-22.log#181402][2021-08-22: IRC #guix channel log]]

  set latest (ls --sort=time --almost-all ~/.cache/guix/checkouts/ | head -1)
  cd ~/.cache/guix/checkouts/$latest
}

@block{@block-name{Org mode, Pdf, LaTex}
  - `guix install texlive-bin` is not enough. It leads to: "I can't find the
    format file `pdflatex.fmt'!" where `sudo texconfig rehash` doesn't help (See
    [[http://tex.stackexchange.com/questions/64894/ddg#64895]])
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

@block{@block-name{GNU GPG}
  https://youtu.be/4-Ks_f8rQFA YouTube: Andrew Tropin - GPG Explained
}

@block{@block-name{main commands}
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

  # Assess substitute availability. report on the availability of pre-built
  # package binaries
  guix weather
}

@block{@block-name{software development commands}
  guix container    # process isolation / run code in 'guix shell -C' containers
  guix pack         # create application bundles
  guix shell        # spawn one-off software environments
}

@block{@block-name{packaging commands}
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

@block{@block-name{plumbing commands}
  guix archive    # manipulate, export, import normalized archives (nars)
  guix copy       # copy items to and from a remote store over SSH
  guix git        # operate on Git repositories
  guix offload    # set up and operate build offloading
  guix processes  # list client processes / currently running sessions
  guix repl       # interactive programming of Guix in Guile
}

@block{@block-name{TODO}
  - `gpg key` instead of `ssh` and `gpg-agent` instead of `ssh-agent`
  - see also private.el
  - auth info - gpg secrets
  [https://anonymousplanet.org/guide.html]

  # Set up secret environment variable
  guix install gnupg
  export SECRET_VAR=`gpg --decrypt /path/to/somekeyfile.gpg 2>/dev/null`

  gpg --keyserver keyserver.ubuntu.com --search-keys email@"@"address.com
  gpg --list-keys

  # add a package to the distribution
  guix import

  # Update package definitions to the latest style
  guix style
  # TODO update my own package definitions
  guix style -L /path/to/channel my-package1 my-package2 ...
  guix style -L ~/dev/guix-packages ...

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
  @block{@block-name{Zoom}
     #+BEGIN_SRC bash :results output
     sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
     sudo flatpak install flathub us.zoom.Zoom
     flatpak run us.zoom.Zoom & disown
     #+END_SRC
  }

  @block{@block-name{Discord}
     #+BEGIN_SRC bash :results output
     sudo flatpak remote-add --if-not-exists flathub https://flathub.org/apps/details/com.discordapp.Discord
     sudo flatpak install flathub com.discordapp.Discord
     flatpak run com.discordapp.Discord & disown
     #+END_SRC
   }
}

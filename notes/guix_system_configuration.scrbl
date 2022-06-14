#lang notes

#+title: Guix System Configuration

@block{@block-name{System Configuration}
  [[https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html][System Configuration]]
  #+BEGIN_SRC fish :results output
    guix system describe
    guix system list-generations

    # instantiate operating system declaration:
    set latest (ls --sort=time --almost-all ~/.cache/guix/checkouts/ | head -1)
    cd ~/.cache/guix/checkouts/$latest
    guix system image -t iso9660 gnu/system/install.scm

    # WARNING: loading compiled file /run/current-system/profile/lib/guile/3.0/site-ccache/guix/i18n.go failed:
    # TODO diff /gnu/store/phynqakkjshw8dnjmx3123k7nv92pqm0-configuration.scm /etc/config.scm
    # try:
    # sudo guix system reconfigure /etc/config.scm
    # instead of:
    # sudo guix system reconfigure /run/current-system/configuration.scm

    # install the `clear` command for clearing / cleaning shell / terminal
    guix install ncurses

    # systemctl status guix-daemon # on a foreign host

    # lists the currently defined services
    sudo herd status

    # `sudo guix system reconfigure configuration.scm` may produce:
    #   guix system: error: symlink: Permission denied: "/var/guix/profiles/system-2-link.new"
    # so try with sudo -E / --preserve-env
    sudo -E guix system reconfigure configuration.scm

    # find operating system declaration:
    find ~/.cache/guix/checkouts -name install.scm

    # create a VM in which the user’s home directory is accessible read-only, and
    # where the ‘/exchange’ directory is a read-write mapping of ‘$HOME/tmp’ on
    # the host and return a script to run that VM
    guix system vm /run/current-system/configuration.scm \
         --expose=$HOME --share=$HOME/tmp=/exchange
    # list scripts for running available / built Guix VMs
    ls --format=single-column /gnu/store/*-run-vm.sh

    guix pull --list-generations=20d
    # list news for last 20 days
    guix pull --news --dry-run --list-generations=20d
    # news from generation range: 200 onwards
    guix pull --news --dry-run --list-generations=200..
    # news for generation range: 200 to 210
    guix pull --news --dry-run --list-generations=200..210

    # list available linux-libre kernels
    ls --format=single-column /gnu/store/*-linux-libre-*/bzImage
    guix show linux-libre | grep version
  #+END_SRC

  @block{@block-name{Services}
  Service is a building block the operating system.
  Service (broadly speaking) extends the functionality of the operating system
  One-shot services stop immediately after their start action has completed.

  @block{@block-name{term-auto}
    - Q: What's the term-auto service? It seems to be my only stopped service
    - A1: "term-auto" is mainly for serial consoles that might need a log-in
      prompt. The regular (VT) ttys use mingetty instead. It's basically always
      stopped - that it could be presented in a less misleading way.
      See [[https://logs.guix.gnu.org/guix/2020-09-23.log#174932][IRC #guix channel log]]
    - A2: There is no ‘term-auto’ service (it's similar to a systemd term@"@"auto
      instance). See [[https://logs.guix.gnu.org/guix/2020-03-23.log#213842][IRC #guix channel log]]
    }
  }
}

@block{@block-name{Time Machine}
  Provides access to other Guix revisions. Run commands from a different revision
  / older version.
  #+BEGIN_SRC fish :results output
    guix time-machine --commit=HEAD   --disable-authentication -- describe
    guix time-machine --commit=<sha1> --disable-authentication -- describe
  #+END_SRC
}

@block{@block-name{Upgrade GuixOS}
  [[https://guix.gnu.org/manual/en/html_node/Upgrading-Guix.html][Upgrade Guix]]

  @block{@block-name{On foreign system (e.g. Ubuntu)}
    #+BEGIN_SRC bash :results output
    sudo --login guix pull
    systemctl restart guix-daemon.service
    #+END_SRC
  }

  @block{@block-name{On Guix System}
    #+BEGIN_SRC bash :results output
    guix pull
    guix upgrade
    sudo guix system reconfigure # /run/current-system/configuration.scm
    # sudo herd restart guix-daemon # is probably not needed
    # see also
    sudo herd status guix-daemon
    #+END_SRC
  }

  [[id:e65e2b2a-062b-49f7-8017-68ec4ef20a5f][Remove Guix from Ubuntu]]
  [[id:69f25a70-c039-488f-9382-91b998b7c0f5][Guix System Configuration]]
}

@block{@block-name{Basic GuixOS system setup with manifest files}
  [[https://guix.gnu.org/cookbook/en/html_node/Basic-setup-with-manifests.html][Basic setup with manifests]]
  #+BEGIN_SRC bash :results output
  # see also guix package --export-manifest
  guix package --list-profiles
  GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
  mkdir -p "$GUIX_EXTRA_PROFILES"/my-project # if it does not exist yets
  guix package --manifest=$HOME/guix-my-project-manifest.scm \
               --profile="$GUIX_EXTRA_PROFILES"/my-project/my-project

  guix package -p "$GUIX_EXTRA_PROFILES"/my-project/my-project --list-installed
  #+END_SRC

  Add to ~/.bashrc (adding to ~/.bash_profile doesn't work):
  #+BEGIN_SRC bash :results output
  GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
  for i in $GUIX_EXTRA_PROFILES/*; do
    profile=$i/$(basename "$i")
    if [ -f "$profile"/etc/profile ]; then
      GUIX_PROFILE="$profile"
      . "$GUIX_PROFILE"/etc/profile
    fi
    unset profile
  done
  #+END_SRC
}

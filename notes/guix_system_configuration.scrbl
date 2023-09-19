#lang notes

@block{@block-name{guix system Command}
  # create a virtual machine in which the user's home directory is accessible
  # read-only, and where the '/exchange' directory is a read-write mapping of
  # '$HOME/tmp' on the host and return a script to run that VM
  guix system vm /run/current-system/configuration.scm \
  --expose=$HOME --share=$HOME/tmp=/exchange
  # list scripts for running available / built Guix VMs
  ls --format=single-column /gnu/store/*-run-vm.sh

  'init'
  Populate the given directory with all the files necessary to run
  the operating system specified in FILE.  This is useful for
  first-time installations of Guix System.  For instance:

  guix system init my-os-config.scm /mnt

  copies to '/mnt' all the store items required by the configuration
  specified in 'my-os-config.scm'.  This includes configuration
  files, packages, and so on.  It also creates other essential files
  needed for the system to operate correctly—e.g., the '/etc',
  '/var', and '/run' directories, and the '/bin/sh' file.

  This command also installs bootloader on the targets specified in
  'my-os-config', unless the '--no-bootloader' option was passed.
}

@block{@block-name{Bootable ISO-9660 installation image}
  # in bash:
  # guix_repo=https://git.savannah.gnu.org/git/guix.git
  # for d in $HOME/.cache/guix/checkouts/*; do
  #     if [ -d "$d" ] && [ ! -L "$d" ]; then
  #         # echo "is dir: $d"
  #         if git --git-dir=$d/.git remote -v | rg --quiet $guix_repo; then
  #             coDir=$d
  #             # echo "Guix channel checkout directory: $coDir"
  #             break
  #         fi
  #     fi
  # done

  set guix_repo https://git.savannah.gnu.org/git/guix.git
  for d in $HOME/.cache/guix/checkouts/*;
      # echo "$d"
      if test -d "$d" && not test -L "$d"
          # echo "is dir: $d"
          git --git-dir=$d/.git remote -v | rg --quiet $guix_repo
          if test $status
              set coDir $d
              printf "Guix channel checkout directory: %s\n" $coDir
              break
          end
      end
  end
  set --local gxCheckout $coDir
  # Instantiate operating system declaration:
  cd $gxCheckout
  # create bootable ISO-9660 installation /gnu/store/...-image.iso
  guix system image -t iso9660 gnu/system/install.scm
  # set --local isoImg /gnu/store/...-image.iso
  #
  # Alternatively download pre-build ISO image:
  set --local url https://ftp.gnu.org/gnu/guix
  # See:
  # 1. https://guix.gnu.org/en/download/latest/
  # 2. https://ci.guix.gnu.org/jobset/images - leads to builds containing
  # '...-image.iso' files for x86_64-linux
  set --local gxVer 1.4.0  # or first 7 chars of the commit id from the git repository
  #
  # Install Guix binaries on top of other OS:
  set --local tarball guix-binary-$gxVer.x86_64-linux.tar.xz
  #
  # Install standalone Guix OS:
  # set --local isoImg guix-system-install-$gxVer.x86_64-linux.iso
  echo \
       wget $url/$isoImg $url/$isoImg.sig
  # View content of the iso image file
  # -t --types, -o --options
  mkdir /tmp/iso && sudo mount -t iso9660 -o loop $isoImg /tmp/iso && ls -la /tmp/iso
  # get the public key and import it:
  #   wget 'https://sv.gnu.org/people/viewgpg.php?user_id=127547' -qO - | gpg --import -
  #
  # note: not sure if the output of pgp can be grep-ed. There might be something
  # about the way gpg outputs text to stdout
  gpg --verify $isoImg.sig    # look for 'Good signature'
  # write the iso image to USB (erase / overwrite its content)
  # '--exclude 7' means 'exclude loop devices' https://askubuntu.com/a/1142405
  lsblk --exclude 7 --nodeps --output MAJ:MIN,PATH,MODEL,TRAN,LABEL,SIZE
  set --local blkDevice /dev/sd<letter><number>
  set --local usbDevice /dev/sd<letter>
  udisksctl unmount --block-device=$blkDevice
  # 'echo sudo ...' for safety
  # bs=BYTES - read and write up to BYTES bytes at a time
  # oflag=sync - use synchronized I/O for data & metadata
  # `... && sync` is probably not needed if the `dd` is used with 'oflag=sync'
  echo \
       sudo dd if=$isoImg of=$usbDevice bs=4M status=progress oflag=sync && sync
}

@block{@block-name{System Configuration}
  System Configuration
  https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html

  TODO see elisp-configuration-service

  ;; analyze system state / show information about the
  guix describe                       # channels currently in use
  guix system describe                # current system (OS kernel, etc.)
  guix home describe                  # home environment generation & channels
  guix package --list-profiles

  ;; According to
  ;;   `info '(dir)Invoking guix package'`
  ;; duration can be only d, w, m (days, weeks, months), however hours
  ;; apparently works for `guix home`, too, however...
  guix home list-generations 1d       # no '=' allowed after 'list-generations'
  ;; ... it doesn't work for `guix pull`. E.g.
  ;; $ date; guix pull --list-generations=1h | grep Generation
  ;; Mon Sep 18 11:08:32 AM CEST 2023
  ;; Generation 699     Sep 18 2023 01:01:58
  ;; Generation 700     Sep 18 2023 09:23:39    (current)
  guix pull --list-generations=1d     # no '(--)describe' parameter exists
  guix package --list-generations=1d  # no '(--)describe' parameter exists
  guix system list-generations 1m     # no '=' allowed after 'list-generations'
  ;; in Emacs
  M-x guix-generations
  M-x guix-last-generations
  M-x guix-generations-by-time

  guix install xdot
  guix system extension-graph /path/to/configuration.scm | xdot -
  guix home extension-graph /path/to/home-configuration.scm | xdot -

  # WARNING: loading compiled file /run/current-system/profile/lib/guile/3.0/site-ccache/guix/i18n.go failed:
  # TODO diff /gnu/store/phynqakkjshw8dnjmx3123k7nv92pqm0-configuration.scm /etc/config.scm
  # try:
  # sudo guix system reconfigure /etc/config.scm
  # instead of:
  # sudo guix system reconfigure /run/current-system/configuration.scm

  # install the `clear` command for clearing / cleaning shell / terminal
  guix install ncurses

  # systemctl status guix-daemon # on a foreign host

  # lists the currently defined GNU Shepherd services
  sudo herd status

  # `sudo guix system reconfigure configuration.scm` may produce:
  #   guix system: error: symlink: Permission denied: "/var/guix/profiles/system-2-link.new"
  # so try with sudo -E / --preserve-env
  sudo -E guix system reconfigure configuration.scm

  # find operating system declaration(s):
  find ~/.cache/guix/checkouts -name install.scm

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

  @block{@block-name{Services}
    Service:
    - building block the operating system.
    - (broadly speaking) extends the functionality of the operating system.
    - can be extensible or non-extensible.
    One-shot services stop immediately after their start action has completed.

    Extensions - connect system services. A service-extension gets the command line
    from the service it is extending.
    Service types - define extension relations.

   (service-extension TARGET COMPUTE DEFAULT-VAL)

    TARGET - "arrow target" in the graph, i.e. name of service which is going to
    be extended.

    COMPUTE - a procedure that, given the parameters of the service, returns a
    list of objects to extend the service of that type.

    DEFAULT-VAL - default value for instances of this service-extension.

    Servive instantiation examples:
      (service guix-service-type   ;; name of the service to start
               ;; initial service
               (guix-configuration
                 (build-accounts 5)
                 (extra-options '("--gc-keep-derivations"))))
      ;;
      (service guix-service-type)  ;; uses the DEFAULT-VAL

    Every '...-service-type' has at least one extension. The only exception is
    the boot service type, which is the ultimate service.

    @block{@block-name{term-auto}
      - Q: What's the term-auto service? It seems to be my only stopped service
      - A1: "term-auto" is mainly for serial consoles that might need a log-in
        prompt. The regular (VT) ttys use mingetty instead. It's basically always
        stopped - that it could be presented in a less misleading way.
        See [[https://logs.guix.gnu.org/guix/2020-09-23.log#174932][IRC #guix channel log]]
      - A2: There is no 'term-auto' service (it's similar to a systemd term@"@"auto
        instance). See [[https://logs.guix.gnu.org/guix/2020-03-23.log#213842][IRC #guix channel log]]
    }
  }
}

@block{@block-name{Various}
  # Provides access to other Guix revisions. Run commands from a different
  # revision / older version
  guix time-machine --commit=HEAD   --disable-authentication -- describe
  guix time-machine --commit=<sha1> --disable-authentication -- describe
  # lock / freeze
  guix describe --format=channels > /tmp/channels.scm
  guix pull --channels=/tmp/channels.scm --allow-downgrades --cores=24 && gxhre --cores=24
  # guix pull --roll-back

  # Upgrade Guix
  # https://guix.gnu.org/manual/en/html_node/Upgrading-Guix.html
  # upgrade GuixOS on a foreign system (e.g. Ubuntu)
  sudo --login guix pull
  systemctl restart guix-daemon.service

  # upgrade GuixOS on a Guix System
  guix pull
  guix upgrade
  sudo guix system reconfigure # /run/current-system/configuration.scm
  # sudo herd restart guix-daemon # is probably not needed
  # see also
  sudo herd status guix-daemon
}

@block{@block-name{Advanced package management}
  Basic setup with manifests
  https://guix.gnu.org/cookbook/en/html_node/Basic-setup-with-manifests.html
  Guix Profiles in Practice
  https://guix.gnu.org/cookbook/en/guix-cookbook.html#Guix-Profiles-in-Practice

  # hint: After setting `PATH', run `hash guix' to make sure your shell refers to `...'.
  # Add to ~/.bash_profile
  GUIX_PROFILE="$HOME/.config/guix/current"
  . "$GUIX_PROFILE/etc/profile"
  # run:
  hash guix
  # to instruct your shell to point to this new guix. It creates
  # '$HOME/.config/guix/current'

  guix package --list-profiles    # prints:
  # $HOME/.config/guix/current
  # $HOME/.guix-profile
  # guix home profile
  $HOME/.guix-home/profile

  # see also guix package --export-manifest
  guix package --list-profiles
  GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
  mkdir -p "$GUIX_EXTRA_PROFILES"/my-project # if it does not exist yets
  guix package --manifest=$HOME/guix-my-project-manifest.scm \
               --profile="$GUIX_EXTRA_PROFILES"/my-project/my-project

  guix package -p "$GUIX_EXTRA_PROFILES"/my-project/my-project --list-installed

  # add to ~/.bashrc (adding to ~/.bash_profile doesn't work):
  GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
  for i in $GUIX_EXTRA_PROFILES/*; do
    profile=$i/$(basename "$i")
    if [ -f "$profile"/etc/profile ]; then
      GUIX_PROFILE="$profile"
      . "$GUIX_PROFILE"/etc/profile
    fi
    unset profile
  done
}

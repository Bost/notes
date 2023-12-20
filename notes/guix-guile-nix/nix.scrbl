#lang notes

@block{@block-name{Nix Source Code}
  Let's read the Nix source code
  https://youtu.be/0tp86yOQ6XY?si=7vk-fjyYJCYqCZln
}

@block{@block-name{Various}
  # configure remote ssh access:
  nix-env --install vim
  sudo vim /etc/nixos/configuration.nix
  # activate
  #   services.openssh.enable = true
  sudo nixos-rebuild switch # user services aren't start/stop automatically. See
  # https://nixos.org/manual/nixos/stable/#sec-changing-config
  # Optionally, for login without using password:
  #    ssh-copy-id ...

  # subscribed channels, essentially pointers to a specific version of nixpkgs.
  nix-channel --list

  # !!!!!!!!
  # Channels are set per user. Running nix-channel --add as a non root user (or
  # without sudo) will not affect configuration in /etc/nixos/configuration.nix
  # !!!!!!!!
  release=23.11 # see nixos-version, https://channels.nixos.org/
  channelName=nixos-$release
  sudo nix-channel --add https://channels.nixos.org/$channelName nixos
  sudo nix-channel --update
  sed "s/22.11/$release/g" /etc/nixos/configuration.nix > /tmp/configuration.$release.nix
  diff /etc/nixos/configuration.nix /tmp/configuration.$release.nix
  # repeat the sed with `sudo ... --in-place ...`
  # sudo sed --in-place "s/22.11/$release/g" /etc/nixos/configuration.nix
  # sudo nixos-rebuild boot --upgrade # probably not needed
  sudo nixos-rebuild switch --upgrade # user services aren't start/stop automatically. See
  # see journalctl --since '5m ago'
  sudo reboot

  # repository with Nix Packages collection
  nixpkgs

  # install package: -i
  nix-env --install nodejs

  # view package definition
  # --query --available
  nix-env -q -a --description nodejs-18.14.1
  nix-env -q -a --json nodejs-18.14.1

  # list available packages
  nix-env -qaP '*' --description

  # warning: name collision in input Nix expressions, skipping '/home/bost/.nix-defexpr/channels_root/nixos'
  # suggestion: remove 'nixos' from either the root channels or the user channels
  $ nix-channel --list
  nixos https://channels.nixos.org/nixos-23.11
  #
  $ sudo nix-channel --list
  [sudo] password for bost: 
  nixos https://channels.nixos.org/nixos-23.11
  #
  $ nix-channel --remove nixos
  uninstalling 'nixos-23.11'

  # Searching for packages in the nix packages collection repository
  nix search nixpkgs <package_name>
  nix --extra-experimental-features nix-command \
      --extra-experimental-features flakes \
      search nixpgks <package_name>

  # Install a package using specific attribute path
  # -A --attr   the arguments are attribute paths. Faster
  nix-env -iA <package_name>
  nix-env --query                    # List installed packages: -q
  nix-env --uninstall <package_name> # Uninstall packages: -e
  nix-env --upgrade                  # Upgrade packages: -u

  # Upgrading Nix
  # On Single-user Nix installation:
  # ??? nixpkgs.nix nixpkgs.cacert are not available on my system ???
  nix-channel --update
  nix-env --install --attr nixpkgs.nix nixpkgs.cacert
  # On Multi-user Nix installation:
  nix-channel --update
  # ??? nixpkgs.nix nixpkgs.cacert are not available on my system ???
  nix-env --install --attr nixpkgs.nix nixpkgs.cacert
  systemctl daemon-reload
  systemctl restart nix-daemon

  # See what installable packages are currently available in the channel:
  nix-env --query --available --attr-path

  # To keep up-to-date with the channel, do:
  nix-channel --update nixpkgs
  nix-env --upgrade '*'

  two basic security models:
  single-user mode:
  there is a single user (typically root) who performs all package management
  operations. Other users cannot perform package management operations.

  multi-user mode:
  all users can perform package management operations - for instance, every user
  can install software without requiring root privileges. Nix ensures that this
  is secure. For instance, it’s not possible for one user to overwrite a package
  used by another user with a Trojan horse.
}

@block{@block-name{NixOS Flakes}
  - like guix time-machine with channels
  https://logs.guix.gnu.org/guix/2023-01-31.log#094922
  See 7.3 Replicating Guix in manual
  https://guix.gnu.org/manual/devel/en/html_node/Replicating-Guix.html
}

@block{@block-name{NixOS Examples}
  $ nix-env -i emacs.yes-no
  installing 'emacs-yes-no-2017-10-01'
  this path will be fetched (0.02 MiB download, 0.07 MiB unpacked):
  /nix/store/wy0p5skg99n4msspjn4wn8p17jf1674n-emacs-yes-no-2017-10-01
  copying path '/nix/store/wy0p5skg99n4msspjn4wn8p17jf1674n-emacs-yes-no-2017-10-01' from 'https://cache.nixos.org'...
  building '/nix/store/d77r8vlp8088izs10w5f3klrnslxc1vk-user-environment.drv'...
  #
  $ nix-env -qaP emacs.yes-no --description
  nixos.emacsPackages.yes-no  emacs-yes-no-2017-10-01  Specify use of `y-or-n-p' or `yes-or-no-p' on a case-by-case basis

  # error: experimental Nix feature 'nix-command' is disabled; use '--extra-experimental-features nix-command' to override
  sudo vim /etc/nixos/configuration.nix
  # add here:
  #   nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # and rebuild the configuration
  sudo nixos-rebuild switch
  #
  # or alternativelly edit the nix.conf:
  #   mkdir -p ~/.config/nix/
  #   vim ~/.config/nix/nix.conf

  nix-env -i emacs.gptel
  cat pkgs/applications/editors/emacs/elisp-packages/recipes-archive-melpa.json
}

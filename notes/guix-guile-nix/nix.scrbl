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

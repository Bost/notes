#lang notes

@block{@block-name{Nix Source Code}
  Let's read the Nix source code
  https://youtu.be/0tp86yOQ6XY?si=7vk-fjyYJCYqCZln
}

@block{@block-name{Various}

  # manipulate or query Nix user environment
  nix-env

  # install package: -i
  nix-env --install nodejs

  #  view package definition
  # --query --available
  nix-env -q -a --description nodejs-18.14.1
  nix-env -q -a --json nodejs-18.14.1
  
  # Searching for packages
  nix search nixpkgs packagename

  # Install a package using specific attribute path
  # -A --attr   the arguments are attribute paths. Faster
  nix-env -iA packagename

  # List installed packages: -q
  nix-env --query

  # Uninstall packages: -e
  nix-env --uninstall packagename

  # Upgrade packages: -u
  nix-env --upgrade

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


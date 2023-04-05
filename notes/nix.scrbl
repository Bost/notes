#lang notes

@block{@block-name{Various}

  # manipulate or query Nix user environment
  nix-env

  # install package
  nix-env -i nodejs

  #  view package definition
  # --query --available
  nix-env -q -a --description nodejs-18.14.1
  nix-env -q -a --json nodejs-18.14.1


  # Searching for packages
  nix search nixpkgs packagename

  # Install a package using specific attribute path
  # -A --attr   the arguments are attribute paths. Faster
  nix-env -iA packagename

  # List installed packages
  nix-env -q

  # Uninstall packages
  nix-env -e packagename

  # Upgrade packages
  nix-env -u

}

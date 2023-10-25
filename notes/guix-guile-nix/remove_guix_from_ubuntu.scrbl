#lang notes

@block{@block-name{Remove / Uninstall / Delete Guix from Ubuntu}
  #+BEGIN_SRC bash :results output
  sudo systemctl stop guix-daemon.service
  # sudo rm /etc/systemd/system/guix-daemon.service
  # sudo mount -o remount,rw /partition/identifier /mount/point
  # mount | column -t | rg /gnu
  sudo rm -rf /gnu
  sudo rm -rf /var/guix/
  sudo rm -rf /var/log/guix/
  sudo rm -rf /etc/guix/
  rm -rf ~/.config/guix
  rm -rf ~/.cache/guix
  rm -rf ~/.guix-profile

  # see https://git.savannah.gnu.org/cgit/guix.git/tree/etc/guix-install.sh
  #+END_SRC
}

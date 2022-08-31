#lang notes

@block{@block-name{Remove Guix from Ubuntu}
  #+BEGIN_SRC bash :results output
  sudo systemctl stop guix-daemon.service
  sudo rm -rf /gnu
  sudo rm -rf /var/guix/
  sudo rm -rf /etc/guix/
  #+END_SRC
}

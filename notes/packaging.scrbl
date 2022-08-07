#lang notes

#+title: Packaging

@block{@block-name{Packaging}
  sudo snap install <package>
  sudo snap help --all
  sudo snap refresh # i.e. update / upgrade

  # install additional ubuntu software
  gnome-software

  # ETL - 1. Extract 2. Transform 3. Load:
  # 1. grab the data in one place
  # 2. modify it,
  # 3. and put it in another place.
  #
  # https://tech.grammarly.com/blog/building-etl-pipelines-with-clojure
  # Stich ETL service https://www.stitchdata.com/
  # https://clojure.org/stories/stitch

  # automatic installation of security (and other) upgrades
  sudo unattended-upgrade -d --dry-run  # -d debug
  sudo unattended-upgrade -d            # -d debug
  sudo unattended-upgrade

  # centos compile git
  sudo yum clean all
  sudo vim /etc/yum.com; proxy=http://<ip:port>
  sudo yum update
  sudo yum groupinstall 'Development Tools'
  sudo yum install openssl-devel curl-devel expat-devel perl-devel asciidoc xmlto

  # centos compile emacs
  sudo yum install texinfo gtk2-devel gnutls-devel libtiff-devel libungif-devel \
       libjpeg-devel libXpm-devel ncurses-devel

  # centos compile guake
  sudo yum install gnome-common GConf2-devel pytgtk2-devel python-vte-devel \
       gnome-python2-gconf python-keybinder pyxdg notify-python


  # :bash list repositories
  sudo grep -rhE ^deb /etc/apt/sources.list*

  # :deb :apt :ppa - only 64bit packages
  deb [arch=amd64] http://...

  # :apt :aptitute :apt-offline
  sudo apt-offline install $HOME/offline-updates
  sudo apt-offline install --allow-unauthenticated $HOME/offline-updates

  # :aptitute
  /etc/apt/apt.conf.d/05proxy
  /etc/apt/apt.conf

  # :apt :aptitude - without proxy
  sudo apt --option Acquire::http::proxy=false ...

  # :dpkg - add-apt-repository needs a single repo
  sudo add-apt-repository ppa:jonathonf/python-3.6
  sudo add-apt-repository ppa:atareao/telegram
  sudo apt update
  sudo apt install telegram python-3.6

  # python setup.py uninstall
  sudo python setup.py install --record files.txt
  sudo xargs rm -rf < files.txt

  # :dpkg :list-ppa
  sudo ppa-purge <ppa:user/ppa-name>

  # withouth "sudo", download source PACKAGE to current directory
  apt source <package>

  # Advanced Package Tool; apt is a replacement for apt-get
  sudo apt install --reinstall <package>

  # When "The following packages have been kept back: ...",
  # see https://askubuntu.com/a/602

  # :ubuntu CLI OS upgrade (GUI upgrade - see `update-manager`)
  # 1. set `Prompt=normal`
  /etc/update-manager/release-upgrades
  Prompt=normal
  #
  # 2. `download package information`; and `install available upgrades`.
  # See also full-upgrade / dist-upgrade / `apt list --upgradable -a`
  sudo apt update && sudo apt upgrade
  #
  # 3. remove / install / upgrade of packages updates and upgrades the OS
  sudo do-release-upgrade # may require: `--devel-release` - see
                          # https://wiki.ubuntu.com/FocalFossa/ReleaseNotes WTF?
  #
  # 4. check for unsupported / unavailable / obsolete packages
  ubuntu-security-status --unavailable

  # :apt :aptitude show installed packages
  dpkg --get-selections

  # :apt :aptitude list of installed files from a packageName (dpkg-query -L works too)
  dpkg -L packageName

  # search the package list for a regex pattern
  # :bash
  apt-cache search ^packageName$
  # :fish
  apt-cache search "^.*google.*\$"

  # list the names of all packages in the system
  apt-cache pkgnames <packagePrefix>
  dpkg --status <package>
  dpkg -s <package>

  sudo dpkg --install <package.deb>
  sudo dpkg --remove  <package.deb>

  # list all installed packages matching regex
  dpkg --list | grep ii | grep -i <regex>
  # package description
  apt-cache show <package>
  aptitude show <package>
  # fix the 'Hash sum mismatch error'
  sudo rm -rf /var/lib/apt/lists
  sudo mkdir -p /var/lib/apt/lists/partial
  sudo apt clean

  # :ubuntu :apt dpkg: mirror: distro: Software Sources List
  # see y-ppa-manager, http://repogen.simplylinux.ch/
  software-properties-gtk
  # ... or edit the list of sources
  /etc/apt/sources.list.d

  # list installed packages; no sudo needed
  # TODO see --clear-selection --set-selection
  dpkg --get-selections | grep -v deinstall

  # :aptitude list expressly installed packages (not just installed as
  # dependencies)
  aptitude search '~i!~M'

  # Fix 'Could not get lock / Unable to lock the administration directory'
  # https://dmorgan.info/posts/linux-lock-files/
  # First method:
  ps aux | grep -i apt
  # if anything found then
  sudo killall apt apt-get
  # Second method:
  # 1. get the process ID of the process holding the lock files:
  lsof /var/lib/dpkg/lock
  lsof /var/lib/apt/lists/lock
  lsof /var/cache/apt/archives/lock
  lsof /var/lib/dpkg/lock-frontend
  # 2. kill any the processes returned by the lsof's above:
  sudo kill -9 <PID>
  # 3. safely remove the lock files:
  sudo rm /var/lib/apt/lists/lock
  sudo rm /var/cache/apt/archives/lock
  sudo rm /var/lib/dpkg/lock
  # 3. reconfigure the packages:
  # -a or --pending  all unpacked but unconfigured packages are configured
  sudo dpkg --configure -a

  # select fastest / best ubuntu mirror
  sudo pip3 install apt-select
  sudo cp /etc/apt/sources.list /etc/apt/sources.list.backup && \
  apt-select && sudo mv sources.list /etc/apt/
}

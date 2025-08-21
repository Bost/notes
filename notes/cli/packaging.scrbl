#lang notes

@block{@block-name{flatpak}
  # offers a sandbox to run applications in isolation

  sudo flatpak upgrade --assumeyes  # -y  answer yes for all questions

  sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
  sudo flatpak install flathub us.zoom.Zoom
  flatpak run us.zoom.Zoom & disown
  sudo flatpak install flathub org.telegram.desktop
  flatpak run org.telegram.desktop & disown

  sudo flatpak remote-add --if-not-exists flathub https://flathub.org/apps/details/com.discordapp.Discord
  sudo flatpak install flathub com.discordapp.Discord
  flatpak run com.discordapp.Discord & disown

  # grant access permissions to a file / directory
  sudo flatpak override org.telegram.desktop --filesystem=~/Downloads
  sudo flatpak override org.telegram.desktop --nofilesystem=/some/path/here

  # installation directory: /var/lib/flatpak/app

  https://gitlab.com/flatpak-repo/com-ankama-Dofus-flatpak
  https://github.com/azert9/ankama-launcher-flatpak

  # build / install / remove flatpak applications from cli
  https://gist.github.com/user5145/9aecebaa8045174958123c9798c93009
}

@block{@block-name{snap}
  # Packaging and deployment system developed by Canonical for OSes with Linux
  # kernel and the systemd.
  # Compare Snap vs Apt
  # https://www.baeldung.com/linux/snap-vs-apt-package-management-system

  sudo snap install <package>
  sudo snap help --all
  sudo snap refresh # i.e. update / upgrade

  sudo snap refresh snap-store
  # error: cannot refresh "snap-store": snap "snap-store" has running apps (snap-store), pids: <...>
  kill <...>
  # or:
  ps -e | grep snap-store #id blocking process
  kill #id
  # sudo snap refresh snap-store
  sudo snap refresh

  # snap: access to a file / directory
  # When: Error initializing settings: Failed saving settings file:
  # - Error: Unable to open settings file /path/to/.bitcoin/settings.json for writing
  sudo snap connect bitcoin-core:removable-media
  # also
  # `snap interfaces` deprecated by `snap connections
  snap connections bitcoin-core
  # start /stop the daemon
  bitcoin-core.daemon -datadir=$HOME/.bitcoin -daemon
  bitcoin-core.cli    -datadir=$HOME/.bitcoin stop

  # `dofus` on Ubuntu may throw:
  # /snap/dofus/3/usr/share/anakama-launcher/zaap: error while loading shared
  # libraries: libffmpeg.so: cannot open shared object file: No such file or
  # directory.
  # snap package definition:
  ldd -v /snap/dofus/3/usr/share/anakama-launcher/zaap | rg libffmpeg.so
  #
  # protontricks - wine for steam
  # Proton - Patched version of Wine, i.e. compatibility layer for Windows games
  sudo apt install --yes protontricks winetricks
  https://dl.winehq.org/wine/source/
  #
  cat /snap/dofus/3/snap/manifest.yaml
  wget https://launcher.cdn.ankama.com/installers/production/Dofus-Setup-x86_64.AppImage \
       -O Dofus-Setup-x86_64.AppImage
  chmod +x Dofus-Setup-x86_64.AppImage
  ./Dofus-Setup-x86_64.AppImage --appimage-extract
  rm -f Dofus-Setup-x86_64.AppImage
  #
  # https://forum.snapcraft.io/t/error-while-loading-shared-libraries/18997/5
  SNAPD_DEBUG=1 SNAP_CONFINE_DEBUG=1 snap run dofus
  # sudo /usr/lib/snapd/snap-discard-ns dofus
  #
  sudo find /snap -name libffmpeg.so
  snap run --shell dofus
  snap run --debug-log dofus
  # setting, i.e. exporting SNAP_LIBRARY_PATH LD_LIBRARY_PATH doesn't help
  # snap is running in some kind of a container.
  echo $SNAP_LIBRARY_PATH
  echo $LD_LIBRARY_PATH
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(dirname /path/to/libffmpeg.so)
  export SNAP_LIBRARY_PATH=$SNAP_LIBRARY_PATH:$(dirname /path/to/libffmpeg.so)
  unset LD_LIBRARY_PATH
  unset SNAP_LIBRARY_PATH
  #
  # following installs some libraries - may or may not not help
  sudo apt install libnss3-dev libgdk-pixbuf2.0-dev libgtk-3-dev libxss-dev
  #
  sudo apt install --yes plocate
  locate libffmpeg.so
  #
  sudo apt install --yes apt-file
  sudo apt-file update
  apt-file search libffmpeg.so
  #
  snap info ffmpeg
  apt policy ffmpeg
  #
  mkdir ~/ffmpeg
  cp /path/to/libffmpeg.so ~/ffmpeg/libffmpeg.so.<postfix>
  chmod +x ~/ffmpeg/*
  # following doesn't help snap is running in some kind of a container.
  sudo rm -f /lib/x86_64-linux-gnu/libffmpeg.so && \
  sudo ln -s ~/ffmpeg/libffmpeg.so.<postfix> /lib/x86_64-linux-gnu/libffmpeg.so
}

@block{@block-name{Various}
  # Run a program with namespaces of other processes.
  nsenter

  # Compare `apt upgrade` vs `apt dist-upgrade` vs `apt full-upgrade`
  apt upgrade      # upgrade packages to their latest versions w/o removing or
                   # adding new packages. Suitable for routine package updates.
  apt full-upgrade # can handle complex package dependencies; for major system
                   # upgrades or significant package changes.
  apt dist-upgrade # alias for full-upgrade

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
  sudo yum install openssl-devel curl-devel expat-devel perl-devel asciidoc \
                   xmlto

  # centos compile emacs
  sudo yum install texinfo gtk2-devel gnutls-devel libtiff-devel \
                   libungif-devel libjpeg-devel libXpm-devel ncurses-devel

  # centos compile guake
  sudo yum install gnome-common GConf2-devel pytgtk2-devel python-vte-devel \
                   gnome-python2-gconf python-keybinder pyxdg notify-python

  # dnf Dandified Yum package manager
  # Package manager forked from Yum, using libsolv as a dependency resolver.
  # It requires a working rpm installation and is meant to be used in chroots

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

  # For "The following packages have been kept back: ..."
  # see https://askubuntu.com/a/602
  # Cautious solution 1:
  sudo apt-get --with-new-pkgs upgrade <list of packages kept back>
  # Cautious solution 2:
  sudo apt-get install <list of packages kept back>
  # Aggressive solution
  sudo apt-get dist-upgrade # force the installation of those new dependencies.

  # Also after `sudo apt update && sudo apt upgrade` when this appears:
  # '.. packages can be upgraded. Run 'apt list --upgradable' to see them.'
  sudo apt-get --with-new-pkgs upgrade <list of packages kept back>

  # :ubuntu CLI OS upgrade (GUI upgrade - see `update-manager`)
  # 1. set `Prompt=normal`
  /etc/update-manager/release-upgrades
  Prompt=normal
  #
  # 2. `download package information`; and `install available upgrades`.
  # See also full-upgrade / `apt list --upgradable -a`
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
  dpkg --status <package> # --status -s

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

#lang notes

@block{@block-name{systemd}
  https://systemd.io/

  A suite of basic building blocks for a Linux system. It provides a system and
  service manager that runs as PID 1 and starts the rest of the system. It's an
  init system. Consists of [...] sections

  # !!! [Service], [Unit] etc. are CAPITAL SENSITIVE !!!

  Systemd Deep-Dive: A Complete, Easy to Understand Guide for Everyone
  https://youtu.be/Kzpm-rGAXos
  Unit vs Service
  systemd manages units, service is a type of unit

  [Service]
  # Default. Will be started as soon as you start it
  Type=simple
  #
  # Almost identical with 'Type=simple'. Considered to not be started until the
  # service-process tell systemd that it's ready. On Debian and/or Ubuntu this
  # is probably set to forking.
  Type=notify

  # Relevant for enable/disable
  [Install]

  man systemctl         # Control the systemd system and service manager
  man systemd.unit      # Unit configuration
  man systemd.service   # service unit configuration
  man 7 systemd.special # default.target

  # list units
       systemctl
       systemctl --all --type service
       systemctl --failed / --state=failed

  # service status
  # `service` is replaced by `systemctl`
       systemctl        status        network-manager
       systemctl        status        <unit>
       systemctl --user status --full tomcat7.service
       systemctl --user status --full <unit>
       systemctl --user start         <unit>
       systemctl --user stop          <unit>
       systemctl --user restart       <unit>
  # The service configuration is reloaded but the service itself is not
  # restarted, that mean, e.g. a web server doesn't loose connections to
  # clients.
  # Not every configuration change supports reload over restart, i.e. not every
  # service file contain: 'ExecReload=...'
       systemctl --user reload        <unit>
       systemctl --user kill          <unit>
       systemctl --user enable        <unit>
       systemctl --user disable       <unit>

  # creates the override-file which will be merged with the original unit
  # configuration
  sudo systemctl edit        httpd.service
  # the entire service file
  sudo systemctl edit --full httpd.service

  # Ubuntu 11.10 or later
  # See GUI composition https://unix.stackexchange.com/a/464321
       systemctl      status  lightdm   # Xfce
       systemctl      status      gdm   # or gdm3 - GNOME
       systemctl      status      kdm   # KDE
  sudo systemctl restart lightdm

  # the systemd manager configuration

  # I.e. look for changes in every systemd-relevant directory and load
  # everything into memory. This should be the preferred way of reloading.

  sudo systemctl daemon-reload

  # all units from the systemctl --failed list
  # see also: service obliteration https://superuser.com/a/936976
       systemctl reset-failed

  # remove unit from the systemctl --failed list
       systemctl reset-failed <unit>

  # switch off bluetooth
       systemctl      status  bluetooth
  sudo systemctl disable bluetooth
  sudo systemctl stop    bluetooth

  # see also: xfce4-session-logout --suspend
  sudo systemctl suspend

  # system logs
  logger "Enter messages into the system log: Hello Logs"
  journalctl --since "1m ago" | grep Hello
  # messages from a specific --user and --boot. See man journalctl
  journalctl --user --boot

  # access kernel logs / system logs
  # -T, --ctime     human-readable timestamp (may be inaccurate!)
  sudo dmesg --ctime

  # something is mounted; similar to /etc/fstab
  mounts

  # can replace cron. systemd timers have some additional features
  timers
  
  # :systemd Control the systemd login manager - logging data
  loginctl

}

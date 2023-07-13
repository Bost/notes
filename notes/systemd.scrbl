#lang notes

@block{@block-name{systemd}
  https://systemd.io/
  A suite of basic building blocks for a Linux system. It provides a system and
  service manager that runs as PID 1 and starts the rest of the system.

  man systemctl         # Control the systemd system and service manager
  man systemd.unit      # Unit configuration
  man systemd.service   # service unit configuration
  man 7 systemd.special # default.target

  # systemd: list units
  systemctl
  systemctl --all --type service
  systemctl --failed / --state=failed

  # systemd: service status
  # `service` is replaced by `systemctl`
  systemctl        status         network-manager
  systemctl        status         <unit>
  systemctl --user status --full  tomcat7.service
  systemctl --user status --full  <unit>
  systemctl --user start          <unit>
  systemctl --user stop           <unit>
  systemctl --user restart        <unit>
  systemctl --user kill           <unit>
  systemctl --user enable         <unit>
  systemctl --user disable        <unit>

  # Ubuntu 11.10 or later
  # See GUI composition https://unix.stackexchange.com/a/464321
  systemctl      status  lightdm   # Xfce
  systemctl      status      gdm   # or gdm3 - GNOME
  systemctl      status      kdm   # KDE
  sudo systemctl restart lightdm

  # systemd: reload the systemd manager configuration
  systemctl daemon-reload

  # systemd: remove all units from the systemctl --failed list
  # see also: service obliteration https://superuser.com/a/936976
  systemctl reset-failed

  # systemd: remove unit from the systemctl --failed list
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

  # access kernel logs / system logs
  # -T, --ctime     human-readable timestamp (may be inaccurate!)
  sudo dmesg --ctime
}

#lang notes

;; TODO add this file to search-notes under `crl`

@block{@block-name{GUI composition}
  https://unix.stackexchange.com/a/464321

  | Component           | Examples         |
  |---------------------+------------------|
  | Desktop Environment | GNOME, KDE, Xfce |
  | Widget Toolkit      | GTK+, QT         |
  | Windowing System    | XOrg, Wayland    |
  | Operating System    | Linux, Windows   |
  | Hardware            |                  |

  XOrg - old, many different programs for setting everything
  Wayland - replaces xorg; uses HW accelerated graphics
  Wayland compositors: sway, hyperland, western

  sway
  pipewire - new audio standard for linux and bsd systems

}

@block{@block-name{GUI}
  KDE GNOME, Xfce work on X Display server ~ X Window Managers
  KDE GNOME, Xfce desktops ~ Desktop Environments

  GTK+ - widget toolkit; used in GNOME, Xfce
  Qt   - widget toolkit; used in KDE

  Display manager - better to be called graphical login manager. GUI displayed
  at the end of the boot process in place of the default shell.
  Different desktop environment use different login managers to keep the visual
  style consistent.

  GNOME uses gdm3
  Xfce  uses lightdm
  KDE   uses kdm

  ??? Graphical Interface (E.g. KDE Plasma, GNOME Shell, Xfce)

  +--------------------------------------------------------------------------+
  |                              Windowing System                            |
  |                (e.g. X = X11 = X Window System, Wayland, Quartz)         |
  |                                                                          |
  |  +--------------------------------+         +-------------------------+  |
  |  |   Display Server (e.g. XOrg)   |  <--->  | Window Manager          |  |
  |  |(i.e. Window Server Compositor) |         | (e.g. KDE, GNOME, Xfce) |  |
  |  +--------------------------------+         +-------------------------+  |
  |                                                                          |
  +--------------------------------------------------------------------------+

  xfwm4          - Window Manager
  startxfce4`    - an (bin/sh) shell script to start an Xfce session or choose
                    Xfce Session from the graphical login manager
  xfce4-session  - Session Manager. Starts up the Xfce Desktop Environment
  xfdesktop      - Desktop Manager for the Desktop Environment

  xfce4-settings-manager - Settings Manager
  `xfsettingsd` - settings daemon, listens to the xfconf channels
  `guix install xfce4-settings`

  xfce4-settings-editor
  - Can be started from `xfce4-settings-manager`
  - Edit ALL settings within `xfconf`,
  - Fraphical counterpart of `xfconf-query`; right mouse click -> Monitor

  xfconf
  Configuration storage and query system / property database; Settings daemon
  implemented as a D-Bus-based configuration storage system.
  `guix install xfconf`

  Xsettingsd
  - lightweight xsettings daemon
  - settings for Xorg applications via the XSETTINGS (? KDE has it's own daemon ?)
  - Java, Wine have different font settings through Fontconfig

  append `xsettingsd &` to `~/.xinitrc` when using `xinit`
  append `xsettingsd &` to `~/.xprofile` when using a Display manager like, e.g. GDM (GNOME), LightDM (Xfce), etc.

  ~/.config/xsettingsd/xsettingsd.conf
  May or may not contain the same config as xfce4-settings-editor -> xsettings.
  Example:
    Net/ThemeName "Matcha-dark-azul"
    Net/IconThemeName "Papirus-Dark"
    Gtk/DecorationLayout "menu:minimize,maximize,close"
    Gtk/FontName "Iosevka Aile 11"
    Gtk/MonospaceFontName "JetBrains Mono 10"
    Gtk/CursorThemeName "Adwaita"
    Xft/Antialias 1
    Xft/Hinting 0
    Xft/HintStyle "hintnote"
    Xft/DPI 184320 # 1024 * DPI
}

@block{@block-name{GSettings & dconf}
  dconf
  - Simple tool for manipulating a key-based dconf database, similar to
    `gsettings`.
  - Replacement for `gconf`.
  - Manages a range settings, like e.g. GDM, application, and proxy settings.

  dconf uses a big binary database like Windows-Registry. Fast to read.
  gconf uses many little XML-file. Human-Readable, portable. Slow to read.

  GSettings
  high-level API for application settings

  gsettings
  - Application Settings configuration (viewing and modifying) tool. Works
    regardless of the GSettings backend in use.
  - Front end for dconf; for modifying the dconf backend storage itself, use the
    dconf tool; but gsettings should be used by preference.

  https://bmaupin.github.io/wiki/operating-systems/linux/ubuntu/ubuntu-gsettings-dconf.html

  dconf-editor             - Graphical editor for gsettings and dconf database
  gsettings-data-convert   - GConf to GSettings data migration
  gsettings-schema-convert - GConf to GSettings schema conversion
}

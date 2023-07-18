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
  |  |   Display Server (e.g. XOrg)   |  <--->  |     Window Manager      |  |
  |  |(i.e. Window Server Compositor) |         | e.g. KDE/KD Plasma,     |  |
  |  |                                |         |      GNOME, Xfce, Sway  |  |
  |  +--------------------------------+         +-------------------------+  |
  |                                                                          |
  +--------------------------------------------------------------------------+

  xfwm4          - Xfce Window Manager
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

@block{@block-name{Sway}
  https://swaywm.org/
  Tiling Wayland compositor and a drop-in replacement for the i3 window manager
  for X11. It works with your existing i3 configuration and supports most of i3's
  features, plus a few extras.
  It works with the Wayland display protocol, not the X Window System.
  ;;
  Sway allows to arrange application windows logically, rather than spatially.
  Windows are arranged into a grid by default which maximizes the efficiency of
  your screen and can be quickly manipulated using only the keyboard.
  ;;
  DRI Direct Rendering Infrastructure
  component of the X Window System that provides hardware-accelerated rendering.
  ;;
  DRI2 Direct Rendering Infrastructure extension
  it's not a later version but a different extension not even compatible with
  the original DRI
  ;;
  KMS Kernel Mode Setting
  DRM Direct Rendering Manager
  Linux kernel subsystems that provide functionalities for the graphical display
  and rendering capabilities
  ;;
  EGL Embedded-System Graphics Library
  interface between rendering APIs (e.g. OpenGL or Vulkan) and the underlying
  native platform window system.

  # video, graphics
  # lists hardware
  # -class CLASS    only show a certain class of hardware
  sudo lshw -class video

  # video, graphics
  # lists all PCI devices, find information about the graphics card
  lspci -k | grep -EA3 'VGA|3D|Display'

  # Get entries from administrative database.
  # getent [OPTION...] database [key ...]
  #
  # Supported databases:
  # ahosts ahostsv4 ahostsv6 aliases ethers group gshadow hosts initgroups
  # netgroup networks passwd protocols rpc services shadow
  getent passwd $USER

}

@block{@block-name{Xfce}
  # :xfce :ubuntu :popup :message desktop notification
  notify-send "Hello World"

  # modify keymaps and pointer button mappings in X
  xmodmap -pm
  # See https://unix.stackexchange.com/a/126795
  # shift       Shift_L (0x32),  Shift_R (0x3e)
  # lock        Caps_Lock (0x42)
  # control     Control_L (0x25),  Control_R (0x69)
  # mod1        Alt_L (0x40),  Alt_R (0x6c),  Meta_L (0xcd)
  # mod2        Num_Lock (0x4d)
  # mod3
  # mod4        Super_L (0x85),  Super_R (0x86),  Super_L (0xce),  Hyper_L (0xcf)
  # mod5        ISO_Level3_Shift (0x5c),  Mode_switch (0xcb)
  # keyboard: print contents of X events
  xev

  xfce4-keyboard-settings # shortcuts keybindings
  # http://docs.xfce.org/xfce/xfconf/xfconf-query
  xfconf-query --list --verbose --channel xfce4-keyboard-shortcuts # -lvc
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/custom/<Super>Tab" --reset
  # following might not be needed
  xfconf-query --channel xfce4-keyboard-shortcuts --list | grep Super
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/default/<Super>Tab" --reset
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/custom/<Super>Tab" --create --type string --set "empty"
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/default/<Super>Tab" --create --type string --set "empty"
  #
  # if changes in the xml don't work, use xfce4-settings-editor
  locate xfce4-keyboard-shortcuts.xml
  find ~ -name xfce4-keyboard-shortcuts.xml
  # ~/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml

  xfce4-session-logout        # or gnome-session-quit
  xfce4-session-logout --help

  # restart xfce when the title bar dissapears from xfwm4 or execute
  #   rm -r ~/.cache/sessions
  pkill -KILL -u $USER

  # reset / remove xfce user settings
  rm -rf \
    ~/.local/share/xfce4/ \
    ~/.cache/sessions/ \
    ~/.cache/xfce4/ \
    ~/.cache/xfce4-indicator-plugin.log \
    ~/.config/xfce4/ \
    ~/.config/xfce4-session/ \
    ~/.config/xubuntu/
  # also: reset xfce4-panel ...
  xfce4-panel --quit
  pkill xfconfd
  rm -rf \ ~/.config/xfce4/panel
           ~/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml
  xfce4-panel # .. and restart it

  sudo systemctl | grep dm
  sudo systemctl restart <dm service>

  # Window Buttons context menu (right-click in the middle of the panel)
  # -> Properties -> Window grouping: -> Never

}

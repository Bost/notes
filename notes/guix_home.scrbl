#lang notes

#+title: Guix Home

@block{@block-name{Guix Home}
info 'Guix Home'

Almost the like `guix system` but for dotfiles.
Achieved by composing services (service extension mechanism)

Manage reproducible development environments.
https://sr.ht/abcdw/rde/ (Andrew Tropin)

In Terminal -> Preferences -> General: switch on "Run command as login shell"
(Otherwise ~/.bash_profile is not sourced)

On Guix, the "plugdev" group does not exist and eudev is used instead of udev.
Thus the suggestion was to use the following rule instead:

# list / search through all existing guix home services
guix home search .

# guix home configuration:
printf "HOME_ENVIRONMENT: %s\n" $HOME_ENVIRONMENT
cat $HOME_ENVIRONMENT/configuration.scm
# see also 'guix home describe'
guix home reconfigure
guix home --keep-failed -L ~/dev/dotfiles/guix/home reconfigure ~/dev/dotfiles/guix/home/home-configuration.scm

# generate a home environment definition from dotfiles and put it under /dst/dir
guix home import /dst/dir

# build some configuration without installing anything
guix home build /path/to/configuration.scm
# try out some configuration without installing anything
guix home container /path/to/configuration.scm
guix home --keep-failed -L ~/dev/dotfiles/guix/home container ~/dev/dotfiles/guix/home/home-configuration.scm

See also guix-lemp-container [https://www.notabug.org/hackware/guix-lemp-container/src/dev/run.sh]
LEMP WordPress Reproducible Environment
}

@block{@block-name{Packages in the current guix home profile}
  ls -lA $HOME_ENVIRONMENT/profile/bin

  [[https://youtu.be/R5cdtSfTpE0][YouTube: System Crafters Live! - A First Look at Guix Home]]
  [[https://systemcrafters.net/live-streams/october-01-2021/][Notes: System Crafters Live! - A First Look at Guix Home]]
  https://guix-home.trop.in/Home-Configuration.html
}

@block{@block-name{Setup IceCat web browser:}
  @block{@block-name{IceCat stuck at Cloudflare "Checking your browser before accessing..."}
     [[https://issues.guix.gnu.org/45179][IceCat stuck at Cloudflare "Checking your browser before accessing..."]]
     Install 'User-Agent Switcher' in IceCat and switch to
       Desktop -> 'Linux / Firefox 83'
     https://addons.mozilla.org/en-US/firefox/addon/uaswitcher/
     https://addons.mozilla.org/en-US/firefox/addon/user-agent-string-switcher/

     https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/

     https://addons.mozilla.org/en-US/firefox/addon/enhancer-for-youtube/
     In options activate the "Place controls within the video player"

     Disable GNU LibreJS
  }

  # Open `about:config` and set:
  privacy.resistFingerprinting = false
  identity.fxaccounts.enabled = true
  #
  # Login to https://accounts.firefox.com/signin and synchronize the account
  # Restart IceCat
  #
  ssh-keygen
  cat ~/.ssh/id_rsa.pub
}

@block{@block-name{GitHub, GitLab:}
  Login to https://gitlab.com add SSH Key https://gitlab.com/-/profile/keys
  Login to https://github.com add SSH Key https://github.com/settings/ssh/new
}

@block{@block-name{Clone repos}
 See home-configuration.scm
}

@block{@block-name{Copy public keys to the available machines}
}

@block{@block-name{Setup xfce keybindings and shortcuts}
  # See http://docs.xfce.org/xfce/xfconf/xfconf-query
  xfconf-query --channel xfce4-keyboard-shortcuts -lv
  xfconf-query --channel xfce4-keyboard-shortcuts \
               --property "/xfwm4/custom/<Super>Tab" --reset
}

@block{@block-name{Setup Displays}
}

@block{@block-name{Setup xfce4-pannel: Position, Icons, etc.}
  Copy the xfce4 configuration from a different machine
  Window Buttons context menu (right-click in the middle of the panel)
  -> Properties -> Window grouping: -> Never
}

@block{@block-name{Fish: setup `systemBinDir` in the `~/.config/fish/config.fish`}
}

@block{@block-name{Setup hashbang in the `~/bin/l` according to the output of `which guile`}
}

@block{@block-name{direnv}
  Utility; updates environment variables for the current shell
  [[https://www.youtube.com/watch?v=pS9JBKdAy4Q&t=795s][Per-Project Dev Environment: direnv]]
}

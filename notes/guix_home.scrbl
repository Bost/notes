#lang notes

@block{@block-name{Guix Home}
  # M-x helm-info-guix
  info 'Guix Home'
  See also GNU Stow https://www.gnu.org/software/stow/ , dotcrafter

  Almost the like `guix system` but for dotfiles.
  Achieved by composing services (service extension mechanism)
  https://guix-home.trop.in/Home-Configuration.html

  Manage reproducible development environments.
  https://sr.ht/abcdw/rde/ (Andrew Tropin)

  In Terminal -> Preferences -> General: switch on "Run command as login shell"
  (Otherwise ~/.bash_profile is not sourced)

  On Guix, the "plugdev" group does not exist and eudev is used instead of udev.
  Thus the suggestion was to use the following rule instead:

  # Delete all the home generations...
  guix home delete-generations 2m    # older than two months
  guix home delete-generations 45d   # older than 45 days
  # Works the same as 'guix package --delete-generations'.
  # Doesn't delete the ~/TIMESTAMP-guix-home-legacy-configs-backup directories

  # list / search through all existing guix home services
  guix home search .

  # guix home configuration:
  printf -- "HOME_ENVIRONMENT: %s\n" $HOME_ENVIRONMENT
  cat $HOME_ENVIRONMENT/configuration.scm
  # see also 'guix home describe'
  guix home reconfigure
  guix home --keep-failed -L $dotf/guix/home reconfigure $dot/guix/home/home-configuration.scm

  # initialize or generate a home environment definition from dotfiles and put
  # it to /dst/dir
  guix home import /dst/dir

  # build some configuration without installing anything
  guix home build /path/to/configuration.scm

  # try out some configuration without installing anything
  guix home container /path/to/configuration.scm
  guix home --keep-failed -L $dotf/guix/home container $dotf/guix/home/home-configuration.scm

  # -L --load-path
  # -c --cores
  guix home --allow-downgrades -c 24 \
            -L $dotf/guix/home -L $dgxp/packages/bost/packages/patches \
            reconfigure $dotf/guix/home/home-config-ecke.scm

  LEMP WordPress Reproducible Environment
  guix-lemp-container
  See also https://www.notabug.org/hackware/guix-lemp-container/src/dev/run.sh

  # Packages in the current guix home profile
  ls -lA $HOME_ENVIRONMENT/profile/bin

  YouTube: System Crafters Live! - A First Look at Guix Home
  https://youtu.be/R5cdtSfTpE0
  Notes: System Crafters Live! - A First Look at Guix Home
  https://systemcrafters.net/live-streams/october-01-2021

  direnv: per-project development environment.
  update environment variables for the current shell
  https://www.youtube.com/watch?v=pS9JBKdAy4Q&t=795s
}

@block{@block-name{TODOs}
  @block{@block-name{Setup IceCat web browser:}
    @block{@block-name{IceCat stuck at Cloudflare "Checking your browser before accessing..."}
       IceCat stuck at Cloudflare "Checking your browser before accessing..."
       https://issues.guix.gnu.org/45179

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

  Copy public keys to the available machines
  Login to https://gitlab.com add SSH Key https://gitlab.com/-/profile/keys
  Login to https://github.com add SSH Key https://github.com/settings/ssh/new
}

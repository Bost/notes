#lang notes

@block{@block-name{guix pull}
  # Update Guix OS distribution & Guix tools: download the latest Guix source code
  # and package descriptions, and deploys it.
  # See ~/.cache/guix/checkouts
  guix pull --cores=12 --verbosity=3
  guix pull      -c 12          -v 3

  # When developing a channel:
  #   guix pull: error: aborting update of channel 'CHANNEL_NAME' to commit <X>,
  #   which is not a descendant of <Y>
  #   hint: This could indicate that the channel has been tampered with and is
  #   trying to force a roll-back, preventing you from getting the latest updates.
  #   If you think this is not the case, explicitly allow non-forward updates.
  guix pull --allow-downgrades
}

@block{@block-name{guix upgrade}
  # Alias for guix package --upgrade
  # Upgrade all installed packages to the latest version of packages found in the
  # distribution currently installed.
  # Examples:
  guix upgrade --cores=12 --verbosity=3
  guix upgrade      -c 12          -v 3
}

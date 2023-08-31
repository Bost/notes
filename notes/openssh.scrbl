#lang notes

@block{@block-name{OpenSSH}
  # OpenSSH Full Guide - Everything you need to get started!
  # https://youtu.be/YS5Zh7KExvE
  #
  # has server, client and other components
  # default port 22

  @block{@block-name{OpenSSH configuration}
    # ~/.ssh is automatically created when ssh runs the first time
    # mkdir -p ~/.ssh && chmod 700 ~/.ssh
    touch ~/.ssh/config
    # readable and writable only by the user and not accessible by others
    chmod 600 ~/.ssh/config # -rw-------
    # Override SSH Config File Option:

    # use all other options but to connect as user root instead of <User>:
    ssh -o "User=root" dev
    # ignore all of the options specified in the ssh configuration file:
    ssh -F /dev/null user@"@"example.com
    ssh -F <configfile> user@"@"example.com
    # see also /etc/ssh/ssh_config

    # example configuration; in Emacs Tramp files can be access by
    # /ssh:pinky-ygg:/path/to/file
    Host pinky-ygg
        HostName 200:554d:3eb1:5bc5:6d7b:42f4:8792:efb8
        Port 50621
        ControlMaster auto
        # Reuse already existing session, ie use the same connection for
        # multiple ssh clients
        # ControlPath denotes a socket that is checked by new connections to see
        # if there is an existing ssh session that can be used.
        ControlPath ~/.ssh/master-%r@%h:%p
        # even after exiting the terminal,the existing session will remain open
        # for 10 minutes
        ControlPersist 10m
        Compression yes
  }

  # SSH Tunneling
  # https://iximiuz.com/en/posts/ssh-tunnels/
  # https://hackertarget.com/ssh-examples-tunnels/
  # mnemonics:
  ssh -N -L local:remote  # -L local
  ssh -N -R remote:local  # -R remote
  ssh -N -D port host     # -D dynamic port forwarding
  #  -N      Don't execute a remote command / don't launch shell.
  #          Useful for just for forwarding ports. See ssh_config.
  #
  # ssh reverse tunnel can be used to access web hook for telegram chat bot
  # during development:
  # https://www.youtube.com/live/rx8_ZFv1IV0?si=f3_ouy4yaSa6RG1-&t=1320
  #
  # Force almost any program to work over proxy; intercepts network calls
  # https://github.com/haad/proxychains
  #
  # TODO ssh can create a VPN w/o additional tools

  # mosh.org (mobile shell) - Remote terminal application.
  # - Allows roaming, supports intermittent connectivity, and provides
  #   intelligent local echo and line editing of user keystrokes.
  # - Replacement for interactive SSH terminals. It's more robust and
  #   responsive, especially over Wi-Fi, cellular, and long-distance links.
  # - Good for keeping connection up

  # exec disc usage command on a remote host and sort results
  ssh HOST_ALIAS du -h --max-depth=1 /path/to/dir | sort -h
  climate ssh-mount / ssh-unmount # climate - command line tools for Linux

  ssh-keygen
  # :github now copy-paste the ~/.ssh/id_rsa.pub to github under
  # "Account settings / SSH keys / Add another public key"
  cat ~/.ssh/id_rsa.pub

  # tail a logfile / file over ssh
  # -t force pseudo-terminal allocation
  ssh -t user@"@"hostname "tail -f /path/to/file"
  # -n redirects stdin from /dev/null
  ssh -n user@"@"hostname "tail -f /path/to/file" &

  # remote login using different / specific shell
  ssh -t USER@"@"HOST "bash -l"

  # :net
  # responds with 'ssh: connect to host ipv6-address port 22: Invalid argument'
  ssh -6 IPV6_ADDRESS
  ping6 -I wlan0 -c 4 IPV6_ADDRESS # responds with 'ping: unknown iface wlan0'

  # compare a remote file with a local file
  ssh user@"@"host cat ./path/to/remotefile | diff ./path/to/localfile -

  # enable remote access / login...
  sudo apt install openssh-server
  sudo systemctl status ssh
  sudo ufw allow ssh
  # ... and copy ssh keys to user@"@"host to enable password-less login, i.e.
  # login to remote host using authorized public key
  ssh-copy-id USER@"@"HOST
  # An alternative to ssh-copy-id:
  cat ~/.ssh/id_rsa.pub | ssh USER@"@"HOST 'cat >> .ssh/authorized_keys'

  # connect using private key instead of password
  ssh -p 2220 -i ./sshkey.private bandit14@"@"localhost

  # sshfs - network filesystem client to connect to SSH servers
  # See http://fuse.sourceforge.net/sshfs.html.
  # mount a folder/filesystem securely over a network
  sshfs USER@"@"HOST:/path/to/dir ./path/to/mount/point

  # Edit text files with VIM over ssh/scp
  # It creates a file in /tmp on the local system and then copies it back once
  # we write the file in vim.
  vim scp://USER@"@"HOST//etc/hosts
  # After the host we have a double //. This references the absolute path. A
  # single slash will have a path that is relative to the users home directory.

  # :sftp - SSH File Transfer from the OpenSSH / FTP over SSL
  # FTPS - FTP over SSL (SSL is deprecated)
  lftp
}

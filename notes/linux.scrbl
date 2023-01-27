#lang notes

@block{@block-name{Heroku}
  heroku login --app <APP-NAME>
  # Use Git to clone the repository / source code to a local machine
  heroku git:clone  --app <APP-NAME>

  # Show configuration / List all environment variables
  heroku config --app <APP-NAME>
  heroku config:get <VARIABLE> --app <APP-NAME>
  heroku config:set <VARIABLE>=<VALUE> --app <APP-NAME>

  # PostgreSQL database command line access
  heroku pg:psql --app <APP-NAME> <DATABASE>

  # Inspect logfile:
  heroku plugins:install heroku-papertrail --app <APP-NAME>
  heroku pt ":type -'ssl-client-cert' -'$MY_TELEGRAM_ID'" --app <APP-NAME> | grep -v -e '^[[:space:]]*$
  # open papertrail
  heroku addons:open papertrail --app <APP-NAME>

  # Shell / Command line access. It reads the .bashrc and/or .bash_profile
  heroku run bash --app <APP-NAME>

  # Install on Ubuntu: `sudo snap install heroku --classic` doesn't work
  # See https://github.com/heroku/cli/issues/822
  curl https://cli-assets.heroku.com/install.sh | sh

  # Install on Guix:
  npm install --global heroku
  # It may lead to:
  #    fatal error: uv.h: No such file or directory
  # which apparently may be ignored. See https://help.heroku.com/1104958
  sudo rm /usr/local/bin/heroku
  sudo ln -s /home/bost/.npm-packages/bin/heroku /usr/local/bin/heroku

  # Restart / Stop and start
  heroku ps:restart --app <APP-NAME>
  heroku ps:scale web=0 --app <APP-NAME> && heroku ps:scale web=1 --app <APP-NAME>

  # Deploy to test / production / etc.
  heroku pipelines:promote --app <APP-NAME>

  # `heroku ...` adds a new environment variable "PORT". Its default value
  # is 5000 when running `heroku local ...`. It can be changed using the '-p'
  # parameter. The value of "PORT" is random when the app is running on a
  # Heroku-server. See `(System/getenv "PORT")`
  heroku local -p 7000
}

@block{@block-name{How to verify your Ubuntu download}
  # Obtain key(s) 0x46181433FBB75451 and 0xD94AA3F0EFE21092 from the Ubuntu key
  # server
  gpg --keyid-format long --keyserver hkp://keyserver.ubuntu.com --recv-keys 0x46181433FBB75451 0xD94AA3F0EFE21092
  # Inspect the key fingerprints
  gpg --keyid-format long --list-keys --with-fingerprint 0x46181433FBB75451 0xD94AA3F0EFE21092
  # Verify the SHA256 checksum file
  gpg --keyid-format long --verify SHA256SUMS.gpg SHA256SUMS
  # Verify the ISO file
  # --ignore-missing  don't fail or report status for missing files
  sha256sum --ignore-missing --check SHA256SUMS
}

@block{@block-name{Udev}
  Userspace Device udev
  https://en.wikipedia.org/wiki/Udev
  device manager for linux kernel; manages nodes in the /dev directory
}

@block{@block-name{Grub}
  templates: /etc/grub.d
  settings: /etc/default/grub

  # on Guix
  guix install grub
  grub-mkconfig   # generate a grub config file.
  grub-mkimage    # make a bootable image of GRUB.
  sudo grub-mkconfig # --output=/boot/grub/grub.cfg

  update-grub     # alias for grub-mkconfig: cat /usr/sbin/update-grub
  # On Ubuntu when "you may need to re-run your boot" appears:
  sudo update-grub
}

@block{@block-name{Bluetooth}
  # BLE Bluetooth Low-Energy
  apt search bluetooth
  sudo apt install bluez blueman pulseaudio-module-bluetooth
  systemctl      status bluetooth
  sudo systemctl enable bluetooth
  sudo systemctl start  bluetooth

  bluetoothctl
  help # list of available commands of bluetoothctl

  # rfkill - enable / disable wireless devices
  sudo rfkill list
  sudo hciconfig hci0 up
  hcitool scan

  # xfce applet; See Settings -> Session and Startup -> Application autostart
  blueman-manager

  # send file to /storage/emulated/0/bluetooth
  bluetooth-sendto --device=XX:XX:XX:XX:XX:XX local-fname
  # See https://ubuntu-users.livejournal.com/439582.html
  # search for the appropriate channel for file transfers
  sdptool search FTP
  obexfs -bXX:XX:XX:XX:XX:XX -B<channel> ~
  sdptool browse XX:XX:XX:XX:XX:XX
  # Browse your cellular's files. List the tree of directories.
  obexftp -b XX:XX:XX:XX:XX:XX -l /
  # use the '@"@"'
  ussp-push XX:XX:XX:XX:XX:XX@"@" local-fname remote-fname
  # upload / push
  obexftp -b XX:XX:XX:XX:XX:XX -c /Download -p local-fname
  # download
  obexftp -b XX:XX:XX:XX:XX:XX -c /Download -d remote-fname
}

@block{@block-name{tcpdump}
  https://www.tcpdump.org/
  tcpdump - command-line packet analyzer
  libpcap - portable C/C++ library for network traffic capture

  # net
  # troubleshooting and security testing
  sudo tcpdump
  # Extract HTTP user agents
  sudo tcpdump -nn -A -s1500 -l | egrep -i 'User-Agent:|Host:'
  # Capture all the plaintext passwords
  sudo tcpdump port http or port ftp or port smtp or port imap or port pop3 or \
       port telnet -l -A | egrep -i -B5 \
       'pass=|pwd=|log=|login=|user=|username=|pw=|passw=|passwd=|password=|pass:|user:|username:|password:|login:|pass |user '
  # Extract HTTP passwords in POST requests
  sudo tcpdump -s 0 -A -n -l | egrep -i "POST /|pwd=|passwd=|password=|Host:"
  # Capture cookies from server and from client
  sudo tcpdump -nn -A -s0 -l | egrep -i 'Set-Cookie|Host:|Cookie:'
}

@block{@block-name{Various}
  https://www.wezm.net/technical/2019/10/useful-command-line-tools/

  see also:
  https://github.com/tldr-pages/tldr
  https://github.com/cheat/cheat

  Master the command line, in one page
  https://github.com/jlevy/the-art-of-command-line

  Climate - the ultimate command line tool for Linux
  https://github.com/adtac/climate

  Compare shells
  https://htmlpreview.github.io/?https://raw.githubusercontent.com/michaelmacinnis/oh/master/doc/comparison.html

  find / search in terminal Shift + Ctrl + F

  sudo apt install --yes ubuntu-restricted-extras # multimedia / video codecs
  sudo apt install --yes vlc                      # video player
  sudo dpkg-reconfigure libdvd-pkg

  basenc --base64
  # "Hello world!" == "SGVsbG8gd29ybGQh"
  https://stackoverflow.com/a/62017480/5151982

  # bat - A cat(1) clone with syntax highlighting and Git integration.
  bat --pager=never README.md
  bat      -P       README.md

  # web server, reverse proxy, load balancer, mail proxy, HTTP cache
  nginx

  # jq - Command-line JSON processor; json formatting; sed for json
  echo "{\"foo\":\"bar\"}" | jq .foo

  # json formatting
  curl 'http://stash.compciv.org/congress-twitter/json/joni-ernst.json' \
       > ernst.json && cat ernst.json | jq '.'

  # cpu mem hdd hardware: system information in a GTK+ window
  hwinfo
  inxi -Fxz
  hardinfo
  sudo dmidecode
  sudo lshw
  cpu-x

  # net Address-Resolution-Protocol
  # MAC address of a network neighbour for a given IPv4 Address
  # display / modify the IP-to-Physical address translation tables for ARP
  arp -a
  # send ARP REQUEST to a neighbour host
  arping
  # the arp scanner
  arp-scan
  # keep track of ethernet/ip address pairings
  arpwatch

  # :net - Network exploration tool and security / port scanner
  nmap

  # TCP proxies; shell-script based HTTP clients / servers;
  # network daemon testing; a SOCKS or HTTP ProxyCommand for ssh
  netcat

  # :net :arp - Network security auditing tool
  hunt

  # query an LDAP server from the command line with ldap-utils
  # ldapsearch ldapadd ldapmodify
  ldap-utils

  # concatenate and print files in reverse (reversed lines)
  tac file.txt > reversed.txt

  # prepend text or line to a file
  echo "1st-line" | cat - file.txt

  # print web page to pdf / screenshot
  google-chrome --headless --disable-gpu --print-to-pdf https://www.eff.or
  google-chrome --headless --screenshot --window-size=1280,169 https://www.eff.or

  # install google-chrome-stable from PPA
  wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
  sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'
  sudo apt update
  sudo apt install google-chrome-stable
  # google-chrome extras
  chrome://version
  chrome://flags
  chrome://net-internals
  chrome://quota-internals
  chrome://network-error/-106

  # :google-chrome :HSTS :HTTP-Strict-Transport-Security
  "This web always encrypts. And it does so using trusted certificate"
  chrome://net-internals/#hsts

  # :net - ports listening for connection (i.e. open ports)
  sudo nmap -sT -O localhost
  sudo nmap -sn IP_RANGE

  # show open ports
  nmap localhost | grep -i open # 631/tcp  open ipp - Internet Printing Protocol
  nmap www.google.com | grep -i open

  # :net IPv4 - CIDR notation
  # 192.168.100.14/24 represents the IPv4 address 192.168.100.14 and its
  # associated routing prefix 192.168.100.0
  # TODO what is the /24 - address range?

  # find active computers on a local network with nmap
  | Standard ICMP ping | nmap -sn     192.168.1.0/24 |
  | TCP SYN Ping       | nmap -sn -PS 192.168.1.0/24 |
  | TCP ACK Ping       | nmap -sn -PA 192.168.1.0/24 |
  | UDP Ping           | nmap -sn -PU 192.168.1.0/24 |
  | IP Protocol Ping   | nmap -sn -PO 192.168.1.0/24 |
  | ARP Ping           | nmap -sn -PR 192.168.1.0/24 |

  # find active computers on a local network with ping
  # works only in bash
  echo 192.168.1.{0..255} | xargs -n1 -P0 ping -c1 | grep "bytes from"

  # search for a file named exactly NAME (not *NAME*)
  locate -b '\NAME'

  # split a file into pieces (with '.' at the end)
  split --bytes 1M --numeric-suffixes --suffix-length=3 foo.txt foo.

  # view PDF documents / files
  evince file.pdf
  libre file.pdf
  # gv, the PostScript and PDF viewer using Ghostscript as a back-end doesn't
  # work

  # centos update
  su -c 'yum update'

  # :net - grouping bandwidth per process; "net top"
  sudo nethogs wlan0

  # top and htop explained; see also atop iotop
  https://peteris.rocks/blog/htop/

  # monitor disk I/O usage
  sudo iotop -oPa

  # :HDD :SSD - disk information
  sudo hdparm -I FILESYSTEM # see: df -h
  sudo hdparm -I /dev/sda1

  # top report / output to stdout: -b batch mode; -n Number of iterations
  top -b -n 1

  # load average explained
  curl --silent https://raw.githubusercontent.com/torvalds/linux/v5.1/kernel/sched/loadavg.c | head -n 8
  # process queuing: load-average > nr-of-processors * cores-per-processor
  uptime               # load average from /proc/uptime
  top -b -n 1 | grep load
  cat /proc/loadavg    # 4. column: processes running/total; 5.: last used pid
  # number of processors
  lscpu | grep "^CPU"
  # cores per processor
  cat /proc/cpuinfo | grep cores

  # download and import gnu-keyring
  wget http://ftp.heanet.ie/mirrors/gnu/gnu-keyring.gpg && \
  gpg --import gnu-keyring.gpg

  # wget - limit the download speed to amount bytes per second
  wget --limit-rate=20k URL

  # download & verify / check bitcoin core wallet
  # The Qt-client may require `sudo apt install libxcb-xinerama0`
  set --local btcVer        23.0
  set --local btcUrl        https://bitcoincore.org/bin/bitcoin-core-$btcVer
  set --local fChecksums    SHA256SUMS
  set --local fSignatures   $fChecksums.asc
  set --local fTarGz        bitcoin-$btcVer-x86_64-linux-gnu.tar.gz
  #
  set --local uChecksums    $btcUrl/$fChecksums
  set --local uSignatures   $btcUrl/$fSignatures
  set --local uTarGz        $btcUrl/$fTarGz
  #
  gpg --keyserver hkps://keys.opengpg.org --refresh-keys
  wget $uChecksums $uSignatures $uTarGz
  sha256sum --ignore-missing --check $fChecksums
  # unzip and untar
  # -z --gzip, -x --extract, -v --verbose, -f --file
  tar --gzip --extract --verbose --file=$fTarGz  # tar zxvf $fTarGz

  # verify file
  gpg --verify file.sig file

  # fs / filesystem - number of inodes; every file or directory requires 1 inode
  df -i
  df --inodes

  # :net - show host name
  hostname --ip-address       # -i; addresses for the host name
  hostname --all-ip-addresses # -I; all addresses for the host

  # :mplayer reset/+/- speed by 10% / toggle OSD states / volume +/-
  backspace / \] / \[ / o / * / "/"

  # postscript to pdf conversion
  ps2pdf

  # :xserver - modifying keymaps and pointer button mappings in X
  xmodmap

  # :xserver - print XKB keyboard description to file in ps-format
  xkbprint :0

  # :ubuntu - change default www-browser
  sudo update-alternatives --config x-www-browser
  sudo update-alternatives --config gnome-www-browser
  # xfce4-settings-manager -> Preferred Applications # on Ubuntu
  # see ~/.local/share/xfce4/helpers
  # test by opening file / URL in the user's preferred / default application
  xdg-open www.wikipedia.org # /usr/bin/browse is symlinked to xdg-open

  # on guix (requires logout and login):
  # xfce4-settings-manager -> Default Applications

  # display file or file system status; alternative to ls
  stat -c "%y %s %n" *

  # :listing - sort by size; -l     use a long listing format
  ls --sort=size -l
  ls --sort=size -l --reverse

  # :listing - files newer than ...
  ls -la (find . -type f -newermt "2022-10-01")
  # listing files older than ...
  ls -la (find . -type f -not -newermt "2022-10-01")

  # :listing - only one column
  ls --format=single-column

  # :listing - only directories, 1 entry per line
  ls -d1 */

  # :listing - count of files in ./path/to/dir
  ls -1 ./path/to/dir | wc -l

  # :listing - show full paths (alias lff)
  # '-d, --directory' list directories themselves, not their contents
  # '-1'              list one file per line
  ls  -lrt -d -1 $PWD/{*,.*}   # all filepaths w/  their attributes
  ls       -d -1 $PWD/{*,.*}   # all filepaths w/o their attributes
  ls       -d -1 $PWD/path/to/file
  exa      -d -1 $PWD/path/to/file

  # :listing - file all extentions / filetypes in current directory
  find ./ -type f | perl -ne 'print $1 if m/\.([^.\/]+)$/' | sort -u

  # line count, word count
  wc /usr/share/common-licenses/GPL-2

  # list current traps; shell function responding to HW / other signals
  trap

  # delete /tmp/xyz$$ on shell exit / shell error
  trap "rm -f /tmp/xyz$$; exit" ERR EXIT

  # fist / last 5 lines from file
  head -n 5 file / tail -n 5 file

  # :listing process environment variables (separated by null-chars)
  cat /proc/PROCESS_ID/environ | tr '\0' '\n'

  # :net :ubuntu - (edit) and re-read proxy definition
  source /etc/environment

  # duplicate files in a given set of directories
  fdupes -r .

  # xfce-panel plugins:
  xfce4-clipman-plugin  # clipboard manager
  xfce4-screenshooter

  # The X server maintains three selections: PRIMARY, SECONDARY and CLIPBOARD.
  # - PRIMARY: for copying and pasting via the middle mouse button.
  #
  # Copy text to clipboard so that it is available in a text editor
  echo "foo" | xsel -bi # -b --clipboard; -i --input
  # show normal clipboard content in/on the terminal / command line
  xsel --clipboard

  # pipe to clipboard - doesn't work
  # cat file > /dev/clip
  # pipe from clipboard
  # cat /dev/clip

  # copy file content to clipboard
  cat file.ext | xclip -i -selection clipboard

  # wait for 10 pastings of the content file.ext to x-clipboard and quit
  xclip -loops 10 -verbose file.ext

  # put "test" to x-clipboard / put x-clipboard content to file.ext
  echo "test" | xclip / xclip -o > file.ext

  # run command only when load average is below a certain threshold
  # (default is 0.8)
  echo "rm -rf /unwanted-large/folder" | batch

  # change file mode bits of file according to reference-file
  chmod --reference reference-file file

  # change file mode bits - verbose example(s)
  chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir
  chmod          -R u=rwx,g=rwx,o=rwx /path/to/dir

  # remove all files except survivor.txt
  rm -f !(survivor.txt)

  # insert autocompletition result (use together with other progs)
  Esc *

  # :batch - run / execute a command at:
  echo "ls -l" | at midnight    # a given time
  at -f script.sh now + 1 hour  # 1 hour from now
  at -f script.sh now + 30 min  # 30 minutes from now
  watch date                    # periodically / repeatedly every 2 seconds

  # echo with formatting
  printf -- "Line: %05d %15.3f Result: %+15d\n" 1071 3,14156295 32589

  # simple python3 server
  python3 -m http.server 8000 --bind 127.0.0.1

  # simple python server
  python -m SimpleHTTPServer 8001

  # cross-platform HTTP/2 web server with automatic HTTPS
  caddy -host example.com

  # :python high-level file operations
  import shutil

  # :python concatenate / merge / join two lists (not arrays)
  # https://www.pythoncentral.io/the-difference-between-a-list-and-an-array/
  [1, 2] + [4, 5]

  # :args run the last command as root
  sudo !!

  # real and effective user and group IDs
  id user

  # google domain / sice specific search
  SearchText site:bartoszmilewski.com

  # net, networking, DNS lookup, domain information groper
  # convert hostname / domainname <-> IP address
  host www.google.com
  # www.google.com has:
  #  IPv4 172.217.23.196 (142.250.74.195)
  #  IPv6 2a00:1450:4014:80e::2004
  dig -x 172.217.23.196 +short
  # nslookup is deprecated in favor of dig
  nslookup www.google.com | tail -2 | head -1 | awk "{print $2}"
  # interrogate DNS name servers
  dig www.google.com
  # query wikipedia for keyword - doesn't work
  dig +short txt <keyword>.wp.dg.cx

  # make block or character special files
  mknod

  # create directory tree with multiple subdirs
  mkdir -p ./path/{sub1,sub2}/{1..100}/{src,bin,bak}

  # auto-create partent dir "./pth" and do --preserve=mode,ownership,timestamps
  cp --parents -p                                   ./pth/src.ext ./pth/dst.ext
  cp --parents --preserve=mode,ownership,timestamps ./pth/src.ext ./pth/dst.ext

  # mv README.text README.txt ; cp file file.bak
  mv README.{text,txt} ; cp file{,.bak}

  | fist 5 lines from file | head -n 5 file |
  | last 5 lines from file | tail -n 5 file |

  # get date (timestamp) in a given format
  date +"%Y-%m-%d_%H-%M-%S"

  # free and used memory in the system
  free -h

  # file or filesystem status
  stat FILE_OR_FILESYSTEM
  # example
  stat ~/.bashrc
  #   File: /home/bost/.bashrc -> /gnu/store/va8k3h6cnjp487fz83hs5rq5jd486qv3-bashrc
  #   Size: 50          Blocks: 0          IO Block: 4096   symbolic link
  # Device: 804h/2052d	Inode: 11797326    Links: 1
  # Access: (0777/lrwxrwxrwx)  Uid: ( 1000/    bost)   Gid: (  998/   users)
  # Access: 2022-02-04 19:07:49.863635641 +0100
  # Modify: 2022-01-14 01:22:15.702395911 +0100
  # Change: 2022-01-14 01:22:15.702395911 +0100
  #  Birth: 2022-01-14 01:22:15.702395911 +0100
  stat /dev/sda1
  #  File: /dev/sda1
  #  Size: 0          Blocks: 0          IO Block: 4096   block special file
  #Device: 5h/5d	Inode: 192         Links: 1     Device type: 8,1
  #Access: (0660/brw-rw----)  Uid: (    0/    root)   Gid: (  988/    disk)
  #Access: 2022-02-04 13:41:32.711999884 +0100
  #Modify: 2022-02-04 13:41:32.711999884 +0100
  #Change: 2022-02-04 13:41:32.711999884 +0100
  # Birth: -

  # enable / disable devices and files for paging and swapping
  swapon
  swapoff
  # summary about used swap devices
  swapon --show

  # join lines of two files on a common field
  join

  # total / summarize size of dir; estimate file space usage
  du -s dir
  du -sh dir
  du -sh --exclude={.git,.atom} dir
  # see also ncdu

  # size of ./path/to/dir with subdirs, exclude files matching pattern
  du -csh --exclude={.git,.atom} ./ | sort --human-numeric-sort
  du --total --separate-dirs --human-readable --exclude={.git,.atom} ./ \
      | sort --human-numeric-sort

  # jump to ./path/to/dir, execute command and jump back
  (cd ./path/to/dir && ls) # works only in bash

  # stop-watch; ctrl-d to stop; measure execution time; or try to install
  # stopwatch
  time read

  # process ID of a running program
  pidof process-name

  # find and kill processIDs belonging processName
  kill $(pidof processName)

  # :telnet terminate session
  Ctrl-\] Enter quit Enter

  # download fileX.txt and save it under different location / name
  wget http://server/fileX.ext -O ./path/to/fileY.ext

  # download fileX.txt and save it under different location / name
  curl -O http://server/fileX.txt > ./path/to/fileY.txt

  # :net ask http://ifconfig.me about myself (ua: User Agent)
  curl ifconfig.me/ip/host/ua/port/

  # :net test connection with disabled proxy
  curl --noproxy "*" -X GET http://www.google.com

  # enforce using http_proxy instead of https_proxy in case of
  # SSL23_GET_SERVER_HELLO
  curl -v --proxy $http_proxy https://www.google.com

  # show request/response headers
  curl -v URL

  # in bash: (doesn't work in fish)
  curl --request GET \
   "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=test"

  curl --request POST -H 'Content-Type: application/json' -d '{"x":"1", "y":"2"}' URL
  curl --request POST --form variable=value URL

  # :iproute2 :net - like ifconfig. State of network interfaces
  ip address
  # show / manipulate routing, devices, policy routing and tunnels
  ip address show eth0
  # routing table
  ip route
  # list routes with pretty output format
  routel     # just a wrapper arount `ip route` ?
  # Address Resolution Protocol table
  ip neighbour

  # :net what is my IP address
  curl ifconfig.me
  # See also
  https://resolve.rs/

  # exec disc usage command on a remote host and sort results
  ssh HOST_ALIAS du -h --max-depth=1 /path/to/dir | sort -h
  climate ssh-mount / ssh-unmount # climate - command line tools for Linux

  # recursively compare dirA with dirB; show only filenames: -q (quiet)
  diff -rq dirA dirB | sort

  # sort via 2nd key (?column?)
  sort -k2 file.csv

  # :diff outputs the files in two columns, side by side, separated by spaces
  sdiff file1 file0

  # output line-numbers
  diff --unchanged-line-format="" --old-line-format="" \
       --new-line-format=":%dn: %L" fold fnew

  # new line separator for each grep result sh script
  grep "pattern" /path/to/file | awk '{print $0,"\n"}'

  # find files and open them in gvim
  gvim $(find . -name "*fileToSearch*")

  # :gzip list compressed, uncompressed size, compression ratio etc.
  gzip -l ./path/to/file.gz

  # write output to stdout; zcat and gunzip -c are identical
  gunzip -c / zcat

  # commit log since ...
  svn log -r \{2017-01-01\}:HEAD REPO_URL/MODULE > svn.log

  # search in commit logs since ... and show changed / affected files
  # (--verbose)
  svn log --revision \{2017-01-01\}:HEAD --no-auth-cache --non-interactive \
      --verbose --username '...' --password '...' \
      --search STR1 --search STR2 REPO_URL/MODULE

  # checkout; also for http://IP:PORT/path; https://IP:PORT/path
  svn co --username SVN_LOGIN svn://IP:PORT/path

  # error: E120106: ra_serf: The server sent a truncated HTTP response body.
  svn cleanup && svn update

  # last revision number
  svn info REPO_URL/MODULE

  # when: svnrdump: E000022: Couldn't get lock on destination repos after 10
  # attempts
  svn propdel --revprop -r0 svn:rdump-lock URL

  # system information (kernel version etc.)
  uname -a

  # tail a (log)file over ssh
  # -t force pseudo-terminal allocation
  ssh -t user@"@"hostname "tail -f /path/to/file"
  # -n redirects stdin from /dev/null
  ssh -n user@"@"hostname "tail -f /path/to/file" &

  ssh-keygen
  # :github now copy-paste the ~/.ssh/id_rsa.pub to github under
  # "Account settings / SSH keys / Add another public key"
  cat ~/.ssh/id_rsa.pub

  # Execute a command as another user
  pkexec

  # :systemd Control the systemd login manager - logging data
  loginctl

  # last logged-in users
  last

  # :processsor :cpu :architecture :cores 32 (i686) /64 (x86_64) bit
  lscpu
  getconf LONG_BIT

  # number of processors / available processing units
  cat /proc/cpuinfo | grep processor | wc -l
  nproc

  # Report processors related statistics
  mpstat
  mpstat -P ALL
  # Display five reports of statistics for all processors at two second
  # intervals
  mpstat -P ALL 2 5

  # :processor :cpu :mem :hdd :hardware system information for console & IRC
  # -Fz filter out privacy sensitive info
  inxi -Fxz
  inxi --full --extra 1 --filter

  # :nice :cpulimit find and delete *.jar and *.class when idling
  ionice -c3 find . -name "*.jar" -or -name "*.class" -delete

  # :nice :cpulimit change the priority of process 2222 to minimum
  # (-19 max, +19 min prio)
  renice +19 2222

  # :nice :cpulimit launch process with lowest priority
  nice -n +19 COMMAND

  # :nice :cpulimit limits the CPU usage of a process to max 10%
  cpulimit --limit 5 COMMAND

  # :nice :cpulimit :ps show statistics for a process nr. 7695
  ps -o pid,user,command,nice -p 7695
  | ps f         | process tree                                        |
  | ps fx        | process tree of all processes                       |
  | ps u         | user's processes ; ps -aux / ps aux - are different |
  | ps -e        | every process on the system: standard syntax        |
  | ps ax        | every process on the system: BSD syntax             |
  | ps --windows | show windows as well as cygwin processes (-W)       |

  # distro name and ver
  cat /etc/*-release
  cat /proc/version

  # :ubuntu show OS version
  lsb_release -a
  cat /etc/issue

  # :ubuntu after update / upgrade see
  /usr/share/update-notifier/notify-reboot-required

  # run fsck on next reboot
  sudo touch /forcefsck

  # remove old kernels - see dotfiles/bin/remove-old-kernels

  # checksum current directory; "-print0" and "-0" handle filenames with spaces
  # ... with filenames, i.e. renaming detection
  find . -type f -print0 | xargs -0 sha1sum   | sha1sum | awk '{print $1}'
  # alternative
  find . -type f | xargs -I "{}" sha1sum "{}" | sha1sum | awk '{print $1}'
  # ... without filenames, i.e. no renaming detection
  find . -type f -print0 | xargs -0 sha1sum   | awk '{print $1}' | sha1sum | awk '{print $1}'
  # alternative
  find . -type f | xargs -I "{}" sha1sum "{}" | awk '{print $1}' | sha1sum | awk '{print $1}'

  # create a SHA checksum file containing checksums
  sha256sum file1.ext file2.txt > SHA256SUMS.asc
  # read SHA checksums from the SHA256SUMS.asc file and check / verify them
  #  against the SHA checksums of the files in the current directory
  sha256sum --check SHA256SUMS.asc | grep OK
  sha256sum -c      SHA256SUMS.asc | grep OK

  # :ps full command line; command is separated by the \0 byte
  tr '\0' ' ' < /proc/PROCESS_ID/cmdline

  # :ps :top :htop all info related to a process
  ls /proc/PROCESS_ID

  # :ps :top :htop currend working dir of PROCESS_ID
  cat /proc/PROCESS_ID/cwd

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

  # shortcuts keybindings
  xfce4-keyboard-settings
  # https://forum.manjaro.org/t/cant-switch-windows-with-super-tab/2406/4
  #
  # http://docs.xfce.org/xfce/xfconf/xfconf-query
  xfconf-query --list --verbose --channel xfce4-keyboard-shortcuts # -lvc
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/custom/<Super>Tab" --reset
  # following might not be needed
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/default/<Super>Tab" --reset
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/custom/<Super>Tab" --create --type string --set "empty"
  xfconf-query --channel xfce4-keyboard-shortcuts --property "/xfwm4/default/<Super>Tab" --create --type string --set "empty"
  #
  # if changes in the xml don't work, use xfce4-settings-editor
  locate xfce4-keyboard-shortcuts.xml
  find ~ -name xfce4-keyboard-shortcuts.xml
  # ~/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml

  #
  gnome-session-quit
  xfce4-session-logout

  # difference between nohup, disown, & https://unix.stackexchange.com/a/148698
  # - puts the job in the background, that is, makes it block on attempting to
  # read input, and makes the shell not wait for its completion.
  &
  # - removes the process from the shell's job control, but it still leaves
  # it connected to the terminal. One of the results is that the shell won't
  # send it a SIGHUP. Obviously, it can only be applied to background jobs,
  # because you cannot enter it when a foreground job is running.
  disown

  # - disconnects the process from the terminal, redirects its output to
  # nohup.out and shields it from SIGHUP. One of the effects (the naming one)
  # is that the process won't receive any sent SIGHUP. It is completely
  # independent from job control and could in principle be used also for
  # foreground jobs (although that's not very useful).
  nohup

  # doesn't create nohup.out
  nohup command >/dev/null 2>&1
  nohup command >/dev/null 2>&1 & disown

  # :kill :killall :signals
  man 7 signal
  man signal

  # :virtualbox restart clipboard
  killall VBoxClient && VBoxClient --clipboard & disown

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

  # also:
  xfce4-panel --quit
  pkill xfconfd
  rm -rf ~/.config/xfce4/panel ~/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml
  xfce4-panel

  sudo systemctl | grep dm
  sudo systemctl restart <dm service>

  # anti-freeze / WD40
  killall -SIGUSR2 emacs
  killall -HUP emacs

  # restart or halt machine using the keyboard:
  # https://youtu.be/BdRIhFcf4Do
  # 1. check the priviledges - `1` means all priviledges for sysrq. `0` means no
  # priviledges.
  cat /proc/sys/kernel/sysrq
  # how to obtain privileges:
  echo "1" | sudo tee /proc/sys/kernel/sysrq
  # Press ~Alt + sysrq~ buttons (i.e. the ~Alt+PrintScreen~ / ~M-<print>~) and
  # type "reisub" for restart / "reisuo" for halting the machine.
  # Note: On Guix when unter tty (i.e. ~C-M-<f1>~) ~M-<print>~ swichtes to the
  # last tty (tty switching is also possible by ~M-'some-f-key'>~)

  # search man pages for "topic"
  man -k topic / apropos -r topic

  # brief description of CMD / help for shell built ins
  whatis CMD / help

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

  # sshfs - network filesystem client to connect to SSH servers
  # See http://fuse.sourceforge.net/sshfs.html.
  # mount a folder/filesystem securely over a network
  sshfs USER@"@"HOST:/path/to/dir ./path/to/mount/point

  # mound windows shares under linux
  sudo mount.cifs //WINDOWS_MACHINE/path/to/dir path/to/dir \
       -o user=WINDOWS_USERNAME

  # :virtualbox mount shared folder
  sudo mount -t vboxsf share /home/username/share/

  # readonly to readwrite
  sudo mount -o remount,rw /partition/identifier /mount/point

  # mounted filesystems - table layout; -t --table
  mount | column -t

  # filter out some columns, remove table headers, etc. with `column`:
  guix package --list-available="emacs-(ace-jump-helm-line|yasnippet).*" \
  | column --table --table-noheadings --table-columns NAME,VER,OUT,PATH \
           --table-hide OUT --table-order VER,NAME,PATH \
           --output-separator ' ║ '
  # filter out some columns, remove table headers, etc. with `awk`:
  guix package --list-available="emacs-(ace-jump-helm-line|yasnippet).*" \
  | awk '{print "("$1, "\""$2"\"", $4")"}'

  # error: Requested formats are incompatible for merge and will be merged into
  # mkv.
  youtube-dl -f bestvideo[ext=mp4]+bestaudio[ext=m4a] URL

  # align csv file
  cat data.csv | column -t -s ';'

  # :xml command line XML tool (formating)
  xmllint

  # shared library dependencies
  ldd -v $(which vim)

  # :library find out if libgconf is installed
  ldconfig -p | grep libgconf

  # info about ELF files
  readelf -v $(which vim)

  # :cygwin command-line installer
  apt-cyg --mirror \
      http://ftp-stud.hs-esslingen.de/pub/Mirrors/sources.redhat.com/cygwin/x86

  # :cygwin print unix form of filename
  cygpath -u filename

  # zip content of ./path/to/dir to ./path/to/file.zip
  zip --recurse-paths --encrypt ./path/to/file.zip ./path/to/dir
  zip  -r              -e       ./path/to/file.zip ./path/to/dir

  unzip ./path/to/file.zip -d ./path/to/extract-dir
  # unzip and untar in one step / with one command
  # -z, --gzip, --gunzip, --ungzip   Filter the archive through gzip
  tar -zxvf file.tar.gz

  # :listing :compression list file content
  tar --list --file FILE.tar.xz
  tar --list --file FILE.tar.gz
  tar --list --file FILE.tar.bz2
  tar --list --file FILE.tbz2
  tar --list --file FILE.tgz
  tar --list --file FILE.7z
  unzip -l file.zip

  # tar / untar
  tar czf ./path/to/tarfile.gz file0 file1
  tar xzf ./path/to/tarfile.gz

  # Remove all files previously extracted from a tar(.gz) file
  tar -tf ./path/to/file.tar.gz | xargs rm -r

  # report or omit repeated lines; works only on adjacent duplicate lines
  uniq
  # deduplicate
  sort file.txt | uniq
  awk '!visited[$0]++' file.txt > deduplicated-file.txt

  # :net :ping :traceroute - check connection
  mtr google.com
  mtr --report www.google.com
  ethtool eth0
  ip neigh show | grep REACHABLE
  ip link show

  # :iproute2 :net open / listening ports and PIDs of associated processes.
  # tcp (-t) udp (-u)
  ss -tulpn  # ss - socket statistics replaces obsolete netstat

  crontab -e   # edit entries
  crontab -l   # view / list / show / display entries

  # show everything (battery info etc)
  acpi -V              # Advanced Configuration and Power Interface
  climate battery

  # set / increase / decrease display brightness
  xbacklight -set 10 / -inc 10 / -dec 10

  # power consumption / management diagnosis tool
  sudo powertop

  # :gps convert kml to gps
  gpsbabel -i kml -f in.kml -o gpx -F out.gpx

  # IBM USS OS/390: ebcdic / ascii conversion
  iconv -f IBM-1047  -t ISO8859-1 ebcdic.file > ascii.file
  iconv -f ISO8859-1 -t IBM-1047  ascii.file  > ebcdic.file
  # list all code pages
  iconv -l
  # show mime type strings rather than the more traditional human readable ones
  file --mime fileName

  # show first / last 100 bytes
  tail -c 100 fileName
  head -c 100 fileName

  # :net what is currently using inet
  lsof -P -i -n | cut --fields=1 --delimiter=" " | uniq | tail --lines=+2

  # remove sections from each line of files
  cut

  # list open files / what is currently using a file
  lsof
  # open files whose inet address matches ADDR; -t: terse output
  lsof -i:[ADDR] -t
  # fish: process listening on the PORT_NUMBER
  ps (lsof -i:PORT_NUMBER -t)

  | strace | trace system calls and signals |
  | ltrace | library call tracer            |
  | ftrace | TODO                           |
  | ptrace | trace process                  |
  # what is currently using file / files opened by a running command
  strace COMMAND 2>&1 | grep openat
  # monitor file and network activities of a PROCESS
  # max printed string size 10000
  # *.strace file should be handled correctly by strace-mode emacs plugin
  strace -f -e trace=file,network -s 10000 -o outfile.strace PROCESS ARGS

  # check file types and compare values
  test
  # determine file type / mime type
  file
  file --mime

  # :tabs convert spaces to tabs / tabs to spaces
  expand / unexpand file.txt

  # simple GUIs
  zenity, whiptail

  | collectd | system statistics collection daemon                           |
  | telegraf | plugin-driven server agent for collecting & reporting metrics |

  | Simple Network Management Protocol | snmp      |
  | packet analyzer                    | wireshark |
  | trivial file transfer program      | tftp      |

  # toggle bash / ftp
  ! / exit
  # connect to ipaddress and login with username
  open ipaddress ENTER user username
  # get file from remote computer
  get file / mget file
  # sends site specific commands to remote server
  site

  # System Information Extraction Program:
  sysinfo

  # :fs get extended attributes of filesystem objects (inst attr)
  getfattr

  # extended attributes on XFS filesystem objects
  attr

  # hash message authentication code
  HMAC

  # enterprise cryptographic filesystem for Linux
  ecryptfs

  # :debian-goodies
  # display all the dependencies of the given package and when each dependency
  # was installed
  which-pkg-broke vim
  # list the enhancements for all installed packages
  check-enhancements --installed-packages
  # show installed packages occupying the most space
  dpigs
  # search all files in specified packages
  sudo dgrep "text" vim

  # debian-goodies - check which processes need to be restarted after an upgrade
  sudo needrestart   # replaces & inspired by checkrestart
  # check if the /var/run/reboot-required exists
  ls /var/run/reboot-required
  # list of packages to reboot
  /var/run/reboot-required.pkgs

  # start COMMAND and kill it if it is running still after 5 sec
  timeout 5s COMMAND

  # :net retcode==1 - online; retcode!=1 offline
  nm-online --exit; echo "retcode: $?"

  # wifi net nmcli - command-line tool for controlling NetworkManager
  nm-applet
  man nmcli-examples
  nmcli --ask device wifi list               # 1. list
  nmcli --ask device wifi connect WIFIonICE  # 2. connect
  nmcli --ask device disconnect wlan0        # 3. disconnect
  # general status and operations
  nmcli --ask general # also: nmcli general status

  # display installed packages
  rpm -qa

  # root login shell / console / prompt
  # run login shell as the target user; a command may also be specified
  sudo --login
  sudo -i

  # user management
  groups USER             # groups a user is in
  id                      # real and effective user and group IDs
  cat /etc/group          # available groups
  gpasswd                 # administer /etc/group and /etc/gshadow
  sudo adduser USER
  sudo deluser --remove-home USER             # userdel is a low level utility
  sudo passwd USER        # set / change password for the USER
  sudo usermod --groups GROUP --append USER   # app USER to the GROUP
  # euid - effective user id: number or id; see whoami
  sudo pkill -KILL --euid USER                # logout / logoff different user

  # rename user and his home directory
  # https://www.serverlab.ca/tutorials/linux/administration-linux/how-to-rename-linux-users-and-their-home-directory/
  sudo groupmod -n newuser olduser
  sudo usermod -d /home/newuser -m newuser
  sudo usermod -l newuser olduser

  # Run command as another user
  sudo --set-home --user=otheruser bash -c 'echo "I am $USER, with uid $UID"'
  sudo         -H     -u otheruser bash -c 'echo "I am $USER, with uid $UID"'
  #
  # get the prompt of a different user
  su --login otheruser
  # get the root prompt
  sudo su --login root
  sudo su -

  # run a program in a new session
  setsid

  # Ultimate Plumber: Linux pipes with instant live preview
  https://github.com/akavel/up
  # monitor the progress of data through a pipe
  pv

  # maven
  mvn package
  mvn install / mvn clean # mvn install seems not to be needed
  # https://www.mkyong.com/maven/how-to-enable-proxy-setting-in-maven/
  {M2_HOME}/settings.xml
  # Cleanup local maven repository. It removes all snapshot from more than
  # 6 months: https://gist.github.com/cescoffier/1582615

  # :sftp - SSH File Transfer from the OpenSSH / FTP over SSL
  # FTPS - FTP over SSL (SSL is deprecated)
  lftp

  # :HPKP HTTP Public Key Pinning; similar to HSTS header
  # Create your HPKP hash: https://report-uri.io/home/pkp_hash

  # :net - data transferred today / per month
  sudo vnstat -u -i wlan0 && vnstat

  # :net - managing a netfilter firewall; ufw - uncomplicated firewall
  sudo ufw status numbered
  sudo ufw status numbered
  # if Status: inactive, run: sudo ufw enable
  sudo ufw delete RULE_NUMBER
  sudo ufw allow PORT
  sudo ufw allow PORT/tcp

  # :net :RDP :remote-desktop - `-p` ask for password, `-f` full screen
  rdesktop    -u USER -p - COMPUTER:3389
  rdesktop -f -u USER -p - COMPUTER:3389
  sudo /etc/init.d/xrdp restart

  # shred / permanent delete
  # Warning: shred doesn't work on directories
  shred --verbose --remove path/to/file
  find . -type f -print0 | xargs -0 shred --remove
  # srm doesn't delete hard-linked files
  srm -r path/to/file

  # synchronize system date / system time behind proxy
  curDate="$(wget -S "http://www.google.com/" 2>&1 \
      | grep -E '^[[:space:]]*[dD]ate:' \
      | sed 's/^[[:space:]]*[dD]ate:[[:space:]]*//' \
      | head -1l \
      | awk '{print $1, $3, $2,  $5 ,"GMT", $4 }' \
      | sed 's/,//')"
  sudo date -s "${curDate}"

  # Add and remove modules from the Linux Kernel
  modprobe -a vboxguest vboxsf vboxvideo
  # list(?) linux kernel modules
  modprobe --show-config

  # :vbox
  sudo aptitude install virtualbox-guest-additions-iso
  sudo /etc/init.d/virtualbox restart
  sudo /etc/init.d/virtualbox-guest-utils start

  # atom editor - delete all environment states
  atom --clear-window-state
  # list / backup installed packages to a file
  apm list --installed --bare > ~/dev/dotfiles/.atom/package.list
  # install packages from a file
  apm install --packages-file ~/dev/dotfiles/.atom/package.list
  # update all packages
  apm update
  # restore / synchronise settings
  rsync -avz --include="*/" --include="*.cson" --exclude="*" ~/.atom/* ~/dev/dotfiles/.atom

  # super fast ram disk
  sudo mkdir -p /mnt/ram
  sudo mount -t tmpfs /mnt/ram -o size=8192M

  # mount / umount (usb) disk without 'root' as the mount command.
  udiskie  # a user-level daemon for auto-mounting
  # udisksctl uses udiskds binary launched by udisks2.service.
  # see also udev / udevadm
  # test if /dev/sdc1 is mounted
  udisksctl info    --block-device /dev/sdc1 | rg MountPoints: | rg /
  udisksctl mount   --block-device=/dev/sdc1
  udisksctl unmount --block-device=/dev/sdc1

  # make file acting as / accessible as a pseudo ("fake") block-based device.
  udisksctl loop-setup  -f disk.img
  udisksctl unmount     -b /dev/loop8
  udisksctl loop-delete -b /dev/loop8

  # Change the label on an ext2/ext3/ext4 filesystem
  e2label

  # intercept stdout to a log file
  cat file | tee -a file.log | cat /dev/null

  # find 20 biggest files
  du -a ~ 2>/dev/null | sort -n -r | head -n 20

  # -h, --no-dereference   affect symbolic links instead of any referenced file
  chown -h myuser:mygroup mysymbolic

  # SMBIOS - System Management BIOS
  # DMI table - Desktop Management Interface
  dmidecode
  sudo dmidecode --type bios
  sudo dmidecode --type baseboard
  # see also: system chassis processor memory cache connector slot

  # Setup Wake on LAN https://tek.io/37ZXhPs
  sudo ethtool -s INTERFACE wol g  # list of interfaces: ip addr
  # test:
  #    sudo systemctl suspend   # or: sudo poweroff
  # then
  wakeonlan MAC_ADDRESS
  # TODO add powernap

  # fully resolve the link; report errors; see also: realpath
  readlink --canonicalize --verbose LINKNAME
  # fix broken link: ln -fsn
  ln --force --symbolic --no-dereference TARGET LINKNAME
  # -L, --dereference  : follow symbolic, i.e. copy content
  cp --dereference --recursive ~/.guix-home/ /tmp/
  # -l, --link  : hard link files instead of copying

  # Create bootable usb drive https://askubuntu.com/q/372607
  # (like with usb-creator-gtk)
  # '--exclude 7' means 'exclude loop devices' https://askubuntu.com/a/1142405
  # --nodeps      don't print slaves or holders
  # TRAN          device transport type. E.g. usb / sata / ...
  # lsblk --exclude 7 --nodeps \
  #       --output PATH,MODEL,TRAN,LABEL,PARTLABEL,SIZE \
  #      | rg --invert-match /dev/ram | sed 's/\<LABEL/FSLABEL/g'
  lsblk --output PATH,MODEL,TRAN,LABEL,PARTLABEL,SIZE | sed 's/LABEL/LBL/g' | sed 's/\<LBL/FSLBL/g'
  set --local isoImg      /path/to/file.iso
  set --local blkDevice /dev/sd<letter><number>
  set --local usbDevice /dev/sd<letter>
  udisksctl unmount --block-device=$blkDevice
  # TODO check if the size $usbDevice is large enough for the $isoImg
  # oflag=sync - use synchronized I/O for data & metadata
  echo \
       sudo dd bs=4M if=$isoImg of=$usbDevice status=progress oflag=sync && sync
  # or try:
  echo \
       sudo dd bs=4M if=$isoImg of=$usbDevice status=progress conv=fdatasync && sync

  # create temporary file
  mktemp

  # printer: Command-Line Printing and Options
  http://localhost:631/help/options.html

  # YAML: YAML Ain't Markup Language
  # human-readable data-serialization language. Python style indentation
  # [] lists, {} maps
}

@block{@block-name{Cross-platform widget toolkit for creating GUIs}
  Which version is installed?
  @block{@block-name{GTK+}
     # https://stackoverflow.com/a/126193/5151982
     # ubuntu: bash:
     dpkg -l libgtk* | grep -e '^i' | grep -e 'libgtk-*[0-9]'
     # or even better
     pkg-config --modversion gtk+-3.0
     pkg-config --modversion gtk+-2.0
  }

  @block{@block-name{Qt}
     https://stackoverflow.com/a/39901086/5151982
     https://stackoverflow.com/a/40112592/5151982
     qmake-qt5 --version
     qmake --version
     qtdiag
   }
}

@block{@block-name{Find zombie process}
  # https://vitux.com/how-to-create-a-dummy-zombie-process-in-ubuntu/
  ps axo stat,ppid,pid,comm | grep -w defunct

  // https://vitux.com/how-to-create-a-dummy-zombie-process-in-ubuntu/
  // compile: cc zombie.c -o zombie

  #include <stdlib.h>
  #include <sys/types.h>
  #include <unistd.h>
  #include <stdio.h>

  int main () {
    pid_t child_pid;child_pid = fork();
    if (child_pid > 0) {
      int sleep_time = 60;
      printf("Sleeping for %d seconds\n", sleep_time);
      sleep(sleep_time);
      printf("Waking up and terminating\n");
    }
    else {
      exit(0);
    }
    return 0;
  }
}

@block{@block-name{sed & awk}
  sed - stream editor
  awk - written by Alfred V. Aho, Peter J. Weinberger, Brian W. Kernighan
  text processing, data extraction, reporting tool

  # cut huge file: content between lines 10 and 20 / print 5th line
  sed -n "10,20p" /path/to/file / sed -n 5p /path/to/file

  # cut huge file: content between lines 10 and 20
  # see https://unix.stackexchange.com/a/47423
  awk 'NR >= 10 && NR <= 20' /path/to/file > /path/to/cut-file
  # awk is for tabular data

  # replace / substitute 1st occurence
  sed --in-place "s/foo/FOO/" /path/to/file

  # append two lines at the end of the file / EOF
  sed --in-place '$ aline3\nline4' /path/to/file  # doesnt work

  # add 'FOO' behind the 1st occurence of '#+title: something'
  sed --in-place "s/\(foo: .*\)/\1\nFOO/" /path/to/file

  # replace all occurences of "foo" (globally)
  sed --in-place "s/foo/FOO/g" /path/to/file

  # remove / delete empty lines (globally)
  sed --in-place '/^\s*$/d' /path/to/file

  # remove / delete line matching 'pattern'
  sed --in-place "/pattern/d" /path/to/file

  # remove / delete line matching pattern and one following line
  # see https://stackoverflow.com/a/4398433/5151982
  sed --in-place --regexp-extended "/^#\+title:.*/,+1d" *.scrbl

  # replace newlines with space
  sed ':a;N;$!ba;s/\n/ /g'

  # :ascii :ebcdic fix new lines and empty chars; \x85 - hexadecimal char
  sed "s/\x85/\n/g" <log.txt >log.nl.txt; \
  sed "s/\x85/\n/g" <log.nl.txt >log.nl.00.txt

  # ignore lines between 'marker1' and 'marker2'
  # see https://stackoverflow.com/a/40433880/5151982
  mysql_install_db 2>&1 | sed '/^$marker1/,/$marker2$/d'
}

@block{@block-name{tail vs less}
  Instead of `tail -f ...` try to use `less +F ...`.
  `tail -f ...` is better when watching multiple files at the same time.
  See https://www.brianstorti.com/stop-using-tail/
}

@block{@block-name{rsync}
  # rsync must be installed on the target machine!
  # March 2022: the scp Secure Copy Protocol is obsolete! Use sftp or rsync

  # recursive copy `dotfiles` and `cheat` to server:~/dev/
  # i.e. create `server:~/dev/dotfiles/` and `server:~/dev/cheat/`
  rsync -avz dotfiles cheat server:~/dev/
  # recursive copy of only the content of `dotfiles` and `cheat`.
  # i.e. create only the `server:~/dev/`
  rsync -avz dotfiles/ cheat/ server:~/dev

  # recursive copy only certain types of files using include option
  rsync -havz --include="*/" --include="*.sh" --exclude="*" "$src" "$dst"

  # copy multiple files from remote machine to a local machine
  rsync -a USER@"@"HOST:/remote/path/file1 :/remote/path/file2 /local/path

  # copy files from 'src' to 'dst' excluding everything in 'dir' directories
  # --dry-run -n --human-readable -h --archive -a --verbose -v --compress -z
  rsync -nhavz          --exclude='dir' --exclude='*.jpg' src/ dst
  rsync -nhavz --delete --exclude='dir' --exclude='*.jpg' src/ dst | grep deleting
  # copy files from 'src' to 'dst' excluding everything in 'dir' directories
  # exclude hidden files and directories
  rsync -nhav          --exclude=".*" --exclude=".*/" src/ dst
  rsync -nhav --delete --exclude=".*" --exclude=".*/" src/ dst | grep deleting

  # move content of a directory within another directory with the same folders
  rsync -nha          --remove-source-files backup/ backupArchives
  rsync -nha --delete --remove-source-files backup/ backupArchives | grep deleting

  # rsync options: short / long versions
  # -h --human-readable
  # -a --archive # -rlptgoD (no -H,-A,-X); recursive & preserve almost everything
  # -v --verbose
  # -z --compress
  # -r --recursive
  # -n --dry-run
  # -p --perms   # preserve permissions
}

@block{@block-name{Disk Devices}
  # usb, drive, drives, disk, list block devices, fdisk, mount, udevadm,
  # udiskie, udisksctl, block-device, boot
  lsblk --nodeps
  lsblk --output PATH,MODEL,TRAN,LABEL,PARTLABEL,SIZE | sed 's/LABEL/LBL/g' | sed 's/\<LBL/FSLBL/g'
  # find mounted filesystem
  findmnt --real --output TARGET,SOURCE,SIZE,LABEL,PARTLABEL | sed 's/LABEL/LBL/g' | sed 's/\<LBL/FSLBL/g'
  blkid         # locate/print block device attributes; show the UUIDs
  ls -la /dev/usb

  # Label filesystem and partition
  sudo e2label /dev/sd<letter><number> <some-filesystem-label>
  sudo tune2fs -L "<some-filesystem-label>" /dev/sd<letter><number>
  sudo parted /dev/sd<letter> name <number> "some-partition-label"

  # Format disk / usb drive
  # 1. erase everything on the device
  # convert and copy a file; bs=BYTES  read & write up to BYTES at a time
  set --local deviceFile /dev/sd<letter>    # see lsblk
  sudo dd if=/dev/zero of=$deviceFile bs=4k status=progress && sync
  # 2. make a new partition on the device
  sudo fdisk     $deviceFile
  sudo mkfs.ext4 $deviceFile
  sudo eject     $deviceFile

  # partition manipulation: resize / create / delete partitions
  parted                 # from the command line
  sudo gparted & disown  # GUI; requires `parted` on Guix
  cfdisk # basic partitioning functionality with a user-friendly interface
  fdisk  # advanced partitioning functionality
  # TODO see partprobe: https://opensource.com/article/18/9/swap-space-linux-systems
  # e.g. resize 3rd partition and use all free / available space
  parted /dev/sda resize 3 100%

  # flush file system buffers
  sync

  # :usb :drive gnome userspace virtual fs
  mount | grep gvfs; cd ...
}

@block{@block-name{Swap}
  # Linux swap: what it is and how to use it
  # https://averagelinuxuser.com/linux-swap/
  # New installations of Ubuntu 18.04 use a swap file instead of swap partition
  # 8 * 1024 * 1048576 MB = 8 * 1073741824 B = 8589934592 B = 8GB
  # TODO create swap file using fallocate
  # see https://www.tecmint.com/add-swap-space-on-ubuntu/
  /proc/sys/vm/swappiness
  /proc/sys/vm/vfs_cache_pressure
  # See swapspace - the swap file manager
  # http://www.pqxx.org/development/swapspace/
  set swapfile /swapfile
  sudo dd if=/dev/zero of=$swapfile count=8388608 bs=1024 status=progress
  # sudo fallocate --length 8G $swapfile
  sync   # synchronize cached writes to persistent storage
  # permissions should be: -rw------- 1 root root
  sudo chmod 0600 $swapfile # ls -la $swapfile
  sudo mkswap $swapfile
  sudo swapon $swapfile
  swapon --summary
  free -h
}

@block{@block-name{Sound}
  # sound audio music jack jackd supercollider overtone
  # https://webcamtests.com/
  # https://mictests.com/
  # https://askubuntu.com/questions/1128694/logitech-c920-microphone-not-working-in-ubuntu-18-04
  sudo alsa force-reload
  speaker-test
  arecord / aplay
  pacmd list-sources
  # 1. verify iterface in:
  qjackctl
  # then A. "pause" pulseaudio while qjackctl runs and "respawn" pulseaudio when
  # qjackctl is terminated.
  pasuspender qjackctl
  # or alternatively to A.:
  # B. kill the existing pulseaudio process, start the jack_control process and
  # re-start the pulseaudio process.
  pulseaudio --kill
  jack_control start && jack_control exit
  pulseaudio --start
  # see also jack active ports & some extra info
  jack_lsp
  jack_lsp --connections  # list connections to/from each port
}

@block{@block-name{Sway}
  https://swaywm.org/
  Tiling Wayland compositor and a drop-in replacement for the i3 window manager
  for X11. It works with your existing i3 configuration and supports most of i3's
  features, plus a few extras.

  Allows to arrange application windows logically, rather than spatially. Windows
  are arranged into a grid by default which maximizes the efficiency of your
  screen and can be quickly manipulated using only the keyboard.
}

@block{@block-name{Concurrent Versions System}
  # restart cvs daemon
  sudo /etc/init.d/cvsd restart / start / stop / status

  # diff tagX tagY
  cvs diff -r tagX -r tagY

  # get clean copy
  cvs update -C ./path/to/file.ext

  # :cvs get revision 1.11
  cvs update -P -C -r 1.11 ./path/to/file.ext

  # checkout module from branch or tag
  cvs checkout -r branchOrTag module

  # commit file with multi-line commit message
  cvs commit -m "fst-comment-line\nsnd-comment-line" path/to/file.ext

  # update file
  cvs log    -P -d ./path/to/file.ext

  # reminder to leave in 15 minutes / at 13:55
  leave +15 / leave 1355

  # delete NormalTag from file.ext in version 1.17
  cvs tag    -d -r 1.17 NormalTag ./path/to/file.ext

  # delete BranchTag from file.ext in version 1.17
  cvs tag -B -d -r 1.17 BranchTag ./path/to/file.ext

  # move   BranchTag to   file.ext in version 1.19
  cvs tag -B -F -r 1.19 BranchTag ./path/to/file.ext

  # create BranchTag on   file.ext in version 1.19
  cvs tag -b    -r 1.19 BranchTag ./path/to/file.ext

  # move   NormalTag to   file.ext in version 1.63
  cvs tag    -F -r 1.63 NormalTag ./path/to/file.ext

  # version and tags
  cvs log file.ext
  cvs status -v file.ext

  # list files associated with a tag; (no blank between -r and TAGNAME)
  cvs -q rlog -R -N -S -rTAGNAME MODULENAME

  # debug and trace info
  cvs -d cvs -t -d :pserver:faizal@"@"localhost:/myrepos \
      ci -m "test" -l "src/foo/Foo.ext"

  #
  cvs add file.ext
}

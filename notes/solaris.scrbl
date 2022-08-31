#lang notes

@block{@block-name{Solaris}
  # checksum
  /usr/bin/digest -a sha1
  # wget location
  /usr/sfw/bin/wget
  # full command line (needs: sudo rootsh -i -u ... )
  /usr/ucb/ps -auxww
  # displays information about processors
  psrinfo
  # net: ipconfig
  /usr/sbin/ifconfig -a
}

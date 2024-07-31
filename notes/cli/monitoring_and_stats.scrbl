#lang notes

@block{@block-name{Various}
  # vmstat [options] [delay [count]]
  vmstat 2 10

  # :net - grouping bandwidth per process; "net top"
  sudo nethogs wlan0
  
  # :top :htop explained; see also atop iotop
  https://peteris.rocks/blog/htop/
  
  # monitor disk I/O usage
  sudo iotop -oPa
  
  # HDD SSD - hard disk / hard drive information
  sudo hdparm -I FILESYSTEM # see: df -h
  sudo hdparm -I /dev/sda1
  
  # load average explained
  curl --silent https://raw.githubusercontent.com/torvalds/linux/v5.1/kernel/sched/loadavg.c | head -n 8
  # process queuing: load-average > nr-of-processors * cores-per-processor
  uptime               # load average from /proc/uptime
  # top report / output to stdout:
  # -n --iterations Number of iterations
  # -b --batch mode
  top --batch-mode --iterations 1
  cat /proc/loadavg    # 4. column: processes running/total; 5.: last used pid
  # number of processors
  lscpu | grep "^CPU"
  # cores per processor
  cat /proc/cpuinfo | grep cores

  # :ps full command line; (needed b/c command is separated by the \0 byte)
  tr '\0' ' ' < /proc/PROCESS_ID/cmdline
  
  # :ps :top :htop - all information related to PROCESS_ID
  ls /proc/PROCESS_ID
  
  # :ps :top :htop - current working directory of PROCESS_ID
  cat /proc/PROCESS_ID/cwd

  # see lsblk
  set --local diskRoot /dev/sd<letter>
  set --local diskPart /dev/sd<letter><number>
  #
  # SMART status of the hdd drive / all SMART information about the device
  sudo smartctl --all $diskRoot     # -a, --all
  # all SMART and non-SMART information about the device.
  sudo smartctl --xall $diskRoot    # -x, --xall

}

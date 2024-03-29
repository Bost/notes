#lang notes

@block{@block-name{MS Windows}
  # Microsoft Management Console
  mmc

  # windows alternative to locate / which
  where telnet

  # show or set host name
  hostname -i

  # command line: logoff and force
  shutdown /l /f

  # list group members
  net group <groupName> /domain

  # drive: drives: list mapped drives
  net use

  # drive: drives: map \\sever\path under P:

  # drive: drives: unmap P:
  net use P: /delete

  # user account information
  net user <username> /domain

  # open ports
  netstat -a | find "LISTENING" # on linux `netstat` is obsolete, use `ss`

  # set system variable system-wide
  setx NAME value /m

  # hardware: system info
  msinfo32 /report msinfo32.txt
  msinfo32 /nfo C:\TEMP\TEST.NFO | type C:\TEMP\TEST.NFO | grep <pattern>
  systeminfo

  # batch: line continuation
  ^

  # OS version; save system info
  systeminfo /s srvmain /u maindom\hiropln /p p@"@"ssW23 /fo table

  # Windows Management Instrumentation Command-Line (WMIC)
  wmic /?

  # db2: windows:
  echo %DB2CLP%

  # checksum [HashAlgorithm] must be uppercased
  CertUtil -hashfile path\to\file SHA1

  # checksum [HashAlgorithm] must be uppercased
  CertUtil -hashfile path\to\file [HashAlgorithm]

  # checksum
  FCIV.exe

  # cygwin: ps: show windows as well as cygwin processes (-W)
  ps --windows

  # cygwin: bash: print windows form of filename
  cygpath -w filename

  # cygwin: command-line installer
  apt-cyg --mirror http://ftp-stud.hs-esslingen.de/pub/Mirrors/sources.redhat.com/cygwin/x86

  # cygwin: bash: print unix form of filename
  cygpath -u filename

  # net: protocol stats and current TCP/IP connections using NBT (NetBIOS over TCP/IP).
  nbtstat -A <ip address>

  # UNC path: Store current dir for the POPD
  pushd \\computer.domain\path\to\dir

  ## UNC path: Change to the dir stored by the PUSHD
  popd

  # Robust File Copy for Windows: mirror directory structure
  robocopy src dst /mir

  # battery
  powercfg /batteryreport # win10
  powercfg /energy # win7

  # WinRM: Windows Remote Management protocol
}

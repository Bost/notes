#lang notes

@block{@block-name{Various}
  Team Win Recovery Project TWRP
  https://www.mojandroid.sk/twrp-recovery-android-aplikacia/

  Android Debug Bridge ADB
  Can be used to connect an android device with a computer using an USB cable or a
  wireless connection. It can be used to execute commands on the phone or transfer
  data between the device and the computer.
  See also chrome://inspect/#devices

  Android Package Kit (APK)
  Android Open Source Project (AOSP)
  OTA (over-the-air) updates
  OEM Original Equipment Manufacturer, the one who sells and manages SW (e.g.
      Samsung, Motorola, Google)

  WebUSB
  JavaScript API specification for securely providing access to USB devices from
  web applications

  Android distributions:
  LineageOS
  postmarketOS
  GrapheneOS
  Replicant
  /e/OS

  | Feature / Aspect   | LineageOS                                    | postmarketOS                                      | GrapheneOS                                                         | Replicant                                      |
  |--------------------+----------------------------------------------+---------------------------------------------------+--------------------------------------------------------------------+------------------------------------------------|
  | Base Platform      | Android (AOSP)                               | Alpine Linux                                      | Android (AOSP, hardened)                                           | Android (AOSP, fully free SW)                  |
  | Device Support     | Wide and mature (hundreds of models)         | Growing but limited (~250+ devices, many partial) | Narrow (mainly Google Pixel devices)                               | Very narrow (handful of older Samsung devices) |
  | User Interface     | Android-like with customization              | Linux UIs (Phosh, GNOME, Plasma, Sxmo, etc.)      | Stock-like Android with strong security controls                   | Basic Android, stripped of non-free components |
  | App Compatibility  | Full Android apps + optional GMS/microG      | Native Linux apps; Android via Waydroid           | Full Android apps; sandboxed Play Services optional                | Only free Android apps; no Google Play support |
  | Installation       | Easy, OTA and recovery-supported             | Complex, pmbootstrap-based                        | Moderate (official installers for Pixel devices)                   | Difficult; limited devices and outdated HW     |
  | Target Audience    | General users, privacy-conscious users       | Linux enthusiasts, hackers, experimenters         | Security/privacy-focused users, journalists, activists             | Free SW purists; FOSS advocates                |
  | Lifecycle Focus    | Extend device support beyond OEM updates     | 10-year lifecycle, true Linux experience          | Maximum security and privacy on modern HW                          | 100% SW freedom, but outdated and limited HW   |
  | Privacy & Security | De-Googling possible, privacy tools built-in | Full control, minimal proprietary blobs           | Hardened kernel, memory safety, app sandboxing, default: no Google | Fully free stack, no modern security hardening |

}

@block{@block-name{/e/OS}
  leading pro-privacy operating system for smartphones.
  fully deGoogled mobile operating system + selected applications =
  privacy-enabled internal environment for mobile phones
}


@block{@block-name{postmarketOS}
  Extend the life of consumer electronics. By empowering people to have full
  control of their devices, we promote a healthier and more sustainable society.
}

@block{@block-name{Replicant}
  https://www.replicant.us/
  A fully free mobile operating system (Android distribution). Emphasis on
  freedom & privacy/security

  Relation between Replicant and Guix:
  https://redmine.replicant.us/projects/upstreaming/wiki/GuixBuildSystem
}

@block{@block-name{GrapheneOS}
  https://grapheneos.org
  Private & secure mobile operating system with Android app compatibility.
  Non-profit open source project.
}

@block{@block-name{LineageOS}
  A (mostly?) free and open-source operating system for various devices, based
  on the Android mobile platform. For smartphones, tablets, and set-top boxes,
  etc. Successor to CyanogenMod

  wget https://spflashtool.com/download/SP_Flash_Tool-5.1916_Linux.zip
  sudo add-apt-repository ppa:linuxuprising/libpng12
  sudo apt update
  sudo apt install libpng12-0
  bash -c ./flash_tool.sh

  https://dlcdnet.asus.com/pub/ASUS/EeePAD/Zenpad/Z300M/Z300M_UnlockTool_Android_N_9003_signed.zip
  https://dlcdnet.asus.com/pub/ASUS/EeePAD/Zenpad/Z300CL/Z300CL_UnlockTool_Android_P01T_1_signed.zip
  https://dlcdnet.asus.com/pub/ASUS/EeePAD/Zenpad/Z300CL/Z300CL_UnlockTool_Android_P01T_signed.zip

  http://dlcdnet.asus.com/pub/ASUS/EeePAD/Zenpad/Z300CL/Z300CL_UnlockTool_Android_N_9003_signed.zip

  set cmd "ip address"
  eval $cmd
  #...

  set host $USER@"@"<ip address>
  set src $host:/home/$USER
  $src/dev/navig/Osmand
  $src/dev/navig/osmand-api-demo

  rsync -avz gradle-4.6-bin.zip $src/dev/navig/osmand-api-demo/gradle/wrapper
  rsync -avz $src/.gradle ~/.gradle/
  rsync -avz $src/.m2 ~/.m2/

  # edit gradle-wrapper.properties
  distributionUrl=gradle-4.6-bin.zip

  inst android-sdk
  $HOME/Downloads/android
  # unzip and then
  tools/bin/sdkmanager --licenses
  sudo rsync -avz licenses/ $ANDROID_HOME/licenses/
  sudo chown -R $USER:$USER /usr/lib/android-sdk/
  rsync -avz $host:/usr/lib/android-sdk/ /usr/lib/
}

@block{@block-name{Androind: USB transfer with Media Transfer Protocol}
  PTP Picture Transfer Protocol
  MTP Media Transfer Protocol

  ls -la /dev/usb
  ls -la /dev/bus/usb/
  cd /run/user/$UID/gvfs             # in bash
  # id --user  - print only the effective user ID
  cd "/run/user/"(id --user)"/gvfs"  # in fish-shell
  jmtpfs --listDevices
  mtp-detect
  fusermount
  # Filesystem in Userspace (FUSE)
  # SP Flash Tool
  # Smart Phone Flash Tool

  # http://reactivated.net/writing_udev_rules.html
  # https://framagit.org/tyreunom/guix-android
  # https://github.com/whoozle/android-file-transfer-linux/issues/301

  cd /media/bost/elements/Pictures
  aft-mtp-cli    # guix install aft-mtp-cli
  Phone> cd DCIM/Camera/
  Phone> ls
  Phone> get filename.ext # download / transfer: phone    -> computer
  Phone> put filename.ext #  upload  / transfer: computer -> phone
  Phone> rm filename.ext  # delete from phone

  # On Ubuntu:
  # mtp://SAMSUNG_SAMSUNG_Android_R58N10C71EM/
  #
  # $ lsusb
  # ...
  # Bus 005 Device 003: ID 04e8:6860 Samsung Electronics Co., Ltd Galaxy A5 (MTP)
  # ...
  # $ findmnt | grep fuse
  #     /sys/fs/fuse/connections   fusectl     fusectl         rw,nosuid,nodev,noexec,relatime
  #       /run/user/1000/gvfs      gvfsd-fuse  fuse.gvfsd-fuse rw,nosuid,nodev,relatime,user_id=1000,group_id=1000
  #       /run/user/1000/doc       portal      fuse.portal     rw,nosuid,nodev,relatime,user_id=1000,group_id=1000
  # $ jmtpfs --listDevices
  # Device 0 (VID=04e8 and PID=6860) is a Samsung Galaxy models (MTP).
  # Available devices (busLocation, devNum, productId, vendorId, product, vendor):
  # 5, 3, 0x6860, 0x04e8, Galaxy models (MTP), Samsung
  # $ lsmod | grep -i fuse # shows nothing, i.e. no fuse kernel module is loaded
  # $ id   # user groups
  # uid=1000(bost) gid=1000(bost) groups=1000(bost),4(adm),24(cdrom),27(sudo),30(dip),46(plugdev),121(lpadmin),132(lxd),133(sambashare),138(libvirt),998(docker)
  # $ mount | rg fuse
  # fusectl on /sys/fs/fuse/connections type fusectl (rw,nosuid,nodev,noexec,relatime)
  # gvfsd-fuse on /run/user/1000/gvfs type fuse.gvfsd-fuse (rw,nosuid,nodev,relatime,user_id=1000,group_id=1000)
  # portal on /run/user/1000/doc type fuse.portal (rw,nosuid,nodev,relatime,user_id=1000,group_id=1000)

  # on Ubuntu
  sudo apt install android-tools-adb android-tools-fastboot
  groups # check plugdev membership

  adb devices -l      # list all connected devices with device qualifiers
  # -T, --ctime     human-readable timestamp (may be inaccurate!)
  sudo dmesg --ctime | grep usb
  # [...] usb 5-3: new high-speed USB device number 4 using xhci_hcd
  # [...] usb 5-3: New USB device found, idVendor=04e8, idProduct=6860, bcdDevice= 4.00
  # [...] usb 5-3: New USB device strings: Mfr=1, Product=2, SerialNumber=3
  # [...] usb 5-3: Product: SAMSUNG_Android
  # [...] usb 5-3: Manufacturer: SAMSUNG
  # [...] usb 5-3: SerialNumber: R58N10C71EM
  adb logcat          # view device log
  adb logcat --clear  # -c clear / flush the entire log and exit
  # directs command to the device or emulator with the given serial number or
  # qualifier. Overrides ANDROID_SERIAL
  adb logcat -s <specific device>
  adb logcat -s bt_stack  # do `logcat` for bluetooth
  adb shell
  adb push src dst

  # on GuixOS
  guix install android-udev-rules
  # android-udev-rules provides a set of udev rules to allow using Android
  # devices with tools such as `adb' and `fastboot' without root privileges.
  # Need to create `udev-service-type' the `operating-system' configuration.
  # Additionally, an `adbusers' group must be defined and your user added to it.
  # _Simply installing this package will not have any effect._ It is meant to be
  # passed to the `udev' service.
}

@block{@block-name{Extensible Host Controller Interface XHCI}
  computer interface specification that defines a register-level description of a
  host controller for Universal Serial Bus (USB), which is capable of interfacing
  with USB 1.x, 2.0, and 3.x compatible devices. The specification is also
  referred to as the USB 3.0 host controller specification. It improves on the
  pre-existing Open Host Controller Interface (OHCI) and the Universal Host
  Controller Interface (UHCI)
}

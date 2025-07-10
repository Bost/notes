#lang notes

@block{@block-name{Various}
  Team Win Recovery Project TWRP
  https://www.mojandroid.sk/twrp-recovery-android-aplikacia/

  Android Debug Bridge ADB
  Can be used to connect an android device with a computer using an USB cable or a
  wireless connection. It can be used to execute commands on the phone or transfer
  data between the device and the computer.
  See also chrome://inspect/#devices

  Android Package Kit APK
}

@block{@block-name{LineageOS}

  # OS for smartphones, tablets, and set-top boxes, based on Android; mostly
  # free and open-source Successor to CyanogenMod

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

#lang notes

@block{@block-name{Various CompSec related stuff}
  The Hitchhiker’s Guide to Online Anonymity
  https://anonymousplanet.org/guide.html

  Why was Facebook down for five hours?
  https://youtu.be/-wMU8vmfaYo

  TOFU Trust on first / TUFU trust Upon First Use:
  Authentication scheme used by client software which needs to establish a trust
  relationship with an unknown or not-yet-trusted endpoint

  Downgrade / Roll-Back Attacks:
  Trick users into installing older, known-vulnerable software packages.
  There is no way to answer the general question "is X the latest commit of
  branch B ?". Rollback detection prevents just rollbacks, but there's no
  mechanism in place to tell whether a given mirror is stale.

  TUF The Update Framework:
  https://theupdateframework.io/
  Helps developers maintain the security of software update systems, providing
  protection even against attackers that compromise the repository or signing
  keys. TUF provides a flexible framework and specification that developers can
  adopt into any software update system.
}

@block{@block-name{GnuPG GNU Privacy Guard}
  Origin: PGP Pretty Good Privacy ;
  GnuPG / GPG - GNU Privacy Guard implements OpenPGP standard
  GPG primary key pair can: sign / encrypt / authenticate / certify
  GPG subkey pairs can do everything except one thing ...
  (TODO https://youtu.be/4-Ks_f8rQFA?t=400)

  gpgv - Verify OpenPGP signatures
  Stripped-down version of gpg which is only able to check signatures.

  Nov 17, 2020: Creating and Managing a GPG Key Pair
  https://youtu.be/1vVIpIvboSg

  Feb 17, 2018: Basic File Encryption with GPG key pairs!
  https://youtu.be/DMGIlj7u7Eo

  pinentry:
  collection of dialog programs that allow GnuPG to read passphrases and PIN
  numbers in a secure manner. There are versions for the common GTK, Qt and the
  text terminal (Curses).

  public key fingerprint:
  short sequence of bytes used to identify a longer public key created by
  applying a cryptographic hash function to a public key. Fingerprints are
  shorter than the keys they refer to, they can be used to simplify certain key
  management tasks.

  # examples
  gpg --output file.txt.gpg --encrypt --recipient foo@"@"domain.org file.txt
  gpg --output file.txt --decrypt file.txt.gpg

  # -A NUM, --after-context=NUM
  # man gpg | grep --after-context=2 show-usage
  Key usage / flags: E=encryption, S=signing, C=certification, A=authentication
}

@block{@block-name{Message / File authentication with GPG}
  M-x helm-info-gnupg
  # see also:
  #   directory: $HOME/.gnupg/
  #   keybox:    $HOME/.gnupg/pubring.kbx

  # On GuixOS:
  guix install pinentry # needed for GnuPG's interface to passphrase input

  # On QubeOS:
  # Create a qube without networking. If 'split-gpg' is needed see
  # https://www.qubes-os.org/doc/split-gpg/ (E.g. The 'qubes-gpg-split' is
  # needed in the vault of the QubeOS).

  # public & private key pair creation:
  gpg --expert --full-generate-key
  # gpg --batch --generate-key ... # see bottom of the webpage:
  # https://www.gnupg.org/documentation/manuals/gnupg/Unattended-GPG-key-generation.html#Unattended-GPG-key-generation

  # creating a key for signing and encrypting is the easiest. Choose:
  # "(9) ECC and ECC" - "can do all" key with Elliptic Curve Cryptography.
  #                   (i.e. ECC primary key and ECC encryption subkey).
  # "(1) Curve 25519" - (this probably annoys the NSA the most :-)

  # Create ASCII armored output. The default is to create the binary OpenPGP
  # format.
  gpg --armor --export > /path/to/pub_key.gpg
  gpg --send-keys --keyserver keyserver.ubuntu.com /path/to/pub_key.gpg

  # create <my-secret-file>.asc
  gpg --clear-sign <my-secret-file>
  # Enter your name and email. Comment is usually left empty.

  gpg --import pub_key.asc
  # check the signature. It may produce several warnings!
  gpg --verify <my-secret-file>.asc | grep --ignore-case "good\|bad"

  # suppress warnings - not recommended
  gpg          --edit-key KEYID trust
  gpg --expert --edit-key KEYID trust
  # can be used to generate subkeys; each of them can have only one purpose.

  # delete keys (add '--with-colons' when writing scripts)
  gpg --list-keys          # add: --keyid-format LONG
  gpg --list-secret-keys   # add: --keyid-format LONG
  gpg --list-keys          # See: gpg --list-secret-keys | grep uid
  gpg --delete-secret-keys [uid1] [uid2] # or keyid1 keyid2
  gpg --delete-keys        [uid1] [uid2] # or keyid1 keyid2

  ASC file is an armored ASCII file contains a digitally signed message
  encoded as plain-text. ASC file includes a key as clear-signed text
  verifiable with gpg.

  @block{@block-name{Message example}
    -----BEGIN PGP SIGNED MESSAGE-----
    Hash: SHA256

    [... Text of the message ...]

    =============================
    Message verification steps:
    1. Obtain the public key:
      gpg --keyserver DNS_SERVER_OR_IP --recv-key KEYID
    2. Save the this whole message to a file: <received-file.txt>
    3. Verify the message authenticity:
      gpg --verify <received-file.txt>

    -----BEGIN PGP SIGNATURE-----
    [...]
    -----END PGP SIGNATURE-----
  }
}

@block{@block-name{Andrew Tropin}
  https://fosstodon.org/@"@"abcdw

  @block{@block-name{gpg explained}
    `gpg key (structure)` is better than `ssh` and `gpg-agent` is better than
    `ssh-agent` GPG key structure: https://youtu.be/4-Ks_f8rQFA?t=370

    Prefer using gpg keys (over ssh keys) for github-related tasks, i.e. signing
    commits and tags, encrypting passwords and authenticating on remote servers.
    It is easier to maintain only one gpg key.

    gpg-agent:
    Secret key management for GnuPG, can emulate ssh-agent
    ~/.gnupg/gpg-agent.conf
    content - see https://youtu.be/4-Ks_f8rQFA?t=2064

    ~/.gnupg/sshcontrol
    ssh git@"@"github.com

    TODO `sudo herd status | rg gpg-agent` is empty???
    See https://youtu.be/4lJaVzxO_Bs?t=1466
    Google-search 'add-home-gnupg-agent-to-shepherd'
  }

  Dev Team Secrets with gpg, git and gopass
  https://www.youtube.com/watch?v=EB9cW9RjiSs&list=PLZmotIJq3yOJab8-of7gMYrXkZyAjWPOw&index=48
}

@block{@block-name{Return-Oriented Programming ROP }
  Exploit technique that allows an attacker to execute code in
  the presence of security defenses such as executable space protection and code
  signing.

  Gain control of the call stack to hijack program control flow and then execute
  instructions already present in the machine's memory, called "gadgets"
}

@block{@block-name{DNS leak}
  When using an anonymity or privacy service:
  All traffic originating from your computer must be routed through the
  anonymity network. If any traffic leaks outside of the secure connection to
  the network, any adversary monitoring your traffic will be able to log your
  activity.
}

@block{@block-name{DNS and IP requests}
  The browser will send a clear text unencrypted DNS request to some DNS servers
  asking basically "So what’s the IP address of www.someserver.com?".

  Even in an incognito Window, using HTTPS and using a private or your own DNS
  service

  The ISP can tamper with the DNS responses even if you are using a private DNS.
  (Man-in-the-middle) Rendering the use of a private DNS service useless.

  Many devices and apps (Smart TVs 70%, Game Consoles 46%) will use hardcoded
  DNS servers bypassing any system setting you could set. For these devices,
  you will have to force them to stop using their hardcoded DNS service which
  could make them stop working properly.

  A solution: encrypted DNS using DoH (DNS over HTTPS38), DoT (DNS over TLS39)
  with a private DNS server (this can be self-hosted locally with a solution
  like pi-hole, remotely hosted with a solution like nextdns.io or using the
  solutions provider by your VPN provider or the Tor network). This should
  prevent your ISP or some go-between from snooping on your requests... except
  it might not help.
}

@block{@block-name{TLS Transport Layer Security}
  protocol; successor of deprecated SSL Secure Sockets Layer
}

@block{@block-name{DNS Domain Name System}
}

@block{@block-name{BGP Border Gateway Protocol}
  Exchange routing and reachability information among autonomous systems (AS) on
  the Internet.

  Routing decisions are based on paths, network policies, or rule-sets
  configured by a network administrator.
}

@block{@block-name{RPKI Resource Public Key Infrastructure}
  a.k.a Resource Certification
}

@block{@block-name{IRR Internet Routing Registry}
  database of Internet route objects for determining, and sharing route and
  related information used for configuring routers, with a view to avoiding
  problematic issues between Internet service providers.
}

@block{@block-name{SNI Server Name Indication}
}

@block{@block-name{ECH Encrypted Client Hello}
  - previously eSNI
  - only Firefox-based browsers supports ECH (Jan 2022); not enabled by default
    ECH and eSNI are not supported (Jan 2022) by:
    Amazon (including AWS, Twitch, ...)
    Microsoft (including Azure, OneDrive, Outlook, Office 365, ...)
    Google (including Gmail, Google Cloud, ...)
    Apple (including iCloud, iMessage, ...)
    Reddit YouTube Facebook Instagram Twitter GitHub
  - encrypt everything end to end (in addition to using a secure private DNS
    over TLS/HTTPS) and hide your DNS requests from a 3rd party.

    OCSP https://en.wikipedia.org/wiki/Online_Certificate_Status_Protocol Part
    of the HTTPS TLS validation, used by Firefox-based browsers.

    OCSP leaks serial number of the certificate of the visited website.
    (Mitigiation - see OCSP stapling)

    CRLSets in Chromium-based browsers is arguably better than OCSP.

 - Brave browser supports all Chrome extensions and offers much better privacy
   than Chrome.
}

@block{@block-name{Secure DNS}
  Traditionally, DNS queries are sent in plaintext. Use a resolver that supports
  secure DNS transport. E.g. DNS over HTTPS (DoH), DNS over TLS (DoT).
}

@block{@block-name{RFID Radio-frequency identification}
  Shopping stores will effectively scan (and log) all RFID chips passing through the
  door. (They might be looking for their loyalty cards)

  If you have an Android smartphone, Google probably knows where it is no matter
  what you do. You cannot really trust the settings. The whole operating system
  is built by a company that wants your data. Remember that if it is free then
  you are the product.

  Movement tracking bases on radio interferences from Wi-Fi access points inside
  buildings.
}

@block{@block-name{IMEI International Mobile Equipment Identity}
  Unique, tied to the phone. It is possible but difficult to change it.
}

@block{@block-name{IMSI International Mobile Subscriber Identity}
  Unique, tied to the phone number. Directly hardcoded on the SIM card, cannot
  be changed.
}

@block{@block-name{in the CPUs}
  IME Intel Management Engine, AMD Platform Security Processor
  Small operating systems running directly on the CPU

  How to Hack a Turned-Off Computer, or Running Unsigned Code in Intel
  Management Engine https://www.youtube.com/watch?v=9fhNokIgBMU
}

@block{@block-name{Metadata}
  Knowing that you had a call from an oncologist before then calling your family
  and friends successively.
}

@block{@block-name{SPF Sender Policy Framework}
  Email authentication method designed to detect forging sender addresses
  during the delivery of the email.
}

@block{@block-name{DKIM DomainKeys Identified Mail}
  Email authentication method designed to detect forged sender addresses in
  email (email spoofing), a technique often used in phishing and email spam.
}

@block{@block-name{Whonix}
  Security focused, based on Kicksecure (= hardened Debian with anonymity
  packages). Contains two virtual machines: Workstation, Tor-Gateway. All
  communication goes through Tor.
}

@block{@block-name{Web types}
  Dark web - offers Tor hidden services
  Deep web - "Google doesn't see it. It's not allowed to or can't find it."
  Examples: Protected by password, dynamic content, exclusion via robots.txt
  Service web
}

@block{@block-name{Tor}
  How TOR Works- Computerphile
  https://youtu.be/QRYzre4bf7I

  .onion pages

  Anonymization - Tor implements Onion Routing Protocol, which works essentially
  as a proxy.

  Encrypted connections bounced / hopped around different routers are difficult
  to track. There are typically 3 hops
  Intermediate nodes - handle only encrypted messages
  Exit nodes - handle fully decrypted messages
  Any node in the tor-network knows only what's before and after them when
  forwarding connections.
  Multiple message encryption
  The client, i.e. the first computer / initial node creates a circuit:
  Establishes a shared key with the next node and instructed the other nodes to
  do the say right up to the request destination

  Unsolwable weaknes of Tor:
  If someone controls the entry and exit node(s) the he or she can see
  everything, via colerating messages, e.g. their timestamps on the entry and
  exit nodes.

  A Cell is a Tor message, it has 512 bytes. Any cell must indistinguishable
  from any other cell.
}

@block{@block-name{Tails}
  live OS with optional persistence, installable on external drives, e.g. DVD,
  USB.
}

@block{@block-name{Qubes OS}
  https://www.qubes-os.org/
  Chat: https://app.element.io/#/room/#cybersec-qubes_os:matrix.org
}

@block{@block-name{Distributed Denial of Secrets}
  https://www.ddosecrets.com/
  Successor of WikiLeaks
  https://en.wikipedia.org/wiki/Distributed_Denial_of_Secrets
}

@block{@block-name{JSON hijacking}
  haacked.com/archive/2009/06/25/json-hijacking.aspx/
}

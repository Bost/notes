#lang notes

@block{@block-name{NFS - Network File System}
  access files over a computer network much like local storage is accessed
}

@block{@block-name{Yggdrasil}
  https://yggdrasil-network.github.io/

  Overlay network implementation of a new routing scheme for mesh networks. It is
  designed to be a future-proof decentralised alternative to the structured
  routing protocols commonly used today on the Internet and other networks.
  https://howto.yggno.de/start

  Network protocol and a computer network organized using it with end-to-end
  encryption and automatic routing. Modes of operation:
  - mesh mode
  - overlay mode

  Connection to the "global" Yggdrasil network. Various services operate in this
  network (web, gaming, forums, IRC , file exchange, etc.), the work of which is
  supported by users and enthusiasts from different countries. There are several
  mirror websites.

  Decentralized network. Your computer/device can be turned into a server even
  behind a NAT. you will have a permanent virtual IP6 address to which you can
  connect as a server directly, which means you can put Socks5 even on a
  computer that is behind NAT.

  Alternative to VPN? TODO clarify anonymity:
  Yggdrasil can be compared to a torus. It can also work through Tor and any Socks5.

  From Yggdrasil FAQs:
  Q: [[https://yggdrasil-network.github.io/faq.html#is-yggdrasil-anonymous][Is Yggdrasil anonymous?]]
  A: No, it is not a goal of the Yggdrasil project to provide anonymity. Direct
  peers over the Internet will be able to see your IP address and may be able to
  use this information to determine your location or identity.
  Multicast-discovered peerings on the same network will typically expose your
  device MAC address. Other nodes on the network may be able to discern some
  information about which nodes you are peered with.
}

@block{@block-name{SOCKS}
  protocol for packet exchange between a client and server through a proxy
  server
}

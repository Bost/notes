#lang notes


@block{@block-name{VA Volumetric Attacks}
  Most common and most devastating.
  User Datagram Protocol (UDP) and Internet Control Message Protocol (ICMP) floods are a common means of carrying out VAs.
  UDP and ICMP are connectionless protocols - for fast data transmission without integrity checks.

  VAs commonly use reflection and amplification techniques to overwhelm the target network/service.
  Network Time Protocol (NTP) reflection and amplification DDOS attack - enlists bots to spoof a target system’s IP address while making NTP requests to legitimate NTP servers. NTP servers reply to the targeted system, which overwhelmes it.
}

@block{@block-name{PA Protocol Attacks}
  Consume all the resources of a given target network/service by sending it numerous successive malformed connection requests.
  A SYN flood is a common protocol attack.
  In a normal three-way handshake that establishes a connection between two computers, the client computer sends the host a SYN request.
  The host acknowledges the SYN request by sending back a SYN-ACK message, and then the client computer acknowledges the SYN-ACK by sending an ACK message to establish the connection with the host.
  In a SYN flood, numerous SYN packets are sent to every port on a targeted server using a spoofed IP address.
  The host responds with a SYN-ACK, but because the initial SYN packets were spoofed, there are no responses from the client.
  Eventually, the host computer’s ports will become overwhelmed with half-open connections and, as a result, legitimate connection requests will be denied.
  In addition to SYN floods, there are a number of other similar protocol attacks including Ping of Death, Smurf DDOS and more.
  PAs consume the processing resources of network equipment such as firewalls, load balancers, and servers and they are typically
  - measured in packets per second (pps).
}

@block{@block-name{ALA Application Layer Attacks}
  ALAs target an organization’s web applications whereby the attacker sends numerous, seemingly legitimate, processing requests to the application.
  These attacks require the application to use CPU and memory resources until those resources are exhausted and the application cannot respond to any more requests.
  In an e-commerce site, processes to add an item to a shopping cart and to check out are computationally expensive.
  Attackers who target these application processes with numerous concurrent requests can exhaust the target system’s resources and crash the server.
  - measured in requests per second (rps).
}

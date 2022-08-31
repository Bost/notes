#lang notes

@block{@block-name{Reproducible Development Environment RDE}
  Author: Andrew Tropin
  - GNU/Linux distribution
  - set of tools for managing:
      (reproducible) development environments
      home environments
      operating systems
   - set of predefined
      configurations
      practices
      workflows
   - can be treated as an Emacs distribution

  https://sr.ht/abcdw/rde
  https://github.com/abcdw/rde (Mirror)

  rde feature - a building block of rde configuration.

  Consists of
  1. Configuration Framework CF
  CF provides Feature, which is a record (a list of key-value pairs) consisting of 3 parts:
  values, home-services, system-services
  the values are shared across the features

  sharing values among services is hard using guix service extension mechanism

  features can provide home-service and system-service getters
     (which allows to use:)
}

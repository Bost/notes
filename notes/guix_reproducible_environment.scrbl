#lang notes

@block{@block-name{Andrew Tropin: RDE Reproducible Development Environment}
  A GNU/Linux distribution. A set of (1.) tools for managing (Reproducible)
  Development Environments, Home Environments, Operating Systems and a set of
  (2.) predefined configurations, practices, workflows. (3.) It can be treated
  as an Emacs distribution

  https://sr.ht/abcdw/rde
  https://github.com/abcdw/rde (Mirror)

  RDE feature - a building block of RDE configuration.

  RDE consists of:
  1. Configuration Framework CF
     CF provides Feature, which is a record (a list of key-value pairs)
     consisting of 3 parts: values, home-services, system-services the values
     are shared across the features

  sharing values among services is hard using guix service extension mechanism

  features can provide home-service and system-service getters
     (which allows to use:)
}

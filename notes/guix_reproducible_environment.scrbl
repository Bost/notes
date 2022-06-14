#lang notes

#+title: Reproducible Environment

@block{@block-name{RDE}
  Consists of
  1. Configuration framework CF
  CF provides Feature, which is a record (a list of key-value pairs) consisting of 3 parts:
  values, home-services, system-services
  the values are shared across the features

  sharing values among services is hard using guix service extension mechanism

  features can provide home-service and system-service getters
     (which allows to use:)
}

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

  Andrew Torpin: guix shell: Overview - Notes
  https://github.com/abcdw/notes/blob/master/notes/20211111141408-guix_shell_overview.org

  feature-emacs-eglot ;; lsp-interface for emacs
  feature-clojure     ;;

  @block{@block-name{RDE channel lock / channel freeze}
    YouTube: Andrew Torpin: guix shell: Overview
    https://youtu.be/UMCHuHSlVWk?t=1622
    # full freeze of Guix channels to the versions defined in 'channels.scm'
    guix describe --format=channels > ./channels.scm
    guix time-machine --channels=./channels.scm -- shell REST-OF-GUIX-SHELL-ARGS
  }

  Use rde as a Channel:
  Advanced users may prefer to use only parts of rde they are interested:
  features, services, or just packages. In that case add the following channel
  to channels.scm.
  @lisp{
    (cons*
     (channel
      (name 'rde)
      (url "https://git.sr.ht/~abcdw/rde")
      (introduction
       (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
         "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
     %default-channels)
  }

}

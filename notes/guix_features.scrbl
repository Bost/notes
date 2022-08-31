#lang notes

[[https://ambrevar.xyz/guix-advance/]]

@block{@block-name{Multiple Outputs}
  A package can have multiple “outputs” that serves as a mean to separate
  various component of a program (libraries, extra tools, documentation, etc.).

}

@block{@block-name{Non-propagated Inputs}
  “Inputs,” in Guix parlance, are the package
  dependencies. The user profile and environment only contains the packages the
  user explicitly installed and not necessarily the dependencies of those
  packages.
}

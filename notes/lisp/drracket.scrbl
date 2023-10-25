#lang notes

@block{@block-name{DrRacket}
  DrRacket Plugins
  https://plt.cs.northwestern.edu/snapshots/current/pdf-doc/tools.pdf
  Descriptions of DrRacket’s plugins interface.

  The simplest and best way to extend DrRacket with support for a new language is
  to implement the language via #lang

  DrRacket Scripting
  https://github.com/Metaxal/quickscript

  GRacket:
  GUI-application variant of Racket / GUI Racket implementation. Just a simple
  window for evaluating Racket expressions. Use DrRacket instead of GRacket.

  @block{@block-name{IDE}
    raco pkg install --auto sauron
    raco pkg install https://github.com/tuirgin/drracket-spacemacs-schemes.git
  }
}

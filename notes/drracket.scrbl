#lang notes

#+title: DrRacket

@block{@block-name{DrRacket}
  @hyperlink["https://plt.cs.northwestern.edu/snapshots/current/pdf-doc/tools.pdf"]{DrRacket Plugins}
  Descriptions of DrRacket’s plugins interface.

  The simplest and best way to extend DrRacket with support for a new language is
  to implement the language via #lang

  @block{@block-name{IDE}
    #+BEGIN_SRC bash :results output
    raco pkg install --auto sauron
    raco pkg install @url{https://github.com/tuirgin/drracket-spacemacs-schemes.git}
    #+END_SRC
  }

  @block{@block-name{Scripting}
    @url{https://github.com/Metaxal/quickscript}
  }

  @block{@block-name{GRacket}
    GUI-application variant of Racket / GUI Racket implementation
    Just a simple window for evaluating Racket expressions. Use DrRacket instead of GRacket.
  }
}

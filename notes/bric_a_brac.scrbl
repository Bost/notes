#lang notes

@block{@block-name{Erlang and Elixir}
  https://drive.google.com/file/d/1-Ktouz3lh5uNninCC_9GNsm8tJ8IVCog/view
  Elixir:
  - functional, concurrent, general-purpose programming language
  - for scalable and maintainable applications, network programming, web
    development, IoT etc.
  - compiled to run on the Erlang VM (BEAM)
  - dynamic typing
  - concurrency doesn't require callbacks, async annotations etc.
  - dichotomy between monolith/microservices removed - same code.
  - not good CPU intensive apps: e.g. video games
  - can interface with e.g. C, Rust
  - no difference between assignment and pattern matching

  Erlang:
  - Virtual Machine (BEAM)
  - ecosystem for building fault-tolerant, distributed, multicore, highly
    available systems
  - initially developed for telecommunication systems
  - lightweight processes, message-passing concurrency model, hot code swapping,
    and preemptive scheduling.

}

@block{@block-name{Elixir language}
  no difference between assignment and pattern matching
  pin operator

  lightweith threads (not OS threads)
  all data between processes are shared only with message passing
  per-process garbage collection
  pre-emptive threading

  distributed message passing

  GenServer (short for Generic Server):
  - module, part of the Elixir standard library
  - provides an abstraction for building concurrent, fault-tolerant server
    processes that communicate using message passing.

  - process + state + standard functions expecting messages + message queue
  - long-running
  - Clients interact with GenSever

}

@block{@block-name{Communication protocols}
  imply SW architectural style for distributed environments
  REST (Representational State Transfer) accesses data
  SOAP (Simple Object Access Protocol) performs operations

  @block{@block-name{RESTful API services}
    | HTTP method | CRUD equivalent |
    |-------------+-----------------|
    | GET         | Read            |
    | POST        | Create          |
    | PUT         | Update          |
    | DELETE      | Delete          |
  }
}

@block{@block-name{Various}
  CORS Cross-Origin Resource Sharing
  chrome://flags/#block-insecure-private-network-requests

  CSS Cascade Style Sheets
  The @"@"media rule - different styles for different media types/devices.

  Xen Type-1 hypervisor
  Provides services that allow multiple computer operating systems to execute on
  the same computer hardware concurrently.
}

@block{@block-name{Video Editing}
  Racket's video language
  https://lang.video/

  On Guix: guix build -m render-videos.scm
  https://guix.gnu.org/en/blog/2021/reproducible-data-processing-pipelines/

  BigBlueButton
  https://bigbluebutton.org/

  @block{@block-name{Record terminal session}
    #+BEGIN_SRC shell
    asciinema rec
    #+END_SRC
    in Emacs:
    #+BEGIN_SRC emacs-lisp
    (cl-macrolet ((at (d &rest b)
                      `(run-at-time ,d nil (lambda () (progn ,@"@"b)))))
      (pop-to-buffer-same-window
       (erc :server "irc.libera.chat"
            :port 6667
            :nick "ddWfnZ7G"
            :password ""
            :full-name ""))
      (at 3 (execute-kbd-macro "/join #channelddWfnZ7G"))
      (at 4 (execute-kbd-macro "\r"))
      (at 5  (call-interactively 'eval-expression))
      (at 6  (execute-kbd-macro "(setq erc-fill-static-center 10)"))
      (at 7  (execute-kbd-macro "\r"))
      (at 8  (call-interactively 'execute-extended-command)) ; simulate M-x
      (at 9  (execute-kbd-macro "erc-mode"))
      (at 10 (execute-kbd-macro "\r")))
    #+END_SRC
  }
}

@block{@block-name{Tools for thinking}
  Kinopio
  https://kinopio.club/

  For Political Sciences
  https://www.juliendesrosiers.com/2021/08/21/tools-for-thinking.php

  org-roam-graph
}

@block{@block-name{Solaris}
  # checksum
  /usr/bin/digest -a sha1
  # wget location
  /usr/sfw/bin/wget
  # full command line (needs: sudo rootsh -i -u ... )
  /usr/ucb/ps -auxww
  # displays information about processors
  psrinfo
  # net: ipconfig
  /usr/sbin/ifconfig -a
}

@block{@block-name{Eclipse}
  .metadata/.plugins/org.eclipse.team.cvs.ui/repositoriesView.xml
  #
  METADA_CORE=.metadata/.plugins/org.eclipse.jdt.core;
  # Clean history
  rm -rf .metadata/.plugins/org.eclipse.core.resources/.history;
  # Clean metadata
  rm $METADA_CORE/*.index $METADA_CORE/savedIndexNames.txt;
  # Use this in find-replace dialogue to remove trailing whitespaces
  [\\t ]+$
  # Type syso/sysout and ctrl + space for System.out.println()
  syso/sysout
  # Jump to next error
  Ctrl-.
}
@block{@block-name{IRC}
  https://libera.chat/guides/registration
  Usefull commands:
  /connect irc.libera.chat 6667 YourNick:YourPassword
  /nick YourNick
  /msg NickServ IDENTIFY YourNick YourPassword
  /msg NickServ GROUP
}

@block{@block-name{World Wide Web}
  IPFS InterPlanetary File System
  powers the Distributed Web A peer-to-peer hypermedia protocol designed to
  preserve and grow humanity's knowledge by making the web upgradeable,
  resilient, and more open.
  Uses content-addressing / CID Content Identifier
  https://ipfs.tech/#how
  Content can be pined i.e. saved ("forever") or just cached for some time.
  IPNS InterPlanetary Name System

  HTTP / HTTPS - Hypertext Transfer Protocol
  also hypermedia server protocols
  Location based
}

@block{@block-name{OSE Open Source Ecology}
  Wikipedia: Open Source Ecology
  https://en.wikipedia.org/wiki/Open_Source_Ecology
  Network of farmers, engineers, architects and supporters, whose main goal is
  the eventual manufacturing of the Global Village Construction Set (GVCS)
}

@block{@block-name{Smart the collective brain}
  TED Talk: When ideas have sex
  https://www.ted.com/talks/matt_ridley_when_ideas_have_sex
  The engine of human progress has been the meeting and mating of ideas to make
  new ideas. It's not important how clever individuals are, what really matters
  is how smart the collective brain is.
  No one knows how to make a pencil. The person who chops the trees doesn't know
  how to mine graphite.
  We created the ability to make things we don't even understand.
  It's not the same with language. With language we have to transfer ideas we
  understand.
  It's completely irrelevant if some groups have higher IQ than other groups.
  Relevant is how well people are communicating their ideas and how well they
  are cooperating. Not how clever the individuals are.
}

@block{@block-name{Better Ideas}
  Some DSL languages are too-limiting as programming languages.
  Guix: A most advanced operating system
  https://ambrevar.xyz/guix-advance/index.html
  |                                                 | better ideas                          |
  |-------------------------------------------------+---------------------------------------|
  | Bash, Zsh, Fish                                 | Eshell, Scheme Shell (scsh)           |
  | Octave, R, PARI/GP, most scientific SW          | Common Lisp, Racket and other Schemes |
  | Regular expressions                             | Emacs’ rx, Racket’s PEG, etc.         |
  | Most init system configurations, incl. systemd  | GNU Shepherd                          |
  | cron                                            | mcron                                 |
  | TeX, LaTeX (and all the derivatives), Asymptote | scribble, skribilo                    |

  mcron - see also https://github.com/leahneukirchen/snooze

  Eshell as a main shell
  https://ambrevar.bitbucket.io/emacs-eshell/  (Link rotted)
}

@block{@block-name{Second system syndrome / effect}
  small, elegant, and successful systems to be succeeded by an over-engineered,
  bloated systems, due to inflated expectations and overconfidence.
}

@block{@block-name{Why Do Nigerian Scammers Say They are From Nigeria?}
  It works like a filter. If you believe them, you passed it. And you're likely
  stupid enough to do anything they ask you for. IOW send them money. If you
  don't pass it then it's probably not worth effort to flirt with you.
}

@block{@block-name{Terminals}
  A terminal for a more modern age
  https://github.com/Eugeny/terminus

  Electron-app terminal; scriter - electron alternative
  https://github.com/zeit/hyper
}

@block{@block-name{Google Advanced / Extended Search}
  guix-home-legacy-configs-backup site:logs.guix.gnu.org
  # old discussions
  bost site:logs.guix.gnu.org
  #
  guix filetype:pdf
  define:dictionary
  (beer OR wine) AND cheese
  # search for a job but not at Evil Inc
  job -evil
  # wildcard - will match any word or phrase
  anything * between
  # Return the most recent cached version of a web page
  cache:guix.gnu.org
}

@block{@block-name{Bric a brac}
  shrug ¯\_(ツ)_/¯

  Nikita Voloboev: Everything I Know
  https://wiki.nikiv.dev/

  I want to learn / Learn anything
  https://learn-anything.xyz/

  Five rating emojis / icons
  https://youtu.be/hZJnlxM0c0I?t=162

  Solution of a Differential Equations (DE) is a function, not value
  National Renewable Energy Laboratory: Energy-Cell Efficiencies
  https://youtu.be/czL0ZSscbsM?t=II709
  Fractals
  http://blog.sciencevsmagic.net/science/fractal-machine/
  Go over the book from the Prof. from Karlsruhe

  http://www.zdrojak.cz/clanky/tvorba-moderniho-e-shopu-dokonceni-uzivatelske-casti/
  Sequence of QR Codes / graphical elements (pyramid, cube, sphere, toroid, etc.)

  time bank
  dokaz identity bez udania mena
  google lab tags
  http://www.chromeweblab.com/en-GB/lab-tag-explorer (link rotted)

  Pure vs. mostly harmless functions

  # TODO org-mode-tagging; following doesn't work
  # :org-mode-tagging:                                                      :org:
  # | col1    | col2          |
  # |---------+---------------|
  # | content | other content |
  # :end:
}

@block{@block-name{Languages}
  http://www.ted.com/talks/patricia_kuhl_the_linguistic_genius_of_babies.html?fb_ref=talk
  Learning pronounciation

  How much of a language is silent? What does it look like when you take the
  silence out? (Video 2 Min)
  http://www.theverge.com/2012/7/1/3129227/silenc-project-silent-letters
  http://golancourses.net/ciid/19/silenc/

  idioms, link sur FB, idioms - traduction + correcture
  spelling glyphs
  Comprehension (facile) vs S'exprimer (difficile)
  linguee.com - eu-texts
  le Svejk, l'Etrangeur - examples
  Communication w-/wo- Letters
}

@block{@block-name{Simulation and Emulation}
  What is the difference between a simulator and an emulator?
  https://www.tutorialspoint.com/what-is-the-difference-between-a-simulator-and-an-emulator
  Simulator mimics while Emulator performs
}

@block{@block-name{YouTube}
  | ~>~ | speed 25% faster      |
  | ~<~ | speed 25% slower      |
  | ~c~ | toggle captions       |
  | ~m~ | toggle mute           |
  | ~,~ | move 1 frame forward  |
  | ~.~ | move 1 frame backward |

  YouTube: Making a bolt with double threads
  https://www.youtube.com/watch?v=v96LTfmtDPU&t=626s
}

@block{@block-name{code_aster}
  Finite-Elemente-Method and Numerical Simulation Software
  Structures and Thermomechanics Analysis for Studies and Research
  Forum post: Code_Aster inside a Docker container
  https://www.code-aster.org/forum2/viewtopic.php?id=23453

  MPI Message Passing Interface
  https://de.wikipedia.org/wiki/Message_Passing_Interface
  for parallel computing on distributed systems

  Singularity
  https://singularity.hpcng.org/
  Container System for HPC High Performance Computing Systems
  "Docker for HPC"

  HPC High Performance Computing
}

@block{@block-name{Terminal / Video Recorder / Screen Casting}
  Simple animated GIF screen recorder with an easy to use interface
  https://github.com/phw/peek

  Terminal session recorder
  https://github.com/asciinema/asciinema

  Record your terminal and generate animated gif images or share a web player
  https://github.com/faressoft/terminalizer
}

@block{@block-name{Bike}
  Prepare routes: View Ranger
  https://my.viewranger.com/user/routes/myroutes

  @block{@block-name{Weather}
    Stuttgart
    https://www.wetter.com/deutschland/stuttgart/DE0010287.html
    München
    https://www.wetter.com/deutschland/muenchen/DE0006515.html
  }

  @block{@block-name{Cafe Fietsen}
    Ask about service
    True the wheels
  }

  @block{@block-name{Repair and Prepare}
    Swap normal for tubeless tires
    Baumarkt - Socket for the Nigrin Reifen Dicht (Buy Bildschirmreiniger)
    Cut plasters

    @block{@block-name{Bicycle chain}
      YouTube: When to replace chain
      https://youtu.be/gXd-3UnqoaM?t=37

      | stretch | nr-of-gears | verdict |
      |---------+-------------+---------|
      |    0.5% | >= 11       | replace |
      |   0.75% | <= 10       | replace |
      |      1% | any         | replace |

      YouTube: Measure Chain Wear
      https://youtu.be/FzyRCcjRuu0?t=98
      YouTube - GCN Show: Measure Chain Wear
      https://youtu.be/a0xdsTQaFtg
      YouTube - GCN Show: Replace A Bicycle Chain
      https://youtu.be/rWchudX-Tqs

      Chain length - number of speeds
      Chain tools - ask: Is mine the right one?

      TODO How to replace chain - chain tool
      TODO How to adjust speeds
    }
  }
}

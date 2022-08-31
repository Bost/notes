#lang notes

@block{@block-name{Record terminal session}
  @hyperlink["https://asciinema.org/"]{asciinema}
  #+BEGIN_SRC shell
  asciinema rec
  #+END_SRC
  in Emacs:
  #+BEGIN_SRC emacs-lisp
  (require 'erc)
  (cl-macrolet ((at (d &rest b) `(run-at-time ,d nil (lambda () (progn ,@"@"b)))))
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
    (at 10 (execute-kbd-macro "\r"))
    )
  #+END_SRC
}

#lang notes

@block{@block-name{Guile Scripting}
  https://www.gnu.org/software/guile/manual/html_node/Guile-Scripting.html
  On Guix (see `which guile`):

  @block{@block-name{Hash-bang / She-bang variants}
    #!/run/current-system/profile/bin/guile \
    -l utils.scm -e (<some-module-name>) -s
    !#
    ;;;
    #!/usr/bin/guile \
    -l utils.scm -e (<some-module-name>) -s
    !#
    ;;;
    ;;; -S, --split-string=S  process and split S into separate arguments.
    ;;; Used to pass multiple arguments on shebang lines. E.g.:
    ;;;   env             -S "echo foo bar"
    ;;;   env --split-string "echo foo bar"
    ;;; However '--split-string' doesn't work, but '-S' does work.
    #!/usr/bin/env -S guile \\
    -l utils.scm -e (<some-module-name>) -s
    !#
    ;;;
    #!/run/current-system/profile/bin/guile \
    -l utils.scm -e (<some-module-name>) -s
    !#
    ;;; $HOME variable can't be used
    #!/home/<username>/.guix-home/profile/bin/guile \
    -l utils.scm -e (<some-module-name>) -s
    !#
  }

  @block{@block-name{Example}
    #!/run/current-system/profile/bin/guile \
    -l utils.scm -e (exec) -s
    !#

    (write (cdr (command-line)))
    (newline)

    ;; `format` is from (use-modules (ice-9 format))
    ;; Read environment variable and print it:
    ;; ~a outputs an argument like `display`
    (format #t "~a\n" (getenv "HOME"))
    ;; ~a outputs an argument like `write`
    (format #t "~s\n" (getenv "HOME"))

    ;; https://www.draketo.de/software/guile-capture-stdout-stderr.html
    (use-modules (ice-9 rdelim)
                 (ice-9 popen))
    ;;
    (define (read-all port)
      "Return a list of all lines from the PORT."
      (let loop ((res '())
                 (str (read-line port)))
        (if (and str (not (eof-object? str)))
            (loop (append res (list str))
                  (read-line port))
            res)))
    ;;
    (define* (exec command #:rest args)
      "The command must have only one line output. TODO improve it"
      (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
             (result (read-line port))) ; from (ice-9 popen)
        (cons
         (status:exit-val (close-pipe port))
         result)))
  }
}

@block{@block-name{Linking Scheme code together}
  - at run-time:
    load - doesn't work within nested lexical contexts
    load-compiled
  - at expansion-time:
    include - works within nested lexical contexts
    include-from-path
}

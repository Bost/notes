#lang notes

@block{@block-name{Guile Scripting}
  https://www.gnu.org/software/guile/manual/html_node/Guile-Scripting.html
  See also $dgx/etc/teams.scm
  On Guix (see `which guile`):

  @block{@block-name{Guile Scripting: Hash-bang / She-bang variants}
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

    ;; Formatted output like fprintf
    ;; available from (use-modules (ice-9 format))
    ;; Read environment variable and print it:
    ;; '#t' 'a:'~ outputs an argument like `display` to the current output port
    (format #t "~a\n" (getenv "HOME"))
    ;; '#t' 'a:'~ outputs an argument like `write` to the current output port
    (format #t "~s\n" (getenv "HOME"))
    ;; https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html
    ;; https://www.gnu.org/software/guile/docs/docs-1.6/guile-ref/Formatted-Output.html
    (format #t "~a\n" (getenv "HOME")) ;; => #t print to current output port
    (string? (format #f "~a\n" (getenv "HOME"))) ;; => #t ;; i.e. print to string

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

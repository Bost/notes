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

    ;; Read environment variable
    (getenv "HOME")
    (getenv "UNDEF") ; => #f

    ;; Formatted output like fprintf from (use-modules (ice-9 format))
    ;; https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html
    |                        | Destination       | Style     | Result        |                                 |
    |------------------------+-------------------+-----------+---------------+---------------------------------|
    | (format #t "~a\n" "x") | stdout (terminal) | aesthetic | x (newline)   | Print readable form to terminal |
    | (format #t "~s\n" "x") | stdout (terminal) | standard  | "x" (newline) | Print with quotes to terminal   |
    | (format #f "~a\n" "x") | return string     | aesthetic | "x\n"         | Return readable form as string  |
    | (format #f "~s\n" "x") | return string     | standard  | "\"x\"\n"     | Return with quotes as string    |
    ;;
    ;; #t outputs to current output port (usually terminal), #f returns a string
    ;; ~a gives aesthetic/readable format, ~s gives standard/quoted format (like `write` vs `display`)
    ;; ;; Use ~a for human-readable output, ~s for machine-readable/serializable output

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

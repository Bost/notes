#lang notes

@block{@block-name{Shells: Bash & Fish-shell}
  # check if a file contains only binary zeros
  # https://stackoverflow.com/a/20226139/5151982
  # bash
  <file.ext      tr --delete '\0' | read --nchars 1 || echo "All zeroes."
  cat file.ext | tr --delete '\0' | read --nchars 1 || echo "All zeroes."
  # fish-shell
  cat file.ext | tr --delete '\0' | read --nchars 1 \
      && echo -e "\nHas some content." || echo "All zeroes."

  # include other script; also in bash
  source /pth/to/script

  # reload config
  source ~/.config/fish/config.fish
  source ~/.bashrc

  # webconfig.py
  fish_config

  # fish-shell - reload function / alias
  type myfunc

  # shell types; see
  # https://unix.stackexchange.com/a/50667
  # https://unix.stackexchange.com/a/46856
  #
  # Interactive: commands are run with user-interaction from keyboard.
  # E.g. the shell can prompt the user to enter input.
  #
  # Non-interactive: the shell is probably run from an automated process so it
  # can't assume if can request input or that someone will see the output. E.g
  # Maybe it is best to write output to a log-file.
  #
  # Login: shell is run as part of the login of the user to the system. Typically
  # used to do any configuration that a user needs/wants to establish his
  # work-environment.
  #
  # Non-login: Any other shell run by the user after logging on, or which is run
  # by any automated process which is not coupled to a logged in user.

  # Where is PATH variable set? https://askubuntu.com/a/706069/401596
  grep --color -H 'PATH=' \
    ~/.bashrc \
    ~/.profile \
    ~/.bash_profile \
    ~/bash.login \
    ~/.bash_aliases \
    /etc/bash.bashrc \
    /etc/profile \
    /etc/skel/* \
    /etc/skel/.* \
    /etc/profile.d/* \
    /etc/profile.d/.* \
    /etc/environment \
    ~/.guix-home/* \
    ~/.guix-home/.* \
    ~/.guix-profile/* \
    ~/.guix-profile/.* \
    ~/.guix-profile/etc/profile \
    ~/.config/guix/current/etc/* \
    ~/.config/guix/current/etc/.* \
    ~/.config/guix/current/etc/profile \
  2> /dev/null

  # available shells; current shell; change shell
  cat /etc/shells; echo $SHELL; chsh --shell /usr/bin/fish

  # bash fish-shell - sequence from 0 to 10 (both included) increment by 2
  seq 0 2 10

  # bash
  # remove line from shell history (i.e. password) see also ~/.bash_history
  history -d

  # fish-shell see also ~/.config/fish/fish_history
  history delete --contains <substring>
  history delete --contains "history delete --contains"

  # bash fish-shell
  # see what the shell does with the various types of quoting
  # https://unix.stackexchange.com/a/417408
  printf -- '<%s>\n' G "G" 'G' \G "\G" '\G' \\G "\\G" '\\G'

  # bash
  # secure (password) prompt; doesn't work in fish
  read -s

  # fish-shell retval retcode return-code exit-code (in bash $?)
  # retcode interpretation / meaning
  # https://fishshell.com/docs/current/language.html#variables-status
  $status

  # indicate how a command would be interpreted
  type --all <cmd> # all of possible definitions of <cmd>

  # fish-shell
  # show content of foo fn / list fns
  type foo / functions foo / functions -n

  # fish-shell
  functions # list available functions
  abbr      # list available abbreviations
  alias     # list available aliases
  # copy 'foo' fn to a new fn 'bar' / erase the 'bar'
  functions -c foo bar / functions -e bar

  # fish-shell variables
  # unset a shell var
  set --erase myvar        # set -e myvar
  # var scope is local to the current block
  set --local myvar 1      # set -l myvar 1
  # var exported to all child processes (environmetal var)
  set --export  myvar 1    # set -x myvar 1
  set -unexport myvar      # set -u myvar
  # var shared between all current user's fish instances on the current computer
  # preserved across restarts of the shell
  set --universal myvar 1  # set -U myvar 1
  # show info about a var
  set --show myvar         # set -S myvar
  # show info about all vars
  set --show               # set -S

  # fish-shell - all function arguments from 3rd to the last
  $argv[3..-1]

  # fish-shell existence-tests
  test (string escape -- $argv) = "--switch" # string equality / compare
  test -e /path/to/file.txt                  # file exists
  test -L /path/to/link                      # symbolic link exists
  test -d /path/to/dir                       # directory exists
  # true if the length of $myvar is non-zero i.e. non-empty string
  # https://stackoverflow.com/a/47743269; always use "" around the myvar
  test -n "$myvar" && echo "true: defined-and-non-empty: '$var'" || echo "false: undef-or-empty"
  # true if the length of $myvar is zero i.e. empty string
  test -z "$myvar" && echo "true: undef-or-empty" || echo "false: defined-and-non-empty: '$myvar'"
  set -q myvar && echo "myvar is set to '$myvar'" || echo "myvar is unset"
  test -z "$myvar" && echo "myvar is empty: '$myvar'" || echo "myvar is not empty: '$myvar'"

  # bash-shell existence-tests
  # ${var+x} is a parameter expansion which evaluates to nothing if var is
  # unset, and substitutes the string x otherwise.
  # variable blank vs. unset - see https://stackoverflow.com/a/13864829
  if [ -z ${myvar+x} ]; then echo "myvar is unset"; else echo "myvar is set to '$myvar'"; fi
  test -z ${myvar+x} &&  echo "myvar is unset" || echo "myvar is set to '$myvar'"

  # bash string equality / compare
  # See https://tldp.org/LDP/abs/html/comparison-ops.html
  # using double brackets '[[' and ']]' is a bashishm
  [[ $a == z* ]]   # True if $a starts with an "z" (pattern matching)
  [[ $a == "z*" ]] # True if $a is equal to z* (literal matching)
  [ $a == z* ]     # File globbing and word splitting take place
  [ "$a" == "z*" ] # True if $a is equal to z* (literal matching)

  # bash: split a string into an array https://stackoverflow.com/a/10586169
  unset str && str="a   b   c"
  fmtStr=$(echo $str | sed 's#\s#;#g')
  echo "fmtStr: $fmtStr"
  IFS=';' read -r -a array <<< "$fmtStr"
  for idx in $(seq 0 $[${#array[@"@"]} - 1]); do
      echo "$idx: ${array[$idx]}"
  done

  # bash operator: =~ equal tilde operator: test variable against a regex
  unset myvar && myvar="some42"
  if [[ "$myvar" =~ ^[a-z]*[0-9] ]]; then
      echo "Match"
  else
      echo "No match"
  fi

  # bash: test if variable is an array https://stackoverflow.com/a/27254437
  # Warning: if variable is once declared as array/non-array it's not enough
  # just to reassign it a new value. It must be unset!
  unset myvar && myvar=("a" "b")
  unset myvar && myvar="ab"
  declare -p myvar 2> /dev/null | grep -q '^declare \-a' && \
          echo "Array" ||  echo "No array"
  # this doesn't work:
  # test "$(declare -p myvar)" =~ "declare -a" && echo "array" || echo "no array"

  # compute calculate fish-shell
  # examples https://nicolas-van.github.io/programming-with-fish-shell
  math "1 + 2"
  set jobs (math round (nproc) \* 0.9) # should be 22 on ecke
}

@block{@block-name{Bash double brackets}
  https://stackoverflow.com/a/3427931/5151982
  bash extension. In sh-compatible scripts only the single bracket must be used
  (i.e. use the #!/bin/bash shebang line if using use double brackets)

  # correctly handle empty strings or file names with spaces in them:
  [ -f "$file" ] && printf -- "is a regular file\n" || printf -- "else ...\n"
  [[ -f $file ]] && printf -- "is a regular file\n" || printf -- "else ...\n"

  # double brackets lets you use && and || operators for boolean tests and < and >
  # for string comparisons. single bracket cannot do this.

  # =~ does regular expression matches
  [ "$answer" = y -o "$answer" = yes ] && printf -- "then...\n" || printf -- "else...\n"
  [[ $answer =~ ^y(es)?$ ]]            && printf -- "then...\n" || printf -- "else...\n"

  # pattern matching aka globbing for free. Maybe you're less strict about how to
  # type yes. Maybe you're okay if the user types y-anything:
  [[ $ANSWER = y* ]] && printf -- "then...\n" || printf -- "else...\n"
}

@block{@block-name{Key bindings / shortcuts}
  # fish shell key bindings / shortcuts in the...
  # Ctrl+l - clear
  # Ctrl+r - search history
  # \t - TAB, \e - Alt (Esc), \cy - Ctrl+y
  # Ctrl+i - TAB, Ctrl+j - newwline (\n)
  # \el __fish_list_current_token
  # \eo __fish_preview_current_file
  # \ew __fish_whatis_current_token
  # \ck kill-line
  # \cu backward-kill-line

  bind       # ... console
  help bind  # ... web browser
  # list of available shell commands
  /etc/inputrc

  # bash key bindings / shortcuts including
  # Ctrl+l - clear
  # Ctrl+r - search history
  # \t TAB, \e Alt (Esc), \C-y Ctrl+y
  bind -P    # -P List function names and bindings
  help bind

  # bash undo & yank
  # type partial cmd, kill this cmd, check something you forgot, yank the cmd,
  # resume typing
  C-u ... C-y / Ctrl-u ... Ctrl-y

  # bash history
  C-r / Ctrl-r
  # bash abort history
  C-g / Ctrl-g

  # get the parameter / argument of the last command. see bind -P
  M-. / Alt-.
  Esc-.

  # bash swap words
  M-t / Alt-t

  # alias escape command aliases
  \\\[command\]

  # bash set vi bindings
  set -o vi
  # bash disable pathname expansion - pattern matching aka globbing
  set -f
  set -o noglob

  # bash shell writes its input to standard error as it is read
  set -v
  set -o verbose

  # bash shell writes standard error a trace for each command
  set -x
  set -o xtrace

  # scripting loc_variable - visible only within given code block
  local loc_variable=value

  # bash args
  $*   # function arguments
  $@"@"   # all arguments
  !*   # all arguments of the last command
  $$   # TODO check: process ID of the shell / count of arguments
  $!   # process ID of the most recently executed background process
  !$   # last argument of the last command
  $?   # last cmd exit / return code / retcode (0: success); adduser joe; echo $?
  !:-  # last command without the last argument
  :    # if; no-op, nope, empty operation
  > file.txt  # empty file.txt
  $-   # use(?) build-in commands
  # last argument of the previous command. At the shell startup, it gives the
  # absolute filename of the shell script being executed
  $_

  # the cmd takes x and y as if they were pressed during its execution
  (echo x; echo y) | cmd

  # eval expression
  echo $[22 + 33]
  expr 11 + 22
  # ERROR: expr: non-integer argument
  expr $(nproc) \* 0.9
  # ERROR: syntax error: invalid arithmetic operator (error token is ".9")
  echo $[$(nproc) * 0.9]
  echo $[$(nproc) * 95 / 100] # workaround for the above problem

  # bash insert contents of file.txt into input of tr and output results to
  # fileNew.txt
  tr '[A-Z]' '[a-z]' < file.txt > fileNew.txt

  # bash mass file move / copy / rename
  mmv \*.JPG \#1.jpc
  mmv \* \#1.rexx

  # bash visual calender for...
  cal 2 2004   # ... februar 2004
  cal -y 2004  # ... the whole year 2004

  # bash ? define function in bash ?
}

@block{@block-name{Shebang}
  https://rosettacode.org/wiki/Native_shebang
  https://rosettacode.org/wiki/Multiline_shebang

  # shebang / hashbang
  # -e   bash: stop the script after any error
  #!/bin/bash -e
  # -x   bash: debug / trace execution steps
  # set -x; stop on error: set -e
  #!/usr/bin/env fish
}

@block{@block-name{Various commands}
  # bash debug script
  bash -x script

  # fish redirect with grep - see also psub
  # example: transfer /tmp/foo to a virtual machine
  set remoteShell "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p 10022"
  set noWarn "Permanently added '\[localhost\]:10022'"
  rsync -avz --rsh="$remoteShell" /tmp/foo $USER@"@"localhost:/tmp/ 2>&1 | grep -v "$noWarn"
  # or alternatively, suppress all warnings
  set remoteShell "ssh -o LogLevel=ERROR -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p 10022"
  set noWarn "Permanently added '\[localhost\]:10022'"
  rsync -avz --rsh="$remoteShell" /tmp/foo $USER@"@"localhost:/tmp/

  # bash redirect stderr (2) to stdout (1) and save it to command.log
  ./command.sh 2>&1 | tee command.log
  # suppress stderr messagess
  ./script.sh 2> /dev/null
  # separate / combine sdterr and stdout; doesn't work with the tee command
  ./command.sh 1>str.out 2>str.err / ./command.sh &>combined.out
  # type in stuff and wait unit EOF gets typed
  cat >>EOF

  # avoid backticks
  echo "Date is: $(date +%D)"

  # create a script from last executed cmd
  echo "!!" > foo.sh

  # time measurement of a fish function
  # https://github.com/fish-shell/fish-shell/issues/117
  /usr/bin/time --portability fish --command <fn> <prm1> <prm2> ...
  # TODO try out
  function time --description 'Wrapper for time'
  /usr/bin/time --portability /usr/bin/fish --command $argv
  end
  # see also:
  <fn> <prm1> <prm2> ...
  echo $CMD_DURATION

  # xfce: launcher: emacs uses bash variables; -i interactive shell, -c read
  # following command
  bash -i -c ./pth/to/emacs

  # fish-shell bash locate command
  command -v <command>  # fish buildin
  which      <command>  # debian

  # cygwin bash
  # print windows form of filename
  cygpath -w filename

  # bash eval string

  # bash
  # bugs in bash/sh scripts http://www.shellcheck.net/
  sudo apt install shellcheck

  # bash fish-shell
  # help text that matches each argument
  http://explainshell.com/

  # Show numerical values for each of the 256 colors in bash

  # iterate / loop through files in a directory
  for file in /path/to/dir/*.ext; do echo $file; done

  # fish-shell loop over some range
  for i in (seq 50 55); printf "i: %s\n" $i; end

  # syntax - single / double brackets; variables
  https://unix.stackexchange.com/a/416716
  https://www.thegeekstuff.com/2010/06/bash-conditional-expression/
  https://www.cyberciti.biz/faq/unix-linux-bash-script-check-if-variable-is-empty/
  https://www.cyberciti.biz/faq/linux-unix-howto-check-if-bash-variable-defined-not/

  # FILE1 -ot FILE2: FILE1 is older than FILE2
  #        -b FILE:  FILE exists and it's block special
  #        -c FILE:  FILE exists and it's character special
  #        -d FILE:  FILE exists and it's a directory
  #        -e FILE:  FILE exists
  #        -f FILE:  FILE exists and it's a regular file
  #        -g FILE:  FILE exists and it's set-group-ID
  #        -G FILE:  FILE exists and it's owned by the effective group ID
  #        -h FILE:  FILE exists and it's a symbolic link (same as -L)
  #        -k FILE:  FILE exists and has its sticky bit set
  #        -L FILE:  FILE exists and it's a symbolic link (same as -h)
  #        -O FILE:  FILE exists and it's owned by the effective user ID
  #        -p FILE:  FILE exists and it's a named pipe
  #        -r FILE:  FILE exists and read permission is granted
  #        -s FILE:  FILE exists and has a size greater than zero
  #        -S FILE:  FILE exists and it's a socket
  #        -t FD:    file descriptor FD is opened on a terminal
  #        -u FILE:  FILE exists and its set-user-ID bit is set
  #        -w FILE:  FILE exists and write permission is granted
  #        -x FILE:  FILE exists and execute (or search) permission is granted
}

@block{@block-name{Bash Startup Files}
  info "(bash) Bash Startup Files"

  @block{@block-name{Invoked as an interactive login shell, or with '--login'}
     Execution order:
     /etc/profile
     ~/.bash_profile (typically sources ~/.bashrc by the command `. ~/.bashrc`)
     ~/.bash_login
     ~/.profile
  }

  @block{@block-name{Invoked as an interactive non-login shell}
     ~/.bashrc
  }

  @block{@block-name{Invoked non-interactively}
  }

  @block{@block-name{Invoked with name 'sh'}
  }

  @block{@block-name{Invoked in POSIX mode}
  }

  @block{@block-name{Invoked by remote shell daemon}
  }

  @block{@block-name{Invoked with unequal effective and real UID/GIDs}
  }
}

@block{@block-name{Other shells}
  Rash: From Reckless Interactions to Reliable Programs
  https://willghatch.net/publications/rash-gpce-2018-preprint.pdf

  scsh Scheme Shell - latest release May 16 2006, ver0.6.7;
  https://scsh.net/

  TODO globbing
  https://github.com/willghatch/racket-rash/issues/64
  Github Issue: Question mark '?' char in a string #64
  https://github.com/willghatch/racket-rash/issues/67
  Github Issue: Regexes in grep #67
  https://docs.racket-lang.org/peg/index.html
  globbing vs. PEG expressions
}

@block{@block-name{Iterate over array/list}
  # /bin/sh portability: ubuntu dash vs. guix /bin/sh
  # ??? the snippet doesn't work
  export LIST="a:b:c"
  for e in $LIST; do printf -- "%s\n" $e; done

  # in bash:
  for e in "a" "b" "c"; do printf -- "%s\n" $e; done
  for e in "a" "b" "c"; do echo $e; done

  # in fish:
  set fruits "apple" "banana" "cherry" # i.e. list / array
  count $fruits     # 3
  # arrays in fish are indexed from 1, i.e. index starts from 1
  echo $fruits[1] # Output: apple
  echo $fruits[2] # Output: banana
  set --append fruits "orange" "grape"
  set --prepend fruits "whiskey"
  count $fruits     # 6
  echo $fruits      # whiskey apple banana cherry
  set --erase fruits[1]
  count $fruits     # 5
  for e in "apple" "banana" "cherry"; printf "%s\n" $e; end
}

@block{@block-name{Multiline Comments}
  The shell will elide the empty output from the process substitution.
  See:
  https://stackoverflow.com/a/12797512
  https://stackoverflow.com/a/47374030/5353461
  Works also allows commenting part of a line, which can be useful.
  # the \ is line continuation char
  echo "foo" \
      `# this line is comment` \
      ;
  echo "bar"
}

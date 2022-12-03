#lang notes

@block{@block-name{Find and Grep}
  # redirect; skip the "Permission denied" and "Invalid argument" errors
  find ./ -name file.ext 2>&1 | grep -v "Permission denied\|Invalid argument"
  # see also psub

  # grep / ripgrep / rg
  grep -v, --invert-match # select non-matching lines
  # in grep -L is the complement / invert / negate of -l
  grep -l, --files-with-matches  # print only names of FILEs with selected lines
  grep -L, --files-without-match # only print FILE names containing no match
  # in ripgrep -L, --follow # follow symbolic links

  # bash: find: redirect: separate / combine sdterr and stdout; does not work
  # with the tee command
  ./cmd.sh 1>std.out 2>std.err / ./cmd.sh &>combined.out

  # emacs find - exclude backup files; '+' in the '--exec ... +' - the command
  # is built by appending each selected file name at the end

  # find and delete empty files / dirs
  find . -empty -type f -delete
  find . -empty -tspe d -delete
  find  /path/to/dest -type f -empty -user vivek

  # search for *fileToSearch* in multiple directories
  find ./ foo/ bar/ -name "*fileToSearch*"

  # TODO what does the `-print` switch?

  # find only filenames matching '*.clj' containing 'project'
  find . -name '*.clj' | xargs grep -l 'project'

  # quit / stop search after finding 1st match
  find . ... -print -quit

  # find all files and dirs modified in the last 7 days; between: older: newer:
  find . ... -mtime -7

  # flatteb all xml files from all src subdirs to dst, fork off a new copy
  # process for every file; TODO test it!

  # skip / exclude / do not search
  find . -not -path "*/\.*"  # hidden files and dirs
  find . -not -path "*path/to/exclude*"

  # files filtered by multiple extensions
  find . -type f -name "*.xml" -or -name "*.txt"
  find . -executable -type f
  find . -type d -name "dirname" # directories called dirname

  # grep from a string
  txt ="Some text where the search is done"                    # bash

  # find: recursive search for "Pattern" in ... (with '.' at the end)

  # find: grep-help: recursive search for "Pattern" in ... (with '.' at the end)

  # build and execute command lines from standard input
  xargs

  # search in *.txt files (with spaces in filenames)
  find ./ -type f -name "*.txt" -print0 | xargs -0 grep --files-with-matches "Pattern"

  # find and delete *.jar and *.class when idling
  ionice -c3 find . -name "*.jar" -or -name "*.class" -delete

  # search for File.class in jar files
  find . -name "*.jar" | xargs grep File.class

  # new line separator for each grep result sh script

  # grep: colorize grep in less
  grep --color=always pattern file | less -R

  # grep: lines containing any upper character
  grep "[[:upper:]]" file

  # grep: intersection between two files
  grep -Fx -f file1 file2

  # grep: search for "Pattern" and print 2/4 lines before/after matching line
  grep -B 2 -A 4 "Pattern"
  grep --before-context=2 --after-context=4 "Pattern"

  # grep: print only filenames of the files containing "Pattern"
  grep "Pattern" * | cut -f1 -d:

  # zgrep: search possibly compressed files for a regular expression
  zgrep "Pattern" myfile.gz / zgrep 'GET /blog' access_log.gz

  # find images; -o --only-matching; -P --perl-regexp

  # between: older: newer:
  find ~/Pictures -type f -newer
  find ~/Pictures -type f -not -newermt "2016-02-01"

  # find all files recursively newer than given time
  find ./ -newermt $(date +%Y-%m-%d -d '1 day ago') -type f -print
  # fish: find recent Guix-build logs
  l (find /var/log/guix/drvs/ -type f -newermt (date +%Y-%m-%d\ %H:%M -d '60 minutes ago') -name "*.drv.gz")
  l (find /var/log/guix/drvs/ -type f -newermt (date +%Y-%m-%d\ %H:%M -d '30 minutes ago') -name "*.drv.gz")

  # substitute / replace all occurences of ... with ...
  find ./ -type f -name "*.fish" -print0 | xargs -0 sed --in-place "s/apt-get/apt/g"

  # skip the "Permission denied" and "Invalid argument" errors
  find ./ -name file.ext 2>&1 | grep -v "Permission denied\|Invalid argument"

  #  recursively count LOC (lines of code) in all source files (python & sql)
  find ./ -name "*.py" -or -name "*.sql" | xargs wc -l

  # search for IPv4 addresses; -E --extended-regexp, -o --only-matching

  # match integer and floating point numbers
  # -o --only-matching, -P --perl-regexp - grep capture group with \K
  echo "aaa 123 0.0 bbb" | grep -o  "\([[:digit:]]*\.[[:digit:]]*\|[[:digit:]]*\)"
  echo "aaa 123 0.0 bbb" | grep -oP "\K([0-9]+\.[0-9]+|[0-9]+)"
  echo "aaa 123 0.0 bbb" | grep -o  "\([[:digit:]]*\.[[:digit:]]*\|[[:digit:]]*\)"
  echo "aaa 123 0.0 bbb" | grep -oP "\K([0-9]*\.[0-9]*|[0-9]*)"
  echo "aaa 123 0.0 bbb" | grep -oP "\K([[:digit:]]*\.[[:digit:]]*|[[:digit:]]*)"

  # match a version number
  # -o --only-matching, -P --perl-regexp - grep capture group with \K
  echo "version: 01.02.02" | grep -oP "([0-9]{1,}\.)+[0-9]{1,}"
  echo "version: 0.0.0"    | grep -oP "([0-9]{1,}\.)+[0-9]{1,}"
  echo "version: 51.19.7"  | grep -oP "([0-9]{1,}\.)+[0-9]{1,}"
  echo "version: 5.19.7"   | grep -oP "([0-9]{1,}\.)+[0-9]{1,}"
  # match linux-libre version number
  guix show linux-libre | head | grep version | grep -oP "([0-9]{1,}\.)+[0-9]{1,}"

  # grep
  # \Z  matches the EOF end-of-file

  # https://www.putorius.net/linux-find-command.html
  # Listing Matched Files with Find Command
  find . -type f -iname "my*" -ls

  # Save output to a file
  find ~/test/ -type f -fprint output.txt

  # Run / Execute any command on each matched file from the find command.
  # e.g. find all regular files in the ~/test/ and copy them into the /var/tmp/
  # \; - denotes the end of command
  # see also https://www.baeldung.com/linux/find-exec-command

  # -execdir runs the command from the subdirectory of the matched file.

  # Prompt for Approval Before Executing Commands

  # find files with extension; fdfind (Ubuntu) / fd (Guix) is aliased to `f`
  # https://github.com/sharkdp/fd
  fd --extension rkt
  fd          -e rkt
  find -name "*.rkt"
  # regex:
  fd --hidden --full-path '.envrc$' /path/to/dir
  f  --hidden --full-path '.envrc$' ~

  # combine fdfind and ripgrep; show full files paths for the mathes
  rg "<search-regex>" (f "<file-extention>$" /path/to/dir)
  rg "<search-regex>" (f "<file-extention>$" (pwd))
  rg "dotspacemacs/layers" (f -e scm -e c -e h '.*' ~/.emacs.d)
  # word boundaries
  rg "\bword\b" (f -e scm -e c -e h '.*' ~/.emacs.d)
  rg -w "word" (f -e scm -e c -e h '.*' ~/.emacs.d)
  # -g, --glob Include or exclude files and directories. Precede a glob with a !
  #            to exclude it.
  rg -g '*.{el}' -w "deleted" ~/.emacs.d
  # TODO consider making an alias for this, or a default setting.
  # See https://github.com/BurntSushi/ripgrep/discussions/2011
  # ack, ag, git-grep, GNU grep, rg, https://beyondgrep.com/feature-comparison/
  rg --hidden --glob '!.git'
  rg --hidden --iglob-regex '^.git$' ...
  rg --glob-regex '^.gitlab-ci.yml$' ...

  # rg manual file types / extensions (globing)
  rg -g '*.{scm,c,h}' -w "operating-system" ~/dev/guix ~/dev/guile
  # fdfind manual file types / -e, --extension
  rg -w "operating-system" (f -e scm -e c -e h '.*' ~/dev/guix ~/dev/guile)
  rg "instrumented" (f -e scm -e c -e h '.*'  ~/dev/guix/ ~/dev/guile/)

  grep "Spacemacs is ready." (find ~/.emacs.d/ -type f -name '*.el')
  rg "Spacemacs is ready." (find ~/.emacs.d/ -type f -name '*.el')
  find ~/.emacs.d/ -type f -name "*.el" -print0 | xargs -0 grep --files-with-matches "Spacemacs is ready."

  # ripgrep
  rg --type racket  --word-regexp SearchText
  rg     -t racket  --word-regexp SearchText
  rg --type clojure --word-regexp SearchText
  rg     -t clojure --word-regexp SearchText

  # fish-shell, ripgrep and sed the stream editor
  # fish-shell arrays start with 1
  set oldNew "oldText" "newText"
  set sFiles (rg --files-with-matches --type racket $oldNew[1])
  sed --in-place "s/$oldNew[1]/$oldNew[2]/g" $sFiles
}

#lang notes

@block{@block-name{Find and Grep / RipGrep}
  #+BEGIN_SRC fish :results output
  # filesize is exactly 1033 bytes
  find . -type f -size 1033c -user bandit7 -group bandit6 | sort

  # -a, --text
  # Process a binary file as if it were text; equivalent to --binary-files=text
  grep --binary-files=text -oP "[[:alnum:]]{4,}" binary-mixed-with-text.txt

  # look up all indices of all Info manuals.
  info --apropos "search-string"
  # search in all Info documents for some string, not just in the indices
  # zgrep - search possibly compressed files for a regular expression
  find /usr/share/info -type f -name "*.gz" -print0 | xargs -0 zgrep -l "search-string"

  # redirect; skip the "Permission denied" and "Invalid argument" errors
  find . -name file.ext 2>&1 | grep -v "Permission denied\|Invalid argument"
  # see also psub

  # grep / ripgrep / rg
  grep -v, --invert-match # exclude matching / select non-matching lines
  # in grep -L is the complement / invert / negate of -l
  grep -l, --files-with-matches  # print only names of FILEs with selected lines
  grep -L, --files-without-match # only print FILE names containing no match
  # in ripgrep -L, --follow # follow symbolic links

  # bash: find: redirect: separate / combine sdterr and stdout; does not work
  # with the tee command
  ./cmd.sh 1>std.out 2>std.err / ./cmd.sh &>combined.out

  # emacs find - exclude backup files; '+' in the '--exec ... +' - the command
  # is built by appending each selected file name at the end

  find . -empty -type f -delete  # delete empty files
  find . -empty -type d -delete  # delete empty directories
  find /path/to/dest -type f -empty -user vivek

  find . foo/ bar/ -name "*.txt"  # in multiple (three) directories

  # The -print action prints the name of the current file on the standard
  # output. It is performed on all files for which the whole expression is true,
  # unless it contains an action other than -prune or -quit.
  # Action examples:
  #   -print -delete -exec -execdir -ok -okdir -fls -fprint -fprintf -ls -print
  #   -printf
  find . -print -quit             # quit / stop search after finding 1st match

  xargs # build and execute command lines from standard input
  # only names matching '*.clj', contains 'project', w/ spaces in names
  find . -name '*.clj' | xargs grep --files-with-matches 'project'
  find . -name '*.jar' | xargs grep File.class     # search in jar files
  find . -type f -name '*.txt' -print0 | xargs -0 grep --files-with-matches "Pattern"

  # all files & directories modified in the last 7 days; between: older: newer:
  find . ... -mtime -7

  # flatteb all xml files from all src subdirs to dst, fork off a new copy
  # process for every file; TODO test it!

  find . -not -path "*/\.*"            # skip / exclude hidden files and dirs
  find . -not -path "*path/to/exclude*"
  # skip / exclude survivor dir
  set sdir ~/.local/share/spacemacs/elpa/28.2/develop
  find $dr -not -path "$sdir/archives*" -and -not -path "$sdir" # rm -rf {} +

  find . -type f -name "*.xml" -or -name "*.txt" # filtered by extensions
  find . -executable -type f                     # executable files
  find . -type l                                 # links
  find . -type d -name "dirname"                 # directories called dirname

  # grep from a string / variable. Works only in bash
  txt="Some text where 123 the search is done"
  grep --only-matching -e "[-+]\?[0-9]*\.\?[0-9]\+" <<< ${txt}

  # find: recursive search for "Pattern" in ... (with '.' at the end)
  grep -nir "Pattern" --exclude-dir={.git,CVS} --include=\*.{el,clj,cljs,cljc} ./

  # find: grep-help: recursive search for "Pattern" in ... (with '.' at the end)
  grep -nir "Pattern" --exclude-dir={.git,CVS} --include=\*.{log,propeties,cfg,txt} ./

  # find and delete *.jar and *.class when idling
  ionice -c3 find . -name "*.jar" -or -name "*.class" -delete

  # new line separator for each grep result sh script

  grep --color=always pattern file | less -R # colorize grep in less
  grep "[[:upper:]]" file                    # lines containing any upper char

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
  find . -newermt $(date +%Y-%m-%d -d '1 day ago') -type f -print
  # fish: find recent Guix-build logs
  l (find /var/log/guix/drvs/ -type f -newermt (date +%Y-%m-%d\ %H:%M -d '60 minutes ago') -name "*.drv.gz")
  l (find /var/log/guix/drvs/ -type f -newermt (date +%Y-%m-%d\ %H:%M -d '30 minutes ago') -name "*.drv.gz")

  # substitute / replace all occurences of ... with ...
  find . -type f -name "*.fish" -print0 | xargs -0 sed --in-place "s/apt-get/apt/g"

  # skip the "Permission denied" and "Invalid argument" errors
  find . -name file.ext 2>&1 | grep -v "Permission denied\|Invalid argument"

  #  recursively count LOC (lines of code) in all source files (python & sql)
  find . -name "*.py" -or -name "*.sql" | xargs wc -l

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
  # In the default Guix installation the ripgrep `rg` is not available and grep
  # is build with --disable-perl-regexp, i.e. no -P --perl-regexp can be used.
  # I.e. the chars +(){} must be escaped by a backslash in the regexp.
  guix show linux-libre | head | grep version | grep -o "\([0-9]\{1,\}\.\)\+[0-9]\{1,\}"

  # ripgrep: print only first capture group $1
  ls /gnu/store/*-emacs-pippel-* | rg  '(.*):' --replace '$1'

  # scap for open ports in the range 31000 to 31999 and print them using \K for
  # capture group and sort them
  ss -tulpn | grep -oP "0.0.0.0:\K(31[0-9]{3})" | sort

  # grep
  # \Z  matches the EOF end-of-file

  # Save output to a file
  find ~/test/ -type f -fprint output.txt

  # Run / Execute any command on each matched file from the find command.
  # e.g. find all regular files in the ~/test/ and copy them into the /var/tmp/
  # \; - denotes the end of command
  # see also https://www.baeldung.com/linux/find-exec-command

  # -execdir runs the command from the subdirectory of the matched file.

  # Prompt for Approval Before Executing Commands

  # find files with extension; fdfind (Ubuntu) / fd (Guix) - aliased to `f`
  # https://github.com/sharkdp/fd
  # -e, --extension
  # -H, --hidden
  # -a, --absolute-path
  # -I, --no-ignore
  # -p, --full-path   search pattern is matched against the full path
  # regex:
  fd --extension rkt   # find -name "*.rkt"
  fd --no-ignore --hidden --full-path '.envrc$' /path/to/dir
  fd --no-ignore --absolute-path <some-file-ignored-by-git>

  # combine fdfind and ripgrep; show full files paths for the mathes
  rg "<search-regex>" (f "<file-extention>$" /path/to/dir)
  rg "<search-regex>" (f "<file-extention>$" (pwd))
  rg "dotspacemacs/layers" (f -e scm -e c -e h '.*' ~/.emacs.d)

  # word boundaries
  # -g, --glob Include or exclude files and directories. Precede a glob with a !
  #            to exclude it.
  rg --no-ignore-vcs -g '*.{el}' -w "info-constant-ref-item\b" ~/.emacs.d
  # with -w much less is returned
  rg --no-ignore-vcs -g '*.{el}' -w "\sdelete\s" ~/.emacs.d
  rg --no-ignore-vcs -g '*.{el}' -w "\bdelete\b" ~/.emacs.d

  # TODO consider making an alias for this, or a default setting.
  # See https://github.com/BurntSushi/ripgrep/discussions/2011
  # ack, ag, git-grep, GNU grep, rg, https://beyondgrep.com/feature-comparison/
  rg --hidden --no-ignore-vcs --glob '!.git'
  rg --hidden --no-ignore-vcs --iglob-regex '^.git$' ...
  rg --no-ignore-vcs --glob-regex '^.gitlab-ci.yml$' ...

  # the .gitignore contains 'elpa/', therefore the --no-ignore-vcs is needed
  rg --no-ignore-vcs -g '*.{el}' "Buffer is read-only" ~/.emacs.d/
  rg --no-ignore-vcs -g '*.{el,c}' "'elpa" $dev/.spguimacs.d/ $dev/emacs-28-2

  # rg manual file types / extensions (globing)
  rg -g '*.{scm,c,h}' -w "operating-system" $dgx $dev/guile
  # fdfind manual file types / -e, --extension
  rg -w "operating-system" (f -e scm -e c -e h '.*' $dgx $dev/guile)
  rg "instrumented" (f -e scm -e c -e h '.*'  $dgx/ $dev/guile/)

  set s "Spacemacs is ready" # s="Spacemacs is ready"
  grep $s (find ~/.emacs.d/ -type f -name '*.el')
  rg --no-ignore-vcs $s (find ~/.emacs.d/ -type f -name '*.el')
  # -l --files-with-matches
  find ~/.emacs.d/ -type f -name "*.el" -print0 | xargs -0 grep -l $s

  # ripgrep
  rg --type racket  --word-regexp SearchText
  rg     -t racket  --word-regexp SearchText
  rg --type clojure --word-regexp SearchText
  rg     -t clojure --word-regexp SearchText

  # fish-shell, ripgrep and sed the stream editor
  # first item of a fish-shell arrays starts at / is indexed with 1 not 0
  set oldNew "oldText" "newText"
  set sFiles (rg --files-with-matches --type racket $oldNew[1])
  sed --in-place "s/$oldNew[1]/$oldNew[2]/g" $sFiles

  # -w, --word-regexp
  # -g, --glob <GLOB>...
  # search through all Guix and Guile source code
  rg --no-ignore-vcs -g '*.{scm,c,h}' -w "word\\s" $dgx $dev/guile
  # -t, --type <TYPE>...
  rg --no-ignore-vcs -tlisp -tc -w "word"  $dgx $dev/guile

  # search for sexp, see https://docs.rs/regex/1.9.5/regex/#syntax
  rg --no-ignore-vcs -tlisp -tc       "(['\"\\[\\(\s]|^)load-path(['\"\\]\\)\s]|\$)" $dgx $dev/guile
  rg --no-ignore-vcs -g '*.{scm,c,h}' "(['\"\\[\\(\s]|^)load-path(['\"\\]\\)\s]|\$)" (pwd)
  rg                 -g '*.{scm,c,h}' "(['\"\\[\\(\s]|^)x-x(['\"\\]\\)\s]|\$)"       $dev/notes/notes/testfile.scrbl
  #+END_SRC
}

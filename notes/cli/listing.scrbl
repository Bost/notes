#lang notes

@block{@block-name{Listing}
  # listing: list contents of directories in a tree-like format.
  tree -f (pwd) # full path prefix for each file.
  lff

  # listing: sort by size; -l     use a long listing format
  ls --sort=size -l            # ls -lS    largest first
  ls --sort=size -l --reverse  # ls -lrS   smallest first

  # listing: files newer than ...
  ls -la (find . -type f -newermt "2022-10-01")
  # listing: files older than ...
  ls -la (find . -type f -not -newermt "2022-10-01")

  # listing: only one column
  ls --format=single-column

  # listing: only directories, 1 entry per line
  # -a, --all          do not ignore entries starting with .
  # -A, --almost-all   do not list implied . and ..
  # -d, --directory    list directories themselves, not their contents
  # -1                 list one file per line.  Avoid '\n' with -q or -b
  ls -d1 */

  # listing: count of entries / files in /path/to/dir
  ls --almost-all -1 /path/to/dir | wc -l

  # listing: show full paths (alias lff)
  ls  -lrt -d -1 $PWD/{*,.*}   # all filepaths w/  their attributes
  ls       -d -1 $PWD/{*,.*}   # all filepaths w/o their attributes
  ls       -d -1 $PWD/path/to/file
  exa      -d -1 $PWD/path/to/file

  # listing: file all extentions / filetypes in current directory
  find ./ -type f | perl -ne 'print $1 if m/\.([^.\/]+)$/' | sort -u

  # https://www.putorius.net/linux-find-command.html
  # listing: matched files with find command
  # -iname  like -name but case insensitive
  find . -type f -iname "my*" -ls

  ;; dired:
  | ~@"("~             | M-x dired-hide-details-mode                       |
  |                    | toggle listing details                            |
  |                    | See `(describe-variable 'dired-listing-switches)` |

  # find all scm files and sort them by size largest first
  fd -g '*.{scm}' $dgx | xargs -I {} ls -lS {} | \
     sort --key=5 --numeric-sort --reverse # 5th column

  # listing: print only the file size; doesn't work without '--human-readable'
  ls --human-readable --size (readlink (which guile))
  16K /gnu/store/1gd9nsy4cps8fnrd1avkc9l01l7ywiai-guile-3.0.9/bin/guile

  # list only links. '-ls' lists current file in 'ls -dils'
  find /path/to/dir -maxdepth 1 -type l -ls
}

#lang notes

@block{@block-name{CVS Concurrent Versions System}
  # restart cvs daemon
  sudo /etc/init.d/cvsd restart / start / stop / status

  # diff tagX tagY
  cvs diff -r tagX -r tagY

  # get clean copy
  cvs update -C ./path/to/file.ext

  # :cvs get revision 1.11
  cvs update -P -C -r 1.11 ./path/to/file.ext

  # checkout module from branch or tag
  cvs checkout -r branchOrTag module

  # commit file with multi-line commit message
  cvs commit -m "fst-comment-line\nsnd-comment-line" path/to/file.ext

  # update file
  cvs log    -P -d ./path/to/file.ext

  # reminder to leave in 15 minutes / at 13:55
  leave +15 / leave 1355

  # delete NormalTag from file.ext in version 1.17
  cvs tag    -d -r 1.17 NormalTag ./path/to/file.ext

  # delete BranchTag from file.ext in version 1.17
  cvs tag -B -d -r 1.17 BranchTag ./path/to/file.ext

  # move   BranchTag to   file.ext in version 1.19
  cvs tag -B -F -r 1.19 BranchTag ./path/to/file.ext

  # create BranchTag on   file.ext in version 1.19
  cvs tag -b    -r 1.19 BranchTag ./path/to/file.ext

  # move   NormalTag to   file.ext in version 1.63
  cvs tag    -F -r 1.63 NormalTag ./path/to/file.ext

  # version and tags
  cvs log file.ext
  cvs status -v file.ext

  # list files associated with a tag; (no blank between -r and TAGNAME)
  cvs -q rlog -R -N -S -rTAGNAME MODULENAME

  # debug and trace information
  cvs -d cvs -t -d :pserver:faizal@"@"localhost:/myrepos \
      ci -m "test" -l "src/foo/Foo.ext"

  #
  cvs add file.ext
}

#lang notes

#+title: Eclipse

@block{@block-name{Eclipse}
  #+BEGIN_SRC bash :results output
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
  #+END_SRC
}

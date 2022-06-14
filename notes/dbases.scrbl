#lang notes

#+title: Dbases

@block{@block-name{MariaDB}
  [[https://en.wikipedia.org/wiki/MariaDB][MariaDB]] - free, open source, commercially supported MySQL fork.
}

@block{@block-name{Redis}
[[https://redis.io/][Redis]]
- in-memory data structure store, used as a database, cache, and message broker
- Sessions can be used when dealing with multiple users accessing a single web
  site. Session is a data store for belonging to each visitor. Cookies are the
  mechanism for associating a visitor with a session
}

@block{@block-name{Change data capture}
  CDC Change data capture
  set of software design patterns used to determine and track the data that has changed so that action can be taken using the changed data.
  ** Timestamps on rows
  ** Version numbers on rows
  ** Status indicators on rows
  ** Time/Version/Status on rows
  ** Triggers on tables
  ** Event programming
  ** Log scanners

  #+BEGIN_SRC bash :results output
  exit 1 # just in case ...
  # oracle: execute script.sql
  @"@"C:\path\to\script.sql
  #+END_SRC
}

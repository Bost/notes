#lang notes

@block{@block-name{PostgreSQL}
  #+BEGIN_SRC bash :results output
  # connect to localhost
  psql <dbname>
  psql <dbname> <username>
  # connect to remote host
  psql -h localhost -p 5432 -U postgres -W   # for no password - press Enter
  # test content
  SELECT count(*) FROM pg_catalog.pg_tables;
  # show password requirements
  # 'usename' without 'r' is correct!
  SELECT usename, passwd IS null FROM pg_shadow;
  # remove password
  ALTER ROLE <username> PASSWORD null;
  # set password to: foo
  ALTER ROLE <username> PASSWORD 'foo';
  #
  # CLI commands
  # \?         psql commands
  # \h         SQL commands
  # \d         list tables, views, sequences (also relations ?)
  # \conninfo  current database connection
  # \dt        list tables and owners
  # \z         list all tables, views, and sequences
  # \q         exit
  #+END_SRC
}

#lang notes

@block{@block-name{Various}
  # ???
  db2init

  # load ixf file
  db2 drop table <schema>.<table>
  DB2 IMPORT FROM <file.ixf> OF IXF create into <schema>.<table>
  DB2 IMPORT FROM <file.ixf> OF IXF insert into <schema>.<table>

  # init db2 environment
  db2cmd -i -w db2clpsetcp

  # license info / add license
  db2licm -l / db2licm -a db2conpe.lic

  db2 CONNECT TO database USER userID USING password
  db2 get connection state
  db2 CATALOG TCPIP NODE $node_name REMOTE $ip_addr SERVER $port
  db2 CATALOG DATABASE $database_name AT NODE $node_name AUTHENTICATION server
  db2 UNCATALOG NODE $node_name
  db2 UNCATALOG DATABASE $database_name
  db2 TERMINATE
  db2 list db directory > db.txt | gvim db.txt
  db2 list node directory > node.txt | gvim node.txt
  db2 list tables
  # execute script.sql from normal / command line processor (=>) shell
  db2 -vf script.sql -t / !db2 -vf script.sql -t;
  db2 -tvf script.sql -z file.log
  # execute script.sql from normal shell (Befehlsfenster)
  db2 -td; -v -f script.sql
  # in mysql: limit N
  db2 select * from DBASE.TABLE fetch first 2 rows only
  # error description for sqlcode=-302
  db2 ? sql302

  # get version
  db2 SELECT GETVARIABLE('SYSIBM.VERSION') FROM SYSIBM.SYSDUMMY1

  # get db2cc version
  java -cp ./path/to/db2jcc.jar com.ibm.db2.jcc.DB2Jcc -version
}

#lang notes

@block{@block-name{On Ubuntu}
  Error log: /var/log/mysql/error.log
  #+BEGIN_SRC bash :results output
  sudo apt install mysql-server mysql-client

  # configure dbase after installation
  sudo mysql_secure_installation
  #+END_SRC

  #+BEGIN_SRC mysql :results output
  -- DROP USER IF EXISTS '<username>'
  CREATE DATABASE Test;
  CREATE USER '<username>'@"@"'localhost' IDENTIFIED BY '<password>';
  GRANT ALL PRIVILEGES ON *.* TO '<username>'@"@"'localhost' WITH GRANT OPTION;
  #+END_SRC
}

@block{@block-name{Various commands}
  #+BEGIN_SRC bash :results output
  # Finally!!!
  # Can’t log into phpMyAdmin: mysqli_real_connect(): (HY000/1698): Access denied for user ‘root’@"@"’localhost’
  # https://devanswers.co/phpmyadmin-access-denied-for-user-root-localhost/
  # MySQL root password reset:
  sudo mysql --user=root mysql  # or: sudo mysql -root mysql
  UPDATE mysql.user SET authentication_string=null WHERE User='root';
  ALTER USER 'root'@"@"'localhost' IDENTIFIED WITH mysql_native_password BY '';
  flush privileges;
  quit
  #  Also in `/etc/phpmyadmin/config.inc.php` activate all lines with:
  $cfg['Servers'][$i]['AllowNoPassword'] = TRUE;

  # view users
  SELECT user,authentication_string,plugin,host FROM mysql.user;

  # login
  mysql -uroot -p

  # import script from OS shell command line
  mysql -u <username> -p database_name < file.sql
  # import script from MySQL command line
  use db_name;
  source file.sql;

  # (remote) connect / login to mysql; 3306 is the default port
  mysql --user=<username> --password=<password> --host=<host_name> --port=<port_num> --database=<db-name>
  mysql      -u<username>          -p<password>      -h<host_name>      -P<port_num>          -D<db_name>
  # example
  mysql -u root -p -h localhost -D employees

  # connect / login to localhost: prompts for password
  mysql -u <username> -p

  # connect / login to localhost: empty password
  mysql -u$USER

  # unblock mysql
  mysqladmin -u [username] -p flush-hosts

  # execute script.sql as the root user
  mysql -u root -t < script.sql

  # in db2 fetch first N rows only
  mysql select * from mantis.state_mantis_id limit 10

  mysql show databases
  mysql show tables in <dbaseName>
  mysql show users # `mysql desc mysql.user` does probably the same

  # describe table
  mysql show columns in <tableName>

  # server start
  /usr/bin/mysqld_safe &
  #+END_SRC
}

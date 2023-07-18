#lang notes

@block{@block-name{Heroku}
  heroku login --app <APP-NAME>
  # Use Git to clone the repository / source code to a local machine
  heroku git:clone  --app <APP-NAME>

  # Show configuration / List all environment variables
  heroku config --app <APP-NAME>
  heroku config:get <VARIABLE> --app <APP-NAME>
  heroku config:set <VARIABLE>=<VALUE> --app <APP-NAME>

  # PostgreSQL database command line access
  heroku pg:psql --app <APP-NAME> <DATABASE>

  # Inspect logfile:
  heroku plugins:install heroku-papertrail --app <APP-NAME>
  heroku pt ":type -'ssl-client-cert' -'$MY_TELEGRAM_ID'" --app <APP-NAME> | grep -v -e '^[[:space:]]*$
  # open papertrail
  heroku addons:open papertrail --app <APP-NAME>

  # Shell / Command line access. It reads the .bashrc and/or .bash_profile
  heroku run bash --app <APP-NAME>

  # Install on Ubuntu: `sudo snap install heroku --classic` doesn't work
  # See https://github.com/heroku/cli/issues/822
  curl https://cli-assets.heroku.com/install.sh | sh

  # Install on Guix:
  npm install --global heroku
  # It may lead to:
  #    fatal error: uv.h: No such file or directory
  # which apparently may be ignored. See https://help.heroku.com/1104958
  sudo rm /usr/local/bin/heroku
  sudo ln -s /home/bost/.npm-packages/bin/heroku /usr/local/bin/heroku

  # Restart / Stop and start
  heroku ps:restart --app <APP-NAME>
  heroku ps:scale web=0 --app <APP-NAME> && heroku ps:scale web=1 --app <APP-NAME>

  # Deploy to test / production / etc.
  heroku pipelines:promote --app <APP-NAME>

  # `heroku ...` adds a new environment variable "PORT". Its default value
  # is 5000 when running `heroku local ...`. It can be changed using the '-p'
  # parameter. The value of "PORT" is random when the app is running on a
  # Heroku-server. See `(System/getenv "PORT")`
  heroku local -p 7000
}


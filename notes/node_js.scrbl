#lang notes

#+title: Node.js

@block{@block-name{NodeJS}
  nvm - Node Version Manager @url{https://github.com/nvm-sh/nvm}

  #+BEGIN_SRC bash :results output
    ## install nodejs with (behind) or without proxy:
    ## 1.
    set nodeJsVer 16
    # 2.1 without proxy
    curl -fsSL https://deb.nodesource.com/setup_$nodeJsVer.x | sudo -E bash -
    sudo apt install --yes nodejs
    ## 2.2 behind proxy
    # sudo apt-key adv --keyserver-options \
    #      http-proxy="http://<proxy-ip>:<proxy-port>/" \
    #      --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 68576280
    # sudo apt-add-repository \
    #     "deb https://deb.nodesource.com/node_$nodeJsVer.x "(lsb_release -sc)" main"
    ## verify
    node --version
    ## list repositories: see list-ppa
    ## complete uninstall; see also https://stackoverflow.com/a/11178106/5151982
    sudo rm -rf \
      /usr/local/lib/node_modules/npm \
      /usr/local/bin/npm \
      /usr/local/bin/node \
      /usr/local/share/man/man1/node.1

    # :npm :nodejs :javascript dependency management
    # -L, --location      Follow redirects
    curl -o- -L https://yarnpkg.com/install.sh | bash  # also update
    yarn add                  # add package to use in your current package
    yarn global add <package>
    yarn init                 # initialize development of a package
    yarn install              # install all dependecies defined in package.json
    yarn publish              # publish package to a package manager
    yarn remove               # remove unused package from your current package
    yarn upgrade

    # nodejs is an alias to the node command
    npm install --global <package>
    npm outdated
    npm update --global
    npm list

    # install the package and save is inside of the dependencies the package.json
    npm install --save <package>

    # install the electron package
    npm install --global --unsafe-perm=true electron
    npm search electron\*

    # :security - don't execute postinstall hooks https://youtu.be/24tQRwIRP_w?t=952
    npm config set ignore-scripts true

    # :nodej
    npm install --verbose <package> / npm install -dd <package>
    npm config list
    npm config set color=false
    npm config set progress=false
    npm install --no-colors --verbose result-core
    npm cache verify / npm cache clean / npm cache clean --force
    npm config set registry https://registry.npmjs.org/ [or http]
    npm config set proxy "http://<ip:port>/"
    npm config set https-proxy "https://<ip:port>/"
  #+END_SRC
}

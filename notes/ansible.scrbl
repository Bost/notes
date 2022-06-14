#lang notes

#+title: Ansible

@block{@block-name{Ansible}
  fictional instantaneous communication systems - Ursula K. Le Guin, 1966, novel Rocannon's World
  IT automation & (app lifecycle) orchestration
  Automation language & engine
  Agentless

  @block{@block-name{Ansible tower}
    Expand automation
    Precise role and responsibilities definition
  }

  @block{@block-name{Management of}
    Infrastructure
    Networks
    OSs
    Services
  }

  @block{@block-name{Galaxy}
    community provided roles, playbooks and modules
    ansible-galaxy command line
    galaxy roles are directly runnable
  }

  @block{@block-name{Playbooks}
    yaml
    desired state of something / actions / commands
    Playbooks contains:
    Roles - special kind of playbook

    @block{@block-name{How to run - from the ansible control node:}
      Ad-hoc:
      # check-mode / dry run
      - ansible -C
      - ansible <inventory> -m
      - ansible web -a /bin/date
      - ansible web -m ping
      # ensure the openssl package is up-to-date
      - ansible web -s -m yum -a "name=openssl state=latest"

      Playbooks:
      - ansible-playbook my-playbook.yml
      # check-mode / dry run
      - ansible-playbook -C
    }

    @block{@block-name{Plays}
      @block{@block-name{Tasks}
        Tasks run sequentially
        Tasks trigger handlers; handlers run in parallel at the end of plays
        Tasks contain:
        @block{@block-name{Modules}
        }
      }
    }
  }

  @block{@block-name{Variables}
  }

  @block{@block-name{Inventories}
     list of hosts, ranges

     Modules - control of system resources, packages, files etc.
     }
  }

  @block{@block-name{CloudForms}
    RedHat, Virtualisation, Cloud infrastructure / Hybrid Cloud Management
    Workload migration & allocation; Virtual Platforms
    written in Ruby
  }

  @block{@block-name{Puppet}
    Infrastructure management & delivery
    Automation: Moving existing micro services running in VMs
    Ruby (C++, Clojure ???)

    @block{@block-name{PuppetSever}
       in Clojure
    }
}

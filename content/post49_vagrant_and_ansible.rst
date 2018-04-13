Vagrant and Ansible
===================

:date: 2018-04-13
:tags: clojure, vagrant, ansible
:category: Programming
:authors: Tobias Pleyer
:summary: A quick summary how to set up Vagrant in combination with Ansible.


This week I worked through Daniel Higginbotham's book "Deploying your first
Clojure App - from the Shadows". In this book he explains how to set up a
comprehensive deployment chain with reproducible results via a use of
`Vagrant`_ in combination with `Ansible`_.

.. _Vagrant: https://www.vagrantup.com/
.. _Ansible: http://docs.ansible.com/

Of course he also explains how to install these tools. Strangely on my machine
(Linux Mint 18.1 Cinnamon 64-bit) these commands were not enough to get my
system up and running. So here the quick summary what was necessary to set me
up:

.. code:: bash

    $ # This line is unchanged
    $ sudo bash -c "cd /usr/local/bin && curl -fsSLo boot https://github.com/boot-clj/boot-bin/releases/download/latest/boot.sh && chmod 755 boot"
    $ # This too
    sudo apt-get install software-properties-common
    sudo apt-add-repository ppa:ansible/ansible
    sudo apt-get update
    sudo apt-get install ansible
    $ # Change - manual installation
    $ wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | sudo apt-key add -
    $ sudo apt-get update
    $ # Get rid of all old stuff
    $ sudo apt-get purge virtualbox virtualbox-qt virtualbox-dkms virtualbox-guest-utils virtualbox-guest-x11
    $ # Make sure everything is gone
    $ dpkg --list | grep -i virtualbox # should give no results
    $ # Now install the correct version, before virtualbox-5.2 couldn't be found
    $ sudo apt-get install virtualbox-5.2
    $ # In order to use virtualbox-5.2 I needed a newer Vagrant, so I took the
    $ # newest from the website
    $ wget https://releases.hashicorp.com/vagrant/2.0.3/vagrant_2.0.3_x86_64.deb
    $ sudo dpkg -i vagrant_2.0.3_x86_64.deb

What confuses me is that these tools are all registered in the package index.
So supposedly `sudo apt-get install virtualbox vagrant` should have done the
trick. But after I installed the virtualbox version my package manager
suggested, even though successful, I didn't have any executable that I could
run. Consequently Vagrant failed to set up because it couldn't find a suitable
provider. Luckily VirtualBox comes with a good `download guide for Linux`_.

.. _download guide for Linux: https://www.virtualbox.org/wiki/Linux_Downloads

What Daniel describes in his book is the following setup: Using Vagrant to
manage one or more virtual machines and then using Ansible to set up the
machines and perform the necessary steps to bring the application online. He
does this via three shell scripts: build, provision and deploy. These scripts
use the same playbook and alter behaviour via tag inclusion and exclusion.

I don't have a lot to complain about that, because it is good pratice to use
tools like Docker or Vagrant to ensure a defined state to work with and other
tools to take care of deploying your application. I just found a small
optimisation: `Vagrant combines nicely with Ansible`_, so instead of
maintaining an *install* tag and manage provisioning manually it is possible to
give Vagrant an Ansible playbook to use for provisioning the virtual machine.
After that calling `vagrant up` for the first time will run the Ansible
playbook to set up the machine, but subsequent calls will not call the script
again. This is probably not ideal for every (complex) setup, but for small
hobby projects that is totally acceptable.

.. _Vagrant combines nicely with Ansible: http://docs.ansible.com/ansible/latest/scenario_guides/guide_vagrant.html

So my *Vagrantfile* looks like this

.. code:: ruby

    # -*- mode: ruby -*-
    # vi: set ft=ruby :

    Vagrant.configure("2") do |config|
      config.vm.box = "ubuntu/trusty64"

      config.vm.network :forwarded_port, guest: 80, host: 8000
      config.vm.network :forwarded_port, guest: 8080, host: 8080

      config.vm.provider "virtualbox" do |v|
        v.memory = 2048
      end

      config.vm.provision "ansible" do |ansible|
        ansible.verbose = "v"
        ansible.playbook = "playbooks/provision.yml"
      end
    end

Additionally if you use Vagrant with Ansible in this way, Vagrant will
automatically generate an inventory file, which can be reused by other Ansible
playbooks. The default location is
`.vagrant/provisioners/ansible/inventory/vagrant_ansible_inventory`. Since I
specified in the Vagrantfile to be verbose when running Ansible, Vagrant is
also printing the full command it uses when running Ansible, which can be
easily copied to a shell script and used for a deploy script. Here is mine:

.. code:: bash

    #!/bin/sh

    # Export Ansible specific environment variables
    export PYTHONUNBUFFERED=1
    export ANSIBLE_FORCE_COLOR=true
    export ANSIBLE_HOST_KEY_CHECKING=false
    export ANSIBLE_SSH_ARGS='-o UserKnownHostsFile=/dev/null -o IdentitiesOnly=yes -o ControlMaster=auto -o ControlPersist=60s'

    # Run the Ansible playbook with the inventory created by Vagrant
    ansible-playbook \
        --connection=ssh \
        --timeout=30 \
        --limit="default" \
        --inventory-file=.vagrant/provisioners/ansible/inventory/vagrant_ansible_inventory \
        -v \
        playbooks/deploy.yml

And this is the very basic directory structure for my `DevOps`_ needs:

.. _DevOps: https://en.wikipedia.org/wiki/DevOps

.. code:: bash

    $ tree
    .
    ├── deploy
    ├── playbooks
    │   ├── deploy.yml
    │   └── provision.yml
    └── Vagrantfile

Thus, assuming I have these files in the folder *devops*, all I have to do is
the following:

.. code:: bash

    $ cd devops
    $ vagrant up  # This will also install all requirements via Ansible or do
                  # nothing if the virtual machine is already provisioned
    $ ./deploy    # Shell wrapper around an Ansible call

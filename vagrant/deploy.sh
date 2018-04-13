#!/bin/sh

# Export envirronment variables understood by Ansible
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

#!/bin/sh

if [ ! -x /usr/local/bin/ansible ]; then
    echo "install ansible"
    exit -1
fi

cd ${HOME}/common/setup/ansible-macos

echo "setup macOS package"
ansible-playbook -i hosts macos.yml

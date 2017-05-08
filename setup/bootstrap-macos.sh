#!/bin/sh

printf "check Command Line Tools ... "

if [ -x /Library/Developer/CommandLineTools/usr/bin/clang ]; then
    echo "installed"
else
    echo "not exist"
    xcode-select --install
    if [ $? -eq 0 ]; then
        while true; do
            printf "Please install CommandLineTools (Y/n) "
            read yn
            case ${yn} in
                [yY]*) break ;;
                *) exit ;;
            esac
        done
    fi

    if [ -x /Library/Developer/CommandLineTools/usr/bin/clang ]; then
        echo "installed Command Line Tools."
    else
        echo "can't install Command Line Tools."
        exit -1
    fi
fi

printf "check Homebrew..."
if [ -x /usr/local/bin/brew ]; then
    echo "installed"
else
    echo "not exist"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    if [ -x /usr/local/bin/brew ]; then
        echo "installed Homebrew."
    else
        echo "can't install Homebrew."
        exit -1
    fi
fi

printf "check Ansible..."
if [ -x /usr/local/bin/ansible ]; then
    echo "installed"
else
    echo "not exist"
    brew install ansible

    if [ -x /usr/local/bin/ansible ]; then
        echo "installed Ansible."
    else
        echo "can't install Ansible."
        exit -1
    fi
fi

echo "fetch dotfiles..."
if [ ! -d ${HOME}/common ]; then
    git clone https://github.com/fukusaka/dotfiles.git ${HOME}/common
fi

if [ -d ${HOME}/common ]; then

    if [ ! -d ${HOME}/common/setup/ansible-macos/roles/kadaan.atom-packages ]; then
            ansible-galaxy install --roles-path=roles kadaan.atom-packages
    fi

    ${HOME}/common/setup/setup-macos-common.sh
fi

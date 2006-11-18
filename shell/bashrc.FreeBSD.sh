#
#export PATH=${PATH}:/usr/sfw/bin:/opt/sfw/bin

# 色付け
if [ -x /opt/sfw/bin/dircolors ]; then
    eval `/opt/sfw/bin/dircolors -b`
    alias l='/opt/sfw/bin/ls -FNB --color=auto --format=long'
    alias ls='/opt/sfw/bin/ls -FNB --color=auto'
    alias ll='/opt/sfw/bin/ls -FNB --color=auto --format=long --full-time'
    alias p='/bin/ps -ef'
else
    alias l='/bin/ls -l -F'
    alias ls='/bin/ls -F'
    alias ll='/bin/ls -l -F'
    alias p='/bin/ps aux'
fi


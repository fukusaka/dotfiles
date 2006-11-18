# 色付け
if [ -x /usr/bin/dircolors ]; then
    eval `dircolors -b`
    alias l='/bin/ls -FNB --color=auto --format=long'
    alias ls='/bin/ls -FNB --color=auto'
    alias ll='/bin/ls -FNB --color=auto --format=long --full-time'
    alias p='/bin/ps aux'
else
    alias l='/bin/ls -l -FB'
    alias ls='/bin/ls -FB'
    alias ll='/bin/ls -l -FB'
    alias p='/bin/ps aux'
fi

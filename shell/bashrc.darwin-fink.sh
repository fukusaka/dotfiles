. /sw/bin/init.sh

# 標準設定
PS1="% "

# 色付け
if [ -x /sw/bin/dircolors ]; then
    eval `/sw/bin/dircolors -b`
    alias l='/sw/bin/ls -FNB --color=auto --format=long'
    alias ls='/sw/bin/ls -FNB --color=auto'
    alias ll='/sw/bin/ls -FNB --color=auto --format=long --full-time'
    alias p='/sw/bin/ps aux'
else
    alias l='/bin/ls -l -FB'
    alias ls='/bin/ls -FB'
    alias ll='/bin/ls -l -FB'
    alias p='/bin/ps aux'
fi

export EDITOR=/usr/bin/vi
export TEXEDIT='/usr/bin/vi %s'
test -x /usr/bin/pager && export PAGER=/usr/bin/pager


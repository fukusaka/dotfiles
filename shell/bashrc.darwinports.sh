# Darwinportsの組み入れ
#export PATH=${PATH}:/opt/local/bin:/opt/local/sbin
export PATH=/opt/local/bin:/opt/local/sbin:${PATH}

## 色付け
if [ -x /opt/local/bin/gls ]; then
    alias l='/opt/local/bin/gls -FNB --color=auto --format=long'
    alias ls='/opt/local/bin/gls -FNB --color=auto'
    alias ll='/opt/local/bin/gls -FNB --color=auto --format=long --full-time'
    alias p='/bin/ps aux'
else
    alias l='/bin/ls -l -FB'
    alias ls='/bin/ls -FB'
    alias ll='/bin/ls -l -FB'
    alias p='/bin/ps aux'
fi

# ~/.bash_profile: executed by bash(1) for login shells.

umask 002

if [ -f '~/.bashrc' ]; then . '~/.bashrc'; fi

################################################################

PATH=.:${HOME}/bin:${PATH}
INFOPATH=/usr/info:~/info
CVSROOT=/home/cvsroot
export PATH INFOPATH CVSROOT

# コマンドの指定
export EDITOR=vi
export PAGER=/usr/bin/less
export TEX=ptex
#export CC=egcc

# 言語の指定
#LANG=ja_JP.eucJP
LANG=ja_JP.ujis
#LC_ALL=ja_JP.ujis
LC_COLLATE=ja_JP.ujis
LC_MESSAGES=ja_JP.ujis
export LANG LC_ALL LC_COLLATE LC_MESSAGES

export LESSCHARSET=japanese-euc

#function emacs () {
#    ( XMODIFIERS='@im=none' ; /usr/bin/emacs )
#}

# プリンターの指定
export PRINTER="gs360"

# News Server の設定
# setenv NNTPSERVER news.hoge.hoge.ac.jp
#setenv NEWSSPOOL /var/spool/news

# proxy サーバごしに WWW を見る場合に lynx や www.el に必要な設定
http_proxy=http://localhost:3128/
# ftp_proxy=http://proxy.hoge.hoge.ac.jp:999/
# wais_proxy=http://proxy.hoge.hoge.ac.jp:999/
# gopher_proxy=http://proxy.hoge.hoge.ac.jp:999/
export http_proxy
#export http_proxy ftp_proxy wais_proxy gopher_proxy

if [ -n "$PS1" ]; then
# This shell is interactive
    PS1="% "
    # stty (tty type の設定)
    stty erase ^H
    stty intr ^C
    stty susp ^Z
    stty start undef
    stty stop undef
#    tty cs8 -istrip -parenb
    eval `dircolors -b`
    eval `dircolors -b`
    if [ "$LS_COLORS" == "" ]; then
	alias  ls='\ls -FN'
    else
	alias  ls='\ls -FN --color=auto'
    fi
fi

# use PostgresSQL

if [ -f /etc/postgresql/postgresql.env ]; then
    . /etc/postgresql/postgresql.env
fi

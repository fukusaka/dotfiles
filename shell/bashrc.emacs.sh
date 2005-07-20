# ~/.bashrc
# $Id: bashrc 111 2003-09-14 15:33:41Z fukusaka $

# 対話シェルで無ければ、以後実行しない。
[ -z "$PS1" ] && return

PS1="% "

# 色付け無し
alias l='/usr/bin/ls -l -FB'
alias ls='/usr/bin/ls -FB'
alias ll='/usr/bin/ls -l -FB'
alias p='/bin/ps aux'

# ファイルオープンの修正
export EDITOR='emacsclient'
export TEXEDIT='emacsclient +%d %s'
export PAGER=${HOME}/bin/pageremacs

# quick hack
if [ "x`uname`" = "xDarwin" ]; then
alias l='/sw/bin/ls -l -FB'
alias ls='/sw/bin/ls -FB'
alias ll='/sw/bin/ls -l -FB'
unset LC_ALL
fi

# Local Variables:
# mode: shell-script
# End:

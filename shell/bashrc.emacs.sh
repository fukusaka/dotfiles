# ~/.bashrc
# $Id: bashrc 111 2003-09-14 15:33:41Z fukusaka $

# 対話シェルで無ければ、以後実行しない。
[ -z "$PS1" ] && return

PS1="% "

# 色付け無し
alias l='/bin/ls -l -FB'
alias ls='/bin/ls -FB'
alias ll='/bin/ls -l -FB'
alias p='/bin/ps aux'

# ファイルオープンの修正
export EDITOR='emacsclient'
export TEXEDIT='emacsclient +%d %s'
export PAGER=${HOME}/bin/pageremacs

# Local Variables:
# mode: shell-script
# End:

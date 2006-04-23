#

# umask
umask 022

# 標準設定
PS1="% "

# 言語の指定
#export LANG=ja_JP.eucJP
export LANG=ja_JP.UTF-8
export LC_TIME=C

# パスの指定
#export PATH=${PATH}:${HOME}/bin
export INFOPATH=~/info:/usr/info:/usr/share/info
	
# コマンドの指定
export EDITOR=/usr/bin/vi
export PAGER=/usr/bin/less
export TEXEDIT='/usr/bin/vi %s'

# その他
#export CANNAHOST=localhost

## TeX 設定
#TEXINPUTS=.//:${TEXCONFDIR}//:
#TEX=ptex
#TEXEDIT=${EDITOR}
#export TEX TEXINPUTS TEXEDIT

# Local Variables:
# mode: shell-script
# End:

# ~/.bashrc: executed by bash(1) for non-login shells.

if [ -n "$PS1" ]; then
# This shell is interactive
    PS1="% "
    # stty (tty type ������)
    stty erase ^H
    stty intr ^C
    stty susp ^Z
    stty start undef
    stty stop undef
#    tty cs8 -istrip -parenb
    eval `dircolors -b`
    if [ "$LS_COLORS" == "" ]; then
	alias  ls='\ls -FN'
    else
	alias  ls='\ls -FN --color=auto'
    fi

    if [ "$EMACS" == "t" -a "$TERM" == "dumb" ]; then
    	export EDITOR='emacsclient'
    	export TEXEDIT='emacsclient +%d %s'
    	export PAGER=${HOME}/bin/pageremacs
    else
    	export EDITOR=/usr/bin/vi
    	export TEXEDIT='/usr/bin/vi %s'
    	export PAGER=/usr/bin/less	
    fi
else
    export EDITOR=/usr/bin/vi
    export TEXEDIT='/usr/bin/vi %s'
    export PAGER=/usr/bin/less	
fi

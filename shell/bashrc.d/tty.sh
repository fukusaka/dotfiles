if [ "$PS1" != "" ]; then
# This shell is interactive
    PS1="% "
    # stty (tty type ¤ÎÀßÄê)
    stty erase ^H
    stty intr ^C
    stty susp ^Z
    stty start undef
    stty stop undef
#    tty cs8 -istrip -parenb
fi

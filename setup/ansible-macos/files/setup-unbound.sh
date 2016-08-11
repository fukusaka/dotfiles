#!/bin/sh

UNBOUND_CONTROL_KEY=/usr/local/etc/unbound/unbound_control.key

UNBOUND_PLIST=/usr/local/opt/unbound/homebrew.mxcl.unbound.plist
UNBOUND_REAL_PLIST=/Library/LaunchDaemons/homebrew.mxcl.unbound.plist

# control.key
if [ ! -f $UNBOUND_CONTROL_KEY ]; then
    unbound-control-setup
    brew unlink unbound
    brew link unbound
fi

# plist
cmp $UNBOUND_PLIST $UNBOUND_REAL_PLIST || {
        sudo cp $UNBOUND_PLIST $UNBOUND_REAL_PLIST
        sudo launchctl load $UNBOUND_REAL_PLIST
}

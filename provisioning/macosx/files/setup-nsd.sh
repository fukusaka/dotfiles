#!/bin/sh

NSD_REAL_CONF=/usr/local/etc/nsd/nsd-real.conf
NSD_CONTROL_KEY=/usr/local/etc/nsd/nsd_control.key

NSD_PLIST=/usr/local/etc/nsd/homebrew.mxcl.nsd.plist
NSD_REAL_PLIST=/Library/LaunchDaemons/homebrew.mxcl.nsd.plist

NSD=$(brew --prefix nsd)/sbin/nsd
NSD_CONF=$(${NSD} -h 2>&1 | grep configfile | awk '{print $NF}' | sed 's/\.$//')

if [ ! -x $NSD ]; then
    echo "not install nsd"
    exit -1
fi

# config
if [ "x$(readlink $NSD_CONF)" != "x$NSD_REAL_CONF" ]; then
    ln -s $NSD_REAL_CONF $NSD_CONF
fi

# control.key
if [ ! -f $NSD_CONTROL_KEY ]; then
    nsd-control-setup
    brew unlink nsd
    brew link nsd
fi

# plist
cmp $NSD_PLIST $NSD_REAL_PLIST || {
        sudo cp $NSD_PLIST $NSD_REAL_PLIST
        sudo launchctl load $NSD_REAL_PLIST
}

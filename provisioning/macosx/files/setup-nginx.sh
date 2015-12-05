#!/bin/sh

NGINX_PLIST=/usr/local/opt/nginx/homebrew.mxcl.nginx.plist
NGINX_REAL_PLIST=/Library/LaunchDaemons/homebrew.mxcl.nginx.plist

# plist
test -f $NGINX_REAL_PLIST && cmp $NGINX_PLIST $NGINX_REAL_PLIST || {
        sudo cp $NGINX_PLIST $NGINX_REAL_PLIST
        sudo launchctl load $NGINX_REAL_PLIST
}

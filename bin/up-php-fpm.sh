#!/bin/bash

VERS="
5.5.30
"
#5.4.36
#5.3.29

sudo killall php-fpm
for ver in $VERS; do
        sudo /opt/phpbrew/php/php-${ver}/sbin/php-fpm
done

# Script behaviour

This shows the order and the parameters given to the install scripts

## rpm

upgrade
-> postinstall - 2
-> preremove - 1

remove
-> preremove - 0

install
-> postinstall - 1

## deb

upgrade
-> preremove - upgrade $version
-> postinstall - configure $version

remove
-> preremove - remove

install
-> postinstall - configure

## apk

upgrade
-> nothing

remove
-> preremove - $version

install
-> postinstall - $version

## pacman

upgrade
-> nothing

remove
-> preremove - $version

install
-> postinstall - $version

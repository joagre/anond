#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <distro>"
    echo "Example: $0 anond-0.2"
    exit 1
fi

DISTRO=${1}

cd local/${DISTRO}
bin/anond -s
killall -q tail
killall -q screen

#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <distro>"
    echo "Example: $0 anond-0.2"
    exit 1
fi

DISTRO=${1}

killall beam.smp
killall tail
killall screen
rm -fr install
mkdir install
mv ${DISTRO}.tgz install
cd install
tar xf ${DISTRO}.tgz
cd ${DISTRO}

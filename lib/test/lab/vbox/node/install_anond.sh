#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <distro>"
    echo "Example: $0 anond-0.2"
    exit 1
fi

DISTRO=${1}
NA=1.1.1.1:5000
DS=2.2.2.2:6700
MODE=node

killall -q beam.smp
killall -q tail
killall -q screen
rm -fr local
mkdir local
cp ${DISTRO}.tgz local
cd local
tar xf ${DISTRO}.tgz
cd ${DISTRO}

case "${MODE}" in
    node)
        sed -e "s/0.0.0.0:50000/${NA}/g" -e "s/ds.anond.org:6700/${DS}/g" etc/anond.conf > etc/local-anond.conf
        ;;
    ds)
        sed -e "s/0.0.0.0:6700/${DS}/g" etc/anond.conf > etc/local-anond.conf
esac

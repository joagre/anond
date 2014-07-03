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

# Kill and remove all
killall -q beam.smp
killall -q tail
killall -q screen
rm -fr local

# Unpack distro
mkdir local
mv ${DISTRO}.tgz local
cd local
tar xf ${DISTRO}.tgz
cd ${DISTRO}

# Prepare etc/local-anond.conf
if [ "${MODE}" = "node" ]; then
    sed -e "s/ds.anond.org:6700/${DS}/g" etc/anond.conf > etc/local-anond.conf
else
    cp etc/anond.conf etc/local-anond.conf
fi

# A handy screenrc
cat > screenrc <<EOF
screen -t anond 1 tail --follow=name --retry bin/anond -i -c etc/local-anond.conf
screen -t daemon.log 0 tail --follow=name --retry daemon.log
screen -t dbg 1 tail --follow=name --retry dbg.log
screen -t error 1 tail --follow=name --retry error.log
EOF

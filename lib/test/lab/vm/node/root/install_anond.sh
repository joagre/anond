#!/bin/sh

if [ $# -ne 3 ]; then
    echo "Usage: $0 <distro-vsn> <directory-server> node | ds"
    echo "Example: $0 anond-0.2 192.168.1.3:6700 node"
    echo "Example: $0 anond-0.2 0.0.0.0:6700 ds"
    exit 1
fi

BASE_DIR=$(dirname $(readlink -f $0))
DISTRO_VSN=${1}
DIRECTORY_SERVER=${2}
NODE_TYPE=${3}

# Stop anond and remove old distro
${BASE_DIR}/stop_anond.sh
rm -fr anond *.log *.db

# Unpack new distro
tar xf ${DISTRO_VSN}.tgz
mv ${DISTRO_VSN} anond

# Create anond/etc/local-anond.conf
case ${NODE_TYPE} in
    node)
	sed -e "s/127.0.0.1:6700/${DIRECTORY_SERVER}/g" anond/etc/anond.conf > anond/etc/local-anond.conf
	;;
    ds)
	sed -e "s/127.0.0.1:6700/${DIRECTORY_SERVER}/g" anond/etc/anond-ds-only.conf > anond/etc/local-anond.conf
	;;
esac

# A handy screenrc
cat > screenrc <<EOF
screen -t anond 1 anond/bin/anond -i -c anond/etc/local-anond.conf
screen -t daemon.log 2 tail --follow=name --retry daemon.log
screen -t dbg.log 3 tail --follow=name --retry dbg.log
screen -t error.log 4 tail --follow=name --retry error.log
EOF

#!/bin/sh

if [ $# -ne 4 ]; then
    echo "Usage: $0 <host-only-ip> <path-to-distro> <directory-server> node | ds"
    echo "Example: $0 10.0.0.3 /tmp/anond-0.2.tgz 192.168.1.3:6700 node"
    echo "Example: $0 10.0.0.3 /tmp/anond-0.2.tgz 0.0.0.0:6700 ds"
    exit 1
fi

ROOT_PASSWD=mortuta42
BASE_DIR=$(dirname $(readlink -f $0))
HOST_ONLY_IP=${1}
PATH_TO_DISTRO=${2}
DISTRO_VSN=$(basename ${PATH_TO_DISTRO} .tgz)
DIRECTORY_SERVER=${3}
NODE_TYPE=${4}

# Copy resources to the vm
sshpass -p ${ROOT_PASSWD} scp -o StrictHostKeyChecking=no ${PATH_TO_DISTRO} root@${HOST_ONLY_IP}/root || exit 100
sshpass -p ${ROOT_PASSWD} scp -o StrictHostKeyChecking=no ${BASE_DIR}/vm/node/root/install_anond.sh root@${HOST_ONLY_IP}/root || exit 200
sshpass -p ${ROOT_PASSWD} scp -o StrictHostKeyChecking=no ${BASE_DIR}/vm/node/root/start_anond.sh root@${HOST_ONLY_IP}/root || exit 300
sshpass -p ${ROOT_PASSWD} scp -o StrictHostKeyChecking=no ${BASE_DIR}/vm/node/root/stop_anond.sh root@${HOST_ONLY_IP}/root || exit 400

# Install anond 
sshpass -p ${ROOT_PASSWD} ssh -o StrictHostKeyChecking=no root@${HOST_ONLY_IP} "/root/install_anond.sh ${DISTRO_VSN} ${DIRECTORY_SERVER} ${NODE_TYPE}" || exit 500

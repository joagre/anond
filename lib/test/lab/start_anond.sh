#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <host-only-ip>"
    echo "Example: $0 10.0.0.3"
    exit 1
fi

BASE_DIR=$(dirname $(readlink -f $0))
ROOT_PASSWD=mortuta42
HOST_ONLY_IP=${1}

sshpass -p ${ROOT_PASSWD} ssh -o StrictHostKeyChecking=no root@${HOST_ONLY_IP} "/root/start_anond.sh" || exit 100

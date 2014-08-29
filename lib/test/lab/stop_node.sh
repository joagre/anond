#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <host-only-ip>"
    echo "Example: $0 11.0.0.3"
    exit 1
fi

ROOT_PASSWD=mortuta42
HOST_ONLY_IP=${1}

sshpass -p ${ROOT_PASSWD} ssh -o StrictHostKeyChecking=no root@${HOST_ONLY_IP} "/sbin/halt" || exit 100

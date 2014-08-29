#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <bridge-ip>"
    echo "Example: $0 192.168.1.3"
    exit 1
fi

ROOT_PASSWD=mortuta42
BRIDGE_IP=${1}

sshpass -p ${ROOT_PASSWD} ssh -o StrictHostKeyChecking=no root@${BRIDGE_IP} "/sbin/halt" || exit 100

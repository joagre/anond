#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <host-only-ip>"
    echo "Example: $0 10.0.0.3"
    exit 1
fi

ROOT_PASSWD=mortuta42
HOST_ONLY_IP=${1}

VBoxManage controlvm node-${HOST_ONLY_IP} poweroff > /dev/null &2>1
sleep 2
VBoxManage unregistervm --delete node-${HOST_ONLY_IP} || exit 100

#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <host-only-ip>"
    echo "Example: $0 11.0.0.3"
    exit 1
fi

HOST_ONLY_IP=${1}

VBoxManage startvm node-${HOST_ONLY_IP} --type gui || exit 100

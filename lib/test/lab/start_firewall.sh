#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <bridge-ip>"
    echo "Example: $0 192.168.1.3"
    exit 1
fi

BRIDGE_IP=${1}

VBoxManage startvm firewall-${BRIDGE_IP} --type gui || exit 100

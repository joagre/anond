#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <bridge-ip>"
    echo "Example: $0 192.168.1.3"
    exit 1
fi

ROOT_PASSWD=mortuta42
BRIDGE_IP=${1}

VBoxManage controlvm firewall-${BRIDGE_IP} poweroff > /dev/null &2>1
sleep 2
VBoxManage unregistervm --delete firewall-${BRIDGE_IP} || exit 100

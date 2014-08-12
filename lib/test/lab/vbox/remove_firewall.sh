#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: $0 <root-passwd> <ip>"
    echo "Example: $0 mortuta42 192.168.1.3"
    exit 1
fi

ROOT_PASSWD=${1}
IP=${2}
VBoxManage controlvm firewall-${IP} poweroff > /dev/null &2>1
VBoxManage unregistervm --delete firewall-${IP} || exit 100

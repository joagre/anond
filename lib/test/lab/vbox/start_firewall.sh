#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <ip>"
    echo "Example: $0 192.168.1.3"
    exit 1
fi

IP=${1}
VBoxManage startvm firewall-${IP} --type gui || exit 100

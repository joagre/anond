#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: $0 <host-only-ip> <firewall-ip>"
    echo "Example: $0 11.0.0.3 11.0.1.3"
    exit 1
fi

ROOT_PASSWD=mortuta42
ORIGIN_HOST_ONLY_IP=11.0.0.2
ORIGIN_FIREWALL_IP=11.0.1.2
HOST_ONLY_IP=${1}
FIREWALL_IP=${2}

# Clone node
VBoxManage clonevm --register --name node-${HOST_ONLY_IP} node || exit 100

# Start node
VBoxManage startvm node-${HOST_ONLY_IP} --type gui || exit 200

# Change the ip-address on the host-only network adapter
sshpass -p ${ROOT_PASSWD} ssh -o ConnectTimeout=120 -o ConnectionAttempts=120 -o StrictHostKeyChecking=no root@${ORIGIN_HOST_ONLY_IP} "sh -c 'sed -e \"s/address ${ORIGIN_HOST_ONLY_IP}/address ${HOST_ONLY_IP}/\" -e \"s/gateway ${ORIGIN_FIREWALL_IP}/gateway ${FIREWALL_IP}/\" /etc/network/interfaces.in > /etc/network/interfaces; /sbin/halt'" || exit 300

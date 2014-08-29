#!/bin/sh

if [ $# -ne 3 ]; then
    echo "Usage: $0 <host-only-ip> <bridge-ip> <gateway-ip>"
    echo "Example: $0 11.0.1.3 192.168.1.3 192.168.1.254"
    exit 1
fi

ROOT_PASSWD=mortuta42
ORIGIN_HOST_ONLY_IP=11.0.1.2
ORIGIN_BRIDGE_IP=192.168.1.2
ORIGIN_GATEWAY_IP=192.168.1.254
HOST_ONLY_IP=${1}
BRIDGE_IP=${2}

# Clone firewall
VBoxManage clonevm --register --name firewall-${BRIDGE_IP} firewall || exit 100

# Start firewall
VBoxManage startvm firewall-${BRIDGE_IP} --type gui || exit 200

# Change the ip-address on the bridge and host-only network adapters
sshpass -p ${ROOT_PASSWD} ssh -o ConnectTimeout=120 -o ConnectionAttempts=120 -o StrictHostKeyChecking=no root@${ORIGIN_HOST_ONLY_IP} "sh -c 'sed -e \"s/address ${ORIGIN_HOST_ONLY_IP}/address ${HOST_ONLY_IP}/\" -e \"s/address ${ORIGIN_BRIDGE_IP}/address ${BRIDGE_IP}/\" -e \"s/gateway ${ORIGIN_GATEWAY_IP}/gateway ${GATEWAY_IP}/\" /etc/network/interfaces.in > /etc/network/interfaces; /sbin/halt'"

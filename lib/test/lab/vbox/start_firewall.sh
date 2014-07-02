#!/bin/sh

if [ $# -ne 5 ]; then
    echo "Usage: $0 <root-passwd> <interface> <ip> <netmask> <new-ip>"
    echo "Example: $0 mortuta42 eth0 192.168.1.2 255.255.255.0 192.168.1.3"
    exit 1
fi

ROOT_PASSWD=${1}
INTERFACE=${2}
IP=${3}
NETMASK=${4}
NEW_IP=${5}

# Clone firewall
VBoxManage clonevm --register --name firewall-${NEW_IP} firewall

# Start firewall
VBoxManage startvm firewall-${NEW_IP} --type gui || exit 200

# Change the ip-address on the bridged network adapter
sshpass -p ${ROOT_PASSWD} ssh -n -f -o ConnectTimeout=120 -o ConnectionAttempts=120 -o StrictHostKeyChecking=no root@${IP} "sh -c 'ifconfig ${INTERFACE} ${NEW_IP} netmask ${NETMASK} > /tmp/${NEW_IP}.log 2>&1 &'" || exit 300

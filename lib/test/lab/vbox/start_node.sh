#!/bin/sh

if [ $# -ne 3 ]; then
    echo "Usage: $0 <root-passwd> <interface> <firewall-ip>"
    echo "Example: $0 mortuta42 eth0 192.168.1.3"
    exit 1
fi

ROOT_PASSWD=${1}
INTERFACE=${2}
FIREWALL_IP=${3}

# Clone firewall
#VBoxManage clonevm --register --name node-${FIREWALL_IP} node

# Start firewall
#VBoxManage startvm node-${FIREWALL_IP} --type gui || exit 200

# Tell firewall to initialize node
IP=`VBoxManage guestproperty get node-${FIREWALL_IP} "/VirtualBox/GuestInfo/Net/0/V4/IP" | sed -e "s/Value: //"`
sshpass -p ${ROOT_PASSWD} scp -o StrictHostKeyChecking=no $(dirname $0)/prepare_init_node.sh root@${FIREWALL_IP}:/root || exit 300
sshpass -p ${ROOT_PASSWD} scp -o StrictHostKeyChecking=no $(dirname $0)/init_node.sh root@${FIREWALL_IP}:/root || exit 400
sshpass -p ${ROOT_PASSWD} ssh StrictHostKeyChecking=no root@${FIREWALL_IP} "sh /root/prepare_init_node.sh ${ROOT_PASSWD} ${IP}" || exit 500

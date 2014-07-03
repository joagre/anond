#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: $0 <root-passwd> <ip>"
    echo "Example: $0 192.168.1.3"
    exit 1
fi

ROOT_PASSWD=${1}
IP=${1}

# Initialize node
sshpass -p ${ROOT_PASSWD} scp -o ConnectTimeout=120 -o ConnectionAttempts=120 -o StrictHostKeyChecking=no -o StrictHostKeyChecking=no $(dirname $0)/init_node.sh root@${IP}:/root || exit 100
sshpass -p ${ROOT_PASSWD} ssh -o StrictHostKeyChecking=no root@${IP} "/root/init_node.sh" || exit 200

#ip route add default via 192.168.56.101 dev eth0

#!/bin/sh
BASE_DIR=$(dirname $(readlink -f $0))
${BASE_DIR}/stop_anond.sh
screen -d -m -c screenrc

CC=gcc
CMD_DIR=$(dir$(lastword $(MAKEFILE_LIST)))../priv
CMD_PATH=$(CMD_DIR)/procket

all: dirs $(CMD_PATH)

dirs:
	-@mkdir -p $(CMD_DIR)

$(CMD_PATH):
	$(CC) $(PROCKET_CFLAGS) -Wall -o $(CMD_PATH) -L. procket_cmd.c -lancillary

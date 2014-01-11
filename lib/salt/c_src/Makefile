include ../../../support/erl.mk

all:
	$(CC) -c $(CFLAGS) -g -Wall -fPIC -I$(ERL_TOP)/lib/erlang/lib/erl_interface-*/include -Wall -Werror -I/usr/local/include/sodium salt_nif.c -o salt_nif.o
	$(CC) salt_nif.o $(LDFLAGS) -shared -L$(ERL_TOP)/lib/erlang/lib/erl_interface-*/lib -lerl_interface -lei -L/usr/local/lib -Wl,-R/usr/local/lib -lsodium -o ../priv/salt_nif.so

clean:
	rm -rf *.o ../priv/salt_nif.so

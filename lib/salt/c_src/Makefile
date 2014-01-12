include ../../../support/erl.mk
include ../../../support/c_src.mk

ifeq ($(OS),Darwin)
    LDFLAGS=-bundle -flat_namespace -undefined suppress
else
    LDFLAGS=-shared
endif

all:
	$(CC) -c -Wall -Werror -fPIC -I$(wildcard $(ERL_TOP)/lib/erlang/erts-*/include) -I/opt/local/include/sodium salt_nif.c -o salt_nif.o
	$(CC) salt_nif.o $(LDFLAGS) -L$(wildcard $(ERL_TOP)/lib/erlang/lib/erl_interface-*/lib) -lerl_interface -lei -L/opt/local/lib -lsodium -o ../priv/salt_nif.so

clean:
	rm -rf *.o ../priv/salt_nif.so

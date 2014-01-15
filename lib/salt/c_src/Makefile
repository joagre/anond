include ../../../support/erl.mk
include ../../../support/c.mk

ifeq ($(OS),Darwin)
    LDFLAGS=-bundle -flat_namespace -undefined suppress
else
    LDFLAGS=-shared
endif

all:
	$(CC) $(CFLAGS) -c -Wall -Werror -fPIC salt_nif.c -o salt_nif.o
	$(CC) $(LDFLAGS) salt_nif.o $(ERL_TOP)/lib/erlang/usr/lib/liberl_interface.a $(ERL_TOP)/lib/erlang/usr/lib/libei.a /usr/local/lib/libsodium.a -o ../priv/salt_nif.so

clean:
	rm -rf *.o ../priv/salt_nif.so

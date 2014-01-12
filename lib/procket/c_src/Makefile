include ../../../support/erl.mk
include ../../../support/c_src.mk

ifeq ($(OS),Darwin)
    LDFLAGS=-bundle -flat_namespace -undefined suppress
else
    LDFLAGS=-shared
endif

all:
	$(MAKE) -f Makefile.ancillary
	$(MAKE) -f Makefile.cmd
	$(CC) -c -Wall -fPIC -I$(wildcard $(ERL_TOP)/lib/erlang/erts-*/include) procket.c -o procket.o
	$(CC) procket.o -L. -lancillary $(LDFLAGS) -L$(wildcard $(ERL_TOP)/lib/erlang/lib/erl_interface-*/lib) -lerl_interface -lei -o ../priv/procket.so

clean:
	rm -f *.a *.o ../priv/procket ../priv/procket.so

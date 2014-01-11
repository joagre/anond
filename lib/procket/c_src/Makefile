include ../../../support/erl.mk

all:
	$(MAKE) -f Makefile.ancillary
	$(MAKE) -f Makefile.cmd
	$(CC) -c -g -Wall -fPIC -I$(ERL_TOP)/lib/erlang/lib/erl_interface-*/include -I$(ERL_TOP)/lib/erlang/lib/erl_interface-*/include procket.c -o procket.o
	$(CC) procket.o -L. -lancillary -shared -L$(ERL_TOP)/lib/erlang/lib/erl_interface-*/lib -lerl_interface -lei -o ../priv/procket.so

clean:
	rm -f *.a *.o ../priv/procket ../priv/procket.so

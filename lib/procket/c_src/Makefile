include ../../../support/erl.mk
include ../../../support/c.mk

ifeq ($(OS),Darwin)
    LDFLAGS=-bundle -flat_namespace -undefined suppress
else
    LDFLAGS=-shared
endif

all: ../priv/procket.so

../priv/procket.so: procket.o
	$(CC) $(LDFLAGS) procket.o ./libancillary.a $(ERL_TOP)/lib/erlang/usr/lib/liberl_interface.a $(ERL_TOP)/lib/erlang/usr/lib/libei.a -o ../priv/procket.so

procket.o: procket.c
	$(MAKE) -f Makefile.ancillary
	$(MAKE) -f Makefile.cmd
	$(CC) $(CFLAGS) -c -Wall -Werror -fPIC procket.c -o procket.o

clean:
	rm -f *.a *.o ../priv/procket ../priv/procket.so
	rm -fr ../priv/procket.dSYM

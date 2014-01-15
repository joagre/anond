OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/lib/erlang/usr/include -I/usr/include/sodium -I/usr/local/include/sodium -I/opt/local/include/sodium
LIBSODIUM=/opt/local/lib/libsodium.a

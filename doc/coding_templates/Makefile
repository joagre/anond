include ../../support/erl.mk

ERL_SOURCES:=$(wildcard *.erl)
ERL_OBJECTS:=$(ERL_SOURCES:%.erl=%.beam)

all: $(ERL_OBJECTS)

clean:
	rm -f *.beam

%.beam: %.erl
	$(ERLC) -I ../../lib  $(ERLC_FLAGS) $<

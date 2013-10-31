all: $(ERL_OBJECTS)

$(ERL_OBJECTS): $(ERL_HEADERS)

clean:
	rm -f ../ebin/*.beam

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

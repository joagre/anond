include erl.mk

.PHONY: dialyzer

# salt, procket, tunctl and test excluded
LIBS=common ds jsx node overseer util

dialyzer: dialyzer.plt
	$(DIALYZER) -q --no_check_plt --plt $< -r $(LIBS:%=../lib/%/ebin)

dialyzer.plt: dialyzer_init.plt
	rm -f $@ ; cp $< $@
	$(DIALYZER) --check_plt --plt $@ || true

DIALYZER_PLT_APPS=compiler crypto erts kernel sasl stdlib tools

dialyzer_init.plt:
	rm -f $@
	@echo "BEWARE: This will take several minutes the first time..."
	$(DIALYZER) --build_plt --output_plt $@ \
	    -r $(DIALYZER_PLT_APPS:%=$(ERL_TOP)/lib/erlang/lib/%-*/ebin)

clean:
	rm -f dialyzer.plt

mrproper:
	rm -f dialyzer_init.plt

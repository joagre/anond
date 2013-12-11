include support/erl.mk

LIBS=common ds jsx node overseer procket test tunctl util

all:
	(cd bin; $(MAKE) all)
	for lib in $(LIBS) ; do \
		(cd lib/$$lib && $(MAKE) all) || exit 1; \
	done

dialyzer: all
	(cd support; $(MAKE) dialyzer)

clean:
	(cd support; $(MAKE) clean)
	(cd bin; $(MAKE) clean)
	(cd build; $(MAKE) clean)
	for lib in $(LIBS) ; do \
		(cd lib/$$lib && $(MAKE) clean) || exit 1; \
	done

mrproper: clean cleanfluff
	(cd support; $(MAKE) mrproper)

cleanfluff:
	find . \( -name erl_crash.dump -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;

release:
	(cd build; make)

###
### startup rules
###

debug:
	$(ERL) -pa lib/common/ebin -pa lib/ds/ebin -pa lib/jsx/ebin -pa lib/node/ebin -pa lib/overseer/ebin -pa lib/procket/ebin -pa lib/test/ebin -pa lib/tunctl/ebin -pa lib/util/ebin

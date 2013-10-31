all: start.boot start_clean.boot

start.boot start_clean.boot: config.reltool
	./mkboot config.reltool .

clean:
	rm -f *.script *.boot

include ../vsn.mk

TARGET_DIR=anond-${VSN}
ARCHIVE_FILE=anond-${VSN}.tgz

all: rebuild clean
	mkdir ${TARGET_DIR}
	./mktarget ../bin/config.reltool ${TARGET_DIR}
	strip ${TARGET_DIR}/erts-*/bin/beam.smp
	strip ${TARGET_DIR}/erts-*/bin/child_setup
	strip ${TARGET_DIR}/erts-*/bin/ct_run
	strip ${TARGET_DIR}/erts-*/bin/erl
	strip ${TARGET_DIR}/erts-*/bin/erlexec
	strip ${TARGET_DIR}/erts-*/bin/inet_gethost
	sed '/# @DEVEL_ONLY_START/,/# @DEVEL_ONLY_STOP/d' ../bin/anond > ${TARGET_DIR}/bin/anond
	chmod +x ${TARGET_DIR}/bin/anond
	cp ../LICENSE ../README.md ../THANKS ${TARGET_DIR}
	cp -r ../etc ../doc ${TARGET_DIR}
	tar --exclude=.svn -z -c -v -f ${ARCHIVE_FILE} ${TARGET_DIR}

rebuild:
	(cd ..; env RELEASE=true make clean all)

clean:
	rm -f *.tgz
	rm -fr ${TARGET_DIR} ${ARCHIVE_FILE}

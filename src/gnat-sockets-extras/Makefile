include Makefile.project
-include .config

EXECUTABLES=$(GENERATED_EXECUTABLES) $(SCRIPTS)

all: build metrics

build: fix-whitespace $(GENERATED_SOURCES)
	gnatmake -p -P $(PROJECT)

test: build metrics
	@./tests/build
	@./tests/run

install: build test
	install -t ${HOME}/bin/ $(EXECUTABLES)

clean:
	gnatclean -P $(PROJECT) || true
	find . -name "*~" -type f -print0 | xargs -0 -r /bin/rm
	rm -f **/*.o **/*.ali
	if [ ! -z "$(GENERATED_SOURCES)" ]; then rm -f $(GENERATED_SOURCES); fi
	rmdir bin || true
	rmdir obj || true

distclean: clean
	rm -f $(GENERATED_EXECUTABLES)
	rm -f obj/*.ad[sb].metrix
	rmdir bin || true
	rmdir obj || true

fix-whitespace:
	@find src tests -name '*.ad?' | xargs --no-run-if-empty egrep -l '	| $$' | grep -v '^b[~]' | xargs --no-run-if-empty perl -i -lpe 's|	|        |g; s| +$$||g'

metrics:
	@gnat metric -P $(PROJECT)

-include Makefile.project_rules

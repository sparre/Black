include Makefile.project
-include .config

EXECUTABLES=$(GENERATED_EXECUTABLES) $(SCRIPTS)

all: build

build: fix-whitespace $(GENERATED_SOURCES)
	gnatmake -p -P $(PROJECT)

test: build
	@./tests/build
	@./tests/run

install: build test
	install -t ${HOME}/bin/ $(EXECUTABLES)

clean:
	gnatclean -P $(PROJECT) || true
	find . -type f \( -name "*~" -o -name "*.o" -o -name "*.ali" \) -print0 | xargs -0 -r /bin/rm
	if [ ! -z "$(GENERATED_SOURCES)" ]; then rm -rf $(GENERATED_SOURCES); fi
	rmdir bin || true
	rmdir obj || true

distclean: clean
	rm -f $(GENERATED_EXECUTABLES)
	rmdir bin || true
	rmdir obj || true

fix-whitespace:
	@find src tests -name '*.ad?' | xargs --no-run-if-empty egrep -l '	| $$' | grep -v '^b[~]' | xargs --no-run-if-empty perl -i -lpe 's|	|        |g; s| +$$||g'

-include Makefile.project_rules

.PHONY: all build test install clean distclean fix-whitespace


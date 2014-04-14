
.PHONY: src test

TEST_SUITES = wql_scanner_tests  wql_parser_tests

test: src
	./run_tests $(TEST_SUITES)

src:
	cd src && $(MAKE)

clean:
	cd src && $(MAKE) clean


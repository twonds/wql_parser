
include ../support/include.mk

YECC_FILE = wql_parser_core.yrl
YECC_ERL = $(YECC_FILE:.yrl=.erl)

ERL_SRC := $(ERL_SRC) $(YECC_ERL)

all: $(ERL_OBJ)

%.erl: %.yrl
	erlc $<

clean:
	rm -f $(ERL_OBJ) $(YECC_ERL)


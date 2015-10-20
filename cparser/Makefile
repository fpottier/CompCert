# Temporary Makefile, used to work on the grammar(s).

include ../Makefile.menhir

FLAGS    := $(MENHIR_FLAGS) -la 2
SRC      := $(HOME)/dev/menhir/src
MENHIR   := $(SRC)/_stage2/menhir.native
COMPCERT := pre_parser
MSGS     := handcrafted.messages
COMMAND  := $(MENHIR) $(MENHIR_MODE) $(COMPCERT).mly $(FLAGS)

all:
# Compile Menhir.
	make -C $(SRC)
	make -C $(SRC) bootstrap
# Produce a description of the automaton.
	$(COMMAND) --dump --explain
	sleep 2
#	dot -Tpdf -o $(COMPCERT).pdf $(COMPCERT).dot
# Compute the symbolic FOLLOW sets.
	$(COMMAND) -lg 3 2>$(COMPCERT).follow
# Re-generate the list of error messages.
	$(COMMAND) --list-errors >$(COMPCERT).messages
	@ echo "Number of error states that involve spurious reductions:"
	@ grep WARNING pre_parser.messages | wc -l

update:
# Update the auto-comments in $(MSGS).
	@ mv -f $(MSGS) $(MSGS).bak
	@ if ! $(COMMAND) --update-errors $(MSGS).bak > $(MSGS) ; then \
	  cp $(MSGS).bak $(MSGS) ; \
	fi

compare:
# Check $(MSGS) for completeness.
# We assume $(COMPCERT).messages is up-to-date.
	$(COMMAND) --compare-errors $(COMPCERT).messages --compare-errors $(MSGS)

compile:
	$(COMMAND) --compile-errors $(MSGS)

interpret:
# Interpret one sentence (interactive).
	$(COMMAND) --trace --interpret

interpret_error:
# Interpret one error sentence (interactive).
	$(COMMAND) --interpret-error

clean:
	rm -f $(COMPCERT).conflicts
	rm -f $(COMPCERT).messages
	rm -f $(COMPCERT).follow
	rm -f $(COMPCERT).dot
	rm -f $(COMPCERT).pdf
	rm -f lr.csv

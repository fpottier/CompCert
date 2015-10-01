# Temporary Makefile, used to work on the grammar(s).

SRC      := $(HOME)/dev/menhir/src
MENHIR   := $(SRC)/_stage2/menhir.native
COMPCERT := pre_parser
FLAGS    := --no-stdlib --lalr -la 2
MSGS     := handcrafted.messages

all:
# Compile Menhir.
	make -C $(SRC)
	make -C $(SRC) bootstrap
# Produce a description of the automaton.
# Compute the dependency graph.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --dump --explain --graph
	sleep 2
	dot -Tpdf -o $(COMPCERT).pdf $(COMPCERT).dot
# Compute the symbolic FOLLOW sets.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) -lg 3 2>$(COMPCERT).follow
# Re-generate the list of error messages.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --list-errors >$(COMPCERT).messages
	@ echo "Number of error states that involve spurious reductions:"
	@ grep WARNING pre_parser.messages | wc -l
#	sleep 2
# Re-generate the list of error messages for the canonical automaton.
#	$(MENHIR) $(COMPCERT).mly $(FLAGS) --canonical --list-errors >$(COMPCERT).canonical

update:
# Update the auto-comments in $(MSGS).
	@ mv -f $(MSGS) $(MSGS).bak
	@ if ! $(MENHIR) $(COMPCERT).mly $(FLAGS) --update-errors $(MSGS).bak > $(MSGS) ; then \
	  cp $(MSGS).bak $(MSGS) ; \
	fi

compare:
# Check $(MSGS) for completeness.
# We assume $(COMPCERT).messages is up-to-date.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --compare-errors $(COMPCERT).messages --compare-errors $(MSGS)

compile:
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --compile-errors $(MSGS)

interpret:
# Interpret one sentence (interactive).
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --trace --interpret

interpret_error:
# Interpret one error sentence (interactive).
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --interpret-error

interpret_error_canonical:
# Interpret one error sentence in the canonical automaton (interactive).
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --interpret-error --canonical

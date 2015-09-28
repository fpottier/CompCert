.PHONY: all interpret interpret_canonical

# Temporary Makefile, used to work on the grammar(s).

SRC      := $(HOME)/dev/menhir/src
MENHIR   := $(SRC)/_stage2/menhir.native
COMPCERT := pre_parser
FLAGS    := --no-stdlib --lalr -la 2

all:
# Compile Menhir.
	make -C $(SRC)
	make -C $(SRC) bootstrap
# Produce a description of the automaton.
# Compute the dependency graph.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --dump --explain --graph
	dot -Tpdf -o $(COMPCERT).pdf $(COMPCERT).dot
# Compute the symbolic FOLLOW sets.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) -lg 3 2>$(COMPCERT).follow
# Re-generate the list of error messages.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --list-errors >$(COMPCERT).messages
# Re-generate the list of error messages for the canonical automaton.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --canonical --list-errors >$(COMPCERT).canonical

interpret:
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --trace --interpret-error

interpret_canonical:
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --trace --interpret-error --canonical

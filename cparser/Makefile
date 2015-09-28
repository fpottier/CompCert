.PHONY: all interpret

# Temporary Makefile, used to work on the grammar(s).

SRC      := $(HOME)/dev/menhir/src
MENHIR   := $(SRC)/_stage1/menhir.native
COMPCERT := pre_parser
FLAGS    := --no-stdlib --lalr -la 2

all:
# Compile Menhir.
	make -C $(SRC)
# Produce a description of the automaton.
# Compute the symbolic FOLLOW sets.
# Compute the dependency graph.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --dump --explain --graph -lg 3 2>$(COMPCERT).follow
	dot -Tpdf -o $(COMPCERT).pdf $(COMPCERT).dot
# Re-generate the list of error messages.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --list-errors >$(COMPCERT).messages
# Re-generate the list of error messages for the canonical automaton.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --canonical --list-errors >$(COMPCERT).canonical

interpret:
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --trace --interpret


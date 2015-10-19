# Temporary Makefile, used to work on the grammar(s).

SRC      := $(HOME)/dev/menhir/src
MENHIR   := $(SRC)/_stage2/menhir.native
COMPCERT := pre_parser
FLAGS    := --no-stdlib -la 2 --table \
--on-error-reduce primary_expression \
--on-error-reduce postfix_expression \
--on-error-reduce unary_expression \
--on-error-reduce cast_expression \
--on-error-reduce multiplicative_expression \
--on-error-reduce additive_expression \
--on-error-reduce shift_expression \
--on-error-reduce relational_expression \
--on-error-reduce equality_expression \
--on-error-reduce and_expression \
--on-error-reduce exclusive_or_expression \
--on-error-reduce inclusive_or_expression \
--on-error-reduce logical_and_expression \
--on-error-reduce logical_or_expression \
--on-error-reduce conditional_expression \
--on-error-reduce assignment_expression \
--on-error-reduce expression
MSGS     := handcrafted.messages

all:
# Compile Menhir.
	make -C $(SRC)
	make -C $(SRC) bootstrap
# Produce a description of the automaton.
	$(MENHIR) $(COMPCERT).mly $(FLAGS) --dump --explain
	sleep 2
#	dot -Tpdf -o $(COMPCERT).pdf $(COMPCERT).dot
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

test:
	$(MAKE) -C .. -f Makefile.extr ccomp
	../ccomp test.c

clean:
	rm -f $(COMPCERT).conflicts
	rm -f $(COMPCERT).canonical
	rm -f $(COMPCERT).messages
	rm -f $(COMPCERT).follow
	rm -f $(COMPCERT).dot
	rm -f $(COMPCERT).pdf
	rm -f lr.csv

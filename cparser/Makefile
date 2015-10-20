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

%.messages.raw: %.messages
	@ $(COMMAND) --echo-errors $< > $@

# We compile deLexer.ml to native code, because running it as an ocaml script
# is way too slow.
deLexer: deLexer.ml
	@ ocamlopt -o $@ str.cmxa $<

generate: $(MSGS).raw deLexer
# Re-generate the C files in tests/generated.
	@ rm -f tests/generated/*.c
# Read $(MSGS).raw, line by line.
# For each sentence, create a new C file.
# A sentence takes the form "<start symbol>: SYMBOLS...".
# We cut the start symbol away,
# and translate the rest to concrete C syntax using the deLexer.
	@ f=0 ; \
	while read -r line ; do \
	  filename=`printf "tests/generated/%03d.c" $$f` ; \
	  echo "$$line" \
	    | cut --fields="1" --delimiter=" " --complement \
	    | ./deLexer \
	    > $$filename ; \
	  f=$$((f+1)) ; \
	done < $<
# Note: due to the lexer hack, I believe it could be the case that
# beginning with a token stream xs, de-lexing xs, followed with
# lexing & parsing, yields a token stream other than xs. As a result,
# it could happen that one of the concrete sentences that we produce
# does not lead to an error in the desired state. Hopefully, this
# won't happen in practice. If it did happen, maybe we could let the
# de-lexer produce unique identifiers every time it is asked for a
# VAR_NAME.

clean:
	rm -f $(COMPCERT).conflicts
	rm -f $(COMPCERT).messages
	rm -f $(COMPCERT).follow
	rm -f $(COMPCERT).dot
	rm -f $(COMPCERT).pdf
	rm -f lr.csv
	rm -f deLexer


.PHONY: all clean

SOURCES := $(wildcard *.c)
TARGETS := \
  $(patsubst %.c,%.ccomp.err,$(SOURCES)) \
  $(patsubst %.c,%.gcc.err,$(SOURCES)) \
  $(patsubst %.c,%.clang.err,$(SOURCES))

CCOMP   := ../../../ccomp
GCC     := gcc
CLANG   := clang

all: $(TARGETS)

clean:
	@ rm -f $(TARGETS) *~

%.ccomp.err: %.c
	@ echo $(CCOMP) -c $<
	@ if $(CCOMP) -c $< 2>$@ ; then \
	  echo "UNEXPECTED SUCCESS: $(CCOMP) -c $< SUCCEEDED!" >>$@ ; \
	fi

%.gcc.err: %.c
	@ echo $(GCC) -c $<
	@ if $(GCC) -c $< 2>$@ ; then \
	  echo "UNEXPECTED SUCCESS: $(GCC) -c $< SUCCEEDED!" >>$@ ; \
	fi

%.clang.err: %.c
	@ echo $(CLANG) -c $<
	@ if $(CLANG) -c $< 2>$@ ; then \
	  echo "UNEXPECTED SUCCESS: $(CLANG) -c $< SUCCEEDED!" >>$@ ; \
	fi


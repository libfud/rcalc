RUSTC = rustc 
LIBDIR = src/lib

MATRIX_SRC = $(wildcard src/lib/matrix/*.rs src/lib/matrix/*/*.rs src/lib/matrix/*/*/*.rs)
TYPES_SRC = $(wildcard src/lib/types/*.rs src/lib/types/*/*.rs src/lib/types/*/*/*.rs)
PARSE_SRC = $(wildcard src/lib/parse/*.rs src/lib/parse/*/*.rs src/lib/parse/*/*/*.rs)
CALC_SRC = src/main.rs $(wildcard src/calc/*.rs src/calc/*/*.rs src/calc/*/*/*.rs)

.PHONY: all clean

all: bin/rcalc

bin/rcalc: $(CALC_SRC) $(LIBDIR)/libtypes.dummy $(LIBDIR)/libparse.dummy
	$(RUSTC) -L $(LIBDIR) src/main.rs --out-dir bin/

$(LIBDIR)/libtypes.dummy: $(TYPES_SRC) $(LIBDIR)/libmatrix.dummy
	$(RUSTC) $(LIBDIR)/types/types.rs -L $(LIBDIR) -O --out-dir $(LIBDIR)
	touch $@

$(LIBDIR)/libmatrix.dummy: $(MATRIX_SRC)
	$(RUSTC) $(LIBDIR)/matrix/matrix.rs -O --cfg use_fancy --out-dir $(LIBDIR)
	touch $@

$(LIBDIR)/libparse.dummy: $(PARSE_SRC) $(LIBDIR)/libtypes.dummy
	$(RUSTC) $(LIBDIR)/parse/parse.rs -L $(LIBDIR) -O --out-dir $(LIBDIR)
	touch $@

clean:
	rm bin/rcalc $(LIBDIR)/*.rlib $(LIBDIR)/*.dummy

RUSTC = rustc 
LIBDIR = src/lib

.PHONY : rcalc clean libs types matrix parse

rcalc :
	$(RUSTC) -L $(LIBDIR) src/main.rs -o bin/

libs : 	matrix types

types: 
	$(RUSTC) $(LIBDIR)/types/types.rs -L $(LIBDIR) -O --out-dir src/lib

matrix: 
	$(RUSTC) $(LIBDIR)/matrix/matrix.rs -O --out-dir src/lib

parse:
	$(RUSTC) $(LIBDIR)/parse/parse.rs -L $(LIBDIR) -O --out-dir src/lib

clean :
	rm bin/rcalc

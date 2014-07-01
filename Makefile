RUSTC = rustc --out-dir bin/

.PHONY : rcalc clean

run : rcalc

rcalc :
	$(RUSTC) src/main.rs

clean :
	rm bin/rcalc

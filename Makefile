RUSTC = rustc --opt-level=3 --out-dir bin/

.PHONY : rcalc clean

run : rcalc
	bin/rcalc

rcalc :
	$(RUSTC) main.rs

clean :
	rm bin/rcalc

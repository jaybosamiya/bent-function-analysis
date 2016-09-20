all : test.native entropy.native

test.native : boolean.ml boolean.mli test.ml
	corebuild test.native

entropy.native : boolean.ml boolean.mli entropy.ml
	corebuild entropy.native

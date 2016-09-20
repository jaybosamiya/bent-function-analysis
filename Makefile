all : test.native

test.native : boolean.ml boolean.mli test.ml
	corebuild test.native

all : test.native entropy.native

test.native : boolean.ml boolean.mli test.ml
	corebuild -pkg zarith test.native

entropy.native : boolean.ml boolean.mli entropy.ml
	corebuild -pkg zarith entropy.native

clean :
	rm -rf _build/ *.native

BUILD=corebuild -pkg zarith

all : test.native entropy.native calc_entropy.native

%.native : %.ml boolean.ml boolean.mli
	$(BUILD) $@

clean :
	rm -rf _build/ *.native

all: src/*
	ocamlbuild -r -tag debug src/mc.byte

.PHONY: clean
clean:
	ocamlbuild -clean

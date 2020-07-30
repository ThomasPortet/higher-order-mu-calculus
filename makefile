all: src/*
	ocamlbuild -tag debug src/mc.byte

.PHONY: clean
clean:
	ocamlbuild -clean

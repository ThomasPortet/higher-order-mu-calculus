all: src/*
	ocamlbuild src/mc.byte

.PHONY: clean
clean:
	ocamlbuild -clean

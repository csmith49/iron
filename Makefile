# build flags
ocb_flags = -r -use-ocamlfind -pkgs "containers"
ocb = ocamlbuild $(ocb_flags)

# rules
.phony: all
all: cma

test:
	$(ocb) parsing_test.native
	./parsing_test.native

cma:
	$(ocb) iron.cma

.phony: clean
clean:
	$(ocb) -clean

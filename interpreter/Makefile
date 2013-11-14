.PHONY: dummy all clean yacc .force

YACC_DIR = yacc

OCAMLBUILD = ocamlbuild -use-ocamlfind -I thrift -I thrift/gen-ocaml

default: yacc flowlog.native
	@echo "Compiled FlowLog. If you would rather build everyting, use 'make all'"

all: $(patsubst %.ml,%.native,$(wildcard *.ml))

%.native: .force
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@

clean:
	$(OCAMLBUILD) -clean

yacc:
	$(MAKE) -C $(YACC_DIR)

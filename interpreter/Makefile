.PHONY: dummy all clean yacc .force

YACC_DIR = yacc

OCAMLBUILD = ocamlbuild -use-ocamlfind -I thrift -I thrift/gen-ocaml

default: yacc flowlog.native
	@echo "Compiled FlowLog. If you would rather build everyting, use 'make all'"

tests: yacc Tests.native
	@echo "Built Flowlog tests."

exp: yacc Temptest.native
	@echo "Built temporary test file"

all: $(patsubst %.ml,%.native,$(wildcard *.ml))

%.native: .force
	$(OCAMLBUILD) -cflag -g -cflag -ppopt -cflag -lwt-debug $@

clean:
	$(OCAMLBUILD) -clean

yacc:
	$(MAKE) -C $(YACC_DIR)

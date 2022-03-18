# Configuration variables
include Makefile.conf

# Build dbcert
all: extr

extr: .depend.extr
	$(MAKE) -C . cli

.depend.extr: proofextr
	$(OCAMLDEP) $(MLFILES) > .depend.extr
	$(OCAMLDEP) $(MLIFILES) >> .depend.extr

proofextr: Makefile.coq qcert
	$(MAKE) -f Makefile.coq -C .

# include .depend.extr

## CLI

PROGRAM=dbcert

MLIFILES=extraction/sql_query_to_js.mli
MLFILES=extraction/sql_query_to_js.ml extraction/sql_compiler.ml extraction/dbcert.ml

CMI=$(MLIFILES:%.mli=%.cmi)
CMX=$(MLFILES:%.ml=%.cmx)
CMO=$(MLFILES:%.ml=%.cmo)
QCMX=qcert_lib.cmxa
DCMX=utils.cmx basics.cmx sqlcontext.cmx coq_sql_algebra.cmx sql_ast.cmx sql_parser.cmx sql_lexer.cmx toCoq.cmx sqlparser.cmx

OTHERLIB=-linkpkg -package calendar,uri
QCERTLIB=-I $(QCERT)/_build/default/compiler -I $(QCERT)/_build/default/compiler/.qcert_lib.objs/byte -open Qcert_lib
DCERTLIB=-I ./extraction

FLAGS=-rectypes
COMPILEFLAGS=-cclib -lunix

OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep -I extraction
# OCAMLDEP=ocamlfind ocamldep -slash -ml-synonym .mlpack -I datacert/plugins -I datacert/plugins/common -I datacert/plugins/sql_parser -I extraction

cli: $(PROGRAM)

.SUFFIXES:.ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $(FLAGS) $(OTHERLIB) $(QCERTLIB) $(DCERTLIB) $<

.ml.cmx:
	$(OCAMLOPT) -c $(FLAGS) $(OTHERLIB) $(QCERTLIB) $(DCERTLIB) $<

.ml.cmo:
	$(OCAMLC) -c $(FLAGS) $(OTHERLIB) $(QCERTLIB) $(DCERTLIB) $<

$(PROGRAM): $(CMI) $(CMO) $(CMX)
	$(OCAMLOPT) $(FLAGS) $(OTHERLIB) $(QCERTLIB) $(DCERTLIB) -o $@ $(COMPILEFLAGS) $(QCMX) $(DCMX) $(CMX)

# Clean everything but Q*cert
cleanmost: Makefile.coq
	$(MAKE) -f Makefile.coq cleanall
	$(MAKE) -C extraction cleanall
	rm -f $(PROGRAM)
	rm -f Makefile.coq
	rm -f Makefile.coq.conf
	rm -f _CoqProject

# Clean everything
cleanall:
	$(MAKE) -C $(QCERT) cleanall
	$(MAKE) cleanmost

# Generate _CoqProject and Makefile.coq
Makefile.coq Makefile.coq.conf: _CoqProject $(FILES)
	coq_makefile -f _CoqProject $(FILES) -o Makefile.coq INSTALLDEFAULTROOT = DBCert
	(echo 'VERSION=1.0') >> Makefile.coq
	(echo 'NAME=datacert-$(VERSION)') >> Makefile.coq
	(echo 'export::  ') >> Makefile.coq
	(echo '	mkdir -p export/$(NAME)') >> Makefile.coq
	(echo '	for i in `find . -type d | grep -v export | grep -v \.svn `; do mkdir -p export/$(NAME)/$$i; done') >> Makefile.coq
	(echo '	cp Licence*.txt Makefile export/$(NAME)') >> Makefile.coq
	(echo '	for i in `find . -name \*.v | grep -v export | grep -v \.svn`; do cp $$i export/$(NAME)/$$i; done') >> Makefile.coq
	(echo '	cd export; tar zcf $(NAME).tar.gz $(NAME)') >> Makefile.coq
	(echo '.PHONY: doc doc/glob') >> Makefile.coq
	(echo 'doc/glob :') >> Makefile.coq
	(echo '	rm -f $(VOFILES)') >> Makefile.coq
	(echo '	$(MAKE) COQDOCFLAGS="-dump-glob doc/glob" all') >> Makefile.coq
	(echo '	rm -f doc/*.html') >> Makefile.coq
	(echo 'doc : doc/glob') >> Makefile.coq
	(echo '	 $(COQDOC) --glob-from doc/glob -d doc $(VFILES)') >> Makefile.coq

_CoqProject: Makefile.conf _CoqProject.base
	cp _CoqProject.base _CoqProject
ifneq ($(QCERT),)
	(echo " ";) >> _CoqProject
	(echo "-R $(QCERT)/compiler/core Qcert";) >> _CoqProject
endif

test:
	./dbcert -link tests/simple/org1.sql
	node ./dbcertRun.js tests/simple/org1.js tests/simple/db1.json

package:
	rm -rf ./$(DBNAME)
	mkdir -p ./$(DBNAME)
	cp -R $(QCERT) ./$(DBNAME)/qcert
	(echo 'JAVASCRIPT=yes') > ./$(DBNAME)/qcert/Makefile.config
	rm -rf ./$(DBNAME)/qcert/.git
	rm -rf ./$(DBNAME)/qcert/ocaml/_build
	rm -rf ./$(DBNAME)/qcert/ocaml/_build_js
	rm -rf ./$(DBNAME)/qcert/test/sql/tpc
	cp -R ./datacert ./$(DBNAME)/datacert
	cp -R ./extraction ./$(DBNAME)/extraction
	cp -R ./jsql ./$(DBNAME)/jsql
	cp -R ./tests ./$(DBNAME)/tests
	rm ./$(DBNAME)/tests/unit.sql
	rm ./$(DBNAME)/tests/simple/*ala.json
	rm ./$(DBNAME)/tests/simple/*ala.sql
	rm ./$(DBNAME)/jsql/poc/Test.v
	cp -R ./runtime ./$(DBNAME)/runtime
	cp ./dbcertRun.js ./$(DBNAME)
	cp Makefile Makefile.coq.local _CoqProject.base ./$(DBNAME)
	cp README.package ./$(DBNAME)/README.md
	cp dbcert.docker ./$(DBNAME)
	(echo '## Qcert') >> ./$(DBNAME)/Makefile.conf
	(echo 'QCERT=./qcert') >> ./$(DBNAME)/Makefile.conf
	(echo '') >> ./$(DBNAME)/Makefile.conf
	(echo '## DataCert') >> ./$(DBNAME)/Makefile.conf
	(echo 'VERSION=1.0') >> ./$(DBNAME)/Makefile.conf
	(echo 'NAME=datacert-$(VERSION)') >> ./$(DBNAME)/Makefile.conf
	(echo '') >> ./$(DBNAME)/Makefile.conf
	(echo '## DBCERT') >> ./$(DBNAME)/Makefile.conf
	(echo 'DBVERSION=1.0') >> ./$(DBNAME)/Makefile.conf
	(echo 'DBNAME=db-$(DBVERSION)') >> ./$(DBNAME)/Makefile.conf
	(echo '') >> ./$(DBNAME)/Makefile.conf
	(echo '.PHONY: qcert') >> ./$(DBNAME)/Makefile.conf
	$(MAKE) -C ./$(DBNAME) cleanall
ifneq ($(TEST),)
	$(MAKE) -C ./$(DBNAME) all
	$(MAKE) -C ./$(DBNAME) test
	$(MAKE) -C ./$(DBNAME) cleanall
endif
	tar cvfz ./$(DBNAME).tar.gz ./$(DBNAME)


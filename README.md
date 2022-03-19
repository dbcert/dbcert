# DBCert

A runtime and a runner for [the Coq certified compiler from SQL to JavaScript](https://framagit.org/formaldata/sqltonracert).


## Requirements

This work is known to compile with Node.js 10, OCaml-4.09.1, Coq-8.11.*, and JsAst-2.0.0.

### Install Node.js

https://nodejs.org/en/download/releases/

### Install Opam

```
sudo curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh > install.sh
echo | sudo sh install.sh
eval $(opam env)
opam switch create 4.09.1 || true
opam switch set 4.09.1
opam update || true
```

### Prepare Coq dependencies
```
opam repo add coq-released https://coq.inria.fr/opam/released
```

<!-- ### Install OCaml dependencies -->

<!-- ``` -->
<!-- opam install -y --jobs=2 dune menhir base64 js_of_ocaml js_of_ocaml-ppx re calendar uri -->
<!-- ``` -->

<!-- ### Install Coq dependencies -->

<!-- ``` -->
<!-- opam repo add coq-released https://coq.inria.fr/opam/released -->
<!-- opam install -y coq.8.11.2 coq-jsast.2.0.0 -->
<!-- ``` -->

<!-- ### Install Q*cert -->

<!-- You need [Q*Cert](https://github.com/querycert/qcert), on the branch `master`. -->

<!-- You have to set the path to Q*cert in the `Makefile.conf` file. -->

### Compilation

Have OCaml and Coq executables in your path:

```
opam install -y .
```

<!-- ## Tests -->

<!-- After compilation try the compiler with: -->

<!-- ``` -->
<!-- make test -->
<!-- ``` -->

## Examples

To compile SQL to JavaScript:
```
dbcert src/tests/simple/org1.sql
```
To compile SQL to JavaScript and link the runtime:
```
dbcert -link src/tests/simple/org1.sql
```
To run the compiled JavaScript:
```
node src/dbcertRun.js src/tests/simple/org1.js src/tests/simple/db1.json
```

<!-- ## Packaging -->

<!-- To create a `.tgz` with all the source code for distribution: -->
<!-- ``` -->
<!-- make package -->
<!-- ``` -->

<!-- To test a full compilation before packaging: -->
<!-- ``` -->
<!-- TEST=true make package -->
<!-- ``` -->

## License

This code is released under the Apache 2.0 license (see LICENSE for details).

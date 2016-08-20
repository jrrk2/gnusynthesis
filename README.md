* An OCaml framework for logic synthesis and fault-tolerance

Written by Jonathan Richard Robert Kimmitt <jonathan AT kimmitt.co.uk> (formerly jonathan.kimmitt AT anglia.ac.uk)
in part-credit for the degree of doctor of philosophy at
Anglia Ruskin University.

Supervisors:

+ George Wilson <George.Wilson AT anglia.ac.uk>
+ David Greaves <david.greaves.10 AT gmail.com>
+ Marcian Cirstea <marcian.cirstea AT anglia.ac.uk>

Examiners:

+ Anil Madhavapeddy <avsm2 AT cl.cam.ac.uk>
+ John O'Donnell <John.ODonnell AT glasgow.ac.uk>

Last built on Ubuntu 15.10 (wily) using ocaml 4.01.0 and the opam package manager.

sudo apt-get install ocaml ocaml-native-compilers autoconf opam camlidl libreadline-dev
opam init
eval `opam config env`
opam switch 4.01.0
eval `opam config env`
opam install xmlm camlp5 camlzip camlidl
git clone git@bitbucket.org:jrrk/gnusynthesis.git
cd gnusynthesis
make

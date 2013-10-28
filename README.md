FlowLog
=======

To use the interpreter, you need:

* XSB, a Prolog engine. This can be downloaded for free at:
    http://xsb.sourceforge.net/downloads/downloads.html

* The latest version of OCaml.

* The Frenetic package along with its dependencies. These can be found at:
    http://www.github.com/frenetic-lang
  (and via opam)

* To run analysis via Alloy, download either Alloy Analyzer:
    http://alloy.mit.edu/alloy/download.html
  or the Aluminum "Minimal Alloy" analyzer:
  	http://cs.brown.edu/research/plt/dl/aluminum/

* The "yojson" package (opam install yojson)
  (We use this to output dependency graphs in JSON format.)

* The "thrift" package.

-----------------------------
Legacy notes: 

For the pvs verification, you need PVS, which can be downloaded at:
http://pvs.csl.sri.com/download.shtml

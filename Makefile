default: all

-include Makefile.ocaml

update_generated_doc::
	cd src && (ocamldoc_pyexpander monad_intf.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)

clean::

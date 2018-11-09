open Types

(** Monadic reference operations (deprecated) *)
(* FIXME should be mref_ops? *)
type ('a,'s) mref = {
  get: unit -> ('a,'s) m;
  set: 'a -> (unit,'s) m;
}

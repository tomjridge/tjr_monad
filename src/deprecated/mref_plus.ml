open Monad_ops

(** Monadic reference operations (with_ref version) *)
(* NOTE with_ref is not 'a -> 'a m; so we cannot perform any monadic
   actions when we read the ref; this is correct: with_ref is supposed
   to be an atomic update, so should not eg go to disk *)
type ('a,'s) mref = {
  get: unit -> ('a,'s) m;
  set: 'a -> (unit,'s) m;
  with_ref: 'b. ('a -> ('b*'a)) -> ('b,'s) m;  (* execute some functional update on state *)
}

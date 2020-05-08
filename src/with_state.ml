(** A type to allow parameterization over a generic notion of "part of
   state" *)
open Monad_ops

(** A type to allow parameterization over a generic notion of "part of
   state". Presumably the part of the state is locked while the
   monadic operation completes, and then the lock is released. *)
type ('s,'t) with_state = {
  with_state: 
    'a. 
      (* a function which has access to the state *)
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       (* and returns a value of type 'a in the monad *)
       ('a,'t) m)
    -> ('a,'t)m
}

(** NOTE see also with_imperative_ref in Tjr_monad *)

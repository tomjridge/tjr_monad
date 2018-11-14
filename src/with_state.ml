(** A type to allow parameterization over a generic notion of "part of
   state" *)
open Monad_ops

(** A type to allow parameterization over a generic notion of "part of
   state". Presumably the part of the state is locked while the
   monadic operation completes, and then the lock is released. *)
type ('s,'t) with_state = {
  with_state: 
    'a. 
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       ('a,'t) m) 
    -> ('a,'t)m
}

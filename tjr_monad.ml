
(* Use a phantom type variable 't to specialize monad ops to a
   particular monad *)

module Monad : sig
  type ('a,'t) m

  type 't monad_ops = {
    return: 'a. 'a -> ('a,'t) m;
    bind: 'a 'b. ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m;
  }
end = struct
  type ('a,'t) m

  type 't monad_ops = {
    return: 'a. 'a -> ('a,'t) m;
    bind: 'a 'b. ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m;
  }
end


(* FIXME we may want following in a separate package to avoid
   dependence on lwt *)
open Monad

module Lwt_monad_instance = struct
  type lwt
  let lwt_ops : lwt monad_ops = {
    return=Obj.magic Lwt.return;
    bind=Obj.magic Lwt.bind
  }
  let to_lwt : ('a,lwt) m -> 'a Lwt.t = fun x -> Obj.magic x
end

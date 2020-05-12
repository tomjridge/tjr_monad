(** A generic state-passing monad.

The type ('a, 't state_passing) m is the type of the monad *)

open Monad_intf

type 'a state_passing

module Internal = struct
  type ('a,'t) mm = 't -> 'a * 't
  module type ISO = sig
    val to_m: ('a,'t)mm -> ('a,'t state_passing)m
    val from_m: ('a,'t state_passing)m -> ('a,'t)mm
  end
  module Make(Iso:ISO) = struct 
    open Iso
    let return : 'a -> ('a,'t) m = fun a -> 
      to_m (fun t -> (a,t))

    let bind : ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m =
      fun a ab -> to_m (fun t ->
          (from_m a) t |> fun (a,t') ->
          (from_m (ab a)) t' |> fun (b,t'') ->
          (b,t''))

    let of_fun : ('t -> 'a * 't) -> ('a,'t state_passing)m = fun x -> to_m x
    let to_fun : ('a,'t state_passing)m -> ('t -> 'a * 't) = fun x -> from_m x

  end
  module Iso:ISO = struct
    let to_m x = Obj.magic x
    let from_m x = Obj.magic x
  end
  module Made = Make(Iso)
end
open Internal.Made


let monad_ops () : 't state_passing monad_ops = {
  return=Obj.magic return;
  bind=Obj.magic bind
}

let of_fun = of_fun
let to_fun = to_fun

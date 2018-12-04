open Monad_ops

type imperative


module Internal = struct
  type ('a,'t) mm = 'a
  let return : 'a -> ('a,'t) mm = fun a -> a
  let bind : ('a,'t) mm -> ('a -> ('b,'t) mm) -> ('b,'t) mm =
    fun a ab -> ab a
  let from_m = fun (a:('a,'t) mm) -> (a:'a)
  let _ = from_m
  let to_m : 'a -> ('a,'t) mm = fun x -> x 
end
open Internal

let monad_ops : imperative monad_ops = {
  return=Obj.magic return;
  bind=Obj.magic bind
}

let from_m (x:('a,imperative) m) : 'a = Obj.magic (from_m x)
let to_m (x:'a) : ('a,imperative) m = Obj.magic (to_m x)


(* FIXME or state passing with unit, to delay evaluation? then
   implement a run method *)

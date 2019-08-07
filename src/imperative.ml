(** A trivial monad, where bind is just function composition, and
   return is the identity. Used when performing imperative operations
   eg Unix.read/write. AKA the identity monad *)

open Monad_ops

type imperative

module Internal = struct
  type 'a mm = 'a
  let return (x:'a) : 'a mm = x
  let bind (x:'a mm) (f:'a -> 'b mm) : 'b mm = x |> f
  let to_a (x:('a,imperative)m) : 'a mm = Obj.magic x
  let of_a (x:'a mm) : ('a,imperative)m = Obj.magic x
end

let imperative_monad_ops,of_m,to_m = 
  let open Internal in
  let return (type a) (x:a) : (a,imperative) m = of_a (return x) in
  let bind (type a b)
      (x:(a,imperative) m) 
      (f:(a -> (b,imperative) m)) 
    : (b,imperative) m = 
    x |> to_a |> fun x -> bind x (fun a -> to_a (f a)) |> of_a
  in
  let of_m (x:('a,imperative)m) : 'a = x |> to_a in
  let to_m (x:'a) : ('a,imperative) m = x |> of_a in
  { return; bind },of_m,to_m

let _ = imperative_monad_ops
let _ = to_m
let _ = of_m

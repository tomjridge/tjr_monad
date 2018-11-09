open Types

type 'a state_passing

(* the type ('a, 't state_passing) m is the type of the monad *)

module Ignore = struct
  (* this just to make sure the types are correct *)
  type ('a,'t) mm = 't -> 'a * 't
  let return : 'a -> ('a,'t) mm = fun a -> fun t -> (a,t)
  let bind : ('a,'t) mm -> ('a -> ('b,'t) mm) -> ('b,'t) mm =
    fun a ab t -> 
      a t |> fun (a,t') ->
      ab a t' |> fun (b,t'') ->
      (b,t'')
  (* let get_world () : t mm = fun t -> (t,t) *)
  let with_world (f:'t -> 'a * 't) : ('a,'t) mm = 
    f
  let run ~(init_state:'t) (a:('a,'t) mm) : 'a * 't = 
    a init_state |> fun (a,final_state) -> (a,final_state) 

end
open Ignore

let monad_ops () : 't state_passing monad_ops = {
  return=Obj.magic return;
  bind=Obj.magic bind
}

let with_world (f:'t->'a * 't) : ('a, 't state_passing) m = Obj.magic (with_world f)

let run ~(init_state:'t) (a:('a,'t state_passing) m) : 'a * 't =
  Obj.magic (run ~init_state (Obj.magic a))


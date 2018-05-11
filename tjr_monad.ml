
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


type 'a state_passing

module State_passing_instance = struct
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
    
        
end


type imperative

module Imperative_instance = struct

  module Ignore = struct
    type ('a,'t) mm = 'a
    let return : 'a -> ('a,'t) mm = fun a -> a
    let bind : ('a,'t) mm -> ('a -> ('b,'t) mm) -> ('b,'t) mm =
      fun a ab -> ab a
    let from_m = fun (a:('a,'t) mm) -> (a:'a)
    let _ = from_m
    let to_m : 'a -> ('a,'t) mm = fun x -> x 
  end
  open Ignore

  let monad_ops : imperative monad_ops = {
    return=Obj.magic return;
    bind=Obj.magic bind
  }

  let from_m (type a) (x:('a,imperative) m) : 'a = Obj.magic (from_m x)
  let to_m (type a) (x:'a) : ('a,imperative) m = Obj.magic (to_m x)

end

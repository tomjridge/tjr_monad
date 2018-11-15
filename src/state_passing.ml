open Monad_ops

module State_passing_type = struct
type 'a state_passing
end
include State_passing_type

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

  let with_state
      ~(get:'t -> 's)
      ~(set:'s -> 't -> 't) 
      ~(f:(state:'s -> set_state:('s -> (unit,'t)mm) -> ('a,'t)mm))
    : 
      ('a,'t)mm
    =
    let ( >>= ) = bind in
    let set_state s = fun t -> (),set s t in
    (fun t -> get t,t) >>= fun s ->
    f ~state:s ~set_state

  let _ = with_state

end
open Ignore

let monad_ops () : 't state_passing monad_ops = {
  return=Obj.magic return;
  bind=Obj.magic bind
}

(* get a truly generic version of monad ops? *)
let monad_ops' : 't state_passing monad_ops = (
  let bind = fun a b -> (Obj.magic (bind (Obj.magic a) (Obj.magic b))) in
  {
    return=(fun x -> (Obj.magic (return x)));
    bind
  } : 't state_passing monad_ops)

(* NOTE the type should match the Ignore type, module renaming of 't
   to state_passing 't and mm to m *)

(* FIXME may want to add to_state_passing : 'a mm -> 'a state_passing
   m; from_state_passing: 'a state_passing mm -> 'a m ; ; then have to
   run to/from through the type expression eg for functional args to
   functions *)
let with_state 
    ~(get:'t -> 's) 
    ~(set:'s -> 't -> 't) 
    ~(f: (state:'s -> 
         set_state:('s -> (unit,'t state_passing)m) -> 
      ('a,'t state_passing)m))
  :
  ('a,'t state_passing)m
  =
  Obj.magic (with_state ~get:(Obj.magic get) ~set:(Obj.magic set) ~f:(Obj.magic f))


let with_world (f:'t->'a * 't) : ('a, 't state_passing) m = Obj.magic (with_world f)

let run ~(init_state:'t) (a:('a,'t state_passing) m) : 'a * 't =
  Obj.magic (run ~init_state (Obj.magic a))


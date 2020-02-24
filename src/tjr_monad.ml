(** Generic monads *)


(** {2 Monad ops type} *)

include Monad_ops


(** {2 State passing} *)

type 'a state_passing = 'a State_passing.state_passing

(** Conversions to and from function types *)

let sp_to_fun = State_passing.to_fun

let sp_of_fun = State_passing.of_fun

module State_passing = State_passing

let state_passing_monad_ops () = State_passing.monad_ops ()


(** {2 Imperative} *)
module Imperative = Imperative
type imperative = Imperative.imperative
let imperative_monad_ops = Imperative.imperative_monad_ops

(** {2 With-state} *)

type ('s,'t) with_state = ('s,'t) With_state.with_state = {
  with_state: 
    'a. 
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       ('a,'t) m)
    -> ('a,'t)m
}
(** {%html:<pre>
type ('s,'t) with_state = ('s,'t) With_state.with_state = {
  with_state: 
    'a. 
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       ('a,'t) m)
    -> ('a,'t)m
}
</pre> %} *)


(** {2 Iteration} *)

let iter_m ~monad_ops f x = 
  let { bind; return } = monad_ops in
  let ( >>= ) = bind in
  let rec loop f x = 
    f x >>= function
    | None -> return x
    | Some x' -> loop f x'
  in
  loop f x

let _ : 
monad_ops:'a monad_ops -> ('b -> ('b option, 'a) m) -> 'b -> ('b, 'a) m
= iter_m


(** A generic version of join, which waits for each m to resolve *)
let join_seq ~monad_ops =
  let { bind; return } = monad_ops in
  let ( >>= ) = bind in
  let rec loop = function
    | [] -> return ()
    | [x] -> x
    | x::xs -> x >>= fun () -> loop xs  (* FIXME are we sure >>= is tail recursive, ie can loop indefinitely? does it matter here? elsewhere? *)
  in
  fun (xs:(unit,'t)m list) ->
    loop xs

(** {2 Events} *)

module Event = Event


(** {2 With_lwt} *)

module With_lwt = struct
  open Event
  open Lwt


  type lwt

  (* FIXME rename to monad_ops? or lwt_monad_ops? *)
  let lwt_monad_ops : lwt monad_ops = {
    return=Obj.magic Lwt.return;
    bind=Obj.magic Lwt.bind
  }

  let return = lwt_monad_ops.return
  let ( >>= ) = lwt_monad_ops.bind

  let to_lwt : ('a,lwt) m -> 'a Lwt.t = fun x -> Obj.magic x
  let from_lwt: 'a t -> ('a,lwt) m = Obj.magic

  module Internal = struct
    let to_ev : 'a t * 'a u -> 'a event = Obj.magic
    let from_ev: 'a event -> 'a t * 'a u = Obj.magic 

    let ev_create () : ('a event,lwt) m = Lwt.task () |> to_ev |> return
    let ev_wait ev : ('a,lwt) m = 
      ev |> from_ev |> fun (t,_u) -> 
      from_lwt t
    let ev_signal ev a : (unit,lwt) m =
      ev |> from_ev |> fun (_t,u) -> 
      Lwt.wakeup u a |> return
  end

  open Internal

  let lwt_event_ops : lwt event_ops = { 
    ev_create;
    ev_wait;
    ev_signal
  }

  let with_ref r = 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
    { with_state }
end

type lwt = With_lwt.lwt
let lwt_monad_ops = With_lwt.lwt_monad_ops


(** {2 Util} *)

let with_imperative_ref ~monad_ops = 
  let return = monad_ops.return in
  fun r -> 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun r' -> r:=r'; return ())
    in
    { with_state }

(** Generic monads *)

(** {2 Summary of main types} *)

include Summary


(** {2 Monad types} *)

include Monad_intf


(** {2 State passing monad} *)

type 'a state_passing = 'a State_passing.state_passing


(** Conversions to and from function types *)

let sp_to_fun = State_passing.to_fun

let sp_of_fun = State_passing.of_fun

module State_passing = State_passing

let state_passing_monad_ops () = State_passing.monad_ops ()


(** {2 Imperative monad} *)

module Imperative = Imperative

type imperative = Imperative.imperative

let imperative_monad_ops = Imperative.imperative_monad_ops


(** {2 Lwt monad} *)

(* FIXME maybe rename to just Lwt_ *)
module With_lwt = With_lwt

type lwt = With_lwt.lwt

let lwt_monad_ops = With_lwt.lwt_monad_ops
let lwt_event_ops = With_lwt.lwt_event_ops
let lwt_mutex_ops = With_lwt.lwt_mutex_ops
let lwt_file_ops = With_lwt.lwt_file_ops


(** {2 Util} *)

(** WARNING this is not locked *)
let with_imperative_ref ~monad_ops = 
  let return = monad_ops.return in
  fun r -> 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun r' -> r:=r'; return ())
    in
    { with_state }


(** NOTE unfortunately this returns in the monad *)
let with_locked_ref ~monad_ops ~mutex_ops r = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  mutex_ops.create_mutex () >>= fun lck -> 
  let with_state f = 
    mutex_ops.lock lck >>= fun () ->
    f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) >>= fun x ->
    mutex_ops.unlock lck >>= fun () ->
    return x
  in
  return { with_state }



(*
(** {2 With-state} *)

(** Expose this type since it is so common *)
type ('s,'t) with_state = ('s,'t) Monad_intf.with_state = {
  with_state: 
    'a. 
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       ('a,'t) m)
    -> ('a,'t)m
}
(** {[
type ('s,'t) with_state = ('s,'t) Monad_intf.with_state = {
  with_state: 
    'a. 
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       ('a,'t) m)
    -> ('a,'t)m
}
]} *)
*)


(*
(** {2 Iteration} *)

(** NOTE prefer iter_k to iter_m *)

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


(** A generic version of join, which waits for each m to resolve FIXME deprecated *)
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
*)


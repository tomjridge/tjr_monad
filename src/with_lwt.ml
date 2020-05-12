open Monad_intf
open Lwt

type lwt

(* FIXME rename to monad_ops? or lwt_monad_ops? *)
let lwt_monad_ops : lwt monad_ops = {
  return=Obj.magic Lwt.return;
  bind=Obj.magic Lwt.bind
}

let return = lwt_monad_ops.return
let ( >>= ) = lwt_monad_ops.bind

let monad_ops = lwt_monad_ops

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

let event_ops = lwt_event_ops


let with_ref r = 
  let with_state f = 
    f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
  { with_state }

(** FIXME really, we should always use a lock *)
let with_locked_ref r = 
  let lck = Lwt_mutex.create () in
  let with_state f = 
    from_lwt (Lwt_mutex.lock lck) >>= fun () ->
    f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) >>= fun x ->
    Lwt_mutex.unlock lck;
    return x
  in
  { with_state }


(** async for lwt *)
let async : lwt async = 
  fun (f:unit -> (unit,lwt) m) : (unit,lwt) m ->
    Lwt.async (fun () -> f () |> to_lwt); return ()


(** {2 Lwt mutex basic funs} *)


(* FIXME move elsewhere - tjr_monad? *)

(* private; basic support from lwt. NOTE lock and wait are in Lwt.t *)
module Internal_2 : sig
  val create_mutex : unit -> Lwt_mutex.t
  val create_cvar : unit -> 'a Lwt_condition.t
  val lock : Lwt_mutex.t -> unit Lwt.t
  val unlock : Lwt_mutex.t -> unit
  val signal : unit Lwt_condition.t -> unit
  val wait : mut:Lwt_mutex.t -> cvar:'a Lwt_condition.t -> 'a Lwt.t
end = struct 
  let create_mutex () = Lwt_mutex.create() 
  let create_cvar () = Lwt_condition.create() 
  let lock mut = Lwt_mutex.lock mut
  let unlock mut = Lwt_mutex.unlock mut
  let signal cvar = Lwt_condition.broadcast cvar ()
  let wait ~mut ~cvar = Lwt_condition.wait ~mutex:mut cvar
end
include Internal_2


(** {2 Lwt mutex ops, msg queue_ops} *)

module Lwt_mutex_ops = struct
  (* FIXME move mutex and cvar ops from mem_queue to tjr_monad; give
     lwt impl there *)

  let lwt_mutex_ops : ('m,'c,'t) mutex_ops = {
    create_mutex=(fun () ->
        Lwt_mutex.create() |> fun mut ->
        return mut);
    create_cvar=(fun () ->
        Lwt_condition.create() |> fun cvar ->
        return cvar);
    lock=(fun mut ->
        Lwt_mutex.lock mut |> from_lwt);
    unlock=(fun mut ->
        Lwt_mutex.unlock mut |> fun _ -> return ());
    signal=(fun cvar ->
        Lwt_condition.broadcast cvar () |> fun _ -> return ());
    wait=(fun mut cvar ->
        Lwt_condition.wait ~mutex:mut cvar |> from_lwt)
  }

  let mutex_ops = lwt_mutex_ops

end
include Lwt_mutex_ops


let yield () = Lwt.pause ()

let sleep f = Lwt_unix.sleep f

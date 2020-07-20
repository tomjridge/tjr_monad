open Monad_intf
open Lwt

(** {2 Lwt basics} *)

type lwt

(* FIXME rename to monad_ops? or lwt_monad_ops? *)
let lwt_monad_ops : lwt monad_ops = {
  return=Obj.magic Lwt.return;
  bind=Obj.magic Lwt.bind
}

let return = lwt_monad_ops.return
let ( >>= ) = lwt_monad_ops.bind

let monad_ops = lwt_monad_ops

(** Conversions between lwt and our parameterized monad *)
let to_lwt : ('a,lwt) m -> 'a Lwt.t = fun x -> Obj.magic x
let from_lwt: 'a t -> ('a,lwt) m = Obj.magic


(** {2 Lwt events} *)

module Lwt_event_ops = struct
  let to_ev : 'a t * 'a u -> 'a event = Obj.magic
  let from_ev: 'a event -> 'a t * 'a u = Obj.magic 

  let ev_create () : ('a event,lwt) m = Lwt.task () |> to_ev |> return
  let ev_wait ev : ('a,lwt) m = 
    ev |> from_ev |> fun (t,_u) -> 
    from_lwt t
  let ev_signal ev a : (unit,lwt) m =
    ev |> from_ev |> fun (_t,u) -> 
    Lwt.wakeup u a |> return

  let lwt_event_ops : lwt event_ops = { 
    ev_create;
    ev_wait;
    ev_signal
  }
end

let lwt_event_ops = Lwt_event_ops.lwt_event_ops
let event_ops = Lwt_event_ops.lwt_event_ops


(** {2 Lwt mutex ops} *)


(* private; basic support from lwt. NOTE lock and wait are in Lwt.t *)
module Lwt_mutex_ops : sig
  val create_mutex : unit -> Lwt_mutex.t
  val create_cvar  : unit -> 'a Lwt_condition.t
  val lock         : Lwt_mutex.t -> unit Lwt.t
  val unlock       : Lwt_mutex.t -> unit
  val signal       : unit Lwt_condition.t -> unit
  val wait         : mut:Lwt_mutex.t -> cvar:'a Lwt_condition.t -> 'a Lwt.t
  val mutex_ops    : (Lwt_mutex.t, unit Lwt_condition.t, lwt) mutex_ops
end = struct 
  let create_mutex () = Lwt_mutex.create() 
  let create_cvar () = Lwt_condition.create() 
  let lock mut = Lwt_mutex.lock mut
  let unlock mut = Lwt_mutex.unlock mut
  let signal cvar = Lwt_condition.broadcast cvar ()
  let wait ~mut ~cvar = Lwt_condition.wait ~mutex:mut cvar

  let mutex_ops : ('m,'c,'t) mutex_ops = {
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

  let _ = mutex_ops

end

let lwt_mutex_ops = Lwt_mutex_ops.mutex_ops
let mutex_ops = Lwt_mutex_ops.mutex_ops



(** {2 Opening files (convenience)} *)

module Lwt_file_ops = struct

  (** Default file perms: 0o640 *)
  let default_perms = 0o640

  open Lwt 
  open Lwt_unix

  (** Open a file RDWR; create adds an O_CREAT; trunc truncates the
     file to 0 length *)
  let open_file ~fn ~create ~trunc = 
    let x = 
      let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
      openfile fn flgs default_perms >>= fun fd -> 
      (if trunc then ftruncate fd 0 else return ()) >>= fun () ->
      return fd
    in
    x |> from_lwt

  let sync fd = Lwt_unix.fsync fd |> from_lwt

  let close fd = Lwt_unix.close fd |> from_lwt
    
  let lwt_file_ops = object
    method open_file=open_file
    method sync=sync
    method close=close
  end
end

let lwt_file_ops = Lwt_file_ops.lwt_file_ops
let file_ops = lwt_file_ops



(** {2 Other useful defns} *)

let with_ref r = 
  let with_state f = 
    f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
  { with_state }

(** FIXME really, we should always use a lock, unless we know that
   only a single thread accesses the state *)
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

let yield () = Lwt.pause ()

let sleep f = Lwt_unix.sleep f

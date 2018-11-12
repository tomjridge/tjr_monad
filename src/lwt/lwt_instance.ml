open Types
open Lwt


type lwt

let lwt_ops : lwt monad_ops = {
  return=Obj.magic Lwt.return;
  bind=Obj.magic Lwt.bind
}
let to_lwt : ('a,lwt) m -> 'a Lwt.t = fun x -> Obj.magic x
let from_lwt: 'a t -> ('a,lwt) m = Obj.magic

module Internal = struct
  let to_ev : 'a t * 'a u -> 'a event = Obj.magic
  let from_ev: 'a event -> 'a t * 'a u = Obj.magic 

  let ev_create () : ('a event,lwt) m = Lwt.task () |> to_ev |> lwt_ops.return
  let ev_wait ev : ('a,lwt) m = 
    ev |> from_ev |> fun (t,_u) -> 
    from_lwt t
  let ev_signal ev a : (unit,lwt) m =
    ev |> from_ev |> fun (_t,u) -> 
    Lwt.wakeup u a |> lwt_ops.return
end

open Internal

let lwt_event_ops : lwt event_ops = { 
  ev_create;
  ev_wait;
  ev_signal
}

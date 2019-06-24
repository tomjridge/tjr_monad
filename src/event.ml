(** Event with signal (similar to promise) *)
open Monad_ops

(** The type of events that resolve to ['a] *)
type 'a event

(** Multiple threads can be waiting on an event. Only one thread
   should signal the event. It is an error to signal the same event
   more than once. If this happens, the new signalled value is
   discarded and no further actions are taken. FIXME perhaps we should
   throw an exception or something. An event is a bit like a channel
   that multiple readers can register on (and all will receive any
   msgs), and which only carries at most one msg.


    NOTE lwt has two types, ['a t] for promises and ['a u] for
   resolvers. These are implemented using the same underlying
   structure.

*)
type 't event_ops = {
  ev_create: 'a. unit -> ('a event,'t) m;
  ev_wait: 'a. 'a event -> ('a,'t) m;
  ev_signal: 'a. 'a event -> 'a -> (unit,'t)m;
}


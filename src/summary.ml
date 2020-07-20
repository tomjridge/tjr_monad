(**
{[
type 't event_ops = {
  ev_create: 'a. unit -> ('a event,'t) m;
  ev_wait: 'a. 'a event -> ('a,'t) m;
  ev_signal: 'a. 'a event -> 'a -> (unit,'t)m;
}

type 't monad_ops = {
  return : 'a. 'a -> ('a,'t) m;
  bind   : 'a 'b. ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m;
}

type ('mutex,'cvar,'t) mutex_ops = {
  create_mutex : unit -> ('mutex,'t)m;
  create_cvar  : unit -> ('cvar,'t)m;
  lock         : 'mutex -> (unit,'t)m;
  signal       : 'cvar -> (unit,'t)m;  (* FIXME broadcast? *)
  unlock       : 'mutex -> (unit,'t)m;
  wait         : 'mutex -> 'cvar -> (unit,'t)m;
}

type ('s,'t) with_state = {
  with_state: 
    'a. 
      (* a function which has access to the state *)
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       (* and returns a value of type 'a in the monad *)
       ('a,'t) m)
    -> ('a,'t)m
}

]}
*)

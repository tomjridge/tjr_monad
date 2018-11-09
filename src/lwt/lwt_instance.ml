open Types

type lwt
let lwt_ops : lwt monad_ops = {
  return=Obj.magic Lwt.return;
  bind=Obj.magic Lwt.bind
}
let to_lwt : ('a,lwt) m -> 'a Lwt.t = fun x -> Obj.magic x

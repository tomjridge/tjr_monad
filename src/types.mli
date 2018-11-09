(** Use a phantom type variable 't to specialize monad ops to a
   particular monad *)

type ('a,'t) m

type 't monad_ops = {
  return: 'a. 'a -> ('a,'t) m;
  bind: 'a 'b. ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m;
}

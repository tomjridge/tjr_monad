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


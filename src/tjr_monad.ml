include Monad_ops

type 'a state_passing = 'a State_passing.state_passing

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


module State_passing = State_passing


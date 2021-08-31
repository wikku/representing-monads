type name = string

type expr =
  | Lit of int
  | Var of name
  | Lam of name * expr
  | App of expr * expr
  | Callcc of expr
  | Shift of expr
  | Reset of expr

type 'a env
val empty : 'a env
val get : name -> 'a env -> 'a
val extend : name -> 'a -> 'a env -> 'a env

type ('r, 'a) cps = ('a -> 'r) -> 'r

module type Sem = sig
  type value

  val value_to_string : value -> string

  val run : expr -> value
end

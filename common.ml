type name = string

type expr =
  | Lit of int
  | Var of name
  | Lam of name * expr
  | App of expr * expr
  | Callcc of expr
  | Shift of expr
  | Reset of expr

type 'a env = (name * 'a) list
let extend name v env = (name, v) :: env
let get = List.assoc
let empty = []

type ('r, 'a) cps = ('a -> 'r) -> 'r

module type Sem = sig
  type value

  val value_to_string : value -> string

  val run : expr -> value
end

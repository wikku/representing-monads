open Common

type mcont = (* cont list *)
  | Id
  | Pass_to of cont * mcont
and cont =
  | Cps_id
  | Appfun of expr * value env * cont
  | Appval of value * cont
  | Callcc of cont
  | Shift of cont
and value =
  | Int of int
  | Lam of name * expr * value env
  | Cc of cont
  | Delimc of cont
  | Add1 of int
  | Mult1 of int
  | Add2
  | Mult2

let get_int = function
  | Int i -> i
  | _ -> failwith "type error: get_int"

let value_to_string = function
  | Int i -> string_of_int i
  | _ -> "<fun>"


let rec callv f v c mc = match f with
  | Int _ -> failwith "type error: ($)"
  | Lam(x, e, env) -> sem e (extend x v env) c mc
  | Cc(c) -> callc c v mc
  | Delimc(k) -> callc k v (Pass_to(c, mc))
  | Add1(i) -> callc c (Int(i + get_int v)) mc
  | Mult1(i) -> callc c (Int(i * get_int v)) mc
  | Add2 -> callc c (Add1(get_int v)) mc
  | Mult2 -> callc c (Mult1(get_int v)) mc
and callc c v mc = match c with
  | Cps_id -> callmc mc v
  | Appfun(e, env, c) -> sem e env (Appval(v, c)) mc
  | Appval(f, c) -> callv f v c mc
  | Callcc(c) -> callv v (Cc(c)) c mc
  | Shift(c) -> callv v (Delimc(c)) Cps_id mc
and callmc mc v = match mc with
  | Id -> v
  | Pass_to(k, mc) -> callc k v mc
and sem expr env c mc = match expr with
  | Lit(i) -> callc c (Int(i)) mc
  | Var(x) -> callc c (get x env) mc
  | Lam(x, e) -> callc c (Lam(x, e, env)) mc
  | App(e1, e2) -> sem e1 env (Appfun(e2, env, c)) mc
  | Callcc(e) -> sem e env (Callcc(c)) mc
  | Shift(e) -> sem e env (Shift(c)) mc
  | Reset(e) -> sem e env Cps_id (Pass_to(c, mc))


let init_env = empty
  |> extend "succ" (Add1(1))
  |> extend "zero" (Int 0)
  |> extend "+" Add2
  |> extend "*" Mult2

let run expr = sem expr init_env Cps_id Id

open Common

type value =
  | Int of int
  | Id
  | Cps_id
  | Pure of value
  | Succ
  | Add1 of int
  | Mult1 of int
  | Add2
  | Mult2
  | Lam_clos of expr * name * value env
  | App_clos1 of expr * expr * value env
  | App_clos2 of expr * value * value env
  | App_clos3 of value * value
  | Callcc_clos1 of expr * value env
  | Callcc_clos2 of value
  | Callcc_clos3 of value
  | Callcc_clos4 of value * value
  | Shift_clos1 of expr * value env
  | Shift_clos2 of value
  | Shift_clos3 of value
  | Shift_clos4 of value * value
  | Shift_clos5 of value * value * value
  | Reset_clos1 of expr * value env
  | Reset_clos2 of expr * value env * value
  | Apply_snd of value * value

let get_int = function
  | Int i -> i
  | _ -> failwith "type error: get_int"

let value_to_string = function
  | Int i -> string_of_int i
  | _ -> "<fun>"



let rec sem expr env = match expr with
  | Lit(i) -> Pure (Int i)
  | Var(x) -> Pure (get x env)
  | Lam(x, e) -> Pure (Lam_clos (e, x, env))
  | App(e1, e2) -> App_clos1 (e1, e2, env)
  | Callcc(e) -> Callcc_clos1 (e, env)
  | Shift(e) -> Shift_clos1 (e, env)
  | Reset(e) -> Reset_clos1 (e, env)

let rec ($) f v = match f with
  | Int _ -> failwith "type error: Int $"
  | Id -> v
  | Cps_id -> Pure v
  | Pure x -> v $ x
  | Succ -> Pure(Int(get_int v + 1))
  | Add1(x) -> Pure(Int(get_int v + x))
  | Mult1(x) -> Pure(Int(get_int v * x))
  | Add2 -> Pure(Add1(get_int v))
  | Mult2 -> Pure(Mult1(get_int v))
  | Lam_clos(e, x, env) -> sem e (extend x v env)
  | App_clos1(e1, e2, env) -> sem e1 env $ App_clos2(e2, v, env)
  | App_clos2(e2, k, env) -> sem e2 env $ App_clos3(v, k)
  | App_clos3(f, k) -> f $ v $ k
  | Callcc_clos1(e, env) -> sem e env $ Callcc_clos2(v)
  | Callcc_clos2(k) -> v $ Callcc_clos3(k) $ k
  | Callcc_clos3(k) -> Callcc_clos4(k, v)
  | Callcc_clos4(k, v) -> k $ v
  | Shift_clos1(e, env) -> sem e env $ Shift_clos2(v)
  | Shift_clos2(k) -> v $ Shift_clos3(k) $ Cps_id
  | Shift_clos3(k) -> Shift_clos4(k, v)
  | Shift_clos4(k, f) -> Shift_clos5(k, f, v)
  | Shift_clos5(k, f, k') -> k $ f $ Apply_snd(k', v)
  | Reset_clos1(e, env) -> Reset_clos2(e, env, v)
  | Reset_clos2(e, env, k) -> sem e env $ Cps_id $ Apply_snd(k, v)
  | Apply_snd(k, g) -> k $ v $ g

let init_env = empty
  |> extend "succ" Succ
  |> extend "zero" (Int 0)
  |> extend "+" Add2
  |> extend "*" Mult2

let run expr = sem expr init_env $ Cps_id $ Id

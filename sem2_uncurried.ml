open Common

type value =
  | Int of int
  | Fun of (value * (value * (value -> value) -> value) * (value -> value) -> value)

let app (v,a,k,g) = match v with
  | Int _ -> failwith "type error: ($)"
  | Fun f -> f (a, k, g)

let get_int = function
  | Int i -> i
  | Fun _ -> failwith "type error: get_int"

let value_to_string = function
  | Int i -> string_of_int i
  | Fun _ -> "<fun>"


let id x = x
let cps_id (x,g) = g x

let succ = Fun(fun (x,k,g) -> k (Int(get_int x + 1), g))

let int_op op =
  Fun(fun (x,k,g) -> k (Fun(fun (y,k',g') -> k' (Int(op (get_int x) (get_int y)), g')), g))


let rec sem (expr,env,k,g) = match expr with
  | Lit(i) -> k (Int(i), g)
  | Var(x) -> k (get x env, g)
  | Lam(x, e) -> k (Fun(fun (v,k',g') -> sem (e, extend x v env, k', g')), g)
  | App(e1,e2) -> sem (e1, env, (fun (f,g') -> sem (e2, env, (fun (a,g'') -> app (f, a, k, g'')), g')), g)
  | Callcc(e) -> sem (e, env, (fun (f,g') -> app (f, Fun(fun (v,_k', g'') -> k (v, g'')), k, g')), g)
  | Shift(e) ->
    sem (e, env, (fun (f,g') -> app (f, Fun(fun (v,k',g'') -> k (v, fun w -> k' (w, g''))), cps_id, g')), g)
  | Reset(e) -> sem (e, env, cps_id, fun r -> k (r, g))

let init_env = empty
  |> extend "succ" succ
  |> extend "zero" (Int 0)
  |> extend "+" (int_op ( + ))
  |> extend "*" (int_op ( * ))

let run expr = sem (expr, init_env, cps_id, id)

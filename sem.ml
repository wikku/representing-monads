open Common

type value =
  | Int of int
  | Fun of (value -> value)

let ($) = function
  | Int _ -> failwith "type error: ($)"
  | Fun(f) -> f

let get_int = function
  | Int i -> i
  | Fun _ -> failwith "type error: get_int"

let value_to_string = function
  | Int i -> string_of_int i
  | Fun _ -> "<fun>"


let id = Fun(fun x -> x)
let cps_id = Fun(fun x -> Fun(fun g -> g $ x))

let succ = Fun(fun x -> Fun(fun k -> k $ Int(get_int x + 1)))

let int_op op =
  Fun(fun x -> Fun(fun k  -> k  $
  Fun(fun y -> Fun(fun k' -> k' $
  Int(op (get_int x) (get_int y))))))


let rec sem expr env = Fun(match expr with
  | Lit(i) -> fun k -> k $ Int(i)
  | Var(x) -> fun k -> k $ get x env
  (*| Lam(x, e) -> fun k -> k $ Fun(fun v -> Fun(fun k' -> sem e (extend x v env) $ k'))*)
  | Lam(x, e) -> fun k -> k $ Fun(fun v -> sem e (extend x v env))
  | App(e1, e2) -> fun k ->
    sem e1 env $ Fun(fun f -> sem e2 env $ Fun(fun a -> f $ a $ k))
  | Callcc(e) -> fun k ->
    sem e env $ Fun(fun f -> f $ Fun(fun v -> Fun(fun _k' -> k $ v)) $ k)
  | Shift(e) -> fun k ->
    sem e env $ Fun(fun f ->
    f $ Fun(fun v -> Fun(fun k' -> Fun(fun g'' ->
        k $ v $ Fun(fun w ->
        k' $ w $ g''))))
      $ cps_id)
  | Reset(e) -> fun k ->
    Fun(fun g -> sem e env $ cps_id $ Fun(fun r -> k $ r $ g))
  )

let init_env = empty
  |> extend "succ" succ
  |> extend "zero" (Int 0)
  |> extend "+" (int_op ( + ))
  |> extend "*" (int_op ( * ))

let run expr = sem expr init_env $ cps_id $ id
